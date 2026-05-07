
import java.io.File
import sttp.client3.*
import scala.util.control.NonFatal
import java.nio.file.{Files, Paths}
import ujson._
import java.awt.image.BufferedImage
import java.awt.{Graphics2D, RenderingHints}
import javax.imageio.ImageIO
import java.io.ByteArrayOutputStream
import scala.collection.immutable.Seq
import org.jaudiotagger.audio.AudioFileIO
import org.jaudiotagger.tag.FieldKey
import org.jaudiotagger.tag.images.Artwork
import org.jaudiotagger.tag.images.ArtworkFactory


/**
 * Scala program to download album covers from MusicBrainz and embed them into MP3 files.
 * All file names must be in the format: "Artist - Album - Title.mp3" (the '-' is the separator)
 *
 * This program does the following:
 *    - replaces some chars/substrings in the file names from a defined dir (the MP3 source dir);
 *    - searches for a release-group MBID using the album name and artist name;
 *    - fetches all releases for the release-group, filters for official releases, and sorts them by date;
 *    - downloads the front cover image for the most recent release;
 *    - resizes the image to a fixed defined size (typical 500x500 pixels);
 *    - embeds the resized image into the MP3 file's ID3 tag;
 *    - saves the modified MP3 file with the embedded cover image;
 *
 * Requirements:
 *    - MusicBrainz access (musicbrainz.org);
 *    - CoverArtArchive access (coverartarchive.org);
 *
 *
 * Using:
 *    - com.softwaremill.sttp.client3:
 *        . sttp client is an open-source library which provides a clean, programmer-friendly API to describe HTTP
 *          requests and how to handle responses.
 *
 *    - org.jaudiotagger:
 *        . provides a Java library for editing tag information in audio files.
 *
 *    - com.lihaoyi:
 *        . provides all the core building blocks a typical software engineer needs day to day:
 *            . HTTP clients and servers;
 *            . JSON/binary data serialization;
 *            . filesystem operations;
 *            . CLI argument parsing;
 *            . build tooling;
 *            . etc.
 *
 */
object Main extends App {

  private val imgCoverW = 500
  private val imgCoverH = 500

  // the target dir
  private val albumCoverTempImgName = "cover.jpg"

  private val sourceDir = "C:\\Temp\\rvale\\Private\\_Music\\4tag"
  // the target dir
  private val targetDir = "4xinal"
  // the original file name separator (separates Artist - Album - Title)
  private val fileNameSeparator = "-"
  // the value to replace in the original file dir
  private val dirToReplace = "4tag"
  // the string to replace ALL occurrences from LIST in the original file name
  private val fileNameReplace = ""
  // list of chars/strings to replace in the original file name
  private val listRexExp = List(
    " - YouTube ", " - YouTube", "- YouTube ", "- YouTube",
    " -YouTube ", "-YouTube ", " -YouTube", "-YouTube",
    "YouTube", " YouTube", "YouTube ", " YouTube ", "YouTub",
    //" - You ", " - You", "- You", "- You",
    " (Official Video) ", " (Official Video)", "(Official Video) ", "(Official Video)",
    "[OFFICIAL VIDEO]",
    "(official music video)")

  private val backend = HttpURLConnectionBackend()

  private var listFilesWithNoCover = List[String]()



  // ---------------------------------------------------------
  // Search for release-group MBID using album name + artist
  // ---------------------------------------------------------
  private def searchReleaseGroupMBID(artist: String, album: String): Option[String] = {
    // For instance, for artist="U2" and album="War", the search URL is:
    //    https://musicbrainz.org/ws/2/release-group/?query=artist:U2 AND releasegroup:War&fmt=json&limit=1"
    val query = s"artist:$artist AND releasegroup:$album"
    val url = uri"https://musicbrainz.org/ws/2/release-group/?query=$query&fmt=json&limit=1"

    println(s"Search URL: $url")

    val response = basicRequest.get(url).send(backend)

    response.body.toOption.flatMap { json =>
      val parsed: ujson.Value = ujson.read(json)
      val groups = parsed("release-groups").arr

      groups.headOption.map(g => g("id").str)
    }
  }


  // ---------------------------------------------------------
  // 2. Fetch all releases for a release-group, filter official, sort by date
  // ---------------------------------------------------------
  private def lookupReleasesForGroup(groupMBID: String): Seq[(String, String)] = {
    val url = uri"https://musicbrainz.org/ws/2/release-group/$groupMBID?inc=releases&fmt=json"
    println(s"Search for releases for group URL: $url")

    val getBasicReq = basicRequest.get(url)
    println(s"GetBasicRequest: ${getBasicReq}")

    val response = getBasicReq.send(backend)
    println(s"Response: ${response}")

    response.body.toOption.map { json =>
      val parsed: ujson.Value = ujson.read(json)
      val releases = parsed("releases").arr

      releases
        .filter(r => r.obj.get("status") match {
          case Some(ujson.Str(value)) => value == "Official"
          case _ => false
        })
        .sortBy(r => r.obj.get("date") match {
          case Some(ujson.Str(value)) => value
          case _ => "0000-00-00"
        }).reverse  // Try most recent releases first
        .flatMap { r =>
          r.obj.get("id") match {
            case Some(ujson.Str(id)) =>
              Some((id, r.obj.get("date") match {
                case Some(ujson.Str(value)) => value
                case _ => "Unknown"
              }))
            case _ => None
          }
        }
        .toList
      //.toSeq
    }.getOrElse(Seq.empty[(String, String)])
  }


  // ---------------------------------------------------------
  // 3. Try each release until we find a cover
  // ---------------------------------------------------------
  private def downloadFirstAvailableCover(releases: Seq[(String, String)], filename: String = "cover.jpg"): Boolean = {
    for ((mbid, date) <- releases) {
      val url = uri"https://coverartarchive.org/release/$mbid/front"
      println(s"Trying release MBID: $mbid (Date: $date) url: $url")

      val response = basicRequest.get(url).response(asByteArray).send(backend)
      println(s"Response: ${response}")

      response.body match {
        case Right(bytes) =>
          Files.write(Paths.get(filename), bytes)
          println(s"Saved cover from release $mbid to $filename")
          return true

        case Left(_) =>
          println(s"\tWARNING: no cover art for release $mbid")
      }
    }
    false
  }


  // ---------------------------------------------------------
  // 4. High-level helper: artist + album -> cover.jpg
  // ---------------------------------------------------------
  private def fetchAlbumCover(artist: String, album: String, fileName: String): Unit = {
    try {
      println(s"Searching release-group for: $artist - $album")

      val groupMBID = searchReleaseGroupMBID(artist, album)

      if groupMBID.isEmpty then {
        println("\tWARNING: no release-group found")
        return
      }


      println(s"Release-group MBID = ${groupMBID.get}")

      val releases = lookupReleasesForGroup(groupMBID.get)

      if releases.isEmpty then {
        println("\tWARNING: no releases found for this release-group")
        return
      }


      println(s"Found ${releases.size} releases")

      if !downloadFirstAvailableCover(releases, fileName) then
        println("\tWARNING: no cover art found for any release")

    } catch {
      case NonFatal(e) => println(s"ERROR: ${e.getMessage}")
    }
  }


  //
  // From the given directory, recursively obtain and return ALL files.
  private def recursiveListFiles(fileDir: File): Array[File] = {
    println("recursiveListFiles(" + fileDir + ")...")

    val listDirFiles = fileDir.listFiles
    if (listDirFiles == null) return Array.empty[File]

    listDirFiles ++ listDirFiles.filter(_.isDirectory).flatMap(recursiveListFiles)
  }


  //
  // Will try to replace ALL listRexExp strings found in the file name by the str2Replace.
  private def getFileNewName(file: File, listRexExp: List[String], str2Replace: String): String = {
    var newAbsFileName = file.getAbsolutePath

    listRexExp.filter(s => newAbsFileName.indexOf(s) > -1).foreach(s => {
      newAbsFileName = newAbsFileName.replace(s, "")
    })

    println("Get file new name: " + newAbsFileName + ".")

    return newAbsFileName
  }


  //
  // Will try to obtain the ID3v2 MP3 album cover from the title, album and artist info.
  // It searches for album covers from a list of URLs.
  private def setMp3AlbumCover(file: File, artist: String, album: String, title: String): Unit = {
    println(s"Fetching album cover for artist($artist) album($album) title($title)...")

    var coverFileName = artist + "-" + album + ".jpg"

    fetchAlbumCover(artist, album, coverFileName)

    val coverFile = new File(coverFileName)

    println(s"Setting ID3v2Tag...")

    try {
      val audioFile = AudioFileIO.read(file)
      val tag = audioFile.getTagOrCreateAndSetDefault

      tag.setField(FieldKey.ARTIST, artist)
      tag.setField(FieldKey.ALBUM, album)
      tag.setField(FieldKey.TITLE, title)

      if (coverFile.exists) {
        val artwork: Artwork = ArtworkFactory.createArtworkFromFile(coverFile)
        tag.addField(artwork)
        println("Album cover set successfully!")

      } else {
        listFilesWithNoCover = listFilesWithNoCover :+ (artist + " - " + album)
        println("\tWARNING: no cover found.")
      }

      audioFile.commit()

      if (coverFile.exists) {
        coverFile.delete();
      }

    } catch {
      case e: Exception => println(s"Failed to set album cover: ${e.getMessage}")
    }
  }


  // Resize image to specified dimensions
  private def resizeImage(imageFile: File, width: Int, height: Int): Array[Byte] = {
    val originalImage = ImageIO.read(imageFile)
    val resizedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    val g2d: Graphics2D = resizedImage.createGraphics()
    g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    g2d.drawImage(originalImage, 0, 0, width, height, null)
    g2d.dispose()

    val baos = new ByteArrayOutputStream()
    ImageIO.write(resizedImage, "jpg", baos)
    baos.toByteArray
  }


  //
  // For the given file, tries to extrapolate its song name, album and artist from the file's name.
  // For instance, Pink Floyd - Meddle - Echoes.mp3 will be artist="Pink Floyd", album="Meddle" and song="Echoes".
  private def addMp3Tags(
                          file: File,
                          strSeparator: String,
                          listRexExp: List[String],
                          str2Replace: String,
                          strDirToReplace: String,
                          strDirNew: String): Unit = {

    println("\n");

    val fileNew = new File(getFileNewName(file, listRexExp, str2Replace).replace(strDirToReplace, strDirNew))

    if (fileNew.exists) {
      println("\tWARNING: file " + fileNew + " already exists. Continue anyway...")
    }


    // something like: artist - album - title.mp3
    var strFileName = fileNew.getName

    val artistAlbumSeparatorIdx = strFileName.indexOf(strSeparator);
    val strArtist = strFileName.substring(0, artistAlbumSeparatorIdx - 1)
    // something like: album - title.mp3
    strFileName = strFileName.substring(artistAlbumSeparatorIdx + 2);

    val albumTitleSeparatorIdx = strFileName.indexOf(strSeparator);
    val strAlbum = strFileName.substring(0, albumTitleSeparatorIdx - 1)

    // something like: title.mp3
    strFileName = strFileName.substring(albumTitleSeparatorIdx + 2);
    val strTitle = strFileName.substring(0, strFileName.indexOf("."))


    fileNew.getParentFile.mkdirs
    
    // Copy original file to new location if different
    if (!fileNew.equals(file)) {
      Files.copy(file.toPath, fileNew.toPath, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    }

    // Set album cover using JAudioTagger
    setMp3AlbumCover(fileNew, strArtist, strAlbum, strTitle)

  }


  println("\n\n\nMP3 tagging...\n\n")


  // obtain all files from the given dir
  private val listFiles = recursiveListFiles(new File(sourceDir))

  listFiles.filter(_.isFile).foreach {
    addMp3Tags(
      _,                  // the file itself
      fileNameSeparator,  // ... ;-)
      listRexExp,         // list of strings to be replaced, by fileNameReplace, in the original file name
      fileNameReplace,    // string to replace in file name
      dirToReplace,       // 4tag
      targetDir           // 4xinal
    );
  }

  println("\nMissing cover:")
  listFilesWithNoCover.foreach(x => println("\t" + x))

  println("\n\n...MP3 tagging.")

}


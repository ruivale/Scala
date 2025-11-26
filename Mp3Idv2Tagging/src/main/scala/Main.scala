
import java.io.File
import com.mpatric.mp3agic.Mp3File
import com.mpatric.mp3agic.ID3v1
import com.mpatric.mp3agic.ID3v1Tag
import com.mpatric.mp3agic.ID3v2
import com.mpatric.mp3agic.ID3v22Tag
import sttp.client3.*
import scala.util.control.NonFatal
import java.nio.file.{Files, Paths}
import scala.sys.exit
import ujson._
import java.awt.image.BufferedImage
import java.awt.{Graphics2D, RenderingHints}
import javax.imageio.ImageIO
import java.io.ByteArrayOutputStream
import scala.collection.immutable.Seq


/**
 * mp3agic:
 * https://github.com/mpatric/mp3agic
 *
 * Getting ID3v2 album artwork:
 * Mp3File mp3file = new Mp3File("SomeMp3File.mp3");
 *
 * if (mp3file.hasId3v2Tag()) {
 * ID3v2 id3v2Tag = mp3file.getId3v2Tag();
 * byte[] imageData = id3v2Tag.getAlbumImage();
 *
 * if (imageData != null) {
 * String mimeType = id3v2Tag.getAlbumImageMimeType();
 * // Write image to file - can determine appropriate file extension from the mime type
 * RandomAccessFile file = new RandomAccessFile("album-artwork", "rw");
 * file.write(imageData);
 * file.close();
 * }
 * }
 *
 *
 */
object Main extends App {
  private val imgCoverW = 500;
  private val imgCoverH = 500;
  private val backend = HttpURLConnectionBackend()

  // ---------------------------------------------------------
  // 1. Search for release-group MBID using album name + artist
  // ---------------------------------------------------------
  private def searchReleaseGroupMBID(artist: String, album: String): Option[String] = {
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
    val response = basicRequest.get(url).send(backend)

    response.body.toOption.map { json =>
      val parsed: ujson.Value = ujson.read(json)
      val releases = parsed("releases").arr

      releases
        .filter(r => r.obj.get("status").exists(_.str == "Official"))
        .sortBy(r => r.obj.get("date").map(_.str).getOrElse("9999-99-99"))
        .map(r => (r("id").str, r.obj.get("date").map(_.str).getOrElse("Unknown")))
        .toList
      //.toSeq
    }.getOrElse(Seq.empty[(String, String)])
  }

  // ---------------------------------------------------------
  // 3. Try each release until we find a cover
  // ---------------------------------------------------------
  private def downloadFirstAvailableCover(releases: Seq[(String, String)], filename: String = "cover.jpg"): Boolean = {
    for ((mbid, date) <- releases) {
      println(s"Trying release MBID: $mbid (Date: $date)")
      val url = uri"https://coverartarchive.org/release/$mbid/front"
      val response = basicRequest.get(url).response(asByteArray).send(backend)

      response.body match {
        case Right(bytes) =>
          Files.write(Paths.get(filename), bytes)
          println(s"Saved cover from release $mbid to $filename")
          return true
        case Left(_) =>
          println(s"WARNING: no cover art for release $mbid")
      }
    }
    false
  }

  // ---------------------------------------------------------
  // 4. High-level helper: artist + album -> cover.jpg
  // ---------------------------------------------------------
  private def fetchAlbumCover(artist: String, album: String): Unit = {
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

      if !downloadFirstAvailableCover(releases, artist + "-" + album + ".jpg") then
        println("\tWARNING: no cover art found for any release")

    } catch {
      case NonFatal(e) => println(s"ERROR: ${e.getMessage}")
    }
  }





  //
  // From the given directory, recursively obtain and return ALL files.
  //
  private def recursiveListFiles(fileDir: File): Array[File] = {
    println("recursiveListFiles(" + fileDir + ")...")

    val listDirFiles = fileDir.listFiles
    if (listDirFiles == null) return Array.empty[File]

    listDirFiles ++ listDirFiles.filter(_.isDirectory).flatMap(recursiveListFiles)
  }


  /** *
   * //
   * // Will try to replace ALL listRexExp strings found in the file name by the str2Replace.
   * //
   * def renameFile(file: File, listRexExp: List[String], str2Replace: String) = {
   * var newAbsFileName = file.getAbsolutePath
   *
   * listRexExp.filter(s => newAbsFileName.indexOf(s) > -1).foreach( s => {
   * newAbsFileName = newAbsFileName.replace(s, "")
   * })
   *
   * println("\t\tRename file name: " + newAbsFileName+".")
   *
   * file.renameTo(new File(newAbsFileName))
   * }
   */


  //
  // Will try to replace ALL listRexExp strings found in the file name by the str2Replace.
  //
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
  //
  private def setMp3AlbumCover(id3v2Tag: ID3v2): Unit = {
    if (id3v2Tag != null) {
      val title = id3v2Tag.getTitle
      val album = id3v2Tag.getAlbum
      val artist = id3v2Tag.getArtist

      println(s"Fetching album cover for artist($artist) album($album) title($title)...")
      fetchAlbumCover(artist, album)

      //val coverFile = new File("cover.jpg")
      val coverFile = new File(artist + "-" + album + ".jpg")

      if (coverFile.exists) {
        println(s"Setting ID3v2Tag album image...")
        try {
          val resizedImageBytes = resizeImage(coverFile, imgCoverW, imgCoverH)
          id3v2Tag.setAlbumImage(resizedImageBytes, "image/jpeg")
        } catch {
          case e: Exception => println(s"Failed to resize/set album cover: ${e.getMessage}")
        }
      } else {
        println("\tWARNING: no cover found.")
      }
    } else {
      println("\tWARNING: no ID3v2Tag found.")
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
  // For the given file, tries to extrapolate its song name, artist from the file's name.
  // For instance, Pink Floyd - Echoes.mp3 will be artist="Pink Floyd" and song="Echoes".
  //
  private def addMp3Tags(
                          file: File,
                          strSeparator: String,
                          listRexExp: List[String],
                          str2Replace: String,
                          strDirToReplace: String,
                          strDirNew: String): Unit = {

    println("");


    val fileNew = new File(getFileNewName(file, listRexExp, str2Replace).replace(strDirToReplace, strDirNew))

    if (fileNew.exists) {
      println("\tWARNING: file " + fileNew + " already exists. Continue anyway...")
    }

    fileNew.getParentFile.mkdirs;
    fileNew.createNewFile

    val mp3file = new Mp3File(file)
    var id3v1Tag: ID3v1 = null

    if (mp3file.hasId3v1Tag) {
      id3v1Tag = mp3file.getId3v1Tag

    } else {
      // mp3 does not have an ID3v1 tag, let's create one..
      id3v1Tag = new ID3v1Tag()
      mp3file.setId3v1Tag(id3v1Tag)
    }

    // something like: artist - album - title.mp3
    var strFileName = fileNew.getName
    id3v1Tag.setComment(strFileName)

    val artistAlbumSeparatorIdx = strFileName.indexOf(strSeparator);
    val strArtist = strFileName.substring(0, artistAlbumSeparatorIdx - 1)
    // something like: album - title.mp3
    strFileName = strFileName.substring(artistAlbumSeparatorIdx + 2);

    val albumTitleSeparatorIdx = strFileName.indexOf(strSeparator);
    val strAlbum = strFileName.substring(0, albumTitleSeparatorIdx - 1)

    // something like: title.mp3
    strFileName = strFileName.substring(albumTitleSeparatorIdx + 2);
    val strTitle = strFileName.substring(0, strFileName.indexOf("."))



    id3v1Tag.setArtist(strArtist)
    id3v1Tag.setAlbum(strAlbum)
    id3v1Tag.setTitle(strTitle)
    //    id3v1Tag.setTrack("5")
    //    id3v1Tag.setYear("2001")
    //    id3v1Tag.setGenre(12)


    // ID v2
    var id3v2Tag: ID3v2 = null

    if (mp3file.hasId3v2Tag) {
      println("Detected IDv2 (artist: \"" +
        strArtist + "\" album: \"" +
        strAlbum + "\" title: \"" +
        strTitle + "\").")
      id3v2Tag = mp3file.getId3v2Tag

    } else {
      println("WARNING: no IDv2 (artist: \"" +
        strArtist + "\" album: \"" +
        strAlbum + "\" title: \"" +
        strTitle + "\"). Setting a IDv2Tag...")
      // mp3 does not have an ID3v1 tag, let's create one..
      id3v2Tag = new ID3v22Tag()
      mp3file.setId3v2Tag(id3v2Tag)
    }

    id3v2Tag.setArtist(strArtist)
    id3v2Tag.setAlbum(strAlbum)
    id3v2Tag.setTitle(strTitle)


    if (id3v2Tag != null) {
      //      println("\t\t\tDetected (artist: \"" +
      //        strArtist + "\" album: \"" +
      //        strAlbum + "\" title: \"" +
      //        strTitle + "\").")

      setMp3AlbumCover(id3v2Tag)

    } else {
      println("\t\tERROR: no image was set cause IDv2 is null. (artist: \"" +
        strArtist + "\" album: \"" +
        strAlbum + "\" title: \"" +
        strTitle + "\").")
    }


    mp3file.save(fileNew.getAbsolutePath);


    //
    //	  println("\tLength of this mp3 is: " + mp3file.getLengthInSeconds + " seconds")
    //    println("\tBitrate: " + mp3file.getBitrate + " kbps Vbr? " + mp3file.isVbr)
    //    println("\tSample rate: " + mp3file.getSampleRate + " Hz")
    //    println("\tHas ID3v1 tag?: " + mp3file.hasId3v1Tag)
    //    println("\tHas ID3v2 tag?: " + mp3file.hasId3v2Tag)
    //    println("\tHas custom tag?: " + mp3file.hasCustomTag)
  }


  //  // Change these to any MP3 metadata values
  //  val artist = "rem"
  //  val album = "out of time"
  //  //val title = "low"
  //
  //  println("Fetching album cover...")
  //
  //  fetchAlbumCover(artist, album)
  //
  //  println("...fetching album cover.")
  //
  //  exit;


  println("\n\n\nMP3 tagging...\n\n")


  // obtain all files from the given dir
  private val listFiles = recursiveListFiles(new File("C:\\Temp\\rvale\\Private\\_Music\\4tag"))

  listFiles.filter(_.isFile).foreach {
    addMp3Tags(
      _,
      "-",
      List(
        " - YouTube ", " - YouTube", "- YouTube ", "- YouTube",
        " -YouTube ", "-YouTube ", " -YouTube", "-YouTube",
        "YouTube", " YouTube", "YouTube ", " YouTube ", "YouTub",
        //" - You ", " - You", "- You", "- You",
        " (Official Video) ", " (Official Video)", "(Official Video) ", "(Official Video)",
        "[OFFICIAL VIDEO]",
        "(official music video)"),
      "",
      "4tag",
      "4xinal");
  }

  println("\n\n...MP3 tagging.")

}
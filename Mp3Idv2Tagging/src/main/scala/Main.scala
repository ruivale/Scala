import java.io.File
import sttp.client3.*
import sttp.model.Uri
import scala.util.control.NonFatal
import java.nio.file.{Files, Paths}
import java.net.URLEncoder
import java.util.Base64
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

/** Scala program to download album covers from MusicBrainz and embed them into
  * MP3 files. All file names must be in the format: "Artist - Album -
  * Title.mp3" (the '-' is the separator)
  *
  * This program does the following:
  *   - replaces some chars/substrings in the file names from a defined dir (the
  *     MP3 source dir);
  *   - searches for a release-group MBID using the album name and artist name;
  *   - fetches all releases for the release-group, filters for official
  *     releases, and sorts them by date;
  *   - downloads the front cover image for the most recent release;
  *   - resizes the image to a fixed defined size (typical 500x500 pixels);
  *   - embeds the resized image into the MP3 file's ID3 tag;
  *   - saves the modified MP3 file with the embedded cover image;
  *
  * Requirements:
  *   - MusicBrainz access (musicbrainz.org);
  *   - CoverArtArchive access (coverartarchive.org);
  *   - Itunes (itunes.apple.com);
  *   - Discogs (discogs.com);
  *   - Lastfm (Last.fm);
  *   - Spotify (spotify.com);
  *   - Deezer (deezer.com)
  *
  * Using:
  *   - com.softwaremill.sttp.client3:
  *     - sttp client is an open-source library which provides a clean,
  *       programmer-friendly API to describe HTTP requests and how to handle
  *       responses.
  *   - org.jaudiotagger:
  *     - provides a Java library for editing tag information in audio files.
  *   - com.lihaoyi:
  *     - provides all the core building blocks a typical software engineer
  *       needs day to day:
  *       - HTTP clients and servers;
  *       - JSON/binary data serialization;
  *       - filesystem operations;
  *       - CLI argument parsing;
  *       - build tooling;
  *       - etc.
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
  private val fileNameSeparator = " - "
  // the value to replace in the original file dir
  private val dirToReplace = "4tag"
  // the string to replace ALL occurrences from LIST in the original file name
  private val fileNameReplace = ""
  // list of chars/strings to replace in the original file name
  private val listRexExp = List(
    " - YouTube ",
    " - YouTube",
    "- YouTube ",
    "- YouTube",
    " -YouTube ",
    "-YouTube ",
    " -YouTube",
    "-YouTube",
    "YouTube",
    " YouTube",
    "YouTube ",
    " YouTube ",
    "YouTub",
    // " - You ", " - You", "- You", "- You",
    " (Official Video) ",
    " (Official Video)",
    "(Official Video) ",
    "(Official Video)",
    "[OFFICIAL VIDEO]",
    "(official music video)"
  )

  private val backend = HttpURLConnectionBackend()

  private var listFilesWithNoCover = List[String]()

  // ---------------------------------------------------------
  // Search for release-group MBID using album name + artist
  // ---------------------------------------------------------
  private def searchReleaseGroupMBID(
      artist: String,
      album: String
  ): Option[String] = {
    // For instance, for artist="U2" and album="War", the search URL is:
    //    https://musicbrainz.org/ws/2/release-group/?query=artist:U2 AND releasegroup:War&fmt=json&limit=1"
    val query = s"artist:$artist AND releasegroup:$album"
    val url =
      uri"https://musicbrainz.org/ws/2/release-group/?query=$query&fmt=json&limit=1"

    println(s"Search URL: $url")

    val response = basicRequest.get(url).send(backend)

    response.body.toOption.flatMap { json =>
      val parsed: ujson.Value = ujson.read(json)
      val groups = parsed("release-groups").arr

      groups.headOption.map(g => g("id").str)
    }
  }

  // ---------------------------------------------------------
  // Fetch all releases for a release-group, filter official, sort by date
  // ---------------------------------------------------------
  private def lookupReleasesForGroup(
      groupMBID: String
  ): Seq[(String, String)] = {
    val url =
      uri"https://musicbrainz.org/ws/2/release-group/$groupMBID?inc=releases&fmt=json"
    println(s"Search for releases for group URL: $url")

    val getBasicReq = basicRequest.get(url)
    // println(s"GetBasicRequest: ${getBasicReq}")

    val response = getBasicReq.send(backend)
    // println(s"Response: ${response}")

    response.body.toOption
      .map { json =>
        val parsed: ujson.Value = ujson.read(json)
        val releases = parsed("releases").arr

        releases
          .filter(r =>
            r.obj.get("status") match {
              case Some(ujson.Str(value)) => value == "Official"
              case _                      => false
            }
          )
          .sortBy(r =>
            r.obj.get("date") match {
              case Some(ujson.Str(value)) => value
              case _                      => "0000-00-00"
            }
          )
          .reverse // Try most recent releases first
          .flatMap { r =>
            r.obj.get("id") match {
              case Some(ujson.Str(id)) =>
                Some(
                  (
                    id,
                    r.obj.get("date") match {
                      case Some(ujson.Str(value)) => value
                      case _                      => "Unknown"
                    }
                  )
                )
              case _ => None
            }
          }
          .toList
        // .toSeq
      }
      .getOrElse(Seq.empty[(String, String)])
  }

  // ---------------------------------------------------------
  // Try each release until we find a cover
  // ---------------------------------------------------------
  private def downloadFirstAvailableCover(
      releases: Seq[(String, String)],
      filename: String = "cover.jpg"
  ): Boolean = {
    releases.exists { case (mbid, date) =>
      val url = uri"https://coverartarchive.org/release/$mbid/front"
      println(s"Trying release MBID: $mbid (Date: $date) url: $url")
      downloadCoverByUrl(url, filename)
    }
  }

  private def downloadCoverByUrl(url: Uri, filename: String): Boolean = {
    val response = basicRequest.get(url).response(asByteArray).send(backend)
    response.body match {
      case Right(bytes) =>
        Files.write(Paths.get(filename), bytes)
        println(s"Saved cover from $url to $filename")
        true

      case Left(error) =>
        println(s"\tWARNING: failed to download cover from $url: $error")
        false
    }
  }

  // ---------------------------------------------------------
  // Fallbacks: try to search and download cover from iTunes, Discogs and Last.fm
  // ---------------------------------------------------------
  private def searchItunesCoverUrl(
      artist: String,
      album: String
  ): Option[String] = {
    try {
      val query = URLEncoder.encode(s"$artist $album", "UTF-8")
      val url =
        uri"https://itunes.apple.com/search?term=$query&entity=album&limit=5"
      println(s"Trying iTunes search for cover art: $url")

      val response = basicRequest.get(url).send(backend)
      response.body.toOption.flatMap { json =>
        val parsed: ujson.Value = ujson.read(json)
        parsed("results").arr.flatMap { r =>
          r.obj.get("artworkUrl100") match {
            case Some(ujson.Str(value)) => Some(value)
            case _                      => None
          }
        }.headOption
      }
    } catch {
      case NonFatal(e) =>
        println(s"\tWARNING: iTunes search failed: ${e.getMessage}")
        None
    }
  }

  // ---------------------------------------------------------
  // Try to download cover from iTunes search result
  // ---------------------------------------------------------
  private def downloadItunesCover(
      artist: String,
      album: String,
      filename: String
  ): Boolean = {

    try {
      searchItunesCoverUrl(artist, album) match {
        case Some(urlStr) =>
          val hiResUrl =
            urlStr.replaceAll("100x100bb", s"${imgCoverW}x${imgCoverH}bb")
          println(s"Trying iTunes artwork URL: $hiResUrl")
          downloadCoverByUrl(uri"$hiResUrl", filename)

        case None =>
          println("\tWARNING: no iTunes artwork URL found")
          false
      }
    } catch {
      case NonFatal(e) =>
        println(s"\tWARNING: iTunes cover download failed: ${e.getMessage}")
        false
    }
  }

  // ---------------------------------------------------------
  // Try to search and download cover from Discogs search result
  // ---------------------------------------------------------
  private def searchDiscogsCoverUrl(
      artist: String,
      album: String
  ): Option[String] = {
    try {
      val query = URLEncoder.encode(s"$artist $album", "UTF-8")
      val url =
        uri"https://api.discogs.com/database/search?q=$query&type=release&per_page=5"
      println(s"Trying Discogs search for cover art: $url")

      val response = basicRequest.get(url).send(backend)
      response.body.toOption.flatMap { json =>
        val parsed: ujson.Value = ujson.read(json)
        parsed("results").arr.flatMap { r =>
          r.obj.get("cover_image") match {
            case Some(ujson.Str(value)) => Some(value)
            case _                      => None
          }
        }.headOption
      }
    } catch {
      case NonFatal(e) =>
        println(s"\tWARNING: Discogs search failed: ${e.getMessage}")
        None
    }
  }

  // ---------------------------------------------------------
  // Try to download cover from Discogs search result
  // ---------------------------------------------------------
  private def downloadDiscogsCover(
      artist: String,
      album: String,
      filename: String
  ): Boolean = {
    try {
      searchDiscogsCoverUrl(artist, album) match {
        case Some(urlStr) =>
          println(s"Trying Discogs artwork URL: $urlStr")
          downloadCoverByUrl(uri"$urlStr", filename)

        case None =>
          println("\tWARNING: no Discogs artwork URL found")
          false
      }
    } catch {
      case NonFatal(e) =>
        println(s"\tWARNING: Discogs cover download failed: ${e.getMessage}")
        false
    }
  }

  // ---------------------------------------------------------
  // Try to search and download cover from Last.fm search result
  // ---------------------------------------------------------
  private def searchLastfmCoverUrl(
      artist: String,
      album: String
  ): Option[String] = {
    try {
      //
      // From Last.fm:
      //    Here are the details of your new API account.
      //      Application name 	MP3 Album Cover Downloader
      //      API key:  802a3b0b3d3bbcaa57d846a526a9629e
      //      Shared secret:  643b13e12847348a63cfe3a1aafbdec4
      //      Registered to:  ruifilipevale
      val apiKey =
        "802a3b0b3d3bbcaa57d846a526a9629e" // Replace with your Last.fm API key

      val queryArtist = URLEncoder.encode(artist, "UTF-8")
      val queryAlbum = URLEncoder.encode(album, "UTF-8")
      val url =
        uri"https://ws.audioscrobbler.com/2.0/?method=album.getinfo&artist=$queryArtist&album=$queryAlbum&api_key=$apiKey&format=json"
      println(s"Trying Last.fm search for cover art: $url")

      val response = basicRequest.get(url).send(backend)
      response.body.toOption.flatMap { json =>
        val parsed: ujson.Value = ujson.read(json)
        parsed.obj.get("album").flatMap { albumObj =>
          albumObj.obj.get("image").flatMap { images =>
            images.arr
              .find { img =>
                img.obj.get("size").exists(_.str == "extralarge")
              }
              .flatMap { img =>
                img.obj.get("#text").map(_.str).filter(_.nonEmpty)
              }
          }
        }
      }
    } catch {
      case NonFatal(e) =>
        println(s"\tWARNING: Last.fm search failed: ${e.getMessage}")
        None
    }
  }

  // ---------------------------------------------------------
  // Try to download cover from Last.fm search result
  // ---------------------------------------------------------
  private def downloadLastfmCover(
      artist: String,
      album: String,
      filename: String
  ): Boolean = {
    try {
      searchLastfmCoverUrl(artist, album) match {
        case Some(urlStr) =>
          println(s"Trying Last.fm artwork URL: $urlStr")
          downloadCoverByUrl(uri"$urlStr", filename)

        case None =>
          println("\tWARNING: no Last.fm artwork URL found")
          false
      }
    } catch {
      case NonFatal(e) =>
        println(s"\tWARNING: Last.fm cover download failed: ${e.getMessage}")
        false
    }
  }

  // ---------------------------------------------------------
  // Try to search and download cover from Spotify search result
  // ---------------------------------------------------------
  private def getSpotifyAccessToken(): Option[String] = {
    try {
      val clientId =
        "a92b00768d6946f0a1111ef5c82019e4" // Replace with your Spotify app client ID
      val clientSecret =
        "fc83bb8f22ad4724a3381c423ad397c4" // Replace with your Spotify app client secret
      val auth = Base64.getEncoder.encodeToString(
        s"$clientId:$clientSecret".getBytes("UTF-8")
      )
      val url = uri"https://accounts.spotify.com/api/token"
      val body = "grant_type=client_credentials"

      val response = basicRequest
        .post(url)
        .header("Authorization", s"Basic $auth")
        .header("Content-Type", "application/x-www-form-urlencoded")
        .body(body)
        .send(backend)

      response.body.toOption.flatMap { json =>
        val parsed: ujson.Value = ujson.read(json)
        parsed.obj.get("access_token").map(_.str)
      }
    } catch {
      case NonFatal(e) =>
        println(s"\tWARNING: Spotify token request failed: ${e.getMessage}")
        None
    }
  }

  // ---------------------------------------------------------
  // Try to search and download cover from Spotify search result
  // ---------------------------------------------------------
  private def searchSpotifyCoverUrl(
      artist: String,
      album: String
  ): Option[String] = {
    getSpotifyAccessToken().flatMap { token =>
      try {
        val query = URLEncoder.encode(s"artist:$artist album:$album", "UTF-8")
        val url =
          uri"https://api.spotify.com/v1/search?q=$query&type=album&limit=1"
        println(s"Trying Spotify search for cover art: $url")

        val response = basicRequest
          .get(url)
          .header("Authorization", s"Bearer $token")
          .send(backend)

        response.body.toOption.flatMap { json =>
          val parsed: ujson.Value = ujson.read(json)
          parsed.obj.get("albums").flatMap { albums =>
            albums.obj.get("items").flatMap { items =>
              items.arr.headOption.flatMap { item =>
                item.obj.get("images").flatMap { images =>
                  images.arr.headOption.flatMap { img =>
                    img.obj.get("url").map(_.str)
                  }
                }
              }
            }
          }
        }
      } catch {
        case NonFatal(e) =>
          println(s"\tWARNING: Spotify search failed: ${e.getMessage}")
          None
      }
    }
  }

  // ---------------------------------------------------------
  // Try to search and download cover from Spotify search result
  // ---------------------------------------------------------
  private def downloadSpotifyCover(
      artist: String,
      album: String,
      filename: String
  ): Boolean = {
    try {
      searchSpotifyCoverUrl(artist, album) match {
        case Some(urlStr) =>
          println(s"Trying Spotify artwork URL: $urlStr")
          downloadCoverByUrl(uri"$urlStr", filename)

        case None =>
          println("\tWARNING: no Spotify artwork URL found")
          false
      }
    } catch {
      case NonFatal(e) =>
        println(s"\tWARNING: Spotify cover download failed: ${e.getMessage}")
        false
    }
  }

  private def searchDeezerCoverUrl(
      artist: String,
      album: String
  ): Option[String] = {
    try {
      val query = URLEncoder.encode(s"$artist $album", "UTF-8")
      val url = uri"https://api.deezer.com/search/album?q=$query&limit=1"
      println(s"Trying Deezer search for cover art: $url")

      val response = basicRequest.get(url).send(backend)
      response.body.toOption.flatMap { json =>
        val parsed: ujson.Value = ujson.read(json)
        parsed.obj.get("data").flatMap { data =>
          data.arr.headOption.flatMap { album =>
            album.obj
              .get("cover_xl")
              .map(_.str)
              .orElse(
                album.obj.get("cover_big").map(_.str)
              )
          }
        }
      }
    } catch {
      case NonFatal(e) =>
        println(s"\tWARNING: Deezer search failed: ${e.getMessage}")
        None
    }
  }

  private def downloadDeezerCover(
      artist: String,
      album: String,
      filename: String
  ): Boolean = {
    try {
      searchDeezerCoverUrl(artist, album) match {
        case Some(urlStr) =>
          println(s"Trying Deezer artwork URL: $urlStr")
          downloadCoverByUrl(uri"$urlStr", filename)

        case None =>
          println("\tWARNING: no Deezer artwork URL found")
          false
      }
    } catch {
      case NonFatal(e) =>
        println(s"\tWARNING: Deezer cover download failed: ${e.getMessage}")
        false
    }
  }

  // ---------------------------------------------------------
  // High-level helper: artist + album -> cover.jpg
  // ---------------------------------------------------------
  private def fetchAlbumCover(
      artist: String,
      album: String,
      fileName: String
  ): Unit = {
    try {
      println(s"Searching release-group for: $artist - $album")

      val groupMBID = searchReleaseGroupMBID(artist, album)

      if groupMBID.isEmpty then {
        println("\tWARNING: no release-group found")
        if downloadItunesCover(artist, album, fileName) then return
        if downloadDiscogsCover(artist, album, fileName) then return
        if downloadLastfmCover(artist, album, fileName) then return
        if downloadSpotifyCover(artist, album, fileName) then return
        if downloadDeezerCover(artist, album, fileName) then return
        println("\tWARNING: no cover art found from fallbacks")
        return
      }

      println(s"Release-group MBID = ${groupMBID.get}")

      val releases = lookupReleasesForGroup(groupMBID.get)

      if releases.isEmpty then {
        println("\tWARNING: no releases found for this release-group")
        if downloadItunesCover(artist, album, fileName) then return
        if downloadDiscogsCover(artist, album, fileName) then return
        if downloadLastfmCover(artist, album, fileName) then return
        if downloadSpotifyCover(artist, album, fileName) then return
        if downloadDeezerCover(artist, album, fileName) then return
        println("\tWARNING: no cover art found from fallbacks")
        return
      }

      println(s"Found ${releases.size} releases")

      if !downloadFirstAvailableCover(releases, fileName) then
        if !downloadItunesCover(artist, album, fileName) then
          if !downloadDiscogsCover(artist, album, fileName) then
            if !downloadLastfmCover(artist, album, fileName) then
              if !downloadSpotifyCover(artist, album, fileName) then
                if !downloadDeezerCover(artist, album, fileName) then
                  println(
                    "\tWARNING: no cover art found for any release or fallback"
                  )

    } catch {
      case NonFatal(e) => println(s"ERROR: ${e.getMessage}")
    }
  }

  // ---------------------------------------------------------
  // From the given directory, recursively obtain and return ALL files.
  // Note: the method returns an empty array if the given fileDir is not a directory or an I/O error occurs.
  // ---------------------------------------------------------
  private def recursiveListFiles(fileDir: File): Array[File] = {
    println("recursiveListFiles(" + fileDir + ")...")

    val listDirFiles = fileDir.listFiles
    if (listDirFiles == null) return Array.empty[File]

    listDirFiles ++ listDirFiles
      .filter(_.isDirectory)
      .flatMap(recursiveListFiles)
  }

  //
  // Will try to replace ALL listRexExp strings found in the file name by the str2Replace.
  //
  private def getFileNewName(
      file: File,
      listRexExp: List[String],
      str2Replace: String
  ): String = {
    var newAbsFileName = file.getAbsolutePath

    listRexExp
      .filter(s => newAbsFileName.indexOf(s) > -1)
      .foreach(s => {
        newAbsFileName = newAbsFileName.replace(s, "")
      })

    println("Get file new name: " + newAbsFileName + ".")

    return newAbsFileName
  }

  //
  // Will try to obtain the ID3v2 MP3 album cover from the title, album and artist info.
  // It searches for album covers from a list of URLs.
  private def setMp3AlbumCover(
      file: File,
      artist: String,
      album: String,
      title: String
  ): Unit = {
    println(
      s"Fetching album cover for artist($artist) album($album) title($title)..."
    )

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
      case e: Exception =>
        println(s"Failed to set album cover: ${e.getMessage}")
    }
  }

  // Resize image to specified dimensions
  private def resizeImage(
      imageFile: File,
      width: Int,
      height: Int
  ): Array[Byte] = {
    val originalImage = ImageIO.read(imageFile)
    val resizedImage =
      new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    val g2d: Graphics2D = resizedImage.createGraphics()
    g2d.setRenderingHint(
      RenderingHints.KEY_INTERPOLATION,
      RenderingHints.VALUE_INTERPOLATION_BILINEAR
    )
    g2d.setRenderingHint(
      RenderingHints.KEY_RENDERING,
      RenderingHints.VALUE_RENDER_QUALITY
    )
    g2d.setRenderingHint(
      RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON
    )

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
      strDirNew: String
  ): Unit = {

    println("\n");

    val fileNew = new File(
      getFileNewName(file, listRexExp, str2Replace)
        .replace(strDirToReplace, strDirNew)
    )

    if (fileNew.exists) {
      println(
        "\tWARNING: file " + fileNew + " already exists. Continue anyway..."
      )
    }

    // something like: artist - album - title.mp3
    var strFileName = fileNew.getName

    val artistAlbumSeparatorIdx = strFileName.indexOf(strSeparator);
    val strArtist = strFileName.substring(0, artistAlbumSeparatorIdx)
    // something like: album - title.mp3
    strFileName =
      strFileName.substring(artistAlbumSeparatorIdx + strSeparator.length);

    val albumTitleSeparatorIdx = strFileName.indexOf(strSeparator);
    val strAlbum = strFileName.substring(0, albumTitleSeparatorIdx)

    // something like: title.mp3
    strFileName =
      strFileName.substring(albumTitleSeparatorIdx + strSeparator.length);
    val strTitle = strFileName.substring(0, strFileName.indexOf("."))

    fileNew.getParentFile.mkdirs

    // Copy original file to new location if different
    if (!fileNew.equals(file)) {
      Files.copy(
        file.toPath,
        fileNew.toPath,
        java.nio.file.StandardCopyOption.REPLACE_EXISTING
      )
    }

    // Set album cover using JAudioTagger
    setMp3AlbumCover(fileNew, strArtist, strAlbum, strTitle)

  }

  println("\n\n\nMP3 tagging...\n\n")

  // obtain all files from the given dir
  private val listFiles = recursiveListFiles(new File(sourceDir))

  listFiles.filter(_.isFile).foreach {
    addMp3Tags(
      _, // the file itself
      fileNameSeparator, // ... ;-)
      listRexExp, // list of strings to be replaced, by fileNameReplace, in the original file name
      fileNameReplace, // string to replace in file name
      dirToReplace, // 4tag
      targetDir // 4xinal
    );
  }

  println("\nMissing cover:")
  listFilesWithNoCover.foreach(x => println("\t" + x))

  println("\n\n...MP3 tagging.")

}

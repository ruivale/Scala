Here is a complete, ready-to-run Scala example using sttp client that:

Takes artist + title (the track title)

Queries MusicBrainz Search API to obtain the Recording MBID

Looks up the Release MBID associated with that recording

Downloads the front cover from the Cover Art Archive

Saves it as cover.jpg

? Works even if you don't know the album name
? Uses only artist + track title
? Uses simple sttp + Scala standard library



? Complete Scala Example (Scala 3 + sttp)
build.sbt
scalaVersion := "3.3.1"

libraryDependencies += "com.softwaremill.sttp.client3" %% "core" % "3.9.0"

?? Scala Code (copy/paste-ready)
import sttp.client3.*
import sttp.model.Uri
import scala.util.Try
import scala.util.control.NonFatal
import java.nio.file.{Files, Paths}

object FetchCoverFromArtistTitle:

  val backend = HttpURLConnectionBackend()

  // ---------------------------------------------------------
  // 1. Search for a Recording MBID using artist + track title
  // ---------------------------------------------------------
  def searchRecordingMBID(artist: String, title: String): Option[String] =
    val query = s"""recording:"$title" AND artist:"$artist""""
    val url = uri"https://musicbrainz.org/ws/2/recording/?query=$query&fmt=json"

    val response = basicRequest.get(url).send(backend)

    response.body.toOption.flatMap { json =>
      // Very light JSON extraction without a library:
      // Find:  "id": "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"
      val idRegex = """"id"\s*:\s*"([0-9a-fA-F-]{36})"""".r
      idRegex.findFirstMatchIn(json).map(_.group(1))
    }

  // ---------------------------------------------------------
  // 2. Look up which Releases contain this recording
  // ---------------------------------------------------------
  def lookupReleaseMBID(recordingMBID: String): Option[String] =
    val url = uri"https://musicbrainz.org/ws/2/recording/$recordingMBID?inc=releases&fmt=json"

    val response = basicRequest.get(url).send(backend)

    response.body.toOption.flatMap { json =>
      // Extract *first* release MBID
      val idRegex = """"id"\s*:\s*"([0-9a-fA-F-]{36})"""".r
      idRegex.findFirstMatchIn(json).map(_.group(1))
    }

  // ---------------------------------------------------------
  // 3. Download the front cover using the Release MBID
  // ---------------------------------------------------------
  def downloadCover(releaseMBID: String, filename: String = "cover.jpg"): Boolean =
    val url = uri"https://coverartarchive.org/release/$releaseMBID/front"

    val response = basicRequest
      .get(url)
      .response(asByteArray)
      .send(backend)

    response.body match
      case Right(bytes) =>
        Files.write(Paths.get(filename), bytes)
        println(s"Saved cover to $filename")
        true
      case Left(err) =>
        println(s"Failed to fetch cover art: $err")
        false

  // ---------------------------------------------------------
  // 4. High-level helper:
  //    artist + title -> cover.jpg
  // ---------------------------------------------------------
  def fetchCover(artist: String, title: String): Unit =
    try
      println(s"Searching MBID for: $artist - $title")

      val recordingMBID = searchRecordingMBID(artist, title)
      if recordingMBID.isEmpty then
        println("No recording match found")
        return

      println(s"Recording MBID = ${recordingMBID.get}")

      val releaseMBID = lookupReleaseMBID(recordingMBID.get)
      if releaseMBID.isEmpty then
        println("No release found for this recording")
        return

      println(s"Release MBID = ${releaseMBID.get}")

      downloadCover(releaseMBID.get)

    catch
      case NonFatal(e) => println(s"Error: ${e.getMessage}")

  // ---------------------------------------------------------
  // Run example
  // ---------------------------------------------------------
  @main def run() =
    // Change these to any MP3 metadata values
    val artist = "Daft Punk"
    val title  = "Digital Love"

    fetchCover(artist, title)




?? How it works
Step 1 - Search recording

We use MusicBrainz recording search:

recording:"Track Title" AND artist:"Artist Name"


This returns the recording MBID.

Step 2 - Find release MBID

A recording may appear in many releases.
We pick the first release for simplicity:

GET https://musicbrainz.org/ws/2/recording/{recordingMBID}?inc=releases&fmt=json

Step 3 - Fetch cover art

CAA has simple URLs:

https://coverartarchive.org/release/{RELEASE_MBID}/front


This returns a JPEG/PNG image.

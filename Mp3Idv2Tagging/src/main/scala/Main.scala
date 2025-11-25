
import java.io.File
import com.mpatric.mp3agic.Mp3File
import com.mpatric.mp3agic.ID3v1
import com.mpatric.mp3agic.ID3v1Tag
import com.mpatric.mp3agic.ID3v2


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

  //
  // From the given directory, recursively obtain and return ALL files.
  //
  private def recursiveListFiles(fileDir: File): Array[File] = {
    println("\trecursiveListFiles(" + fileDir + ")...\n\n")

    val listDirFiles = fileDir.listFiles

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

    println("\t\tGet file new name: " + newAbsFileName + ".")

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


      //id3v2Tag.setAlbumImage();
    }
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

    val fileNew = new File(getFileNewName(file, listRexExp, str2Replace).replace(strDirToReplace, strDirNew))

    if (fileNew.exists) {
      println("\t\tFile " + fileNew + " already exists. Continue anyway...")
    }

    fileNew.getParentFile.mkdirs;
    fileNew.createNewFile


    // something like: artist - title.mp3
    val strFileName = fileNew.getName
    val strArtist = strFileName.substring(0, strFileName.indexOf(strSeparator))
    val strTitle = strFileName.substring(strFileName.indexOf(strSeparator) + 1, strFileName.indexOf("."))

    val mp3file = new Mp3File(file)

    var id3v1Tag: ID3v1 = null

    if (mp3file.hasId3v1Tag) {
      id3v1Tag = mp3file.getId3v1Tag

    } else {
      // mp3 does not have an ID3v1 tag, let's create one..
      id3v1Tag = new ID3v1Tag()
      mp3file.setId3v1Tag(id3v1Tag)
    }

    id3v1Tag.setArtist(strArtist)
    id3v1Tag.setTitle(strTitle)
    id3v1Tag.setComment(strFileName)
    //    id3v1Tag.setTrack("5")
    //    id3v1Tag.setAlbum("The Album")
    //    id3v1Tag.setYear("2001")
    //    id3v1Tag.setGenre(12)


    // ID v2
    val id3v2Tag = mp3file.getId3v2Tag
    //byte[] imageData = id3v2Tag.getAlbumImage

    if (id3v2Tag != null) {
      setMp3AlbumCover(id3v2Tag)
      //id3v2Tag.setAlbumImage();

    } else {
      println("\t\t\tNo image was set cause IDv2 is null. (" + strFileName + " -> a: " + strArtist + " t: " + strTitle + ").")
    }


    mp3file.save(fileNew.getAbsolutePath);


    println("\t\t\t" + strFileName + " -> a: " + strArtist + " t: " + strTitle + ".\n")

    //
    //	  println("\tLength of this mp3 is: " + mp3file.getLengthInSeconds + " seconds")
    //    println("\tBitrate: " + mp3file.getBitrate + " kbps Vbr? " + mp3file.isVbr)
    //    println("\tSample rate: " + mp3file.getSampleRate + " Hz")
    //    println("\tHas ID3v1 tag?: " + mp3file.hasId3v1Tag)
    //    println("\tHas ID3v2 tag?: " + mp3file.hasId3v2Tag)
    //    println("\tHas custom tag?: " + mp3file.hasCustomTag)
  }


  println("\n\nMP3 tagging...\n\n")


  // obtain all files from the given dir
  private var listFiles = recursiveListFiles(new File("C:\\Temp\\rvale\\Private\\_Music\\4tag"))

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
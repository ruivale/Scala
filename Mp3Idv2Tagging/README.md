TODO:
  - try to create M3U files for each dir, and its subdirs, listing all mp3 files in these dirs;
  - try to create M3U files for all genres;


----------------------------------------------------------------------------------
Here is a complete, ready-to-run Scala example using sttp client that:
 - takes "artist + song" or "artist + title + song";
 - queries multiple search APIs to obtain the recording MBID;
 - looks up the release MBID associated with that recording
 - downloads the front cover from the first 


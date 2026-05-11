TODO:
  - if a MP3 already has a cover image, don't try to set one;
  - try to obtain the musik genre;
  - 


----------------------------------------------------------------------------------
Here is a complete, ready-to-run Scala example using sttp client that:
 - takes "artist + song" or "artist + title + song";
 - queries multiple search APIs to obtain the recording MBID;
 - looks up the release MBID associated with that recording
 - downloads the front cover from the first 


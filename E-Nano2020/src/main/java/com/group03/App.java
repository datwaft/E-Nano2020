package com.group03;

import java.io.IOException;
import java.io.InputStream;
import java.util.regex.Pattern;

import fi.iki.elonen.NanoHTTPD;
import fi.iki.elonen.NanoHTTPD.Response.Status;
  
public class App extends NanoHTTPD {
  Pattern isFile = Pattern.compile("^.*[^/]$");

  public App() throws IOException {
    super(8080);
    start(NanoHTTPD.SOCKET_READ_TIMEOUT, false);
    System.out.println("\nRunning! Point your browsers to http://localhost:8080/ \n");
  }

  public static void main(String[] args) {
    try {
      new App();
    } catch (IOException ioe) {
      System.err.println("Couldn't start server:\n" + ioe);
    }
  }

  @Override
  public Response serve(IHTTPSession session) {
    String uri = session.getUri();
    String file = (isFile.matcher(uri).matches() ? uri : uri + "index.html");
    InputStream fileStream = getClass().getClassLoader().getResourceAsStream(file.substring(1));
    if (fileStream != null) {
      return NanoHTTPD.newChunkedResponse(Status.OK, getMimeTypeForFile(file), fileStream);
    } else {
      return newFixedLengthResponse("<html><body><h1>Error 404: File '" + file + "' not found</h1></body></html>");
    }
  }
}

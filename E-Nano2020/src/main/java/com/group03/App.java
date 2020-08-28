package com.group03;

import java.io.IOException;
import java.io.InputStream;
import java.util.Map;
import java.util.regex.Pattern;

import com.diogonunes.jcolor.AnsiFormat;
import com.diogonunes.jcolor.Attribute;

import fi.iki.elonen.router.RouterNanoHTTPD;
import fi.iki.elonen.NanoHTTPD.Response.Status;
  
public class App extends RouterNanoHTTPD {
  private static Pattern isFile = Pattern.compile("^.*[^/]$");
  private static AnsiFormat fError = new AnsiFormat(Attribute.RED_TEXT(), Attribute.BOLD());
  private static AnsiFormat fWarning = new AnsiFormat(Attribute.YELLOW_TEXT(), Attribute.BOLD());
  private static AnsiFormat fSuccess = new AnsiFormat(Attribute.GREEN_TEXT());
  private static final Integer port = 8080;

  public App() throws IOException {
    super(port);
    this.addMappings();
    this.start(SOCKET_READ_TIMEOUT, false);
    System.out.println("\n" + fSuccess.format("The webserver is running on the port " + port + ".") + "\n" + "The url is http://localhost:" + port + "/" + "\n");
  }

  public static void main(String[] args) {
    try {
      new App();
    } catch (IOException ioe) {
      System.err.println("\n" + fError.format("Couldn't start server:" + ioe) + "\n");
    }
  }

  @Override
  public void addMappings() {
    this.addRoute("/(.)*", ResourceHandler.class);
  }

  public static class ResourceHandler extends StaticPageHandler {
    @Override
    public Response get(UriResource uriResource, Map<String, String> urlParams, IHTTPSession session) {
      String uri = session.getUri();
      String file = (isFile.matcher(uri).matches() ? uri : uri + "index.html");
      InputStream fileStream = getClass().getClassLoader().getResourceAsStream(file.substring(1));
      if (fileStream != null) {
        System.out.println(fSuccess.format("Succesful response to '" + file + "' petition [" + session.getMethod() + "]."));
        return newChunkedResponse(getStatus(), getMimeTypeForFile(file), fileStream);
      } else {
        System.err.println(fWarning.format("Resource not found: '" + file + "' [" + session.getMethod() + "]."));
        return newFixedLengthResponse(Response.Status.NOT_FOUND, MIME_PLAINTEXT, "The requested resource does not exist");
      }
    }
  }
}

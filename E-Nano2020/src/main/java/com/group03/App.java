package com.group03;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

import com.diogonunes.jcolor.AnsiFormat;
import com.diogonunes.jcolor.Attribute;

import org.json.JSONObject;

import fi.iki.elonen.NanoHTTPD.Response.IStatus;
import fi.iki.elonen.router.RouterNanoHTTPD;
  
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
    this.addRoute("/api/.*", CodeHandler.class);
    this.addRoute("/api", CodeHandler.class);

    this.addRoute("/(?!api).*", ResourceHandler.class);
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
        return newFixedLengthResponse(Response.Status.NOT_FOUND, MIME_PLAINTEXT, "The requested resource does not exist.");
      }
    }
  }

  public static class CodeHandler extends DefaultStreamHandler {
    @Override
    public String getMimeType() {
      return MIME_PLAINTEXT;
    }

    @Override
    public IStatus getStatus() {
      return Response.Status.OK;
    }

    @Override
    public InputStream getData() {
      return new ByteArrayInputStream("Your request was successful :D".getBytes());
    }
    @Override
    public Response post(UriResource uriResource, Map<String, String> urlParams, IHTTPSession session) {
      try {
        Map<String, String> response = new HashMap<>();
        session.parseBody(response);

        String postData = response.get("postData");
        
        JSONObject jsonData = new JSONObject(postData);
        String data = jsonData.getString("data");

        System.out.println(fSuccess.format("Succesful response to '" + session.getUri() + "' petition [" + session.getMethod() + "]."));
        System.out.println("The data is: '" + data + "'");
        return newFixedLengthResponse(Response.Status.OK, MIME_PLAINTEXT, "The requested has been successful.\nYour data is: '" + data + "'");
      } catch (IOException | ResponseException e) {
        System.err.println(fWarning.format("Invalid post petition for '" + session.getUri() + "' [" + session.getMethod() + "]."));
        return newFixedLengthResponse(Response.Status.NOT_FOUND, MIME_PLAINTEXT, "The requested resource does not exist.");
      }
    }
  }
}

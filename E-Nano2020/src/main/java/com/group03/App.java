package com.group03;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

import com.group03.utils.MiscUtils;
import com.group03.utils.OutUtils;

import org.json.JSONObject;

import fi.iki.elonen.NanoHTTPD.Response.IStatus;
import fi.iki.elonen.router.RouterNanoHTTPD;
  
public class App extends RouterNanoHTTPD {
  private static Pattern isFile = Pattern.compile("^.*[^/]$");
  private static final Integer port = 8088;

  public App() throws IOException {
    super(port);
    this.addMappings();
    this.start(SOCKET_READ_TIMEOUT, false);
    OutUtils.successFormat("%nThe webserver is running on the port %d.", port);
    OutUtils.normalFormat("The url is http://localhost:%d/%n", port);
  }

  public static void main(String[] args) {
    try {
      new App();
    } catch (IOException ioe) {
      OutUtils.errorFormat("%nCould not start server: %s%n", ioe.getLocalizedMessage());
    }
  }

  @Override
  public void addMappings() {
    this.addRoute("/api.*", CodeHandler.class);
    this.addRoute("/api", CodeHandler.class);
    this.addRoute("/info.*", InfoHandler.class);
    this.addRoute("/info", InfoHandler.class);

    this.addRoute("/(?!(api|info)).*", ResourceHandler.class);
  }

  public static class ResourceHandler extends StaticPageHandler {
    @Override
    public Response get(UriResource uriResource, Map<String, String> urlParams, IHTTPSession session) {
      String uri = session.getUri();
      String file = (isFile.matcher(uri).matches() ? uri : uri + "index.html");
      InputStream fileStream = getClass().getClassLoader().getResourceAsStream(file.substring(1));
      if (fileStream != null) {
        OutUtils.successFormatWithDatetime("Successful response to '%s' static file request [%s].%n", file, session.getMethod());
        return newChunkedResponse(getStatus(), getMimeTypeForFile(file), fileStream);
      } else {
        OutUtils.warningFormatWithDatetime("Static file not found: '%s' [%s].%n", file, session.getMethod());
        return newFixedLengthResponse(Response.Status.NOT_FOUND, MIME_PLAINTEXT, "The requested resource does not exist.");
      }
    }
  }

  public static class InfoHandler extends DefaultStreamHandler {
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
    public Response get(UriResource uriResource, Map<String, String> urlParams, IHTTPSession session) {
      try {

        InputStream infoStream = getClass().getClassLoader().getResourceAsStream("info/info.html");

        String info = MiscUtils.inputStreamToString(infoStream);

        OutUtils.successFormatWithDatetime("Successful response to '%s' request [%s].%n", session.getUri(), session.getMethod());
        return newFixedLengthResponse(Response.Status.OK, MIME_PLAINTEXT, info);
      } catch (Exception e) {
        OutUtils.warningFormatWithDatetime("Invalid request for '%s' [%s].%n", session.getUri(), session.getMethod());
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

        OutUtils.successFormatWithDatetime("Successful response to '%s' request [%s].", session.getUri(), session.getMethod());
        OutUtils.normalFormat("The data is: '%s'.%n", data);
        return newFixedLengthResponse(Response.Status.OK, MIME_PLAINTEXT, "The requested has been successful.\nYour data is: '" + data + "'");
      } catch (IOException | ResponseException e) {
        OutUtils.warningFormatWithDatetime("Invalid request for '%s' [%s].%n", session.getUri(), session.getMethod());
        return newFixedLengthResponse(Response.Status.NOT_FOUND, MIME_PLAINTEXT, "The requested resource does not exist.");
      }
    }
  }
}

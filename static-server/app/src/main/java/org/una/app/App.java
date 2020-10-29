package org.una.app;

import com.google.common.collect.ImmutableMap;
import fi.iki.elonen.NanoHTTPD;
import org.una.utilities.FileUtils;
import org.una.utilities.LogUtils;

import java.io.IOException;
import java.util.regex.Pattern;

import static org.una.utilities.LogUtils.Style;

public class App extends NanoHTTPD {
  public static Pattern isFile = Pattern.compile("^.*[^/]$");
  private static final ImmutableMap<String, String> properties = FileUtils.readProperties("server.properties");
  private static final Integer port;

  static {
    assert properties != null;
    port = Integer.valueOf(properties.get("port"));
  }

  public App() throws IOException {
    super(port);
    start(SOCKET_READ_TIMEOUT, false);
    LogUtils.format(Style.SUCCESS, "%nThe webserver is running on the port %d.", port);
    LogUtils.format(Style.NOTE, "The url is http://localhost:%d/%n", port);
  }

  public static void main(String... args) {
    try {
      new App();
    } catch (IOException ioe) {
      LogUtils.format(Style.ERROR, "%nCould not start server: %s%n", ioe.getLocalizedMessage());
    }
  }

  public Response serve(IHTTPSession session) {
    var uri = session.getUri();
    var filename = (isFile.matcher(uri).matches() ? uri : uri + "index.html").substring(1);
    var stream = FileUtils.readInputStream(filename);
    if (stream != null) {
      LogUtils.formatD(Style.SUCCESS, "Successful response to '%s' static file request [%s].%n", filename, session.getMethod());
      return newChunkedResponse(Response.Status.OK, getMimeTypeForFile(filename), stream);
    } else {
      LogUtils.formatD(Style.WARNING, "Static file not found: '%s' [%s].%n", filename, session.getMethod());
      return newFixedLengthResponse(Response.Status.NOT_FOUND, MIME_PLAINTEXT, "The requested resource does not exist.");
    }
  }
}

/**
 *  _____      _   _                  ____   ___ ____   ___
 * | ____|    | \ | | __ _ _ __   ___|___ \ / _ \___ \ / _ \
 * |  _| _____|  \| |/ _` | '_ \ / _ \ __) | | | |__) | | | |
 * | |__|_____| |\  | (_| | | | | (_) / __/| |_| / __/| |_| |
 * |_____|    |_| \_|\__,_|_| |_|\___/_____|\___/_____|\___/
 *
 * File: App.java
 * Description:
 *    Archivo inicial del router para E-Nano2020, se encarga de servidor los archivos estáticos.
 * Authors:
 * - David Alberto Guevara Sánchez
 *   402450355
 * - Joy Bonilla Fley
 *   402360421
 * - Jose Barrantes Araya
 *   207600954
 * - Natalia Solano Azofeifa
 *   117290958
 * - Luis David Villalobos Gonzalez
 *   117540697
 * Group: 03
 * Schedule: 10am
 * Date of modification: 2020-10-29
 */

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
  private static final Integer port = Integer.valueOf(properties.get("port"));

  public App() throws IOException {
    super(port);
    start(SOCKET_READ_TIMEOUT, false);
    LogUtils.format(Style.SUCCESS, "%nThe webserver is running on the port %s.", LogUtils.bold("%s", port));
    LogUtils.format(Style.NOTE, "The url is http://localhost:%s/%n", LogUtils.bold("%s", port));
  }

  public static void main(String... args) {
    try {
      new App();
    } catch (IOException ioe) {
      LogUtils.format(Style.ERROR, "%nCould not start server: %s%n", LogUtils.bold(ioe.getLocalizedMessage()));
    }
  }

  public Response serve(IHTTPSession session) {
    var uri = session.getUri();
    var filename = (isFile.matcher(uri).matches() ? uri : uri + "index.html").substring(1);
    var stream = FileUtils.readInputStream(filename);
    if (stream != null) {
      LogUtils.formatD(Style.SUCCESS, "Successful response to '%s' static file request [%s].%n", LogUtils.bold(filename), LogUtils.bold("%s", session.getMethod()));
      return newChunkedResponse(Response.Status.OK, getMimeTypeForFile(filename), stream);
    } else {
      LogUtils.formatD(Style.WARNING, "Static file not found: '%s' [%s].%n", LogUtils.bold(filename), LogUtils.bold("%s", session.getMethod()));
      return newFixedLengthResponse(Response.Status.NOT_FOUND, MIME_PLAINTEXT, "The requested resource does not exist.");
    }
  }
}

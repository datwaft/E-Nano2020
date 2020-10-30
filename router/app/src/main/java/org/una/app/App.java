/**
 *  _____      _   _                  ____   ___ ____   ___
 * | ____|    | \ | | __ _ _ __   ___|___ \ / _ \___ \ / _ \
 * |  _| _____|  \| |/ _` | '_ \ / _ \ __) | | | |__) | | | |
 * | |__|_____| |\  | (_| | | | | (_) / __/| |_| / __/| |_| |
 * |_____|    |_| \_|\__,_|_| |_|\___/_____|\___/_____|\___/
 *
 * File: App.java
 * Description:
 *    Archivo inicial del servidor para E-Nano2020, se encarga de procesar las peticiones del cliente.
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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import com.google.common.collect.ImmutableMap;

import org.json.JSONObject;
import org.slf4j.LoggerFactory;
import org.una.utilities.CompileUtils;
import org.una.utilities.DatabaseUtils;
import org.una.utilities.FileUtils;
import org.una.utilities.LogUtils;
import org.una.utilities.LogUtils.Style;
import org.una.utilities.RouterUtils;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.LoggerContext;
import fi.iki.elonen.NanoHTTPD.Response.IStatus;
import fi.iki.elonen.router.RouterNanoHTTPD;
  
public class App extends RouterNanoHTTPD {
  private static final ImmutableMap<String, String> properties = FileUtils.readProperties("server.properties");
  private static final Integer port = Integer.valueOf(properties.get("port"));

  public App() throws IOException {
    super(port);
    this.addMappings();
    this.start(SOCKET_READ_TIMEOUT, false);
    LogUtils.format(Style.SUCCESS, "%nThe router is running on the port %d.", port);
    LogUtils.format(Style.NOTE, "The url is http://localhost:%d/%n", port);
  }

  public static void main(String... args) {
    previous();
    try {
      new App();
    } catch (IOException ioe) {
      LogUtils.format(Style.ERROR, "%nCould not start server: %s%n", ioe.getLocalizedMessage());
    }
  }

  private final static void previous() {
    var loggerContext = (LoggerContext) LoggerFactory.getILoggerFactory();
    var rootLogger = loggerContext.getLogger("org.mongodb.driver");
    rootLogger.setLevel(Level.OFF);
  }

  @Override
  public void addMappings() {//añade rutas
    this.addRoute("/compile", CodeHandler.class);
    this.addRoute("/info", InfoHandler.class);
  }

  public static class InfoHandler extends DefaultStreamHandler {
    @Override
    public String getMimeType() {
      return "application/json";
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
        var info = DatabaseUtils.getInfo();

        LogUtils.formatD(Style.SUCCESS, "Successful response to '%s' request [%s].%n", session.getUri(), session.getMethod());

        var response = newFixedLengthResponse(Response.Status.OK, MIME_PLAINTEXT, info.toString());
        response = RouterUtils.allowCors(response, session);
        return response;
      } catch (Exception e) {
        LogUtils.formatD(Style.WARNING, "Invalid request for '%s' [%s].%n", session.getUri(), session.getMethod());
        var response = newFixedLengthResponse(Response.Status.NOT_FOUND, MIME_PLAINTEXT, "The requested resource does not exist.");
        response = RouterUtils.allowCors(response, session);
        return response;
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
        var post_data = RouterUtils.processData(session);
        var data = post_data.getString("source");

        var output = new HashMap<String, String>();
        output.put("messages", CompileUtils.compileString(data).toString());

        LogUtils.formatD(Style.SUCCESS, "Successful response to '%s' request [%s].", session.getUri(), session.getMethod());
        LogUtils.format(Style.NOTE, "The source code is:%n%s%n", data);

        var response = newFixedLengthResponse(Response.Status.OK, "application/json", new JSONObject(output).toString());
        response = RouterUtils.allowCors(response, session);
        return response;
      } catch (IOException | ResponseException e) {
        LogUtils.formatD(Style.WARNING, "Invalid request for '%s' [%s].%n", session.getUri(), session.getMethod());
        var response = newFixedLengthResponse(Response.Status.NOT_FOUND, MIME_PLAINTEXT, "The requested resource does not exist.");
        response = RouterUtils.allowCors(response, session);
        return response;
      }
    }
  }
}


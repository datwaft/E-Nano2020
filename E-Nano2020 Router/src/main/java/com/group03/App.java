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
 * Date of modification: 2020-09-25
 */

package com.group03;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import com.group03.utils.MiscUtils;
import com.group03.utils.OutUtils;
import com.group03.utils.RouterUtils;

import org.json.JSONObject;

import fi.iki.elonen.NanoHTTPD.Response.IStatus;
import fi.iki.elonen.router.RouterNanoHTTPD;
  
public class App extends RouterNanoHTTPD {
  private static final Integer port = 8099;

  public App() throws IOException {
    super(port);
    this.addMappings();
    this.start(SOCKET_READ_TIMEOUT, false);
    OutUtils.successFormat("%nThe router is running on the port %d.", port);
    OutUtils.normalFormat("The url is http://localhost:%d/%n", port);
  }

  public static void main(String... args) {
    try {
      new App();
    } catch (IOException ioe) {
      OutUtils.errorFormat("%nCould not start server: %s%n", ioe.getLocalizedMessage());
    }
  }

  @Override
  public void addMappings() {//añade rutas
    this.addRoute("/api", CodeHandler.class);
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
        var infoStream = getClass().getClassLoader().getResourceAsStream("info/info.json");
        var info = MiscUtils.inputStreamToString(infoStream);

        OutUtils.successFormatWithDatetime("Successful response to '%s' request [%s].%n", session.getUri(), session.getMethod());

        var response = newFixedLengthResponse(Response.Status.OK, MIME_PLAINTEXT, info);
        response = RouterUtils.allowCors(response, session);
        return response;
      } catch (Exception e) {
        OutUtils.warningFormatWithDatetime("Invalid request for '%s' [%s].%n", session.getUri(), session.getMethod());
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
        var session_body = new HashMap<String, String>();
        session.parseBody(session_body);

        var post_data = session_body.get("postData");
        var data = "";
        if (post_data != null) {
          var jsonData = new JSONObject(post_data);
          data = jsonData.getString("data");
        }

        OutUtils.successFormatWithDatetime("Successful response to '%s' request [%s].", session.getUri(), session.getMethod());
        OutUtils.normalFormat("The data is: '%s'.%n", data);

        var output = new HashMap<String, String>();
        output.put("output", String.format("The request was successful.%nYour data is: '%s'", data));

        var response = newFixedLengthResponse(Response.Status.OK, "application/json", new JSONObject(output).toString());
        response = RouterUtils.allowCors(response, session);
        return response;
      } catch (IOException | ResponseException e) {
        OutUtils.warningFormatWithDatetime("Invalid request for '%s' [%s].%n", session.getUri(), session.getMethod());
        var response = newFixedLengthResponse(Response.Status.NOT_FOUND, MIME_PLAINTEXT, "The requested resource does not exist.");
        response = RouterUtils.allowCors(response, session);
        return response;
      }
    }
  }
}

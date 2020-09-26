/**
 *  _____      _   _                  ____   ___ ____   ___
 * | ____|    | \ | | __ _ _ __   ___|___ \ / _ \___ \ / _ \
 * |  _| _____|  \| |/ _` | '_ \ / _ \ __) | | | |__) | | | |
 * | |__|_____| |\  | (_| | | | | (_) / __/| |_| / __/| |_| |
 * |_____|    |_| \_|\__,_|_| |_|\___/_____|\___/_____|\___/
 *
 * File: App.java
 * Description:
 *    Archivo inicial del servidor para E-Nano2020, se encarga de servir los archivos estáticos y procesesar las peticiones.
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
 * Date of modification: 2020-09-16
 */

package com.group03;

import java.io.IOException;
import java.io.InputStream;
import java.util.regex.Pattern;

import com.group03.utils.MiscUtils;
import com.group03.utils.OutUtils;

import fi.iki.elonen.NanoHTTPD;
import fi.iki.elonen.NanoHTTPD.Response.Status;//status de la respuesta del servidor
  
public class App extends NanoHTTPD {//es un servidor web ligero (para proyectos pequeños) y de código abierto escrito en Java.
  private static Pattern isFile = Pattern.compile("^.*[^/]$");
  private static final Integer port = 8088;//puerto de la aplicación, se puede cambiar

  public App() throws IOException {
    super(port);
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

  public Response serve(IHTTPSession session) {
    String uri = session.getUri();//Return the URI used to open the WebSocket connection
    var file = (isFile.matcher(uri).matches() ? uri : uri + "index.html");//ver que el uri calce con el archivo
    var fileStream = getClass().getClassLoader().getResourceAsStream(file.substring(1));//leer de un archivo, lee el carácter 1 para ver que exista el archivo
    if (fileStream != null) {
      OutUtils.successFormatWithDatetime("Successful response to '%s' static file request [%s].%n", file, session.getMethod());//imprime las solicitudes de los archivos del browser
      return newChunkedResponse(Status.OK, getMimeTypeForFile(file), fileStream);//Es una respuesa fraccionada para cuando la respuesta es muy grande
    } else {
      OutUtils.warningFormatWithDatetime("Static file not found: '%s' [%s].%n", file, session.getMethod());//file not found. OutUtils=librería para salidas
      return newFixedLengthResponse(Response.Status.NOT_FOUND, MIME_PLAINTEXT, "The requested resource does not exist.");
    }
  }
  
}
/**
 *  _____      _   _                  ____   ___ ____   ___
 * | ____|    | \ | | __ _ _ __   ___|___ \ / _ \___ \ / _ \
 * |  _| _____|  \| |/ _` | '_ \ / _ \ __) | | | |__) | | | |
 * | |__|_____| |\  | (_| | | | | (_) / __/| |_| / __/| |_| |
 * |_____|    |_| \_|\__,_|_| |_|\___/_____|\___/_____|\___/
 *
 * File: RouterUtils.java
 * Description:
 *    Utilidades para el router.
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

package org.una.utilities;

import java.io.IOException;
import java.util.HashMap;

import com.google.common.collect.ImmutableList;

import org.json.JSONObject;

import fi.iki.elonen.NanoHTTPD.IHTTPSession;
import fi.iki.elonen.NanoHTTPD.Response;
import fi.iki.elonen.NanoHTTPD.ResponseException;

public class RouterUtils {

  private static final ImmutableList<String> ALLOWED_SITES = ImmutableList.of("same-site", "same-origin");

  public static Response allowCors(Response response, IHTTPSession session) {
    var headers = session.getHeaders();
    var origin = "none";

    var allowed = headers != null &&
      headers.get("sec-fetch-mode") != null &&
      headers.get("sec-fetch-site") != null &&
      headers.get("sec-fetch-mode").equals("cors") &&
      ALLOWED_SITES.contains(headers.get("sec-fetch-site")) &&
      (origin = headers.get("origin")) != null;

    if (allowed) {
      response.addHeader("Access-Control-Allow-Origin", origin);
    }

    return response;
  }
  
  public static JSONObject processData(IHTTPSession session) throws IOException, ResponseException {
    var body = new HashMap<String, String>();
    session.parseBody(body);
    var data = body.get("postData");
    if (data == null) return null;
    return new JSONObject(data);
  }

}


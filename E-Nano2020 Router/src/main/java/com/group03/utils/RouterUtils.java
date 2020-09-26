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
 * Date of modification: 2020-09-25
 */

package com.group03.utils;

import java.util.List;

import fi.iki.elonen.NanoHTTPD.IHTTPSession;
import fi.iki.elonen.NanoHTTPD.Response;

public class RouterUtils {
  
  private static final List<String> ALLOWED_SITES = List.of("same-site", "same-origin");

  private RouterUtils() { }

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

}


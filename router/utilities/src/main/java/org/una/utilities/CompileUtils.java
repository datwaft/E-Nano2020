/**
 *  _____      _   _                  ____   ___ ____   ___
 * | ____|    | \ | | __ _ _ __   ___|___ \ / _ \___ \ / _ \
 * |  _| _____|  \| |/ _` | '_ \ / _ \ __) | | | |__) | | | |
 * | |__|_____| |\  | (_| | | | | (_) / __/| |_| / __/| |_| |
 * |_____|    |_| \_|\__,_|_| |_|\___/_____|\___/_____|\___/
 *
 * File: CompileUtils.java
 * Description:
 *    Compila archivos .java y devuelve su resultado.
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

import com.google.common.collect.ImmutableList;
import org.una.compiler.Compiler;

import org.javatuples.Pair;
import org.json.JSONArray;

import java.io.IOException;
import java.util.regex.Pattern;

public class CompileUtils {

  private static Pattern className = Pattern.compile("class\\s+(\\S+)");
  private static Pattern publicClassName = Pattern.compile("public class\\s+(\\S+)");

  public static JSONArray compileString(String source) {
    var filename = "";
    var publicMatcher = publicClassName.matcher(source);
    if (publicMatcher.find()) {
      filename = publicMatcher.group(1);
    } else {
      var matcher = className.matcher(source);
      if (matcher.find()) {
        filename = matcher.group(1);
      }
    }

    var compiler = new Compiler();
    Pair<Class<?>, ImmutableList<Pair<String, String>>> result = null;
    try {
      result = compiler.compile(source, filename);
    } catch (IOException _ex) {
    } catch (IllegalArgumentException ex) {
      try {
      result = compiler.compile(source, "default");
      } catch (IOException _ex) { }
    }

    return result.getValue1().stream()
      .map(
        (p) -> Pair.with(switch(p.getValue0()) {
          case "MANDATORY_WARNING" -> "warning";
          case "OTHER" -> "note";
          default -> p.getValue0().toLowerCase();
        },
        p.getValue1())
      )
      .map(Pair::toList)
      .map(JSONArray::new)
      .collect(JSONArray::new, JSONArray::put, JSONArray::put);
  }

}




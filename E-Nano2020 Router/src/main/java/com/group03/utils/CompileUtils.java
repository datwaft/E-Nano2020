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
 * Date of modification: 2020-10-04
 */

package com.group03.utils;

import com.group03.compiler.Compiler;

import java.io.IOException;
import java.util.regex.Pattern;

public class CompileUtils {
  private static Pattern class_name = Pattern.compile("class\\s+(\\S+)\\s*\\{");

  public static String compileString(String source) {
    var output = new StringBuilder();

    var file_name = "";
    var matcher = class_name.matcher(source);
    if (matcher.find()) {
      file_name = matcher.group(1);
    }

    var compiler = new Compiler();
    Class<?> compiled_class = null;
    try {
      compiled_class = compiler.compile(source, file_name);
    } catch(IOException | ClassNotFoundException ex) { }

    for (var pair : compiler.getOutput()) {
      switch(pair.getValue0()) {
        case "SUCCESS":
          output.append(String.format("<span style=\"color: green\">%s</span>%n", pair.getValue1()));
          break;
        case "ERROR":
          output.append(String.format("<span style=\"color: red\">%s</span>", pair.getValue1()));
          break;
        case "MANDATORY_WARNING":
        case "WARNING":
          output.append(String.format("<span style=\"color: yellow\">%s</span>", pair.getValue1()));
          break;
        case "NOTE":
        case "OTHER":
          output.append(String.format("<span style=\"color: white\">%s</span>", pair.getValue1()));
          break;
      }
    }
    return output.toString();
  }

}


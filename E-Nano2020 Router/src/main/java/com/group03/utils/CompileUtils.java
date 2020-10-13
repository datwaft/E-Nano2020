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

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.group03.compiler.Compiler;

import org.javatuples.Pair;

import java.io.IOException;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class CompileUtils {
  private static Pattern class_name = Pattern.compile("class\\s+(\\S+)");
  private static Pattern public_class_name = Pattern.compile("public class\\s+(\\S+)");

  private static ImmutableMap<String, String> types_of_messages = ImmutableMap.of(
    "SUCCESS", "<span style=\"color: green\">%s</span>",
    "ERROR", "<span style=\"color: red\">%s</span>",
    "WARNING", "<span style=\"color: yellow\">%s</span>",
    "NOTE", "<span style=\"color: white\">%s</span>"
  );

  public static String compileString(String source) {
    var file_name = "";
    var public_matcher = public_class_name.matcher(source);
    if (public_matcher.find()) {
      file_name = public_matcher.group(1);
    } else {
      var matcher = class_name.matcher(source);
      if (matcher.find()) {
        file_name = matcher.group(1);
      }
    }

    var compiler = new Compiler();
    Pair<Class<?>, ImmutableList<Pair<String, String>>> result = null;
    try {
      result = compiler.compile(source, file_name);
    } catch(IOException | ClassNotFoundException ex) { }

    return result.getValue1().stream()
      .map((p) -> Pair.with(p.getValue0().equals("MANDATORY_WARNING") ? "WARNING" : p.getValue0(), p.getValue1()))
      .map((p) -> Pair.with(p.getValue0().equals("OTHER") ? "NOTE" : p.getValue0(), p.getValue1()))
      .map((p) -> String.format(types_of_messages.get(p.getValue0()), p.getValue1()))
      .collect(Collectors.joining("\n"));
  }

}


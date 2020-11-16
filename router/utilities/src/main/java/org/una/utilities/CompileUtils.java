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
import java.io.FileOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileDescriptor;
import java.io.PrintStream;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.HashMap;

public class CompileUtils {

  private static Map<String, Class<?>> compilations = new HashMap<>();

  public static JSONArray compileString(String source, String filename) {
    var compiler = new Compiler();
    Pair<Class<?>, ImmutableList<Pair<String, String>>> result = null;
    try {
      result = compiler.compile(source, filename);
    } catch (IOException _ex) { }
    if (result.getValue0() != null) {
      compilations.put(filename, result.getValue0());
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

  public static String evaluate(String filename) {
    if (compilations.get(filename) != null) {
      try {
        Class<?> cls = compilations.get(filename);
        Method method = cls.getMethod("main", String[].class);
        String[] params = null;

        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        System.setOut(new PrintStream(buffer));

        method.invoke(null, (Object) params);

        System.setOut(new PrintStream(new FileOutputStream(FileDescriptor.out)));
        String content = buffer.toString();
        buffer.reset();

        return content;
      } catch (Exception ex) {
        return ex.toString();
      }
    } else {
      return "Class not found.";
    }
  }

}

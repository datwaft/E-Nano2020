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
 * Date of modification: 2020-10-01
 */

package com.group03.utils;

import java.net.URI;
import java.util.List;
import java.util.Locale;
import java.util.regex.Pattern;

import javax.tools.DiagnosticCollector;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;

public class CompileUtils {
  private static Locale locale = null;
  private static Pattern class_name = Pattern.compile("public\\s+class\\s+(\\S+)\\s*\\{");

  public static String compileString(String to_compile) {
    var output = new StringBuilder();

    var compiler = ToolProvider.getSystemJavaCompiler();
    var diagnostics = new DiagnosticCollector<JavaFileObject>();

    var file_name = "";
    var matcher = class_name.matcher(to_compile);
    if (matcher.find()) {
      file_name = matcher.group(1);
    }

    var file = new JavaSourceFromString(file_name, to_compile);
    var compilation_utils = List.of(file);
    var task = compiler.getTask(
      null,
      null,
      diagnostics,
      null,
      null,
      compilation_utils
    );
    var success = task.call();

    if (success) {
      var message = String.format("The code compiled successfully.%n");
      OutUtils.successFormatWithDatetime(message);
      output.append(String.format("<span style=\"color: green\">%s</span>%n", message));
    } else {
      var message = String.format("The code didn't compile successfully.%n");
      OutUtils.errorFormatWithDatetime(message);
      output.append(String.format("<span style=\"color: red\">%s</span>%n", message));
    }

    for (var diagnostic : diagnostics.getDiagnostics()) {
      var pos = diagnostic.getLineNumber();
      var location = pos >= 0 ? String.format("line %d", pos) : "unavailable";
      var message = String.format("%s in %s.%n",
        diagnostic.getMessage(locale),
        location
      );
      switch(diagnostic.getKind()) {
        case ERROR:
          OutUtils.errorFormat(message);
          output.append(String.format("<span style=\"color: red\">%s</span>", message));
          break;
        case MANDATORY_WARNING:
        case WARNING:
          output.append(String.format("<span style=\"color: yellow\">%s</span>", message));
          OutUtils.warningFormat(message);
          break;
        case NOTE:
        case OTHER:
          output.append(String.format("<span style=\"color: white\">%s</span>", message));
          OutUtils.normalFormat(message);
          break;
      }
    }
    return output.toString();
  }

  private static class JavaSourceFromString extends SimpleJavaFileObject {
    final String code;

    JavaSourceFromString(String name, String code) {
      super(URI.create(String.format("string:///%s%s", name.replace('.', '/'), Kind.SOURCE.extension)), Kind.SOURCE);
      this.code = code;
    }

    @Override
    public CharSequence getCharContent(boolean ignoreEncodingErrors) {
      return code;
    }

  }

}


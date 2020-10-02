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

import javax.tools.DiagnosticCollector;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;

public class CompileUtils {
  private static Locale locale = null;

  public static String compileString(String to_compile) {
    var output = new StringBuilder();

    var compiler = ToolProvider.getSystemJavaCompiler();
    var diagnostics = new DiagnosticCollector<JavaFileObject>();

    var file = new JavaSourceFromString("", to_compile);
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
      OutUtils.successFormatWithDatetime("The code compiled successfully.%n");
      output.append("The code compiled successfully.\n\n");
    } else {
      OutUtils.errorFormatWithDatetime("The code didn't compile successfully.%n");
      output.append("The code didn't compile successfully.\n\n");
    }

    for (var diagnostic : diagnostics.getDiagnostics()) {
      var pos = diagnostic.getLineNumber();
      var location = pos >= 0 ? String.format("Line %d", pos) : "Unavailable";
      var message = String.format("%s: %s.%n",
        location,
        diagnostic.getMessage(locale)
      );
      switch(diagnostic.getKind()) {
        case ERROR:
          OutUtils.errorFormat(message);
          break;
        case MANDATORY_WARNING:
        case WARNING:
          OutUtils.warningFormat(message);
          break;
        case NOTE:
        case OTHER:
          OutUtils.normalFormat(message);
          break;
      }
      output.append(message);
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


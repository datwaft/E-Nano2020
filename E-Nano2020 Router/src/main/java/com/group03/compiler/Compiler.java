/**
 *  _____      _   _                  ____   ___ ____   ___
 * | ____|    | \ | | __ _ _ __   ___|___ \ / _ \___ \ / _ \
 * |  _| _____|  \| |/ _` | '_ \ / _ \ __) | | | |__) | | | |
 * | |__|_____| |\  | (_| | | | | (_) / __/| |_| / __/| |_| |
 * |_____|    |_| \_|\__,_|_| |_|\___/_____|\___/_____|\___/
 *
 * File: Compiler.java
 * Description:
 *    Es una clase que compila código Java en memoria.
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

package com.group03.compiler;

import java.io.IOException;
import java.net.URL;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.tools.DiagnosticCollector;
import javax.tools.JavaFileObject;
import javax.tools.ToolProvider;

public class Compiler {
  private ClassLoader class_loader = Compiler.class.getClassLoader();
  private List<AbstractMap.SimpleEntry<String, String>> output = new ArrayList<>();

  public void setClassLoader(ClassLoader class_loader) {
    this.class_loader = class_loader;
  }

  private String calculateSimpleClassName(String canonical_class_name) {
    return canonical_class_name.substring(canonical_class_name.lastIndexOf('.') + 1);
  }

  public Class<?> compile(String source, String canonical_class_name) throws ClassNotFoundException, IOException {
		var compiler = ToolProvider.getSystemJavaCompiler();
    var diagnostics = new DiagnosticCollector<JavaFileObject>();

		var class_name = calculateSimpleClassName(canonical_class_name);
		var sources = List.of(new JavaSource(class_name, source));

		var manager = new InMemoryFileManager(compiler.getStandardFileManager(null, null, null));
		var task = compiler.getTask(
      null,
      manager,
      diagnostics,
      null,
      null,
      sources
    );

		var success = task.call();

    if (success) {
      output.add(new AbstractMap.SimpleEntry<>("SUCCESS", "The code compiled successfully.\n"));
    } else {
      output.add(new AbstractMap.SimpleEntry<>("ERROR", "The code compiled with errors.\n"));
    }

    for (var diagnostic : diagnostics.getDiagnostics()) {
      var pos = diagnostic.getLineNumber();
      var location = pos >= 0 ? String.format("Line %d", pos) : "Unavailable";
      String message = String.format("%s: %s.%n",
        location,
        diagnostic.getMessage(null)
      );
      output.add(new AbstractMap.SimpleEntry<>(
        diagnostic.getKind().toString(),
        message
      ));
    }

		if (success) {
			var loader = new ByteClassLoader(new URL[0], class_loader, classesByteArraysMap(manager));
			var klazz = loader.loadClass(canonical_class_name);
			loader.close();
			return klazz;
		} else {
			return null;
		}
	}

  private Map<String, byte[]> classesByteArraysMap(InMemoryFileManager file_manager) {
    var result = new HashMap<String, byte[]>();
    for (var name : file_manager.getClassFileObjectsMap().keySet()) {
      result.put(name, file_manager.getClassFileObjectsMap().get(name).getByteArray());
    }
    return result;
  }

  public List<AbstractMap.SimpleEntry<String, String>> getOutput() {
    return output;
  }
}

/**
 * Inspirado por: https://github.com/verhas/jscc/
 */



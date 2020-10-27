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
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.tools.DiagnosticCollector;
import javax.tools.JavaFileObject;
import javax.tools.ToolProvider;

import org.javatuples.Pair;

import com.google.common.collect.ImmutableList;

public class Compiler {
  private ClassLoader class_loader = Compiler.class.getClassLoader();

  public void setClassLoader(ClassLoader class_loader) {
    this.class_loader = class_loader;
  }

  private String calculateSimpleClassName(String canonical_class_name) {
    return canonical_class_name.substring(canonical_class_name.lastIndexOf('.') + 1);
  }

  public Pair<Class<?>, ImmutableList<Pair<String, String>>> compile(String source, String canonical_class_name) throws IOException {
    ImmutableList<Pair<String, String>> output;

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
      output = ImmutableList.of(Pair.with("SUCCESS", "The code compiled successfully."));
    } else {
      output = ImmutableList.of(Pair.with("ERROR", "The code compiled with errors."));
    }
    
    output = Stream.concat(
      output.stream(), 
      diagnostics.getDiagnostics().stream()
        .map((d) -> Pair.with(
          d.getKind().toString(), 
          String.format("%s: %s.", 
            d.getLineNumber() >= 0 ? String.format("Line %d", d.getLineNumber()) 
                                   : "Unavailable",
                                   d.getMessage(null))
          )
        )
      ).collect(Collectors.collectingAndThen(Collectors.toList(), ImmutableList::copyOf));

		if (success) {
			var loader = new ByteClassLoader(new URL[0], class_loader, classesByteArraysMap(manager));
      try {
        Class<?> klazz = loader.loadClass(canonical_class_name);
        loader.close();
        return Pair.with(klazz, output);
      } catch (Exception ex) {
        return Pair.with(null, output);
      }
		} else {
			return Pair.with(null, output);
		}
	}

  private Map<String, byte[]> classesByteArraysMap(InMemoryFileManager file_manager) {
    return file_manager.getClassFileObjectsMap().entrySet().stream()
      .collect(Collectors.toMap((e) -> e.getKey(), (e) -> e.getValue().getByteArray()));
  }
}

/**
 * Inspirado por: https://github.com/verhas/jscc/
 */



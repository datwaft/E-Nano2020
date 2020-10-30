/**
 *  _____      _   _                  ____   ___ ____   ___
 * | ____|    | \ | | __ _ _ __   ___|___ \ / _ \___ \ / _ \
 * |  _| _____|  \| |/ _` | '_ \ / _ \ __) | | | |__) | | | |
 * | |__|_____| |\  | (_| | | | | (_) / __/| |_| / __/| |_| |
 * |_____|    |_| \_|\__,_|_| |_|\___/_____|\___/_____|\___/
 *
 * File: InMemoryFileManager.java
 * Description:
 *    Sirve para alamcenar archivos en memoria sin tener que pasar por el filesystem.
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

package org.una.compiler;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.tools.FileObject;
import javax.tools.ForwardingJavaFileManager;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.JavaFileObject.Kind;

public class InMemoryFileManager extends ForwardingJavaFileManager<StandardJavaFileManager> {

  private Map<String, InMemoryFileObject> files;

  protected InMemoryFileManager(StandardJavaFileManager fileManager) {
    super(fileManager);
    files = new HashMap<>();
  }

  public Map<String, InMemoryFileObject> getClassFileObjectsMap() {
    return files;
  }

  @Override
  public JavaFileObject getJavaFileForOutput(Location location, String name, Kind kind, FileObject sibling) throws IOException {
    var file = new InMemoryFileObject(name);
    files.put(name, file);
    return file;
  }

}

/**
 * Inspirado por: https://github.com/verhas/jscc/
 */


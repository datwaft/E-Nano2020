/**
 *  _____      _   _                  ____   ___ ____   ___
 * | ____|    | \ | | __ _ _ __   ___|___ \ / _ \___ \ / _ \
 * |  _| _____|  \| |/ _` | '_ \ / _ \ __) | | | |__) | | | |
 * | |__|_____| |\  | (_| | | | | (_) / __/| |_| / __/| |_| |
 * |_____|    |_| \_|\__,_|_| |_|\___/_____|\___/_____|\___/
 *
 * File: ByteClassLoader.java
 * Description:
 *    Es una clase que crea una clase desde un array de bytes.
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

import java.net.URL;
import java.net.URLClassLoader;
import java.util.Map;

public class ByteClassLoader extends URLClassLoader {
  private Map<String, byte[]> class_files;

  public ByteClassLoader(URL[] urls, ClassLoader parent, Map<String, byte[]> class_files) {
    super(urls, parent);
    this.class_files = class_files;
  }

  @Override
  protected Class<?> findClass(final String name) throws ClassNotFoundException {
    if (class_files.containsKey(name)) {
      var class_file = class_files.get(name);
      Class<?> klazz = defineClass(name, class_file, 0, class_file.length);
      releaseClassFile(name);
      return klazz;
    }
    return super.findClass(name);
  }


  private void releaseClassFile(String name) {
    class_files.remove(name);
  }
}

/**
 * Inspirado por: https://github.com/verhas/jscc/
 */


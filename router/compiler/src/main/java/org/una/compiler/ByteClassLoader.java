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
 * Date of modification: 2020-10-29
 */

package org.una.compiler;

import java.net.URL;
import java.net.URLClassLoader;
import java.util.Map;

public class ByteClassLoader extends URLClassLoader {

  private Map<String, byte[]> files;

  public ByteClassLoader(URL[] urls, ClassLoader parent, Map<String, byte[]> files) {
    super(urls, parent);
    this.files = files;
  }

  @Override
  protected Class<?> findClass(final String name) throws ClassNotFoundException {
    if (files.containsKey(name)) {
      var file = files.get(name);
      Class<?> klazz = defineClass(name, file, 0, file.length);
      releaseClassFile(name);
      return klazz;
    }
    return super.findClass(name);
  }

  private void releaseClassFile(String name) {
    files.remove(name);
  }

}

/**
 * Inspirado por: https://github.com/verhas/jscc/
 */


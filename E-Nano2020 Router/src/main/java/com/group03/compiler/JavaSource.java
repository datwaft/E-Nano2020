/**
 *  _____      _   _                  ____   ___ ____   ___
 * | ____|    | \ | | __ _ _ __   ___|___ \ / _ \___ \ / _ \
 * |  _| _____|  \| |/ _` | '_ \ / _ \ __) | | | |__) | | | |
 * | |__|_____| |\  | (_| | | | | (_) / __/| |_| / __/| |_| |
 * |_____|    |_| \_|\__,_|_| |_|\___/_____|\___/_____|\___/
 *
 * File: JavaSource.java
 * Description:
 *    Utilizado para representar código que viene de un string.
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

import java.net.URI;

import javax.tools.SimpleJavaFileObject;

public class JavaSource extends SimpleJavaFileObject {
  private final String code;
  
  JavaSource(final String name, final String code) {
    super(URI.create(String.format("string:///%s%s", name.replace('.', '/'), Kind.SOURCE.extension)), Kind.SOURCE);
    this.code = code;
  }

  @Override
  public CharSequence getCharContent(final boolean ignore_encoding_errors) {
    return code;
  }
}

/**
 * Inspirado por: https://github.com/verhas/jscc/
 */


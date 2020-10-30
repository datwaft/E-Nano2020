/**
 *  _____      _   _                  ____   ___ ____   ___
 * | ____|    | \ | | __ _ _ __   ___|___ \ / _ \___ \ / _ \
 * |  _| _____|  \| |/ _` | '_ \ / _ \ __) | | | |__) | | | |
 * | |__|_____| |\  | (_| | | | | (_) / __/| |_| / __/| |_| |
 * |_____|    |_| \_|\__,_|_| |_|\___/_____|\___/_____|\___/
 *
 * File: FileUtils.java
 * Description:
 *    Archivo de ayuda para leer archivos del filesystem.
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

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Maps;

import java.io.InputStream;
import java.util.Properties;

public class FileUtils {

  public static String readString(String filename) {
    try {
      var stream = FileUtils.class.getClassLoader().getResourceAsStream(filename);
      return MiscUtils.inputStreamToString(stream);
    } catch (Exception ex) {
      LogUtils.format(LogUtils.Style.ERROR, "File '%s' failed to load.", filename);
      return null;
    }
  }

  public static InputStream readInputStream(String filename) {
    try {
      return FileUtils.class.getClassLoader().getResourceAsStream(filename);
    } catch (Exception ex) {
      LogUtils.format(LogUtils.Style.ERROR, "File '%s' failed to load.", filename);
      return null;
    }
  }

  public static ImmutableMap<String, String> readProperties(String filename) {
    var properties = new Properties();
    try (var input_stream = FileUtils.class.getClassLoader().getResourceAsStream(filename)) {
      properties.load(input_stream);
      return Maps.fromProperties(properties);
    } catch (Exception ex) {
      LogUtils.format(LogUtils.Style.ERROR, "File '%s' failed to load.", filename);
      return null;
    }
  }

}

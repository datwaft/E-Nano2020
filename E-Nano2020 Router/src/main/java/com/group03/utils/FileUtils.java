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
 * Date of modification: 2020-10-01
 */

package com.group03.utils;

import java.io.InputStream;
import java.util.Map;
import java.util.Properties;
import java.util.stream.Collectors;

public class FileUtils {

  private FileUtils() { }

  public static String readString(String filename) {
    try {
      var stream = FileUtils.class.getClassLoader().getResourceAsStream(filename);
      var string = MiscUtils.inputStreamToString(stream);
      return string;
    } catch(Exception ex) {
      OutUtils.errorFormatWithDatetime("File '%s' failed to load.", filename);
      return null;
    }
  }

  public static InputStream readInputStream(String filename) {
    try {
      var stream = FileUtils.class.getClassLoader().getResourceAsStream(filename);
      return stream;
    } catch(Exception ex) {
      OutUtils.errorFormatWithDatetime("File '%s' failed to load.", filename);
      return null;
    }
  }

  public static Map<String, String> readProperties(String filename) {
    Properties properties = new Properties();

    try (var input_stream = FileUtils.class.getClassLoader().getResourceAsStream(filename)) {
      properties.load(input_stream);

      return properties.entrySet().stream().collect(
        Collectors.toMap(e -> e.getKey().toString(), e -> e.getValue().toString())
      );
    } catch(Exception ex) {
      OutUtils.errorFormatWithDatetime("File '%s' failed to load.", filename);
      return null;
    }
  }

}

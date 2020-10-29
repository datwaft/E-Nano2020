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

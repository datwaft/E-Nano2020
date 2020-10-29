package org.una.utilities;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.stream.Collectors;

public class MiscUtils {
  public static String inputStreamToString(InputStream inputStream) throws IOException {
    try (var br = new BufferedReader(new InputStreamReader(inputStream, StandardCharsets.UTF_8))) {
      return br.lines().collect(Collectors.joining(System.lineSeparator()));
    }
  }
}

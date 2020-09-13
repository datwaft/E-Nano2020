package com.group03.utils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.stream.Collectors;

public class MiscUtils {
  
  private MiscUtils() { }

  public static String inputStreamToString(InputStream inputStream) throws IOException {
    try (BufferedReader br = new BufferedReader(new InputStreamReader(inputStream, "UTF-8"))) {
      return br.lines().collect(Collectors.joining(System.lineSeparator()));
    }
  }

}


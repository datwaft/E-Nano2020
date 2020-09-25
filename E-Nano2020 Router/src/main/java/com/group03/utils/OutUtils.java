/**
 *  _____      _   _                  ____   ___ ____   ___
 * | ____|    | \ | | __ _ _ __   ___|___ \ / _ \___ \ / _ \
 * |  _| _____|  \| |/ _` | '_ \ / _ \ __) | | | |__) | | | |
 * | |__|_____| |\  | (_| | | | | (_) / __/| |_| / __/| |_| |
 * |_____|    |_| \_|\__,_|_| |_|\___/_____|\___/_____|\___/
 *
 * File: OutUtils.java
 * Description:
 *    Utilidades para el proyecto, principalmente de salida a la consola.
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
 * Date of modification: 2020-09-16
 */

package com.group03.utils;

import java.text.SimpleDateFormat;
import java.util.Date;

import com.diogonunes.jcolor.AnsiFormat;
import com.diogonunes.jcolor.Attribute;

public class OutUtils {

  private static final AnsiFormat fError = new AnsiFormat(Attribute.RED_TEXT(), Attribute.BOLD());
  private static final AnsiFormat fWarning = new AnsiFormat(Attribute.YELLOW_TEXT(), Attribute.BOLD());
  private static final AnsiFormat fSuccess = new AnsiFormat(Attribute.GREEN_TEXT(), Attribute.BOLD());
  private static final AnsiFormat fErrorNormal = new AnsiFormat(Attribute.RED_TEXT());
  private static final AnsiFormat fWarningNormal = new AnsiFormat(Attribute.YELLOW_TEXT());
  private static final AnsiFormat fSuccessNormal = new AnsiFormat(Attribute.GREEN_TEXT());
  private static final SimpleDateFormat dateFormatter = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

  private OutUtils() {}

  public static void normalFormat(String format, Object... args) {
    System.out.println(String.format(format, args));
  }

  public static void successFormat(String format, Object... args) {
    System.out.println(fSuccess.format(String.format(format, args)));
  }

  public static void warningFormat(String format, Object... args) {
    System.out.println(fWarning.format(String.format(format, args)));
  }

  public static void errorFormat(String format, Object... args) {
    System.out.println(fError.format(String.format(format, args)));
  }

  public static void successFormatWithDatetime(String format, Object... args) {
    System.out.println(getSuccessDateTime() + fSuccess.format(String.format(format, args)));
  }

  public static void warningFormatWithDatetime(String format, Object... args) {
    System.out.println(getWarningDateTime() + fWarning.format(String.format(format, args)));
  }

  public static void errorFormatWithDatetime(String format, Object... args) {
    System.out.println(getErrorDateTime() + fError.format(String.format(format, args)));
  }

  private static String getDateTime() {
    return String.format("[%s]%n", dateFormatter.format(new Date()));
  }

  private static String getSuccessDateTime() {
    return fSuccessNormal.format(getDateTime());
  }

  private static String getWarningDateTime() {
    return fWarningNormal.format(getDateTime());
  }

  private static String getErrorDateTime() {
    return fErrorNormal.format(getDateTime());
  }

}

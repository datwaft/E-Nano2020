/**
 *  _____      _   _                  ____   ___ ____   ___
 * | ____|    | \ | | __ _ _ __   ___|___ \ / _ \___ \ / _ \
 * |  _| _____|  \| |/ _` | '_ \ / _ \ __) | | | |__) | | | |
 * | |__|_____| |\  | (_| | | | | (_) / __/| |_| / __/| |_| |
 * |_____|    |_| \_|\__,_|_| |_|\___/_____|\___/_____|\___/
 *
 * File: LogUtils.java
 * Description:
 *    Utilidades para salida hacia la consola.
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

import java.text.SimpleDateFormat;
import java.util.Date;

import static com.diogonunes.jcolor.Ansi.colorize;
import static com.diogonunes.jcolor.Attribute.*;

public class LogUtils {

  private static final SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

  public enum Style {
    ERROR,
    NOTE,
    SUCCESS,
    WARNING
  }

  private enum StyleBold {
    ERROR,
    NOTE,
    SUCCESS,
    WARNING
  }
  
  private static String getStyleFormat(Style style, String string) {
    return switch (style) {
      case ERROR -> colorize(string, RED_TEXT());
      case NOTE -> colorize(string, WHITE_TEXT());
      case SUCCESS -> colorize(string, GREEN_TEXT());
      case WARNING -> colorize(string, YELLOW_TEXT());
    };
  }

  private static String getStyleFormat(StyleBold style, String string) {
    return switch (style) {
      case ERROR -> colorize(string, RED_TEXT(), BOLD());
      case NOTE -> colorize(string, WHITE_TEXT(), BOLD());
      case SUCCESS -> colorize(string, GREEN_TEXT(), BOLD());
      case WARNING -> colorize(string, YELLOW_TEXT(), BOLD());
    };
  }

  private static StyleBold convertStyle(Style style) {
    return switch (style) {
      case ERROR -> StyleBold.ERROR;
      case SUCCESS -> StyleBold.SUCCESS;
      case WARNING -> StyleBold.WARNING;
      default -> StyleBold.NOTE;
    };
  }

  private static String getDate() {
    return String.format("[%s]%n", formatter.format(new Date()));
  }

  public static void format(Style style, String string, Object... args) {
    System.out.println(getStyleFormat(convertStyle(style), String.format(string, args)));
  }

  public static void formatD(Style style, String string, Object... args) {
    System.out.println(getStyleFormat(style, getDate()) + getStyleFormat(convertStyle(style), String.format(string, args)));
  }

  public static void format(String string, Object... args) {
    System.out.println(getStyleFormat(StyleBold.NOTE, String.format(string, args)));
  }

  public static void formatD(String string, Object... args) {
    System.out.println(getStyleFormat(Style.NOTE, getDate()) + getStyleFormat(StyleBold.NOTE, String.format(string, args)));
  }

}


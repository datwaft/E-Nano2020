/**
 *  _____      _   _                  ____   ___ ____   ___
 * | ____|    | \ | | __ _ _ __   ___|___ \ / _ \___ \ / _ \
 * |  _| _____|  \| |/ _` | '_ \ / _ \ __) | | | |__) | | | |
 * | |__|_____| |\  | (_| | | | | (_) / __/| |_| / __/| |_| |
 * |_____|    |_| \_|\__,_|_| |_|\___/_____|\___/_____|\___/
 *
 * File: InMemoryFileObject.java
 * Description:
 *    Sirve para alamcenar archivos en memoria sin tener que pasar por el filesystem.
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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.net.URI;
import java.util.Date;

import javax.lang.model.element.Modifier;
import javax.lang.model.element.NestingKind;
import javax.tools.JavaFileObject;

public class InMemoryFileObject implements JavaFileObject {
  private final String name;
  private final ByteArrayOutputStream bytes = new ByteArrayOutputStream();

  public InMemoryFileObject(final String name) {
    this.name = name;
  }

  @Override
  public URI toUri() {
    return URI.create(String.format("string:///%s%s", name.replace('.', '/'), Kind.SOURCE.extension));
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public InputStream openInputStream() throws IOException {
    return new ByteArrayInputStream(bytes.toByteArray());
  }

  @Override
  public OutputStream openOutputStream() throws IOException {
    return bytes;
  }

  public byte[] getByteArray() {
    return bytes.toByteArray();
  }

  @Override
  public Reader openReader(final boolean ignore_encoding_erros) throws IOException {
    return null;
  }

  @Override
	public CharSequence getCharContent(final boolean ignore_encoding_erros) throws IOException {
		return null;
	}

	@Override
	public Writer openWriter() throws IOException {
		return null;
	}

	@Override
	public long getLastModified() {
		return new Date().getTime();
	}

	@Override
	public boolean delete() {
		return true;
	}

	@Override
	public Kind getKind() {
		return Kind.CLASS;
	}

	@Override
	public boolean isNameCompatible(final String simpleName, final Kind kind) {
		return true;
	}

	@Override
	public NestingKind getNestingKind() {
		return null;
	}

	@Override
	public Modifier getAccessLevel() {
		return Modifier.PUBLIC;
	}
}

/**
 * Inspirado por: https://github.com/verhas/jscc/
 */


::  _____      _   _                  ____   ___ ____   ___
:: | ____|    | \ | | __ _ _ __   ___|___ \ / _ \___ \ / _ \
:: |  _| _____|  \| |/ _` | '_ \ / _ \ __) | | | |__) | | | |
:: | |__|_____| |\  | (_| | | | | (_) / __/| |_| / __/| |_| |
:: |_____|    |_| \_|\__,_|_| |_|\___/_____|\___/_____|\___/
::
:: File: compile-execute-static-server.java
:: Description:
::    Script para compilar y ejecutar el servidor de archivos estáticos.
:: Authors:
:: - David Alberto Guevara Sánchez
::   402450355
:: - Joy Bonilla Fley
::   402360421
:: - Jose Barrantes Araya
::   207600954
:: - Natalia Solano Azofeifa
::   117290958
:: - Luis David Villalobos Gonzalez
::   117540697
:: Group: 03
:: Schedule: 10am
:: Date of modification: 2020-10-29

@echo off
setlocal
title Static Server

echo Moving to folder...
cd "static-server"

echo.
echo Wrapping...
call gradle wrapper

echo.
echo Compiling...
call gradlew build
if errorlevel 1 goto :error

echo.
echo Executing...
call gradlew run
goto :end

:error

echo.
echo The compilation has errors, cannot run.

:end

exit /b %errorlevel%


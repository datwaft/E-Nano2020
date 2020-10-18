::  _____      _   _                  ____   ___ ____   ___
:: | ____|    | \ | | __ _ _ __   ___|___ \ / _ \___ \ / _ \
:: |  _| _____|  \| |/ _` | '_ \ / _ \ __) | | | |__) | | | |
:: | |__|_____| |\  | (_| | | | | (_) / __/| |_| / __/| |_| |
:: |_____|    |_| \_|\__,_|_| |_|\___/_____|\___/_____|\___/
::
:: File: compile-execute-router.java
:: Description:
::    Script para compilar y ejecutar el servidor router.
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
:: Date of modification: 2020-10-17

@echo off
setlocal
title Router

echo Moving to folder...
cd "./E-Nano2020 Router/"

echo.
echo Compiling...
call mvn package
if errorlevel 1 goto :error

echo.
echo Executing...
call mvn exec:java
goto :end

:error

echo.
echo The compilation has errors, cannot run.

:end

exit /b %errorlevel%

::  _____      _   _                  ____   ___ ____   ___
:: | ____|    | \ | | __ _ _ __   ___|___ \ / _ \___ \ / _ \
:: |  _| _____|  \| |/ _` | '_ \ / _ \ __) | | | |__) | | | |
:: | |__|_____| |\  | (_| | | | | (_) / __/| |_| / __/| |_| |
:: |_____|    |_| \_|\__,_|_| |_|\___/_____|\___/_____|\___/
::
:: File: compile-execute.java
:: Description:
::    Script para compilar y ejecutar todos los servidores
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

echo Calling router compiler and executer...
start .\shell-scripts\compile-execute-router.bat
echo Calling static server compiler and executer...
start .\shell-scripts\compile-execute-static-server.bat

exit /b %errorlevel%

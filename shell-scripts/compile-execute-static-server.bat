@echo off
setlocal
title Static Server

echo Moving to folder...
cd "./E-Nano2020 Static Server/"

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


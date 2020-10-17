@echo off
setlocal

echo Calling router compiler and executer...
start .\shell-scripts\compile-execute-router.bat
echo Calling static server compiler and executer...
start .\shell-scripts\compile-execute-static-server.bat

exit /b %errorlevel%

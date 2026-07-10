@echo off
setlocal enabledelayedexpansion
set "LOCAL_IP="

REM Filter for IPv4 Address lines
for /f "tokens=2 delims=:" %%i in ('ipconfig ^| findstr "IPv4"') do (
    set "IP=%%i"
    REM Trim spaces
    set "IP=!IP: =!"
    
	echo %IP%
    REM Check if the IP starts with 192.168
    if "!IP:~0,8!"=="192.168." (
        set "LOCAL_IP=!IP!"
        goto :Found
    )
)
:Found

hledger-web.exe -f "C:/Users/Aiden Hu/.hledger.journal" --host %IP% --port=5000
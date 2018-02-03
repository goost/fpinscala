@echo off
set SCRIPT_DIR=%~dp0
"C:\Program Files\Java\jdk1.8.0_151\bin\java" %SBT_OPTS% -Xmx512m -Xss8M -jar "%SCRIPT_DIR%sbt-launch.jar" %*

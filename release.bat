set version=1

call build.bat
if errorlevel 1 goto :eof

md scheme-format-%version%

copy *.exe scheme-format-%version%
copy *.md scheme-format-%version%
copy LICENSE scheme-format-%version%

del *.zip
7z a scheme-format-%version%.zip scheme-format-%version% -tzip

rd /q /s scheme-format-%version%

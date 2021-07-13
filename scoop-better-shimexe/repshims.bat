@echo off

if not defined SCOOP set SCOOP=%USERPROFILE%\scoop

for %%x in ("%SCOOP%\shims\*.exe") do (
  echo Replacing %%x by new shim.
  copy /B /Y shim.exe "%%~x" >NUL
)

if not defined SCOOP_GLOBAL set SCOOP_GLOBAL=%ProgramData%\scoop

for %%x in ("%SCOOP_GLOBAL%\shims\*.exe") do (
  echo Replacing %%x by new shim.
  copy /B /Y shim.exe "%%~x" >NUL
)

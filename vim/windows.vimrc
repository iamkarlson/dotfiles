"use powershell instead of cmd in windows

"if !&diff
"    set shell=cmd.exe
"    set shellcmdflag=/c\ powershell\ -ExecutionPolicy\ Unrestricted\ -NoProfile\ -NoLogo\ -NonInteractive\ -Command 
"endif

au GUIEnter * simalt ~x

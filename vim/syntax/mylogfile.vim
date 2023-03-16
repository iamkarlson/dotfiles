" Quit when a syntax file was already loaded.
if exists('b:current_syntax') | finish|  endif

syn match logdate '^\(:?\d{2}\){3}\.\d{3}' contained display
syn match classname '\[\w[A-z.]+\]' contained
syn match threadnumber '\[\d+\]' contained 

hi def link logdate Identifier
hi def link classname Type
hi def link threadnumber Number

let b:current_syntax= "nlogfile"

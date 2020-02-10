; readcurve.g $nbexp $inp 
;
; read a curve from the file of integration result
;

set nbexp = $_
set inp = $_

for k = 1 to $nbexp
  if (!eof($inp)) then
    set line = <$inp
    set paramx[$k] := (head($line))
    set integ[$k] := (head(tail($line)))
    set error[$k] := (tail(tail(tail($line))))
;  print ("line" ; $line)
  else
    alert ("Unexpected end_of_file during curve reading !")
    goto fin
  endif
endfor
return 0
exit

=fin
return 1


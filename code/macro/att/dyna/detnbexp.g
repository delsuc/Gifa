; detnbexp.g $liste
;
; determine the number of experiments from the liste of data-sets
;

set liste = $_

set i = 0
open $liste
set line = <$liste

while (!eof($liste))
;  print ("line" ;$line)
;  print ("line" ;(eof($liste)))
  if ((head($line) s! ";") & ($line s! " ")) then
;    print ("line" ;$line)
    set i = (%+1)
  endif
  set line = <$liste
endwhile
set nbexp := $i

close $liste





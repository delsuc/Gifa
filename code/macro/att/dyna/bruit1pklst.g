; bruit1pklst.g $dbno $liste $intmeth $homoth $zone 
; bruit1pklst.g 110 datalst Sumrec 0.8 "57 774 168 956"
;
; determine the error on intensities on a list of experiments, 
; according to the methode of evaluation "Multiexp"
;

set dbno = $_
set liste = $_
set intmeth = $_
set homoth = $_
set zone = $_

set bruit = 0
set err = 0
set nberr = 0
open $liste
set line = <$liste
while (!eof($liste))
;  print ("line" ;$line)
;  print ("line" ;(eof($liste)))
  if ((head($line) s! ";") & ((tail(tail($line))) s! " ")) then
; there are multiple experiments
;    print ("line" ;$line)
    set line = (tail($line))
    set intemp2 = 0
    set intemp = 0
    set nbtemp = 0
    while ((head($line)) s! " ") 
      set expname = (head($line))
      set line = (tail($line))
      quant1pk1exp.g $expname $dbno $intmeth "None" $bruit $homoth $zone 
      set intemp2 = (%+(head($returned)*head($returned)))
      set intemp = (%+(head($returned)))
      set nbtemp = (%+1)
    endwhile
    set intemp = (%/$nbtemp)
    set tmp = (sqrt($nbtemp/($nbtemp-1)*(($intemp2/$nbtemp) - ($intemp*$intemp))))
;    print ("tmp" ; $tmp)
    set err = (% + $tmp)
    set nberr = (% + 1)
  endif
  set line = <$liste
endwhile

set returned := ($err/$nberr)
close $liste



; quant1pklst.g $dbno $liste $errmeth $bruit $homoth $zone
;
; quant1pklst.g 110 datalst Sumrec Baseline 8002 0.8 57/774/168/956
; quantify a peak on a list of data-set
;

set dbno = $_
set liste = $_
set intmeth = $_
set errmeth = $_
set bruit = $_
set homoth = $_
set zone = $_

set i = 0
open $liste
set line = <$liste
set stoname = (tailx($name,"/"))

while (!eof($liste))
;  print ("line" ;$line)
;  print ("line" ;(eof($liste)))
  if ((head($line) s! ";") & ($line s! " ")) then
;    print ("line" ;$line)
    set i = (%+1)
    set paramx[$i] := (head($line))
    set line = (tail($line))
    if ($errmeth s= "Baseline") then
      set expname = (head($line))
      quant1pk1exp.g $expname $dbno $intmeth $errmeth $bruit $homoth $zone 
;      print ("quant1pk1exp.g" ; $expname ;$dbno ;$intmeth ;$errmeth ;$bruit ;$homoth ;$zone) 
      set integ[$i] := (head($returned))
      set error[$i] := (tail($returned))
    elsif ($errmeth s= "Multiexp") then
      set intemp = 0
      set nbtemp = 0
      while ((head($line)) s! " ") 
        set expname = (head($line))
        set line = (tail($line))
        quant1pk1exp.g $expname $dbno $intmeth $errmeth $bruit $homoth $zone 
;       print ("quant1pk1exp.g" ; $expname ;$dbno ;$intmeth ;$errmeth ;$bruit ;$homoth ;$zone) 
        set intemp = (%+(head($returned)))
        set nbtemp = (%+1)
      endwhile
      set integ[$i] := ($intemp/$nbtemp)
;      print ("integ";$integ[$i])
      set error[$i] := (tail($returned))
    endif
  endif
  set line = <$liste
endwhile
set nbexp := $i

;print ("stoname" ; $stoname)
read ("spectra/"//$stoname)

close $liste




;  quant1pk1exp.g $dataset $dbno $intmeth $errmeth $bruit $homoth
;
; quantify a peak on a data-set
;

set dataset = $_
set dbno = $_
set intmeth = $_
set errmeth = $_
set bruit = $_
set homoth = $_
set zone = $_

join ("spectra/"//$dataset)
; evaluate the noise on the dataset
; print ("no.g" ; $zone)
if ($errmeth s= 'Baseline')  then
   no.g $zone
else
   noise 1.0
endif

set f1 = (head($att[$dbno]))
set f2 = (head(tail($att[$dbno])))
set y = (round(ptoi($f1,0,1)))
set x = (round(ptoi($f2,0,2)))

set mask1 = 15
set mask2 = 15
set lf1 = (max(1,$y-$mask1))
set lf2 = (max(1,$x-$mask2))
set ur1 = (min($C_SIZEF1,$y+$mask1))
set ur2 = (min($C_SIZEF2,$x+$mask2))
;set mask1 = (($ur1-$lf1)/2)
;set mask2 = (($ur2-$lf2)/2)

if (($y+$mask1 <= $C_SIZEF1) & ($y-$mask1 >= 1)) then
  set ctry = ($mask1) 
elsif ($y+$mask1 > $C_SIZEF1) then
  set ctry = ($mask1)
;  print ("toto"; ($y+$mask1 - $C_SIZEF1) ; "ctry"; $ctry)
elsif ($y-$mask1 < 1) then
  set ctry = ($mask1 + ($y-$mask1 - 1))
endif
if (($x+$mask2 <= $C_SIZEF2) & ($x-$mask2 >= 1)) then
  set ctrx = ($mask2) 
elsif ($x+$mask2 > $C_SIZEF2) then
  set ctrx = ($mask2)
elsif ($x-$mask2 < 1) then
  set ctrx = ($mask2 + ($x-$mask2 - 1))
endif

; print ("getc"; $lf1; $lf2; $ur1; $ur2)
getc $lf1 $lf2 $ur1 $ur2

; calculate the box size in function of box_f1 and box_f2 
; (cf file 'parameters')
set szboxf1 = ($homoth*($c_sizef1 - 1)*$c_freq1*$box_f1/$c_specwf1)
set szboxf2 = ($homoth*($c_sizef2 - 1)*$c_freq2*$box_f2/$c_specwf2)

set diff1 = (int($szboxf1/2))
set diff2 = (int($szboxf2/2))
;print ('diff1';$diff1;'diff2';$diff2)
set err = 0
if ($intmeth s= "Sumrec") then
;    print ("sumrec";($mask1-$diff1) ;($mask2-$diff2) ;($mask1+$diff1+1) ;($mask2+$diff2+1))
;    print ("sumrec";($ctry-$diff1) ;($ctrx-$diff2) ;($ctry+$diff1+1) ;($ctrx+$diff2+1))
;    sumrec ($mask1-$diff1) ($mask2-$diff2) ($mask1+$diff1+1) ($mask2+$diff2+1)
    sumrec ($ctry-$diff1) ($ctrx-$diff2) ($ctry+$diff1+1) ($ctrx+$diff2+1)
    if ($errmeth s= "Baseline") then
      set err = $sumrec_err
    elsif ($errmeth s= "Multiexp") then
      set err = $bruit
    endif
    return ($sumrec ; $err)
elsif ($intmeth s= "MaxInBox") then
;    print ("sumrec";($mask1-$diff1) ;($mask2-$diff2) ;($mask1+$diff1+1) ;($mask2+$diff2+1))
;    print ("MaxInBox";($ctry-$diff1) ;($ctrx-$diff2) ;($ctry+$diff1+1) ;($ctrx+$diff2+1))
;    sumrec ($mask1-$diff1) ($mask2-$diff2) ($mask1+$diff1+1) ($mask2+$diff2+1)
;    maxinbox.g ($ctry-$diff1) ($ctrx-$diff2) ($ctry+$diff1+1) ($ctrx+$diff2+1)
;    set maxinbox = $returned   ; -- used to be a macro, now a command
    maxinbox ($ctry-$diff1) ($ctrx-$diff2) ($ctry+$diff1+1) ($ctrx+$diff2+1)
    if ($errmeth s= "Baseline") then
      set err = $noise
    elsif ($errmeth s= "Multiexp") then
      set err = $bruit
    endif
    return ($maxinbox ; $err)
endif

; print ("quant1pk1exp:"; $returned)


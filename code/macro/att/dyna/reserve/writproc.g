; writproc.g $data_typ $out $dbno
;
; write the result of a processing for a curve 
;

set data_typ = $_
set out = $_
set dbno = $_

set l = $att[$dbno]
set tmp1 = (tail(tail(tail(tail(tail($l))))))
set t = (tail(tail(tail($l))))
compute_label $dbno
if ($returned s! " ") then
  set label = $returned
else
  set label = "unk"
endif

compute_aa $dbno
if ($returned s! " ") then
  set resnum = $returned
else
  set resnum = "unk"
endif


if ($data_typ s= "R1") then
  fractil_95.g ($nbexp - 3)
  if ($chi2 > $returned) then
    set li = ($R1;$dR1;$chi2;'Warning_chi2')
  else
    set li = ($R1;$dR1;$chi2)
  endif
elsif ($data_typ s= "R2") then
  fractil_95.g ($nbexp - 2)
  if ($chi2 > $returned) then
     set li = ($R2;$dR2;$chi2;'Warning_chi2')
  else
     set li = ($R2;$dR2;$chi2)
  endif
elsif ($data_typ s= "NOE") then
  set li = ($NOE;$dNOE;'none')
elsif ($data_typ s= "T1") then
  fractil_95.g ($nbexp - 3)
  if ($chi2 > $returned) then
     set li = ($T1;$dT1;$chi2;'Warning_chi2')
  else
     set li = ($T1;$dT1;$chi2)
  endif
elsif ($data_typ s= "T2") then
  fractil_95.g ($nbexp - 2)
  if ($chi2 > $returned) then
    set li = ($T2;$dT2;$chi2;'Warning_chi2')
  else
    set li = ($T2;$dT2;$chi2)
  endif
elsif ($data_typ s= "J") then
  fractil_95.g ($nbexp - 3)
  if ($chi2 > $returned) then
     set li = ($J;$dJ;$chi2;'Warning_chi2')
  else
     set li = ($J;$dJ;$chi2)
  endif
endif
fprintf $out "%d\t%s\t%f\t%f\t%f\t%s" $resnum $label (head($li)) (head(tail($li))) (head(tail(tail($li)))) (tail(tail(tail($li))))  *

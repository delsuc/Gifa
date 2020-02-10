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
fprint $out ("Peak" ; $dbno ; $label ; (head($l))//","//(head(tail($l)))//" ppm" ; $tmp1)

if ($data_typ s= "R1") then
  fractil_95.g ($nbexp - 3)
  fprint $out ("Chi2" ; $chi2 ; "Number_of_iterations" ; $iterdone)
  if ($chi2 > $returned) then
    fprint $out ("R1     ";$R1;"  +/-  ";$dR1; "WARNING")
  else
    fprint $out ("R1     ";$R1;"  +/-  ";$dR1)
  endif
  fprint $out ("Iinf   ";$Iinf;"  +/-  ";$dIinf)
  fprint $out ("I0     ";$I0;"  +/-  ";$dI0)
elsif ($data_typ s= "R2") then
  fractil_95.g ($nbexp - 2)
  fprint $out ("Chi2" ; $chi2 ; "Number_of_iterations" ; $iterdone)
  if ($chi2 > $returned) then
    fprint $out ("R2     ";$R2;"  +/-  ";$dR2; "WARNING")
  else
    fprint $out ("R2     ";$R2;"  +/-  ";$dR2)
  endif
  fprint $out ("I0     ";$I0;"  +/-  ";$dI0) 
elsif ($data_typ s= "NOE") then
  fprint $out ("NOE    ";$NOE;"  +/-  ";$dNOE)
elsif ($data_typ s= "T1") then
  fractil_95.g ($nbexp - 3)
  fprint $out ("Chi2" ; $chi2 ; "Number_of_iterations" ; $iterdone)
  if ($chi2 > $returned) then
    fprint $out ("T1     ";$T1;"  +/-  ";$dT1; "WARNING")
  else
    fprint $out ("T1     ";$T1;"  +/-  ";$dT1)
  endif
  fprint $out ("Iinf   ";$Iinf;"  +/-  ";$dIinf)
  fprint $out ("I0     ";$I0;"  +/-  ";$dI0)
elsif ($data_typ s= "T2") then
  fractil_95.g ($nbexp - 2)
  fprint $out ("Chi2" ; $chi2 ; "Number_of_iterations" ; $iterdone)
  if ($chi2 > $returned) then
    fprint $out ("T2     ";$T2;"  +/-  ";$dT2; "WARNING")
  else
    fprint $out ("T2     ";$T2;"  +/-  ";$dT2)
  endif
  fprint $out ("I0     ";$I0;"  +/-  ";$dI0)
elsif ($data_typ s= "J") then
  fractil_95.g ($nbexp - 3)
  fprint $out ("Chi2" ; $chi2 ; "Number_of_iterations" ; $iterdone)
  if ($chi2 > $returned) then
    fprint $out ("J      ";$J;"  +/-  ";$dJ; "WARNING")
  else
    fprint $out ("J      ";$J;"   +/-  ";$dJ)
  endif
  fprint $out ("T2     ";$T2;"  +/-  ";$dT2)
  fprint $out ("I0     ";$I0;"  +/-  ";$dI0)
endif
if ($data_typ s! "NOE") then
 if ($chi2 > $returned) then
    fprintf $out "WARNING, the chi2 is over the 95%% confidence limit at %.2f" $returned *
 endif
endif

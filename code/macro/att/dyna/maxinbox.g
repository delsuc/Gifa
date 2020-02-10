; maxinbox lf1 lf2 ur 1 ur2
;
; computes the value of the maximum data point in the defined
; box. value is returned in $returned 
;
; see also : SUMREC
;
set lf1 = $_
set lf2 = $_
set ur1 = $_
set ur2 = $_
set max = 0.0
; print ($lf1;$lf2;$ur1;$ur2)
for i = $lf1 to $ur1
  for j = $lf2 to $ur2
     if (abs(val2d($i,$j)) > abs($max)) set max = (val2d($i,$j))
  endfor
endfor
return $max

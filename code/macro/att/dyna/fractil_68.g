;  fractil_68 nb_degre_of_freedom
;
; gives the 68% confidence limit on Chi2

set n = $_
if ($n == 1) then
  return 1
elsif ($n == 2) then
  return 2.3
elsif ($n == 3) then
  return 3.53
elsif ($n == 4) then
  return 4.72
elsif ($n == 5) then
  return 5.89
elsif ($n == 6) then
  return 7.04
else
  return ($n*(1-2/(9*$n)+0.4741*sqrt(2/(9*$n)))^3)
endif
  print ($n*(1-2/(9*$n)+0.4741*sqrt(2/(9*$n)))^3)

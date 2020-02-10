; percentile nb_degre_of_freedom
;
; gives the 95% confidence limit on Chi2

set n = $_
if ($n == 1) then
  return 3.84
elsif ($n == 2) then
  return 5.99
elsif ($n == 3) then
  return 7.81
elsif ($n == 4) then
  return 9.49
elsif ($n == 5) then
  return 11.1
elsif ($n == 6) then
  return 12.6
elsif ($n == 7) then
  return 14.1
elsif ($n == 8) then
  return 15.5
elsif ($n == 9) then
  return 16.9
elsif ($n == 10) then
  return 18.3
else
  return ($n*(1-2/(9*$n)+1.6449*sqrt(2/(9*$n)))^3)
endif

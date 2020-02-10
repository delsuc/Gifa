; percentile nb_degre_of_freedom
;
; gives the 95% confidence limit on Chi2

set n = $_
if ($n == 1) then
  set returned := 3.84
elsif ($n == 2) then
  set returned := 5.99
elsif ($n == 3) then
  set returned := 7.81
elsif ($n == 4) then
  set returned := 9.49
elsif ($n == 5) then
  set returned := 11.1
elsif ($n == 6) then
  set returned := 12.6
elsif ($n == 7) then
  set returned := 14.1
elsif ($n == 8) then
  set returned := 15.5
elsif ($n == 9) then
  set returned := 16.9
elsif ($n == 10) then
  set returned := 18.3
else
  set returned := ($n*(1-2/(9*$n)+1.6449*sqrt(2/(9*$n)))^3)
endif

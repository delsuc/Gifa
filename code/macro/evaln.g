; evaln.g
; graphic interface for the EVALN command.
;
; see also : EVALN

  ; 1st check itype
if ($dim == 1) then
	set it = $itype_1d
elsif ($dim == 2) then
	set it = $itype_2d
elsif ($dim == 3) then
	set it = $itype_3d
endif
if ($it != 0 ) \
  alert "WARNING, measuring the noise level on a complex data-set makes sense only in the time domain (FID)"

alert 'Select an empty area of the data-set THEN click on Ok'
if ($dim == 1) then
  print ($zone[1] ; $zone[2] ; $zone[3] ; $zone[4])
  evaln $zone[2] $zone[4]
; modif Terez 15 mai 1997
;  evaln $zone[1] $zone[2]
else
  evaln $zone[1] $zone[2] $zone[3] $zone[4]
endif


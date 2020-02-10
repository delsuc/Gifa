; integ_att
;
; calls quant1pk which
; builds a dialog box permitting to integrate a peak along a dataset list
; in the data base.
    
gm/test2d
alert "Click on the peak you want to integrate"
point_clear
monopoint
set f1 = (itop($pointy[1],2,1))  set f2 = (itop($pointx[1],2,2))

print ($f1 ; $f2)
find att 2 $f1 $f2
if ($pk_fnd_dst < $tol_click ) then
  show_att $pk_fnd
  quant1pk.g 
else
  alert ("No peak found within"; $tol_click; "ppm")
  return 0
endif



; linefitd.g
; graphic interface for the linefitd command.
; permit to fit only one selected peak
;
; see also : linefit.g LINEFITD LINEFIT

if ($dim==3) error "Not implemented yet!"
set ss = $scolor
refmacro 1
showpeaks
print "click on the peak you want to fit"
monopoint
set i = 2  set pic = 1
if ($dim==1) then
  set d = (abs($pk1d_f[1]-$pointx[1]))
  for i = 2 to $npk1d
    set tt = (abs($pk1d_f[$i]-$pointx[1]))
    if ($tt < $d) set pic = $i set d = $tt
    if ($d == 0) goto fin
  endfor
else
  set d = (abs($pk2d_f2f[1]-$pointx[1])+abs($pk2d_f1f[1]-$pointy[1]))
  for i = 2 to $npk2d
    set tt = (abs($pk2d_f2f[$i]-$pointx[1])+abs($pk2d_f1f[$i]-$pointy[1]))
    if ($tt < $d) set pic = $i set d = $tt
    if ($d == 0) goto fin
  endfor
endif
=fin
if ($scolor < 6) then
        scolor ($scolor+1)
else
        scolor 2
endif
showpeak $pic
scolor $ss

dialogbox ('LineFit peak #' // $pic) \
   "Choose line shape" enum "Lorentz,Gauss" p % \
   "Number of iteration" int l_iter $iter \
    *
iter $l_iter
if ($dim == 1) then
   linefitd $pic $p yes no yes 0   ; first do not allow peak to move
   linefitd $pic $p yes yes yes 0 
else
   linefitd $pic $p yes no yes no yes 0
   linefitd $pic $p yes yes yes yes yes 0
endif

if (($dim == 1 & $disp1d!=0)  |  ($dim == 2 & $cdisp2d!=0)) \
     clear 0 show linefit clear 1


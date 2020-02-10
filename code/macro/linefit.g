; linefit.g
; graphic interface for the linefit command.

dialogbox LineFit \
   "Choose line shape" enum "Lorentz,Gauss" p % \
    *
linefit $p
if (($dim == 1 & $disp1d!=0)  |  ($dim == 2 & $cdisp2d!=0)) \
     clear 0 show linefit clear 1


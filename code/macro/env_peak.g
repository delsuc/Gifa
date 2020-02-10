;set up a menu for Peak picking
; 
; see also : button.g startup.g BUTTONBOX


buttonbox 'Peaks' \
   'Eval noise' 'evaln.g' \
   'Peak pick' peak_pick \
   'Add peaks' 'point_cross point->pk showpeaks' \
   'Rem peak' pkrmi \
   'Rem peaks' pkrmz \
   Pkclear 'gm/al_yesno "Ok to remove all peaks?" if ($returned s= "yes") pkclear ref' \
   separator \
   'Rem peaks along diag' pkrm_diag \
   'Peak symetrize' pksym \
   'Peak projection' pkproj \
   separator \
   'SUMREC all peaks' pksumrec \
   'Integ ' 'integ %%' \
   'Show Amoeba' 'Show Amoeba' \
   separator \
   'Show Peaks' 'refpeaks 1' \
   'Hide Peaks' 'refpeaks 0' \
   'PlotPeaks' plotpeaks  \
   separator \
   'Linefit' linefit.g \
   'Linefit one peak' linefitd.g \
   'Show linefit' 'clear 0 show linefit clear 1' \
   Show_fit show_fit \
   Residue 'put data get linefit mult -1 adddata print "Type GET DATA to retreive your data"' \
   separator \
   'PkList' 'pklist %%' \
   PkRead "dialogbox 'Read Peaks' 'Enter file name' message ' '  file _f $pkname * pkread $_f yes ref unset _f" \
   PkWrite "dialogbox 'Write Peaks' 'Enter file name' message ' '  file _f $pkname * pkwrite $_f unset _f" \
   *



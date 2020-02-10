;set-up a menu for MaxEnt
; 
; see also : button.g startup.g BUTTONBOX


buttonbox MaxEnt \
   'Prepare Data' gm/set_region \
   'Eval noise' 'evaln.g zoom 0' \
   'Deconvolution fct...' gm/set_deconv \
   separator \
   Iterations... iter \
   separator \
   "Start iteration" maxent \
   '10 more' "iter (%+10) maxentcont" \
   Restart 'iter 10 get data' \
   separator \
   "Remove background..." gm/rem_back \
   *


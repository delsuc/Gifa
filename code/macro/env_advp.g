; GUI for the advanced processing technics
; 
; see also : button.g startup.g BUTTONBOX

buttonbox 'Advanced proc' \
   'BCorr linear' 'gm/bcorr_x 1' \
   'BCorr Spline' 'gm/bcorr_x 2' \
   'BCorr Polyn.' 'gm/bcorr_x 3' \
   separator \
   '1D Hilbert' 'iftbis ft' \
   '1D invHilb' 'ift ftbis' \
   separator \
   '2D Hilbert F2' 'gm/test2d iftbis f2 ft f2' \
   '2D Hilbert F1' 'gm/test2d iftbis f1 ft f1' \
   '2D Hilbert F12' 'gm/test2d iftbis f12 ft f12' \
   '2D invHilb F2' 'gm/test2d ift f2 ftbis f2' \
   '2D invHilb F1' 'gm/test2d ift f1 ftbis f1' \
   '2D invHilb F12' 'gm/test2d ift f12 ftbis f12' \
   separator \
   'DMX phase' dmxphase \
   'DMX clean' dmxclean \
   'Remove H2O' 'rem_h2o' \
   *

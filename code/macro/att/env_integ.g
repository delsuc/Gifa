; set-up the integration module
; 
;

buttonbox "Integration" \
   "Help (Short)" help_intana \
   separator \
   'Single data-set integration tool' integ_att \
   'Choose the calibration intensities' calib_integ \
   'Write build_up constraints file' cnst_buildup \
   'Write qualitative constraints file' cnst_qualit \
   separator \
   'Copy db to a peak file (slow!)' att->fil \
   "Read peak file" "dialogbox Read 'Enter file name' message ' '  file _f $name * pkread $_f unset _f" \
   'PkList' 'pklist %%' \
   'ShowPeaks' showpeaks \
   separator \
   'Eval noise' 'evaln.g' \
   'Integ ' 'alert ("Integ will erase the current amoeba: do you want to continue ?") integ %%' \
   'Show Amoeba' 'Show Amoeba' \
   'Modify Amoeba' mdfamb \
    'Read Peaks/Amoeba' "dialogbox 'Read Amoeba' 'Enter file basename' message ' '  file _f $pkname * mskread $_f unset _f" \
   'Save Peaks/Amoeba' saveamb \
   separator \
   'Multiple integration tool' mult_integ \
   'Show integration curve' show_curve \
   'Peak movie' film_form \
   *

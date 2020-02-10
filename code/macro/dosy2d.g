; dosy2d.g
;
; this is a Graphic Interface to the dosy2d macro
;
; file will be written at the end of the processing
;
; see also : dosy2d dosy3d dosy3d.g

formbox '2D Dosy processing' \
  'dosy2d $type $proc $size $thres write $file write_param_maxent $file sh ("mv result_dosy.temp"; $file//".result") ' \ 
  'Experiment type' enum 'tabulated,regular' type % \
  'Processing' enum 'MaxEnt,Fit,Fit_2_comp' proc % \
  noreturn 'Size in diffusion dim' int size (2*$si1_2d) \
  'Noise threshold' real thres 10 \
  noreturn '(Only points above this level will be processed)' message \
  'Count' action "count_2d_dosy $thres" \
  noreturn 'the number of colums to process' message \
  'Output file name' file file ($name // '.dosy') \
  separator \
  'Help on dosy' action 'help dosy2d' \
  *


; dosy3d.g
;
; this is a Graphic Interface to the dosy3d macro
;
; file will be written at the end of the processing
;
; see also : dosy2d dosy3d dosy2d.g

if (!$arg) then
formbox '3D Dosy processing' \
  'dosy3d.g $type $proc $size $thres $filein $fileout ' \
  'Input file  name' file filein $name noreturn \
  'Output file name' file fileout ($name // '.dosy') \
  separator \
  'Experiment type' enum 'tabulated,regular' type % \
  'Processing' enum 'MaxEnt,Fit,Fit_2_comp' proc % \
  noreturn 'Size in diffusion dim' int size (2*$si1_2d) \
  'Noise threshold' real thres 10 \
  noreturn '(Only points above this level will be processed)' message \
  'Count' action "count_3d_dosy $thres $filein" \
  noreturn 'the number of colums to process' message \
  separator \
  'Help on dosy' action 'help dosy3d' \
  *

else
  set type = $_
  set proc = $_
  set size = $_
  set thr = $_
  set filein = $_
  set fileout = $_
  dosy3d $filein $type $proc $size $thr $fileout
  write_param_maxent $fileout
  sh ('mv result_dosy.temp'; $fileout // '.result')
endif


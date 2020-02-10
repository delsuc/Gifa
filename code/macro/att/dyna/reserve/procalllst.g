; procalllst.g $data_typ $datalst $noise_coef
;
; quantify all the assignment entries on a list of data-sets
;
; Data-sets list is the name of the file which describes the relaxation data-sets
;    it contains the name of the nmr data-sets, and the duration of the varying delay
; 'Type of data' permits to choose the kind of processing to be performed
;     R1  : T1 relaxation data   A*exp(-t*R1) + B
;     T1  : T1 relaxation data   A*exp(-t/T1) + B
;     R2  : T2 relaxation data   A*exp(-t*R2)
;     T2  : T2 relaxation data   A*exp(-t/T2)
;     noe : (with saturation)/(without saturation) 
;     J 
;     note that R1 - R2 are MUCH faster than T1 - T2 fit !
; 'error bar weight' permits to lobally modify the error bars of the peak integrals
;   by multiplying the with a scalar.

refmacro 0
Print "Current project :"
pwd

;if ($att['largest'] == 0) error "Database Empty !"
dim 2
;if ($zoom == 0) error "Command not available in full ZOOM mode"

if (!$arg) then

  formbox 'Processing tool for Dynamics Analysis' \
   'procalllst.g ("processing/"//$listfile) $dt_tp $coef ' \
   'Data-sets list' string listfile % \
   'Type of data' enum 'R1,R2,NOE,T1,T2,J' dt_tp data_typ \
   'error bar weight' cursor 0.5 10 1 coef 1.0 \
   *

else

  set liste = $_
  set data_typ := $_
  set coef = $_

; store the current zoom window
  set f1min = (itop($zoom_2d[3],2,1))
  set f2min = (itop($zoom_2d[4],2,2))
  set f1max = (itop($zoom_2d[1],2,1))
  set f2max = (itop($zoom_2d[2],2,2))

; determine the number of entries in the selected zoom window
  set u = 0
  foreach i in att within 2 $f1min $f2min $f1max $f2max
   if ($i s! "LARGEST") then
      set u = (%+1)
   endif
  endfor

; initialize 'inprogress'
  initinprogress $u
  set uu = 0

; open output file
  set out = ($liste//"_proc")
  open $out
  set tmp = ('tmp' // int(1000000*$random))
  sh ('pwd >'; $tmp)
  open $tmp
  set pwd = <$tmp
  close $tmp
  sh ('/bin/rm -f';$tmp)
  fprint $out ("# Project :"; $pwd)
  fprint $out ("# Data-set list" ; $liste)
  fprint $out ("# Error bars weigthed by :"; $coef)
  fprint $out ("# Type of data" ; $data_typ)
  fprint $out "# Error bars at 68.3% (+/- sigma)"

; open input file containing the result of integration
  set inp = ($liste//"_integ")
  open $inp
; main loop
  set i = 1
  set line = <$inp
  while (!eof($inp))
    if (head(tail($line)) s= "Number_of_experiments:") then
      set nbexp := (tail(tail($line))) 
      goto process
    endif
    set line = <$inp
  endwhile
  error "This is not an integration file"

=process
; write header
  if (($data_typ s= "R1") | ($data_typ s= "T1") | ($data_typ s= "J")) then
      fractil_95.g ($nbexp - 3)
      set fractil = $returned
  elsif ($data_typ s= 'NOE') then
      set fractil = 'Non relevant'
  else
      fractil_95.g ($nbexp - 2)
      set fractil = $returned
  endif
  fprint $out ("# Warning on chi2 are for value over"; $fractil; "(95% confidence)")
  fprint $out "# RESNUM NAME VALUE VALUE_ERROR Chi2"


  while (!eof($inp))
    set uu = (%+1) inprogress $uu
    if (head($line) s= "Peak") then
      set dbno = (head(tail($line)))
      readcurve.g $nbexp $inp
      if ($returned == 1) then
        goto fin
      endif
      proc1pk.g $data_typ $coef
      writproc.g $data_typ $out $dbno
    endif
    set line = <$inp
  endwhile

=fin
  
  close $out
  close $inp
  inprogress $att["LARGEST"]

  set x1 = (ptoi($f1min,2,1))
  set x2 = (ptoi($f2min,2,2))
  set x3 = (ptoi($f1max,2,1))
  set x4 = (ptoi($f2max,2,2))
  zoom 1 $x3 $x4 $x1 $x2

print " "
print ("Processing results are found in:"; $out)
endif

quit


; procalllst.g $data_typ $datalst
;
; quant1pklst.g 110 datalst Sumrec Baseline 8002 0.8 "57 774 168 956"
; quantify all the assignment entries on a list of data-sets
;

refmacro 0
Print "Current project :"
pwd

;if ($att['largest'] == 0) error "Database Empty !"
dim 2
;if ($zoom == 0) error "Command not available in full ZOOM mode"

if (!$arg) then

  formbox 'Processing tool for Dynamics Analysis' \
   'procalllst.g ("processing/"//$listfile) $dt_tp ' \
   'Data-sets list' string listfile % \
   'Type of data' enum 'R1,R2,NOE,T1,T2,J' dt_tp data_typ \
   *

else

  set liste = $_
  set data_typ := $_

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
  fprint $out ("# Type of data" ; $data_typ)
  fprint $out ("# Error bars at 68.3% (+/- sigma)")

; open input file containing the result of integration
  set inp = ($liste//"_integ")
  open $inp
; main loop
  set i = 1
  set line = <$inp
  while (!eof($inp))
    set uu = (%+1) inprogress $uu
    if (head(tail($line)) s= "Number_of_experiments") then
      set nbexp := (tail(tail($line))) 
    endif
    if (head($line) s= "Peak") then
      set dbno = (head(tail($line)))
      set returned := 0
      readcurve.g $nbexp $inp
      if ($returned == 1) then
        goto fin
      endif
      proc1pk.g $data_typ 
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


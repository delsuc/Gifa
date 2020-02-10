; quantalllst.g $liste $errmeth $intmeth $homoth $zone
;
; quant1pklst.g 110 datalst Sumrec Baseline 8002 0.8 "57 774 168 956"
; quantify all the assignment entries on a list of data-sets
;

;if ($att['largest'] == 0) error "Database Empty !"
dim 2
;if ($zoom == 0) error "Command not available in full ZOOM mode"

if (!$arg) then

  formbox ('Integration/processing of entry '//$pk_fnd) \
   ('quant1pk.g ("processing/"//$listfile) $errmeth $intmeth $coef $dt_tp';$pk_fnd) \
   'Data-sets list' string listfile % \
   'Error evaluation' enum 'Baseline,Multiexp' errmeth 'Baseline' noreturn \
   'error bar weight' cursor 0.5 10 1 coef 1.0 \
   'Integration method' enum 'Sumrec,MaxInBox' intmeth 'MaxInBox' noreturn \
   'Data type' enum 'R1,R2,NOE,T1,T2,J' dt_tp % \
   'Show peak' action ('show_att';$pk_fnd) \
   *

else

  set liste = $_
  set errmeth = $_
  set intmeth = $_
  set coef = $_
  set data_typ := $_
  set peak = $_
  set homoth = 1.0

  if ($errmeth s= 'Baseline' & $intmeth s= 'Sumrec') \
     alert "Warning, with 'Baseline and Sumrec' error bars will be inaccurate"
; store the current zoom window
  set stoname = (tailx($name,"/"))
  set f1min = (itop($zoom_2d[3],2,1))
  set f2min = (itop($zoom_2d[4],2,2))
  set f1max = (itop($zoom_2d[1],2,1))
  set f2max = (itop($zoom_2d[2],2,2))

; eval noise
  if ($errmeth s= 'Baseline') then
    zoom 0 
    alert 'First select an empty area to evaluate noise of each data-set, THEN click on Ok'
    set zonebr = ($zone[1]//"/"//$zone[2]//"/"//$zone[3]//"/"//$zone[4])
  else
    set zonebr = " "
  endif

; main processing
    
    set bruit = 0
    if ($errmeth s= "Multiexp") then
    bruit1pklst.g $peak $liste $intmeth $homoth $zonebr
      set bruit = $returned
    endif
    quant1pklst.g $peak $liste $intmeth $errmeth $bruit $homoth $zonebr
    dim 1 chsize $nbexp
    for s = 1 to $nbexp
      setval $s ($integ[$s])
    endfor
    refmacro 1 zoom 1 1 $nbexp refmacro 0
    
; process the obtained curve
    proc1pk.g $data_typ $coef

if ($data_typ s= "R1") then
  fractil_95.g ($nbexp - 3)
elsif ($data_typ s= "R2") then
  fractil_95.g ($nbexp - 2)
elsif ($data_typ s= "T1") then
  fractil_95.g ($nbexp - 3)
elsif ($data_typ s= "T2") then
  fractil_95.g ($nbexp - 2)
elsif ($data_typ s= "J") then
  fractil_95.g ($nbexp - 3)
endif 

  if ($chi2 > $returned) then
    print ("WARNING, Chi2 is over the 95% confidence limit at:";$returned)
  endif

  read ("spectra/"//$stoname)
  set x1 = (ptoi($f1min,2,1))
  set x2 = (ptoi($f2min,2,2))
  set x3 = (ptoi($f1max,2,1))
  set x4 = (ptoi($f2max,2,2))

  if ($data_typ s! 'NOE') then
    dim 1
    zoom 0 
    showfit1pk.g $data_typ   
    dim 2
  endif

  zoom 1 $x3 $x4 $x1 $x2

endif

quit


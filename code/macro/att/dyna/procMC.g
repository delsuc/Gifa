; procMC.g $datalst $data_typ $nb_MC_iter  $meth $file_switch
;
; quant1pklst.g 110 datalst Sumrec Baseline 8002 0.8 "57 774 168 956"
; quantify all the assignment entries on a list of data-sets
;

refmacro 0
Print "Current project :"
pwd

;if ($att['largest'] == 0) error "Database Empty !"
if ($dim != 2) error "Command available in 2D only !"
;if ($zoom == 0) error "Command not available in full ZOOM mode"

if (!$arg) then

  formbox 'Monte-Carlo tool for Dynamics Analysis' \
   'procMC.g ("processing/"//$listfile) $data_typ $coef $MC $meth $stt' \
   'Data-sets list' string listfile % \
   'Type of data' enum 'R1,R2' data_typ 'R1' noreturn \
   'error bar weight' cursor 0.5 10 1 coef 1.0 \
   'Number of MC iteration' int MC 100 noreturn \
   'MC method' enum 'data,synthetic' meth synthetic \
   'Create detailed file (big)' enum 'yes,no' stt % noreturn \
   'Write batch file' action 'procMC.g ("processing/"//$listfile) $data_typ $coef $MC $meth $stt write' \
   *

else

  set liste = $_
  set data_typ := $_
  set coef = $_
  set MC = $_
  set sc = 1
  set meth = $_
  set stt = $_

  if ($arg) then        ; should be 'write' used for batch file writing
    set tt = $_
    if ($tt s! 'write') error "No addiional argument allowed"
    dialogbox 'Enter Filename' \
       'Enter batch Filename' string file 'batch_file' *
    open $file
    fprint $file ";===================================   MC run   ===================================" 
    fprint $file  ";note: you can start this MC run in batch"
    fprint $file  ";no need to have the assignment module loaded (nor even graphic)"
    fprint $file  ";do the following command in the current project directory"
    fprint $file (";gifa <"; $file; "> log_file &")
    fprint $file " "
    fprint $file "setpath ('/usr/local/gifa/macro/att/dyna /usr/local/gifa/macro/att';$gifapath)"
    fprint $file ("procMC.g"; $liste; $data_typ; $coef; $MC; $meth; $stt)
    fprint $file "exit y"

    close $file
    alert ($file; "successfully written")
    exit
  endif
    

  print "===================================   MC run   ==================================="   
  print "note: you can start this MC run in batch with the following Gifa commands:"
  print "     setpath ('/usr/local/gifa/macro/att/dyna /usr/local/gifa/macro/att';$gifapath)"
  print ("     procMC.g"; $liste; $data_typ; $coef; $MC; $meth; $stt)
  print "     in the project directory"
  print "no need to have the assignment module loaded (nor even graphic)"
  print "=================================================================================="

    set first = (1 == 1)
    
    set integ =  ($liste // '_integ')
    open $integ

    set out = ($liste // '_MC')
    open $out

    if ($stt s= 'yes') then
      set stat = ($liste // '_stat')
      open $stat
    endif

  set tmp = ('tmp' // int(1000000*$random))
  sh ('pwd >'; $tmp)
  open $tmp
  set pwd = <$tmp
  close $tmp
  sh ('/bin/rm -f';$tmp)
  fprint $out ("# Project :"; $pwd)
  fprint $out ("# Data-set list:" ; $liste)
  fprint $out ("# Type of data:" ; $data_typ)
  fprint $out ("# Monte-Carlo Evaluation, with"; $meth; "method,";$MC; "iterations.")
  if ($meth s= 'synthetic') then
    fprint $out "# parameter mean_value standard_deviation"
  elsif ($meth s= 'data') then
    fprint $out "# parameter min_value mean_value max_value"
  endif

    if ($stt s= 'yes') then
  fprint $stat ("# Project :"; $pwd)
  fprint $stat ("# Data-set list:" ; $liste)
  fprint $stat ("# Type of data:" ; $data_typ)
  fprint $stat ("# Monte-Carlo Evaluation, with"; $meth; "method,";$MC; "iterations.")
    endif

    set line = <$integ          ; find $nbexp
    while (index($line,"Number_of_experiments")==0)
        set line = <$integ
    endwhile
    set nbexp := (tail(tail($line)))

    while (!eof($integ))          ; on integ file thru all peaks
      while (head($line) s! "Peak")
         set line = <$integ
         if (eof($integ)) goto break    ; break on eof
      endwhile
        set dbno = (head(tail($line)))
        set sp = (head(tail(tail(tail($att[$dbno])))))
        set aa = (head(tail(tail($spin[$sp]))))
        print ("processing :"; $sys[$aa])
        set descrip = $line
        readcurve.g $nbexp $integ
        if ($returned == 1) then
          goto fin
        endif

        proc1pk.g $data_typ  $coef   ; initial values
        set chi2exp = $chi2

; compute 95% confidence level
      if ($data_typ s= "R1") then
        set freed =  ($nbexp - 3)
      elsif ($data_typ s= "R2") then
        set freed =  ($nbexp - 2)
      endif
      fractil_95.g $freed
      set chi2_95 = $returned

      if ($meth s= 'synthetic') then    ; compute first parameters
        if ($first) then        ; 1st time, create evaluation file
          set tmp = ('gifa_MC' // int(100000*$random))
          open $tmp
          fprint $tmp ';temporary file created by showexp'
          fprint $tmp 'set x = $_'
          fprint $tmp ('return (' // $exp // ')' )
          close $tmp
          set first = ( 1 == 0)
        endif

        for k = 1 to $nbexp
         @($tmp) $paramx[$k]
         set integ_base[$k] = $returned
         set error_base[$k] = $error[$k]
        endfor
        set chi2_test = $chi2_95   ; will keep within 95% confidence
      elsif ($meth s= 'data') then
        for k = 1 to $nbexp
         set integ_base[$k] = $integ[$k]
         set error_base[$k] = $error[$k]
        endfor
        if ($data_typ s= "R1") then
          set mini = $R1
        elsif ($data_typ s= "R2") then
          set mini = $R2
        endif
        set maxi = $mini
        fractil_68.g $freed
        set chi2_test = $returned   ; will keep within 68% confidence
      endif

      set Rmc = 0
      set Rmc2 = 0
      set chi2ok = 0     ; to check the chi2 distribution

    if ($stt s= 'yes') fprint $stat $descrip
    
      for i = 1 to $MC          ; for all realization
          if ($meth s= 'synthetic') then
            for k = 1 to $nbexp
             set integ[$k] = ($integ_base[$k]+$randomg*$error_base[$k]*$sc)
             set error[$k] = ($error_base[$k]*$sc)
            endfor
          elsif ($meth s= 'data') then
            for k = 1 to $nbexp
             set integ[$k] = ($integ_base[$k]+$randomg*$error_base[$k]*$sc)
             set error[$k] = ($error_base[$k]*$sc)
            endfor
          endif
          proc1pk.g $data_typ  $coef
          if ($stt s= 'yes') fprint $stat ($chi2; $p1; $dp1; $p2; $dp2)
          if ($meth s= 'synthetic') then    ; keep every thing
            if ($data_typ s= "R1") then
              set Rmc =  (% + $R1)
              set Rmc2 =  (% + $R1*$R1)
            elsif ($data_typ s= "R2") then
              set Rmc =  (% + $R2)
              set Rmc2 =  (% + $R2*$R2)
            endif
              if ($chi2 <= $chi2_test) then   ; and check chi2 statistic
                set chi2ok = (%+1)
              endif
          else
            if (abs($chi2-$chi2exp)< $chi2_test) then  ; keep only chi2 within 68%
              if ($data_typ s= "R1") then
                set Rmc =  (% + $R1)
                set Rmc2 =  (% + $R1*$R1)
                set mini = (min($mini,$R1))
                set maxi = (max($maxi,$R1))
              elsif ($data_typ s= "R2") then
                set Rmc =  (% + $R2)
                set Rmc2 =  (% + $R2*$R2)
                set mini = (min($mini,$R2))
                set maxi = (max($maxi,$R2))
              endif
              set chi2ok = (%+1)
            endif
          endif
      endfor
; then compute results
      fprint $out $descrip
      if ($chi2exp > $chi2_95) then
        fprintf $out "WARNING, the experimental chi2 : %.2f is over the 95%% confidence limit at %.2f" $chi2exp $chi2_95 *
      endif
      if ($meth s= 'synthetic') then
         set mean = $MC
      elsif ($meth s= 'data') then
         set mean = $chi2ok
      endif
      set Rmc =  (%/$mean)
      set Rmc2 = (%/$mean)
      set dRmc = ( sqrt( ($mean/($mean-1)*($Rmc2 - $Rmc*$Rmc) ) )/$sc)

      if ($meth s= 'synthetic') then
          fprint $out ($data_typ;"   "; $Rmc; "  +/-  "; $dRmc)
          fprint $out (100*$chi2ok/$MC; "% of trials are below the 95 % threshold")
      elsif ($meth s= 'data') then
          fprint $out ($data_typ;"   "; $mini; $Rmc; $maxi)
          fprint $out ("statistitic for"; 100*$chi2ok/$MC; "% of trials, within the 68% chi2 boundary condition")
      endif

      set line = <$integ
    endwhile
=break
    close $out
    close $integ
    print ("MC results are in file:"; $out)
    if ($stt s= 'yes') then
        close $stat
        print ("MC details are in file;"; $stat)
    endif
    sh ('/bin/rm'; $tmp)

endif

=fin
quit


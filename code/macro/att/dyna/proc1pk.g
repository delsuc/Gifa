; proc1pk.g $data_typ $coef
;
; Fit the integration curve of one peak using fitgene for R1 and R2 
; or division for NOE
; or list of of division for dispersion
;

set data_typ = $_
set coef = $_

if (($data_typ s= "R1") | ($data_typ s= "R2") | ($data_typ s= "T1") | ($data_typ s= "T2") | ($data_typ s= "J")) then
  dim 1
  chsize $nbexp
; store the X parameters list
  for i = 1 to $nbexp
    setval $i ($paramx[$i])
  endfor
  put tab

; store the noise list
  set n = 0
  for i = 1 to $nbexp     ; 1st compute mean noise
    set n = (%+$error[$i])
  endfor
  
  noise ($coef*$n)

  for i = 1 to $nbexp
    setval $i ($n/$error[$i])
  endfor
  put window

; get the intensity list
  for i = 1 to $nbexp
    setval $i ($integ[$i])
  endfor

endif


if ($data_typ s= "R1") then

; The spin-lattice relaxation rate constant (R1) is
; determined by nonlinear least-square fitting
; to the equation : Iinf - (Iinf - I0) * exp(-R1 * t)
;
  set exp := '$p3 - ($p3-$p2)*exp(-$p1*$x)'
  set p2 := (val1d(1))
  set p3 := (val1d($si1_1d))
  set p1 := (2/$tab[$si_tab])
  iter 100
  fitgene $exp 3
  set Iinf := $p3
  set dIinf := ($dp3/2)
  set I0 := $p2
  set dI0 := ($dp2/2)
; gives the result in the same unit than the mixing time
  set R1 := $p1
  set dR1 := ($dp1/2)

; errors are divided by 2 because fitgene gives error bars at +/- 2*sigma
; and we want it here at +/- sigma

  print ("R1";$R1;"+/-";$dR1;"Iinf";$Iinf;"+/-";$dIinf;"I0";$I0;"+/-";$dI0) 



elsif ($data_typ s= "R2") then
; The spin-spin relaxation rate constant (R2) is
; determined by nonlinear least-square fitting
; to the equation : I(t) = I0 * exp(-R2 * t)
;
  set p1 := (val1d(1))
  set p2 := (12/$tab[$si_tab])
  iter 100
  set exp := '$p1 * exp(-$p2*$x)'
  if (head(tail($version)) < 4.322) then
    fitgene $exp 2
  else
    fitexp 1
  endif    
  set I0 := $p1
  set dI0 := ($dp1/2)
; gives the result in seconds !!
  set R2 := $p2
  set dR2 := ($dp2/2)
  
  print ("R2";$R2;"+/-";$dR2;"I0";$I0;"+/-";$dI0)



elsif ($data_typ s= "NOE") then

 if (($nbexp != 2) | ($paramx[1] s! "with") | ($paramx[2] s! "without")) then
    alert ("Wrong data type for this peak")
  else
    set NOE := ($integ[1]/$integ[2])
    set tmp1 = ($coef*$error[1])
    set tmp2 = ($coef*$error[2])
    set dNOE := ( sqrt( ($tmp1/$integ[2])^2 + ($tmp2*$integ[1]/($integ[2]^2))^2 ) )
  endif

  print ("NOE";$NOE;"+/-";$dNOE)

elsif ($data_typ s= "disp") then

   for i = 1 to $nbexp
        if ($paramx[$i] == 0) set ix0 = $i   goto nu_zero
   endfor
   error "No reference value (nu=0) for dispersion data"
=nu_zero

set r2cpmg_list := " "
   for i = 1 to $nbexp    
        if ($i != $ix0) then
                set ratio = ($integ[$ix0]/$integ[$i])
                if ($ratio <= 0) then
                        print ("problem with nu="; $paramx[$i]; "log of negative value." )
                        set r2cpmg = "NaN"
                        set dr2cpmg = "NaN"
                else
                        set r2cpmg = ((1/$Tconstant)*log($ratio))
                        set dr2cpmg = ((1/$Tconstant)*($coef*$error[$ix0]/$integ[$ix0] + $coef*$error[$i]/$integ[$i]))
                endif
		sprintf "%s\t%.3f\t%.3f" $r2cpmg_list $r2cpmg $dr2cpmg *
                set r2cpmg_list := $returned
        endif
   endfor
   print ("disp";$r2cpmg_list)


  
elsif ($data_typ s= "T1") then

; The logitudinal relaxation time (T1) is
; determined by nonlinear least-square fitting
; to the equation : Iinf - (Iinf - I0) * exp(-t / T1)
;
  set exp := '$p3 - ($p3-$p2)*exp(-$x/$p1)'
  set p2 := (val1d(1))
  set p3 := (val1d($si1_1d))
  set p1 := ($tab[$si_tab]/2)
  iter 100
  fitgene $exp 3
  set Iinf := $p3
  set dIinf := ($dp3/2)
  set I0 := $p2
  set dI0 := ($dp2/2)
; gives the result in the same unit than the mixing time
  set T1 := $p1
  set dT1 := ($dp1/2)

  print ("T1";$T1;"+/-";$dT1;"Iinf";$Iinf;"+/-";$dIinf;"I0";$I0;"+/-";$dI0)



elsif ($data_typ s= "T2") then
; The transversal relaxation time (T2) is
; determined by nonlinear least-square fitting
; to the equation : I(t) = I0 * exp(-t / T2)
;
  set exp := '$p2 * exp(-$x/$p1)'
  set p2 := (val1d(1))
  set p1 := ($tab[$si_tab]/12)
  iter 100
  fitgene $exp 2
  set I0 := $p2
  set dI0 := ($dp2/2)
; gives the result in secondes !!
  set T2 := $p1
  set dT2 := ($dp1/2)
;
  print ("T2";$T2;"+/-";$dT2;"I0";$I0;"+/-";$dI0)



elsif ($data_typ s= "J") then
; The vicinal coupling constant (J) is 
; determined by nonlinear least-square fitting
; to the equation :
; I(t) = I(0)*cos(pi.J.2d) * exp(-2d/T2)   selon Jorge SANTORO
;
; I(t) = A[cos(pi.J.t1)cos(pi.J.t2)-0.5(sin(pi.J.t1)sin(pi.J.t2)].exp(-t2/T2)
; selon Billeter et al., J. Bio. NMR (1992)

set pi := (4*atan(1.0))

  set exp = '$p3*cos($pi*$p1*$x/500)*exp(-($x/500)/$p2)'

;  set exp = '$p3*(cos($pi*0.0052*$p1)*cos(2*$pi*$p1*$x/1000)-0.5*sin($pi*0.0052*$p1)*sin(2*$pi*$p1*$x/1000))*exp(-(2*$x/1000)/$p2)'

; Estimate an initial value to J ($p1)
  for i = 2 to $si1_1d
     if (val1d($i)*val1d($i-1) < 0) goto found
  endfor
=found

  set p1 := (1000/(4*$tab[$i]))
  set p2 := 0.05
  set p3 := (val1d(1))
;print ('Valeur initiale:';$p1;$p3)
  iter 100
  fitgene $exp 3
  set I0 := $p3
  set dI0 := ($dp3/2)
  set T2 := $p2
  set dT2 := ($dp2/2)
  set J := $p1
  set dJ := ($dp1/2)

  print ("J";$J;"+/-";$dJ;"T2";$T2;"+/-";$dT2;"I0";$I0;"+/-";$dI0)

endif

dim 2








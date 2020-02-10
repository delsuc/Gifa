; search_spin (interactive)
;
; permits click on the data-set, and high-light the spins 
; in the db located in the align tolerance, in F1 and F2 axes
;

alert "Click at the point where you want to search for spins"

point_clear
monopoint
set f1 = (itop($pointy[1],2,1))  set f2 = (itop($pointx[1],2,2))

set tmp = ('tmp' // int(1000000*$random))
open $tmp
fprint $tmp "#PPM   Name  System Residue  Spin#  (Note)"

set fm =    "%6.3f %5s      #%3d       %3s %3d  #%4d %s"
set f1list = " "
set ss = $scolor
print ("F1 Axis")
fprint $tmp ("F1 Axis")
foreach i in spin  within 1 ($f1-$tol_align) ($f1+$tol_align) 
  if ($scolor <6) then
    scolor (%+1)
  else
    scolor 1
  endif
  show_spin $i
  set sp = ($spin[$i])
  set ppm = (head($sp))  set sp = (tail($sp))
  set name = (head($sp)) set sp = (tail($sp))
  set sys = (head($sp))  set sp = (tail($sp))
  set res_num = (head($sys[$sys])) set sp1 = (tail($sys[$sys]))
  set res_name = (head($sp1))
  fprintf $tmp $fm $ppm $name $sys $res_name $res_num $i $sp *
  print ("SPIN"; $i)
  set f1list = ($f1list ; $i)
endfor
scolor $ss

set f2list = " "
set ss = $scolor
print ("F2 Axis")
fprint $tmp ("F2 Axis")
foreach i in spin  within 1 ($f2-$tol_align) ($f2+$tol_align) 
  if ($scolor <6) then
    scolor (%+1)
  else
    scolor 1
  endif
  show_spin $i
  set sp = ($spin[$i])
  set ppm = (head($sp))  set sp = (tail($sp))
  set name = (head($sp)) set sp = (tail($sp))
  set sys = (head($sp))  set sp = (tail($sp))
  set res_num = (head($sys[$sys])) set sp1 = (tail($sys[$sys]))
  set res_name = (head($sp1))
  fprintf $tmp $fm $ppm $name $sys $res_name $res_num $i $sp *
  print ("SPIN"; $i)
  set f2list = ($f2list ; $i)
endfor
scolor $ss
close $tmp

if ($f1list s= " ") then
   alert ("No spin found in F1 axis within"; $tol_click; "ppm")
elsif ($f2list s= " ") then
   alert ("No spin found in F2 axis within"; $tol_click; "ppm")
endif

formbox "List of Spins" DO_NOTHING \
    $tmp text *
sh ("/bin/rm -f";$tmp)



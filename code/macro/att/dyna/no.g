; evalbruit.g $zone
;
; calculate the noise on one experiment according to the 
; method of noise calculation 
;

set zone = $_

set k = 0
;print ("zone" ; $zone)
set zonet = $zone
while (headx($zonet,"/") s! " ")
  set k = (%+1)
  set x[$k] = (headx($zonet,"/"))
  set zonet = (tailx($zonet,"/"))
;  print ($x[$k] ; $k ; $zonet ; $zone)
endwhile
;print ("getc" ; $x[1] ;$x[2] ;$x[3] ;$x[4])
getc $x[1] $x[2] $x[3] $x[4]
evaln 1 1 ($x[3]-$x[1]) ($x[4]-$x[2])



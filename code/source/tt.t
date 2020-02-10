iter 10

dim 1
chsize 20
one mult 20 tm $si1_1d $si1_1d
put tab
zero

set a = 100 set b = 0.3 set c = 50

for i = 1 to $si1_1d
  setval $i ($c - $a*exp(-$b*$tab[$i]))
endfor

addnoise ($a*0.01) 123

noise (%*100)

fitgene '$p3 - $p1*exp(-$p2*$x)' 3

print ('A :';$p1;'+/-';$dp1;'    (';$a;')')
print ('B :';$p2;'+/-';$dp2;'    (';$b;')')
print ('C :';$p3;'+/-';$dp3;'    (';$c;')')

# pour la moulinette
f = Fortran
p = fpp.sh
L = xlf 
moulinette : moulinette.o readtc.o writetc.o
	$L -o moulinette moulinette.o readtc.o writetc.o \
	  /usr/local/gifa/fortran/libgif.a
moulinette.o : moulinette.for sizepb.inc paramfile.inc sizeparam.inc
	$f moulinette
readtc.o : readtc.for paramfile.inc sizeparam.inc
	$p readtc
writetc.o : writetc.for paramfile.inc sizeparam.inc
	$f writetc

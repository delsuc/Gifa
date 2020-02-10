#!/bin/sh
# for FPP,  fpp.sh { -O } file_name
# -O means : to optimize
# -OO means special optimization for concurent call on our Alliant
# -G is special for gifa.for on sgi
# set -v
case $1 in
   -O)	opt=Y
	nom=$2;;
   -OO)	opt=S
	nom=$2;;
   -G)  opt=G
        nom=$2;;
   *)	opt=N
	nom=$1;;
esac
./fpp -hp -D $nom
case $opt in
   Y) f77 +U77 -c -O   $nom.f  ;;
   S) f77 +U77 -c -O   $nom.f  ;;
   N) f77 +U77 -c -g   $nom.f  ;;
   G) f77 +U77 -c -g   $nom.f  ;;
   *) echo 'error in fpp processing';;
esac
case $opt in
   Y) rm  $nom.f ;;
   S) rm  $nom.f ;;
esac

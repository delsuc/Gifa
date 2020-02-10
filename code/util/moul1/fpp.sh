#!/bin/sh
# for FPP,  fpp.sh { -O } file_name
# -O means : to optimize
# -OO means special optimization for concurent call on our Alliant
# set -v
case $1 in
   -O)	opt=Y
	nom=$2;;
   -OO)	opt=S
	nom=$2;;
   *)	opt=N
	nom=$1;;
esac
echo 'makeaix' > recomp.lis
echo $nom  >> recomp.lis
FPP
mv $nom.fpp $nom.f
case $opt in
   Y) xlf -c -pg -O   $nom.f  ;;
   S) xlf -c -pg -O   $nom.f  ;;
   N) xlf -c -g -pg  $nom.f  ;;
   *) echo 'error in fpp processing';;
esac
rm $nom.f

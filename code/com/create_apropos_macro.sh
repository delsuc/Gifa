#!/bin/sh
echo "formbox 'Apropos $1' \ "
echo "'print \" \" ' \ "
echo " Command: message \ "

#first help file
echo   1>&2
echo commands ... 1>&2
cc=0
for i in `grep -li $1 /usr/local/gifa/help/*.hlp `
do
echo $i | sed -e '1,$ s/\.hlp//' | \
       awk -F/ '{ printf" %s action \" help %s \" \\ \n",$NF,$NF}' 
    cc=`expr $cc + 1`
    if [ $cc -ne 3 ] ;then
          echo "noreturn \ "
    else
         cc=0
   fi
done

comm=$1

#the following code test if we're finishing with a noreturn
if [ $cc -ne 0 ] ;then
   echo " ' ' message \ "
fi

# then macros
echo macros ... 1>&2
echo "separator \ "
echo " Macros: message \ "
shift
cc=0
for j in $*
  do
# first grep for $comm, much faster, and removes directory entries
  list=`grep -li $comm $j/*`
  for i in $list
	do
#only those one with $comm in the leading comments
	if awk '/^;.*'$comm'/  { exit 0}    /^[^;]/ {exit 1}' $i  ;then
		echo $i | awk -F/ '{ printf" %s action \" help %s \" \\ \n",$NF,$NF}' 
		cc=`expr $cc + 1`
		if [ $cc -ne 3 ] ;then
			echo "noreturn \ "
		else
			cc=0
		fi
	fi
  done
done
echo "*"
echo ... done 1>&2
exit

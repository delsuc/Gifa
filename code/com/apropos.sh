echo " "
echo "       Commands : "
grep -li $1 /usr/local/gifa/help/*.hlp | \
    awk -F/ '{print $NF}' | sed -e '1,$ s/\.hlp//' | more
echo " "
echo "       Macros : "
# grep -li $1 /usr/local/gifa/macro/* | \
comm=$1
shift
for j in $*
do
  for i  in `grep -l $comm $j/* `
	do
	awk '/^;.*'$comm'/  { print FILENAME; exit }    /^[^;]/ {exit}'  $i |\
	  awk -F/ '{print $NF}' | more
	done
done
exit

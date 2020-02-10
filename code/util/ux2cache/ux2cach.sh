#!/bin/csh
# To help in converting from Bruker/UX-NMR data format to GIFA (cache format)
# modif MAD 29 july 1998  - added AQORDER
# modif MAD 29 aug 2000  - modified freq control, to get rid of that stupid 360 MHz
# modif MAD july 2001  - on a bug discovered by Federico Rastrelli for xWIN-NMR 3.0
# modif MAD oct 2001 - aded support for digital filtering

# set verbose
if ($#argv >= 4) then
   set direc=$1
   set out=$2
   set dim=$3
   set ityp=$4
else
   echo -n 'Bruker file directory : '
   set direc=$<
   echo -n 'Gifa output file      : '
   set out=$<
   echo -n 'Which data: 3D (3)  2D (2) or 1D (1): '
   set dim=$<
   if (($dim != 1) && ($dim != 2) && ($dim != 3)) then
      echo "Warning:error with dimension"
      exit
   endif
   echo -n 'Which is the itype (cf Gifa manual) : '
   set ityp=$<
endif

# Find the data file
if  ( -e $direc/ser ) then
	set file=$direc/ser 
else if ( -e $direc/fid ) then
	set file=$direc/fid
else
	echo "No data file found"
	exit
endif

# Find the spectrometer frequency
if  ( -e $direc/acqus ) then
        set paramf=$direc/acqus
else
   	echo -n 'What is the spectrometer frequency: '
   	set freq=$<
   	echo -n 'What is the byte ordering (0:X32 / 1:DMX): '
   	set byt=$<
endif

# postpone the reading of freq in the $paramf file after all the frequencies are known

if ($?byt == 0 ) set byt=`grep '\$BYTORDA=' $paramf | sed -e 's/##\$BYTORDA= //' `

switch ($dim)
      case 1:
        set head1="1D"
        breaksw
      case 2:
	set head1="F2 axis"
	set head2="F1 axis"
        breaksw
      case 3:
	set head1="F3 axis"
	set head2="F2 axis"
	set head3="F1 axis"
        breaksw
endsw

# Find the acquisition parameters

if  ( -e $direc/acqus ) then
	set param1=$direc/acqus 
else if ( -e $direc/acqu ) then
	set param1=$direc/acqu
endif
if ($?param1 != 0) then
	set size1d=`grep '\$TD=' $param1 | sed -e 's/##\$TD= //' `
	set freq1=`grep '\$SFO1=' $param1 | sed -e 's/##\$SFO1= //' `
	set sw1=`grep '\$SW_h=' $param1 | sed -e 's/##\$SW_h= //' `
        set DECIM=`grep '\$DECIM=' $param1 | sed -e 's/##\$DECIM= //' `
        set DSPFVS=`grep '\$DSPFVS=' $param1 | sed -e 's/##\$DSPFVS= //' `
        set DSPFIRM=`grep '\$DSPFIRM=' $param1 | sed -e 's/##\$DSPFIRM= //' `
else
	echo "No acquisition parameter found for " $head1
	echo -n " Enter size : "
	set size1d=$<
	echo -n " Enter observation frequency : "
	set freq1=$<
	echo -n " Enter spectral width in : "
	set sw1=$<
endif
set freq0=`echo $freq1| sed -e 's/\..*$//'`

if ($dim == 3 || $dim == 2) then
  if  ( -e $direc/acqu2s ) then
	set param2=$direc/acqu2s
  else if ( -e $direc/acqu2 ) then
	set param2=$direc/acqu2
  endif

  if ($?param2 != 0) then
	set size2d=`grep '\$TD=' $param2 | sed -e 's/##\$TD= //' `
	set freq2=`grep '\$SFO1=' $param2 | sed -e 's/##\$SFO1= //' `
	set sw2=`grep '\$SW_h=' $param2 | sed -e 's/##\$SW_h= //' `
  else
        echo "No acquisition parameter found for " $head2
        echo -n " Enter size : "
        set size2d=$<
        echo -n " Enter observation frequency : "
        set freq2=$<
        echo -n " Enter spectral width  : "
	set sw2=$<
  endif
  set freq02=`echo $freq2| sed -e 's/\..*$//'`
  if ($freq02 > $freq0) set freq0=$freq02
endif

if ($dim == 3) then
   echo -n "Enter AQ_ORDER 0 (3-2-1) or 1 (3-1-2) :"
   set aqorder = $<
   if  ( -e $direc/acqu3s ) then
	set param3=$direc/acqu3s 
   else if ( -e $direc/acqu3 ) then
	set param3=$direc/acqu3
   endif
  if ($?param3 != 0) then
	set size3d=`grep '\$TD=' $param3 | sed -e 's/##\$TD= //' `
	set freq3=`grep '\$SFO1=' $param3 | sed -e 's/##\$SFO1= //' `
	set sw3=`grep '\$SW_h=' $param3 | sed -e 's/##\$SW_h= //' `
  else
  	if ($aqorder == 0) then
	  echo "No acquisition parameter found for " $head3
	else
	  echo "No acquisition parameter found for " $head2
	endif
	echo -n " Enter size : "
	set size3d=$<
	echo -n " Enter observation frequency : "
	set freq3=$<
        echo -n " Enter spectral width : "
	set sw3=$<
   endif
   set freq03=`echo $freq3| sed -e 's/\..*$//'`
  if ($freq03 > $freq0) set freq0=$freq03
endif

# now decide the main freq param
if ($?freq == 0 ) then
	set freq=`grep '\$INSTRUM=' $paramf | sed -e 's/##\$INSTRUM= <//' |sed -e 's/amx//' | sed -e 's/>//' `
	if (! `echo $freq | grep '^[0-9][0-9]*$'`) set freq='200'
	if ($freq0 > $freq) set freq=$freq0
endif

echo main spectrometer frequency : $freq MHz


#
#   parameters for ux2cach are as follow :
#   in 1D F1 (F2 - F3)
#   in 2D F1 F2 (F3)   i.e. t1 - acq
#   in 3D F1 F2 F3     i.e. t1 (slowest) - t2 (intermediate) - acq

switch ($dim)
   case 1:
        ux2cach $file $byt $out $dim $ityp $freq $size1d $sw1 $freq1 1 0.0 0.0 1 0.0 0.0 $DECIM $DSPFVS $DSPFIRM
	breaksw
   case 2:
	ux2cach $file $byt $out $dim $ityp $freq $size2d $sw2 $freq2 $size1d $sw1 $freq1 1 0.0 0.0 $DECIM $DSPFVS $DSPFIRM
	breaksw
   case 3:
   	if ($aqorder == 0) then
	   ux2cach $file $byt $out $dim $ityp $freq $size3d $sw3 $freq3 $size2d $sw2 $freq2 $size1d $sw1 $freq1 $DECIM $DSPFVS $DSPFIRM
	else
	   ux2cach $file $byt $out $dim $ityp $freq $size2d $sw2 $freq2 $size3d $sw3 $freq3 $size1d $sw1 $freq1 $DECIM $DSPFVS $DSPFIRM
	endif
endsw


#!/bin/csh
# To help in converting from Varian data format to GIFA (cache format)
# modification of ux2cach.sh
# TS 13.12.99


# set verbose
   echo -n 'To convert a varian nD file to a cache nD file\n'
   echo -n 'enter the requested values at prompts.\n\n'
   echo -n 'For size enter total number of real + imaginary points.\n\n'
   echo -n 'Varian file directory : '
   set direc=$<
   echo -n 'Gifa output file      : '
   set out=$<
   echo -n 'Dimension of dataset: 3D (3) 2D (2) or 1D(1): '
   set dim=$<
   if (($dim != 1) && ($dim != 2) && ($dim != 3)) then
      echo "Warning:error with dimension"
      exit
   endif
   echo -n 'What is the itype (cf Gifa manual) : '
   set ityp=$<
   echo -n 'What is the spectrometer frequency: '
   set freq=$<

# Find the data file
if  ( -e $direc/fid ) then
	set file=$direc/fid 
else
	echo "No data file found"
	exit
endif

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

# Didn't do this because the varian procpar file is hard to figure out- 
# does not seem to save the acquisition
# parameters in a predictable fashion except for 1D datasets

# Find the acquisition parameters
#
#if  ( -e $direc/procpar) then
#	set f=$direc/procpar 
#endif

if ($dim == 1 ) then

        echo "Enter acquisition parameters for " $head1
        echo -n " Enter size : "
        set size1d=$<
        echo -n " Enter observation frequency : "
        set freq1=$<
        echo -n " Enter spectral width in Hz : "
        set sw1=$<
	echo -n " Enter spectral offset in Hz : "
        set offset1=$<
else if ($dim == 2 ) then

	echo "Enter acquisition parameters for " $head1
        echo -n " Enter size : "
        set size2d=$<
        echo -n " Enter observation frequency : "
        set freq2=$<
        echo -n " Enter spectral width in Hz : "
        set sw2=$<
        echo -n " Enter spectral offset in Hz: "
        set offset2=$<
	echo "Enter acquisition parameters for " $head2
        echo -n " Enter size : "
        set size1d=$<
        echo -n " Enter observation frequency : "
        set freq1=$<
        echo -n " Enter spectral width  : "
        set sw1=$<
        echo -n " Enter spectral offset  : "
	set offset1=$<
else 

	echo "Enter acquisition parameters for " $head1
        echo -n " Enter size : "
        set size3d=$<
        echo -n " Enter observation frequency : "
        set freq3=$<
        echo -n " Enter spectral width in Hz : "
        set sw3=$<
        echo -n " Enter spectral offset in Hz : "
        set offset3=$<
        echo "Enter acquisition parameters for " $head2
        echo -n " Enter size : "
        set size2d=$<
        echo -n " Enter observation frequency : "
        set freq2=$<
        echo -n " Enter spectral width  : "
        set sw2=$<
        echo -n " Enter spectral offset  : "
        set offset2=$<
	echo "Enter acquisition parameters for " $head3
        echo -n " Enter size : "
        set size1d=$<
        echo -n " Enter observation frequency : "
        set freq1=$<
        echo -n " Enter spectral width : "
        set sw1=$<
        echo -n " Enter spectral offset : "
        set offset1=$<
endif

   echo -n " Enter VARIAN_MODE (1 or 2) : "
   set vmode=$<

#   parameters for vn2cach are as follow :
#   in 1D F1 (F2 - F3)
#   in 2D F1 F2 (F3)   i.e. t1 - acq
#   in 3D F1 F2 F3     i.e. t1 (slowest) - t2 (intermediate) - acq

switch ($dim)
   case 1:
	 convert_varian $file $out $dim $ityp $freq \
		$size1d $sw1 $freq1 $offset1 	\
		1 0.0 0.0 0.0			\
		1 0.0 0.0 0.0			\
		1
	breaksw
   case 2:
	 convert_varian $file $out $dim $ityp $freq \
		$size1d $sw1 $freq1 $offset1	\
		$size2d $sw2 $freq2 $offset2	\
		1 0.0 0.0 0.0			\
		1
	breaksw
   case 3:
	 convert_varian $file $out $dim $ityp $freq \
		$size1d $sw1 $freq1 $offset1       \
		$size2d $sw2 $freq2 $offset2       \
		$size3d $sw3 $freq3 $offset3	   \
		$vmode
        breaksw
endsw





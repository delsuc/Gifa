C file paramfilev.inc
C
C This file holds the common for the parameters common
C to all the data-set: 1D,2D&3D.

	integer*4    dim
	real*4       frequency,max0
	real*4       empty_array1(9)
        character*16 vers,date


C  These parameters constitute the common for the 1D data-set
C  sp1d=specw of1d=offset it1d=itype of 1D data-set
C  axis1d,n1d1,n1d2: for 1D comming from 3D (2D) data-set (column)

      integer*4  sizeimage1d
      integer*4  axis1d,n1d1,n1d2,it1d
      real*4     sp1d,of1d,max1d,freq1d

C  These parameters constitute the common for the 2D data-set
C  axis2d and n2d: for 2D comming from 3D data-set (plane2d)

        integer*4  sizeimage,itype
        integer*4  si1im,si2im
        integer*4  axis2d,n2d
        real*4     specw1,offset1,specw2,offset2,max2d
        real*4     freq1,freq2

C  These parameters constitute the common for the 3D data-set

        integer*4  sizeimage3d,it3d
        integer*4  si3d1,si3d2,si3d3
        integer*4  nplane
        real*4     sp3d1,of3d1,sp3d2,of3d2,sp3d3,of3d3,
     *           max3d,freq3d1,freq3d2,freq3d3

C  One never knows: an empty array can be necessary one day

        integer   size_empty,size_tot,occupied
        parameter (size_tot=256)

C  58 is the size occupied today by the data.
        parameter (occupied=58)

        parameter (size_empty=size_tot-occupied)
        real*4     empty_array2(size_empty)

        common/paramcom/vers,date,frequency,dim,max0,
     *   empty_array1,
C 20 variables for generic parameters(1-20)
     *   freq1d,sizeimage1d,max1d,
     *   sp1d,of1d,it1d,axis1d,n1d1,n1d2,
C 9 variables for 1D (21-30)
     *   freq1,freq2,sizeimage,
     *   si1im,si2im,max2d,
     *   specw1,offset1,specw2,offset2,itype,axis2d,n2d,
C 13 variables for 2D(31-46)
     *   freq3d1,freq3d2,freq3d3,sizeimage3d,
     *   si3d1,si3d2,si3d3,
     *   max3d,sp3d1,of3d1,sp3d2,of3d2,sp3d3,of3d3,
     *   it3d,nplane,
C 16 variables for 3D(47-66)
     *   empty_array2
C this array is empty for the moment(67-256)
C "256" variables*4 in the common

        real*4     tablo(size_tot)
        equivalence(tablo(1),vers)


C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in CBS/Montpellier
C
C     Authors :       Therese Malliavin
C                     C.B.S. Faculte de Pharmacie
C                     34060 Montpellier   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.

	subroutine peakproj (error,axis,proj,tol)
c IN	: axis,proj,tol
c OUT	: error
c SIDE	: 1D peak table
c
c Load the 1D peak table with content of the 2D peak table projected along one 
c dimension
c
c input:
c pead1d(nbpic1d,pk1d): 1D peak table
c pead2d(nbpic2d,pk2d): 2D peak table
c axis: projection flag: axis = 2, projection along f2 (on f1)
c			 axis = 1, projection along f1 (on f2)
c proj: 'S' or 'M', skyline or mean projection
c tol: precision on the ppm values
c
c output:
c error: error label
c nbpic1d: modified number of 1D peaks
c
	implicit none

#include "sizebase.inc"
#include "peaksize.inc"
#include "paramfilev.inc"                  
 
	integer nbpic1d_new,i,j,axis,error
	real hz_i,hz_j,diff,tol,itohr,htoir
	real sw,off,wdth
	integer si
	character*1 proj
 
c	write(*,*)'axis',axis
	if (axis.eq.1) then
	   si = si2im
	   sw = specw2
	   off = offset2
	endif
	if (axis.eq.2) then
	   si = si1im
	   sw = specw1
	   off = offset1
	endif
	nbpic1d_new = 0
	i = 0
100	continue
	i = i + 1
c	write(*,*)'nbpic2d',nbpic2d
	if (i.gt.nbpic2d) goto 510
c	write(*,*)'si',si,'sw',sw,'off',off,'fr',fr
      if (axis.eq.1) then
          hz_i = itohr(peak2d(i,1),si,sw,off)
          wdth = peak2d(i,2)
      endif
	if (axis.eq.2) then
          hz_i = itohr(peak2d(i,4),si,sw,off)
	    wdth = peak2d(i,5)
      endif
	do j = 1,nbpic1d_new
c	   write(*,*)'peak1d(j,1)',peak1d(j,1),j
c	   write(*,*)'peak2d(i,1)',peak2d(i,1),i
c	   write(*,*)'peak2d(i,4)',peak2d(i,4),i
c	   write(*,*)'si',si,'sw',sw,'off',off,'fr',fr
	   hz_j = itohr(peak1d(j,1),si,sw,off)
	   diff = abs(hz_i - hz_j)
c	   write(*,*)'diff',diff,'ppm_i',ppm_i,'ppm_j',ppm_j,'i',i,'j',j
	   if (diff.le.tol) then 
c loading the amplitude in 1D peak table
c skyline projection
	     if ( (proj.eq.'S') .and. (peak1d(j,3).lt.peak2d(i,3)) ) then
	       peak1d(j,3) = peak2d(i,3)   
	     endif
c mean projection
	     if (proj.eq.'M') then
               peak1d(j,3) = peak1d(j,3) + peak2d(i,3)   
	     endif
	     goto 100
	   endif
	enddo
c	write(*,*)'i,j',i,j
c the peak i should be load in 1D table
	nbpic1d_new = nbpic1d_new + 1
c	write(*,*)'nbpic1d_new',nbpic1d_new
c	write(*,*)'peakmax',peakmax
	if (nbpic1d_new.gt.peakmax)  goto 500
	peak1d(nbpic1d_new,1) = htoir(hz_i,si,sw,off)
	peak1d(nbpic1d_new,2) = wdth
	peak1d(nbpic1d_new,3) = peak2d(i,3)
	peak1d(nbpic1d_new,4) = 0.0
      write(peak1d_id(nbpic1d_new),'(i12)') nbpic1d_new
      call leading(peak1d_id(nbpic1d_new))
      peak1d_id(nbpic1d_new) = peak1d_id(nbpic1d_new) // ' proj'
	goto 100
 
c Too many peaks
500	continue
	error = 1
	call gifaout('too many peaks,change parameters in MINIMAX')
	return
 
c normal exit
510	continue
	nbpic1d = nbpic1d_new
 
	return
	end

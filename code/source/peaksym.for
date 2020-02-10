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

c*************************************************************************************
	subroutine peaksym_add (error,tol)
c IN	: tol
c OUT	: error
c SIDE	: 2D peak table
c
c Symetrize the peak table by adding non symetrical values
c
c input:
c pead2d(nbpic2d,pk2d): 2D peak table
c tol: precision on the ppm values
c
c output:
c error: error label
c nbpic2d: modified number of peaks
c
	implicit none

#include "sizebase.inc"
#include "peaksize.inc"
#include "paramfilev.inc"
 
	integer nbpic_new,i,j,error
	real diff_i,diff_j,diff_ij,diff_ji,itohr,htoir
	real hzf1_i,hzf2_i,hzf1_j,hzf2_j,tol
 
	nbpic_new = nbpic2d
	i = 0
100	continue
	i = i + 1
	if (i.gt.nbpic2d) goto 510
	hzf1_i = itohr(peak2d(i,4),si1im,specw1,offset1)
	hzf2_i = itohr(peak2d(i,1),si2im,specw2,offset2)
	diff_i = abs(hzf1_i - hzf2_i)
c	write(*,*)'ppm1_i',ppm1_i,'ppm2_i',ppm2_i,'diff_i',diff_i
c if the peak is diagonal, it is ok
	if (diff_i.le.tol) goto 100
c if the peak is not diagonal, look for the symmetric
	if (diff_i.gt.tol) then
	   do j = 1,nbpic2d
	      hzf1_j = itohr(peak2d(j,4),si1im,specw1,offset1)
	      hzf2_j = itohr(peak2d(j,1),si2im,specw2,offset2)
	      diff_j = abs(hzf1_j - hzf2_j)
	      diff_ij = abs(hzf1_i - hzf2_j)
	      diff_ji = abs(hzf2_i - hzf1_j) 
	      if ( (diff_j.gt.tol) .and. (diff_ij.le.tol) .and.
     & (diff_ji.le.tol) ) goto 100
	   enddo
c the 2D peak i is no diagonal and has no symmetric peak,
c the symmetric peak should be added
	   nbpic_new = nbpic_new + 1
	   if (nbpic_new.gt.peakmax) goto 500
	   peak2d(nbpic_new,1) = htoir(hzf1_i,si2im,specw2,offset2)
	   peak2d(nbpic_new,2) = peak2d(i,5)
	   peak2d(nbpic_new,3) = peak2d(i,3)
	   peak2d(nbpic_new,4) = htoir(hzf2_i,si1im,specw1,offset1)
	   peak2d(nbpic_new,5) = peak2d(i,2)
	   call trailing( peak2d_id(i),j)
	   peak2d_id(nbpic_new) = peak2d_id(i)(1:j) // ' sym'
	   goto 100
	endif 
 
c Too many peaks
500	continue
	error = 1
	call gifaout('too many peaks,change parameters in MINIMAX')
	return
 
c normal exit
510	continue
	nbpic2d = nbpic_new
 
	return
	end
c*************************************************************************************
	subroutine peaksym_rem (tindex,szindex,error,tol)
c IN	: tol,szindex
c OUT	: error
c WORK	: tindex
c SIDE	: 2D peak table
c
c Symetrize the peak table by removing non symetrical values
c
c input:
c pead2d(nbpic2d,pk2d): 2D peak table
c tol: precision on the ppm values
c
c output:
c error: error label
c nbpic2d: modified number of peaks
c
	implicit none

#include "sizebase.inc"
#include "peaksize.inc"
#include "paramfilev.inc"
  
	integer nbpic_new,i,j,k,szindex,tindex(szindex)
	integer error,itemp
	real diff_i,diff_j,diff_ij,diff_ji,itohr
	real hzf1_i,hzf2_i,hzf1_j,hzf2_j,tol
 
	nbpic_new = nbpic2d
	i = 0
	k = 0
100	continue
	i = i + 1
	if (i.gt.nbpic2d) goto 510
	hzf1_i = itohr(peak2d(i,4),si1im,specw1,offset1)
	hzf2_i = itohr(peak2d(i,1),si2im,specw2,offset2)
	diff_i = abs(hzf1_i - hzf2_i)
c if the peak is diagonal, it is ok
	if (diff_i.le.tol) goto 100
c if the peak is not diagonal, look for the symmetric
	if (diff_i.gt.tol) then
	   do j = 1,nbpic2d
	      hzf1_j = itohr(peak2d(j,4),si1im,specw1,offset1)
	      hzf2_j = itohr(peak2d(j,1),si2im,specw2,offset2)
	      diff_j = abs(hzf1_j - hzf2_j)
	      diff_ij = abs(hzf1_i - hzf2_j)
	      diff_ji = abs(hzf2_i - hzf1_j) 
	      if ( (diff_j.gt.tol) .and. (diff_ij.le.tol) .and.
     & (diff_ji.le.tol) ) goto 100
	   enddo
c the 2D peak i has no symmetric peak; it should be removed
	   nbpic_new = nbpic_new - 1
	   k = k + 1
	   if (k.gt.szindex) goto 500
	   tindex(k) = i
	   goto 100
	endif
 
c Too many peaks to remove
500	continue
	error = 1
	call gifaout('too many peaks to remove')
	return
 
c normal exit
510	continue 
	call bsort(tindex,nbpic2d-nbpic_new) 
	itemp = nbpic2d-nbpic_new
c	do i = 1,itemp
c	   write(*,*)'tindex',tindex(i),i
c	enddo       
	do i=1,pk2d
        call cicopvect(tindex,peak2d(1,i),peak2d(1,i),nbpic2d,itemp)
        call cicopsvect(tindex,peak2d_id(i),peak2d_id(i),nbpic2d,itemp)
      enddo
        nbpic2d = nbpic_new
 
	return
	end

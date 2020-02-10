C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in CBS/Montpellier
C
C     Author          Therese Malliavin :       
C                     C.B.S. Faculte de Pharmacie
C                     34060 Montpellier   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.

c Therese Malliavin,
c
c last modif : nov 1995

	subroutine mkmask (tab,masktab,taille2,taille1,
     & verbose,tindex,factor,slope,thres,radius,signpic,zq)
c
c Determine the peak amoeba table from the peak table
c 
c tab(taille2,taille1): 2D experiment
c masktab(taille2,taille1): amoeba table
c tindex(16*radius): working area
c radius: maximum amoeba radius
c signpic: peak sign
c
c IN	: tab,masktab,taille1,taille2,factor,slope,
c IN    : thres,radius,signpic,verbose
c OUT	: peak2d
c
	implicit none

#include "sizebase.inc"
#include "peaksize.inc"

	integer radius,taille2,taille1,verbose
        integer tindex(16*radius),i,rad
	integer x0,y0,signpic,zq
	real factor,slope,thres
	real masktab(taille2,taille1)
	real tab(taille2,taille1)

	do i = 1,nbpic2d
           x0 = int(peak2d(i,1)+0.5)
           y0 = int(peak2d(i,4)+0.5)
	   masktab(x0,y0) = real(i)
	enddo

	do rad = 1,radius
           call detmsk (masktab,tab,taille1,taille2,
     & verbose,tindex,tindex((8*rad)+1),factor,
     & slope,thres,rad,signpic,zq)
	enddo

	return
	end
	

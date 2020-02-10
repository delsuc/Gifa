C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in CBS/Montpellier
C
C     Authors :       T.E.Malliavin
C                     C.B.S. Faculte de Pharmacie
C                     34060 Montpellier   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.

	subroutine finmsk (masktab,tab,taille1,taille2,
     & indxx,indxy,factor,slope,x0,y0,
     & threshold,signpic)
c
c Finish the peak amoeba determination by resolving conflicts
c 
c IN	: tab,masktab,taille1,taille2,amp,factor,slope,
c IN    : threshold,signpic
c OUT	: peak2d
c
	implicit none

#include "sizebase.inc"
#include "peaksize.inc"

	integer taille2,taille1
	integer u,signx,signy
	integer indxx(8),indxy(8),xpic,ypic
	integer x0,y0,crdx,crdy,signpic,nopic
	real factor,slope,threshold,pente
	real dsdx,dsdy,dst2,pentemin
	real masktab(taille2,taille1)
	real tab(taille2,taille1)

	pentemin = slope
	indxx(1) = x0 + 1
	indxy(1) = y0
	indxx(2) = x0 - 1
	indxy(2) = y0
	indxx(3) = x0
	indxy(3) = y0 + 1
	indxx(4) = x0
	indxy(4) = y0 - 1
	do u = 1,4
	   crdx = indxx(u) 
           crdy = indxy(u)
           if ((crdx.ge.0).and.(crdy.ge.0).and.
     & (crdx.le.taille2).and.(crdy.le.taille1).and.
     & (masktab(crdx,crdy).gt.0.0)) then
	      nopic = int(masktab(crdx,crdy))
              xpic = int(peak2d(nopic,1))
              ypic = int(peak2d(nopic,4))
	      signx = 1
	      signy = 1
              if (x0.ne.xpic) signx = int((x0-xpic)/abs(x0-xpic))
              if (y0.ne.ypic) signy = int((y0-ypic)/abs(y0-ypic))
	      dsdx = real(signx)*(tab(x0,y0) - tab(x0-signx,y0))
	      dsdy = real(signy)*(tab(x0,y0) - tab(x0,y0-signy))
	      pente = (dsdx*real(x0-xpic)) + (dsdy*real(y0-ypic))
	      dst2 = (real(x0-xpic)*real(x0-xpic)) + 
     & (real(y0-ypic)*real(y0-ypic)) 
	      pente = signpic*pente/sqrt(dst2)
	      if (pente.lt.pentemin) then
		 pentemin = pente
		 masktab(x0,y0) = nopic
	      endif
	   endif
	enddo

	return
	end
	

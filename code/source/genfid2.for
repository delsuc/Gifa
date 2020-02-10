C This file is a part of the GIFA program .
C     This software has been developped by the NMR group in GIF/Yvette
C
C     Authors : F.Billois,M.A.Delsuc
C               NMR Laboratory,
C               ICSN-CNRS
C               91190 GIF/YVETTE   FRANCE
C
C
C     This software cannot be used unless the user has been fully 
C licensed to do so form by the above laboratory.
C     This file cannot be copied,duplicated or used in any other program,
C without written permission from the authors.

	subroutine genfid2 (fid,si1,si2,tabindex,inten,
     $          depchim,largeur,borne,ntachefen,sw1,sw2)

c IN	: si1,si2,tabindex,inten,depchim,largeur,borne,ntachefen,sw1,sw2
c OUT	: fid
c
c generate a 2D fid from the spectral parameters
c
	implicit none
#include "controlc.inc"

	integer z,y,si1,si2,i1,i2
	integer ntachefen,tache,c
	real twopi

	parameter (z=2000,y=20000)
	parameter (twopi=6.283185)

	integer tabindex(y,2)

        real fid(si2,si1),inten(y),depchim(z),largeur(z)
	real borne(4),centre1,centre2,t1,dwell1,dwell2
	real am1,c1,s1,am2,c2,s2,w1t1,w2t2,t2,sw1,sw2 
c	real work(si2)

C ************** calcul des fid ***********************************************


	sw1 = borne(1) - borne(3) ! Hz
        sw2 = borne(2) - borne(4)

        dwell1 = 1.0/(2.0*sw1)   !s
	dwell2 = 1.0/(2.0*sw2)

        centre1 = (borne(1) + borne(3)) / 2.0   ! Hz
	centre2 = (borne(2) + borne(4)) / 2.0

        call prpvect(fid,0.0,si1*si2)

	do i1 = 1,si1,2
	   t1 =float(i1-1)*dwell1
	   do tache=1,ntachefen
	     w1t1=(depchim(tabindex(tache,1))-centre1)
     $            *t1*twopi
	     am1 = exp(-largeur(tabindex(tache,1))*t1)
	     c1 = cos(w1t1)*am1*inten(tache)
	     s1 = sin(w1t1)*am1*inten(tache)
		do i2 = 1,si2,2
		  t2 =float(i2-1)*dwell2
		  w2t2=(depchim(tabindex(tache,2))-centre2)
     $                  *t2*twopi 
		  am2 = exp(-largeur(tabindex(tache,2))*t2)
		  c2 = cos(w2t2)*am2
		  s2 = sin(w2t2)*am2
		  fid(i2,i1) = fid(i2,i1) + c1*c2
		  fid(i2+1,i1) = fid(i2+1,i1) + c1*s2
		  fid(i2,i1+1) = fid(i2,i1+1) + s1*c2
		  fid(i2+1,i1+1) = fid(i2+1,i1+1) + s1*s2
		enddo
            enddo
            if (control.eq.1) return
	enddo

        call mltvect(fid,fid,0.5,2*si2)

	do c=1,si1
		fid(1,c)=0.5*fid(1,c)
		fid(2,c)=0.5*fid(2,c)
	enddo

        return

	end

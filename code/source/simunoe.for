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
C
        subroutine simunoe (fid,si1,si2,it,offset1
     $  	           ,offset2,sw1,sw2,nerr)
C IN	: si1,si2,it,offset1,offset2,sw1,sw2
c OUT	: fid,nerr
c
C     This subroutine calculate the 2D spectrum from the intensities
C furnished by CORMA .
C
	implicit none

	integer y,z,si1,si2,it,nerr

	parameter (z=2000,y=20000)

        integer natomfen,ntachefen,tabindex(y,2)

	real inten(y),borne(4),depchim(z),largeur(z)
	real fid(*),sw1,sw2,offset1,offset2 
	real sw1m,sw2m,offset1m,offset2m
        real si1m,si2m

C ******************** saisie des donnees ******************************
 
	nerr=0
	si1m=si1
        si2m=si2
	offset1m=offset1
	offset2m=offset2
	sw1m=sw1
	sw2m=sw2

	call lect (natomfen,ntachefen,inten,tabindex,borne,
     $              depchim,largeur,si1,si2,nerr)

        if (nerr.ne.0)return
        if (si1.eq.0.or.si2.eq.0)then
	offset1=offset1m
	offset2=offset2m
	sw1=sw1m
	sw2=sw2m
	    return 
        endif

	offset1=borne(3)
	offset2=borne(4)

C ******************* calcul des fid  **********************************
 
 	call genfid2(fid,si1,si2,tabindex,inten,depchim,largeur,
     $              borne,ntachefen,sw1,sw2)
	it=3
	return
	end

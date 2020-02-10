C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in CBS/Montpellier
C
C     Authors :       T.E. Malliavin M.A.Delsuc
C                     C.B.S. Faculte de Pharmacie
C                     34060 Montpellier   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C

	subroutine addsimu1d (tdata,unit,error)
c IN	: unit
c INOUT	: tdata
c OUT	: error
c
c add to the current 1D data the line whose paramters are given by user.
c spectral width, frequence and data size are those currently defined
c (found in paramfilev.inc).
c
c
c input:
c sp1d: spectral width (paramfilev.inc)
c freq1d: frequency (paramfilev.inc)
c off1d: offset (paramfilev.inc)
c sizeimage1d: data size (paramfilev.inc)
c tdata(sizeimage1d): current data in the 1D buffer
c point unit: PPM, INDEX or HZ
c
c output:
c tdata(size): modified data in the 1D buffer
c
	implicit none

#include "constant.inc"	
#include "paramfilev.inc"	
#include "controlc.inc"	
	
	real itohr,ptoir,gasdev
	external itohr,ptoir,gasdev
	
	character*(*) unit
	integer error,i
	complex tdata(*)
	real ampl,loc_hz,t2l, phasel,ppml,indl
	real f, p, w, y, z, temp

	save  ampl,loc_hz,ppml,indl,t2l,phasel
	
	call message(' amplitude')
	call getreal2(ampl,error)
	if (error.ne.0) goto 62

	if (unit.eq.'HZ') then	
           call message( ' frequence')
           call getreal2(loc_hz,error)
           if (error.ne.0) goto 62
        elseif (unit.eq.'PPM') then
           call message('chemical shift')
           call getreal2(ppml,error)
           if (error.ne.0) goto 62
           temp = ptoir(ppml,sizeimage1d,sp1d,of1d,freq1d)
           loc_hz = itohr(temp,sizeimage1d,sp1d,of1d)
        elseif (unit.eq.'INDEX') then
           call message('position index')
           call getreal2(indl,error)
           if (error.ne.0) goto 62
           loc_hz = itohr(indl,sizeimage1d,sp1d,of1d)
        else
           goto 63
        endif 
        
        call message( ' linewidth (in Hz)')
        call getfreq2(t2l,sp1d,error)
        if (error.ne.0) goto 62
        if (t2l.lt.0.0) goto 61

        call message( ' phase')
        call getreal2( phasel,error)
        if (error.ne.0) goto 62
c
c multiply by two the first point of the FID
c
	tdata(1) = tdata(1)*2.0
c 
c put the nul frequency at the edges of the spectrum
c before adding a new signal
c	        
	call bruvect(tdata,tdata,sizeimage1d)	

 	f = ((loc_hz-of1d)*2*pi/sp1d) -pi
	p = phasel*pi/180.0
c
c calculate the fid
c
        y = t2l/2000.0
	do i=1,sizeimage1d/2
	   z = ampl*exp(-float(i-1)*y)
	   w = f*float(i-1)+p
	   tdata(i) = tdata(i) + z*cmplx(cos(w),sin(w))
	enddo
c
c put the nul frequency at the middle of the spectrum
c	
	call bruvect(tdata,tdata,sizeimage1d)
	it1d = 1
c
c divide by two the first point of the FID
c
	tdata(1) = tdata(1)/2.0
	return

c negative or nul t2	
61	call gifaout('t2 Should not be 0')
	error = 1
	return
	
c error in getting parameter
62	call gifaout('Error with value')
	return	
	     	
c error with unit value
63	call gifaout('Error with unit value')
	error = 1
	return	
	     	
	end




C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in CBS/Montpellier
C
C     Authors :       T.E.Malliavin M.A.Delsuc
C                     C.B.S. Faculte de Pharmacie
C                     34060 Montpellier   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C

C
	subroutine addsimu3d (tdata,unit,error)
c IN	: unit
c INOUT	: tdata
c OUT	: error
c
c add to the current 3D data the line whose parameters are given by user.
c spectral width, frequence and data size are those currently defined
c (found in paramfilev.inc).
c
c input:
c sp3d1,sp3d2,sp3d3: spectral widthes (paramfilev.inc)
c freq3d1,freq3d2,freq3d3: frequencies (paramfilev.inc)
c of3d1,of3d2,of3d3: offset (paramfilev.inc)
c sizeimage3d: data size (paramfilev.inc)
c tdata(sizeimage3d): current data in the 1D buffer
c point unit: PPM, INDEX or HZ
c
c output:
c tdata(size): modified data in the 1D bufer
c
	implicit none

#include "constant.inc"	
#include "paramfilev.inc"
#include "controlc.inc"	
	
	real itohr,ptoir,gasdev
	external itohr,ptoir,gasdev
	
	character*(*) unit
	integer error,i
	integer i1,i2,i3
	real tdata(*)
	real ampl,freql1,t2l1, phasel1,ppml1,indl1
	real freql2,t2l2, phasel2,ppml2,indl2
	real freql3,t2l3, phasel3,ppml3,indl3
	real  z
	real f1,f2,f3,p1,p2,p3,temp,y1,y2,y3
	real  w1,w2,w3,cw1,sw1,cw2,sw2,cw3,sw3,e1,e2,e3

	save ampl,freql1,freql2,freql3
	save ppml1,ppml2,ppml3,indl1,indl2,indl3
        save t2l1,t2l2,t2l3,phasel1,phasel2,phasel3
c	
c getting amplitude
c	
	call message(' amplitude')
	ampl = 1.0
	call getreal2(ampl,error)
	if (error.ne.0) goto 62
c
c getting line position
c
	if (unit.eq.'HZ') then	
           call message( ' frequence in dim 1')
           call getreal2(freql1,error)
           if (error.ne.0) goto 62
           call message( ' frequence in dim 2')
           call getreal2(freql2,error)
           if (error.ne.0) goto 62 
           call message( ' frequence in dim 3')
           call getreal2(freql3,error)
           if (error.ne.0) goto 62           
        elseif (unit.eq.'PPM') then
           call message('chemical shift in dim 1')
           call getreal2(ppml1,error)
           if (error.ne.0) goto 62
           temp = ptoir(ppml1,si3d1,sp3d1,of3d1,freq3d1)
           freql1 = itohr(temp,si3d1,sp3d1,of3d1)
           call message('chemical shift in dim 2')
           call getreal2(ppml2,error)
           if (error.ne.0) goto 62
           temp = ptoir(ppml2,si3d2,sp3d2,of3d2,freq3d2)
           freql2 = itohr(temp,si3d2,sp3d2,of3d2)
           call message('chemical shift in dim 3')
           call getreal2(ppml3,error)
           if (error.ne.0) goto 62
           temp = ptoir(ppml3,si3d3,sp3d3,of3d3,freq3d3)
           freql3 = itohr(temp,si3d3,sp3d3,of3d3)
        elseif (unit.eq.'INDEX') then
           call message('position index in dim 1')
           call getreal2(indl1,error)
           if (error.ne.0) goto 62
           freql1 = itohr(indl1,si3d1,sp3d1,of3d1)
           call message('position index in dim 2')
           call getreal2(indl2,error)
           if (error.ne.0) goto 62
           freql2 = itohr(indl2,si3d2,sp3d2,of3d2)
           call message('position index in dim 3')
           call getreal2(indl3,error)
           if (error.ne.0) goto 62
           freql3 = itohr(indl3,si3d3,sp3d3,of3d3)           
        else
           goto 63
        endif 
c
c getting linewidthes
c        
        call message( ' linewidth (in Hz) in dim 1')
        call getfreq2(t2l1,sp3d1,error)
        if (error.ne.0) goto 62
        if (t2l1.lt.0.0) goto 61

        call message( ' linewidth (in Hz) in dim 2')
        call getfreq2(t2l2,sp3d2,error)
        if (error.ne.0) goto 62
        if (t2l2.lt.0.0) goto 61

        call message( ' linewidth (in Hz) in dim 3')
        call getfreq2(t2l3,sp3d3,error)
        if (error.ne.0) goto 62
        if (t2l3.lt.0.0) goto 61
c
c getting phases
c
        call message( ' phase in dim 1')
        call getreal2( phasel1,error)
        if (error.ne.0) goto 62
	
        call message( ' phase in dim 2')
        call getreal2( phasel2,error)
        if (error.ne.0) goto 62
	
        call message( ' phase in dim 3')
        call getreal2( phasel3,error)
        if (error.ne.0) goto 62

	do i = 2, si3d1*si3d2,4
	   call mltvect( tdata((si3d3*i)+1),
     *                tdata((si3d3*i)+1),-1.0,2*si3d3)
	enddo
	do i = 2, si3d1,4
	   call mltvect( tdata((si3d3*si3d2*i)+1),
     *                tdata((si3d3*si3d2*i)+1),-1.0,2*si3d2*si3d3)
	enddo
	call bruvect(tdata,tdata,sizeimage3d)
c
c  multiply first complexes points by 2
c
	do i2= 1,si3d2
	   do i1= 1,si3d1
	      tdata(1+si3d3*(i2-1)+si3d3*si3d2*(i1-1) ) =
     *       tdata(1+si3d3*(i2-1)+si3d3*si3d2*(i1-1) )*2.0
	      tdata(2+si3d3*(i2-1)+si3d3*si3d2*(i1-1) ) =
     *       tdata(2+si3d3*(i2-1)+si3d3*si3d2*(i1-1) )*2.0
           enddo
	enddo
	do i3= 1,si3d3
	   do i1= 1,si3d1
	      tdata(i3+si3d3*si3d2*(i1-1) ) =
     *       tdata(i3+si3d3*si3d2*(i1-1) )*2.0
	      tdata(i3+si3d3*si3d2*i1 ) =
     *       tdata(i3+si3d3*si3d2*i1 )*2.0
           enddo
	enddo
	do i3= 1,si3d3
	   do i2= 1,si3d2
	      tdata(i3+si3d3*(i2-1) ) =
     *       tdata(i3+si3d3*(i2-1) )*2.0
	      tdata(i3+si3d3*i2 ) =
     *       tdata(i3+si3d3*i2 )*2.0
           enddo
	enddo
	      
        it3d = 7      
        f1 = ((freql1-of3d1)*2*pi/sp3d1) - pi
 	p1 = phasel1*pi/180.0
        f2 = ((freql2-of3d2)*2*pi/sp3d2) - pi
	p2 = phasel2*pi/180.0
        f3 = ((freql3-of3d3)*2*pi/sp3d3) - pi
	p3 = phasel3*pi/180.0

        y1 = t2l1/2000.0
        y2 = t2l2/2000.0
        y3 = t2l3/2000.0

	do i1 = 1,si3d1,2
	   w1 = f1*float((i1+1)/2-1) + p1
	   cw1 = cos(w1)
	   sw1 = sin(w1)
	   e1 = exp(-float((i1+1)/2-1)*y1)
	   do i2 = 1,si3d2,2
	      w2 = f2*float((i2+1)/2-1) + p2
	      cw2 = cos(w2)
	      sw2 = sin(w2)
	      e2 = exp(-float((i2+1)/2-1)*y2)
	      do i3 = 1,si3d3,2
	         w3 = f3*float((i3+1)/2-1) + p3
		 cw3 = cos(w3)
		 sw3 = sin(w3)
	         e3 = exp(-float((i3+1)/2-1)*y3)
                 z = ampl*e1*e2*e3

		 tdata(i3+si3d3*(i2-1)+si3d3*si3d2*(i1-1) ) =
     *       tdata(i3+si3d3*(i2-1)+si3d3*si3d2*(i1-1) ) + 
     *       (z*cw1*cw2*cw3)
		 tdata(i3+1+si3d3*(i2-1)+si3d3*si3d2*(i1-1) ) =
     *       tdata(i3+1+si3d3*(i2-1)+si3d3*si3d2*(i1-1) ) + 
     *       (z*cw1*cw2*sw3)

		 tdata(i3+si3d3*(i2-1)+si3d3*si3d2*(i1) ) =
     *       tdata(i3+si3d3*(i2-1)+si3d3*si3d2*(i1) ) + 
     *       (z*sw1*cw2*cw3)
		 tdata(i3+1+si3d3*(i2-1)+si3d3*si3d2*(i1) ) =
     *       tdata(i3+1+si3d3*(i2-1)+si3d3*si3d2*(i1) ) + 
     *       (z*sw1*cw2*sw3)

		 tdata(i3+si3d3*(i2)+si3d3*si3d2*(i1-1) ) =
     *       tdata(i3+si3d3*(i2)+si3d3*si3d2*(i1-1) ) + 
     *       (z*cw1*sw2*cw3)
		 tdata(i3+1+si3d3*(i2)+si3d3*si3d2*(i1-1) ) =
     *       tdata(i3+1+si3d3*(i2)+si3d3*si3d2*(i1-1) ) + 
     *       (z*cw1*sw2*sw3)

	  	 tdata(i3+si3d3*(i2)+si3d3*si3d2*(i1) ) =
     *       tdata(i3+si3d3*(i2)+si3d3*si3d2*(i1) ) + 
     *       (z*sw1*sw2*cw3)
		 tdata(i3+1+si3d3*(i2)+si3d3*si3d2*(i1) ) =
     *       tdata(i3+1+si3d3*(i2)+si3d3*si3d2*(i1) ) + 
     *       (z*sw1*sw2*sw3)

	      enddo 
	      if (control.eq.1) return
	   enddo
	   if (control.eq.1) return
	enddo
c
c  divide first complexes points by 2 to remove ridges
c
c	   call mltvect(tdata,tdata,0.5,si2im)
c	   call mltvect(tdata(si2im+1),tdata(si2im+1),0.5,si2im)
c	   do i = 1,si1im
c	      tdata(si2im*(i-1)+1) = tdata(si2im*(i-1)+1)*0.5
c	      tdata(si2im*(i-1)+2) = tdata(si2im*(i-1)+2)*0.5
c	   enddo
	do i2= 1,si3d2
	   do i1= 1,si3d1
	      tdata(1+si3d3*(i2-1)+si3d3*si3d2*(i1-1) ) =
     *       tdata(1+si3d3*(i2-1)+si3d3*si3d2*(i1-1) )*0.5
	      tdata(2+si3d3*(i2-1)+si3d3*si3d2*(i1-1) ) =
     *       tdata(2+si3d3*(i2-1)+si3d3*si3d2*(i1-1) )*0.5
           enddo
	enddo
	do i3= 1,si3d3
	   do i1= 1,si3d1
	      tdata(i3+si3d3*si3d2*(i1-1) ) =
     *       tdata(i3+si3d3*si3d2*(i1-1) )*0.5
	      tdata(i3+si3d3*si3d2*i1 ) =
     *       tdata(i3+si3d3*si3d2*i1 )*0.5
           enddo
	enddo
	do i3= 1,si3d3
	   do i2= 1,si3d2
	      tdata(i3+si3d3*(i2-1) ) =
     *       tdata(i3+si3d3*(i2-1) )*0.5
	      tdata(i3+si3d3*i2 ) =
     *       tdata(i3+si3d3*i2 )*0.5
           enddo
	enddo
c
c put the nul frequency at the middle of the spectrum
c
        call bruvect(tdata,tdata,sizeimage3d)
	do i = 2, si3d1*si3d2,4
	   call mltvect( tdata((si3d3*i)+1),
     *                tdata((si3d3*i)+1),-1.0,2*si3d3)
	enddo
	do i = 2, si3d1,4
	   call mltvect( tdata((si3d3*si3d2*i)+1),
     *                tdata((si3d3*si3d2*i)+1),-1.0,2*si3d3*si3d2)
	enddo

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
	








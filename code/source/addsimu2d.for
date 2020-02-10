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

C
	subroutine addsimu2d (tdata,unit,error)
c IN	: unit
c INOUT	: tdata
c OUT	: error
c
c add to the current 2D data the line whose parameters are given by user.
c spectral width, frequence and data size are those currently defined
c (found in paramfilev.inc).
c
c input:
c specw1,specw2: spectral widthes (paramfilev.inc)
c freq1,freq2: frequencies (paramfilev.inc)
c offset1,offset2: offset (paramfilev.inc)
c sizeimage: data size (paramfilev.inc)
c tdata(sizeimage): current data in the 1D buffer
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
	integer error,i,k,qtype
	real tdata(*)
	real ampl,freql1,t2l1, phasel1,ppml1,indl1
	real freql2,t2l2, phasel2,ppml2,indl2
	real  z, y1, y2, temp
	real  w1,w2,cw,sw,e1,f1,f2,p1,p2

	save ampl,freql1,freql2,ppml1,ppml2,indl1,indl2
        save t2l1,t2l2,phasel1,phasel2
	
	call message( 'phase(0) or amplitude(1) modulation')
	qtype = 1
	call getint2(qtype,error)
	if (error.ne.0) goto 62

	call message(' amplitude')
	call getreal2(ampl,error)
	if (error.ne.0) goto 62
	if (qtype.ne.0 .and. qtype.ne.1) goto 64

	if (unit.eq.'HZ') then	
           call message( ' frequence in dim 1')
           call getreal2(freql1,error)
           if (error.ne.0) goto 62
           call message( ' frequence in dim 2')
           call getreal2(freql2,error)
           if (error.ne.0) goto 62           
        elseif (unit.eq.'PPM') then
           call message('chemical shift in dim 1')
           call getreal2(ppml1,error)
           if (error.ne.0) goto 62
           temp = ptoir(ppml1,si1im,specw1,offset1,freq1)
           freql1 = itohr(temp,si1im,specw1,offset1)           
           call message('chemical shift in dim 2')
           call getreal2(ppml2,error)
           if (error.ne.0) goto 62
           temp = ptoir(ppml2,si2im,specw2,offset2,freq2)
           freql2 = itohr(temp,si2im,specw2,offset2)           
        elseif (unit.eq.'INDEX') then
           call message('position index in dim 1')
           call getreal2(indl1,error)
           if (error.ne.0) goto 62
           freql1 = itohr(indl1,si1im,specw1,offset1)
           call message('position index in dim 2')
           call getreal2(indl2,error)
           if (error.ne.0) goto 62
           freql2 = itohr(indl2,si2im,specw2,offset2)
        else
           goto 63
        endif 
        
        call message( ' linewidth (in Hz) in dim 1')
        call getfreq2(t2l1,specw1,error)
        if (error.ne.0) goto 62
        if (t2l1.lt.0.0) goto 61

        call message( ' linewidth (in Hz) in dim 2')
        call getfreq2(t2l2,specw2,error)
        if (error.ne.0) goto 62
        if (t2l2.lt.0.0) goto 61

        call message( ' phase in dim 1')
        call getreal2( phasel1,error)
        if (error.ne.0) goto 62
	
        call message( ' phase in dim 2')
        call getreal2( phasel2,error)
        if (error.ne.0) goto 62
c
c put the nul frequency at the edges of the spectrum
c if the current data should be conserved
c
        if (qtype.eq.1) then   ! amplitude modulation
           
	   do i = 2, si1im,4
	      call mltvect( tdata(si2im*(i)+1),
     *                tdata(si2im*(i)+1),-1.0,2*si2im)
	   enddo
	   call bruvect(tdata,tdata,sizeimage)
c
c  multiply first complexes points by 2
c
	   do i = 1,si1im
	      tdata(si2im*(i-1)+1) = tdata(si2im*(i-1)+1)*2.0
	      tdata(si2im*(i-1)+2) = tdata(si2im*(i-1)+2)*2.0
	   enddo
	   call mltvect(tdata(si2im+1),tdata(si2im+1),2.0,si2im)	      
	   call mltvect(tdata,tdata,2.0,si2im)
	      
	else                  ! phase modulation
	   
	      call bruvect(tdata,tdata,sizeimage)
c	   
c  multiply first complexes points by 2
c
	      do i = 1,si1im
	      tdata(si2im*(i-1)+1) = tdata(si2im*(i-1)+1)*2.0
	      tdata(si2im*(i-1)+2) = tdata(si2im*(i-1)+2)*2.0
	      enddo
	      call mltvect(tdata,tdata,2.0,si2im)
	   
	endif

	if ( qtype .eq. 1 ) then  ! amplitude modulation
	
	   itype = 3
	   f1 = ((freql1-offset1)*2*pi/specw1) - pi
	   p1 = phasel1*pi/180.0
	   f2 = ((freql2-offset2)*2*pi/specw2) - pi
	   p2 = phasel2*pi/180.0
	   	
	   y1 = t2l1/2000.0
	   y2 = t2l2/2000.0
	   do i = 1,si1im,2
	      w1 = f1*float((i+1)/2-1) + p1
c il faudrait diviser i-1 par 2!
	      cw = cos(w1)
	      sw = sin(w1)
	      e1 = exp(-float((i+1)/2-1)*y1)
	      do k = 1,si2im,2
	         z = e1*ampl*exp(-float((k+1)/2-1)*y2)
	         w2 = f2*float((k+1)/2-1) + p2
	         tdata(k+si2im*(i-1)) = tdata(k+si2im*(i-1)) +
     *               z*cos(w2)*cw
	         tdata(k+1+si2im*(i-1)) = tdata(k+1+si2im*(i-1)) +
     *               z*sin(w2)*cw
	         tdata(k+si2im*i) = tdata(k+si2im*i) +
     *               z*cos(w2)*sw
	         tdata(k+1+si2im*i) = tdata(k+1+si2im*i) +
     *               z*sin(w2)*sw
	      enddo
	      if (control.eq.1) return
	   enddo
c
c  divide first complexes points by 2 to remove ridges
c
	   call mltvect(tdata,tdata,0.5,si2im)
	   call mltvect(tdata(si2im+1),tdata(si2im+1),0.5,si2im)
	   do i = 1,si1im
	      tdata(si2im*(i-1)+1) = tdata(si2im*(i-1)+1)*0.5
	      tdata(si2im*(i-1)+2) = tdata(si2im*(i-1)+2)*0.5
	   enddo

	elseif (qtype .eq.0 ) then ! phase modulation

	   y1 = t2l1/2000.0
	   y2 = t2l2/2000.0
	
	   itype = 1
	   f1 = (freql1-offset1)*2*pi/specw1
	   p1 = phasel1*pi/180.0
	   f2 = ((freql2-offset2)*2*pi/specw2) - pi
	   p2 = phasel2*pi/180.0
	   do i = 1,si1im,1
	      w1 = f1*float(i-1)+p1
	      e1 = exp(-float((i+1)/2-1)*y1)
	      do k = 1,si2im,2
	         z = e1*ampl*exp(-float((k+1)/2-1)*y2)
	         w2 = f2*float((k+1)/2-1)+p2
	         tdata(k+si2im*(i-1)) = tdata(k+si2im*(i-1)) +
     *               z*cos(w2+w1)
	         tdata(k+1+si2im*(i-1))=tdata(k+1+si2im*(i-1)) +
     *               z*sin(w2+w1)
	      enddo
	      if (control.eq.1) return
	   enddo
c	   
c  divide first complexes points by 2 to remove ridges
c
	   call mltvect(tdata,tdata,0.5,si2im)
	   do i = 1,si1im
	      tdata(si2im*(i-1)+1) = tdata(si2im*(i-1)+1)*0.5
	      tdata(si2im*(i-1)+2) = tdata(si2im*(i-1)+2)*0.5
	   enddo

	endif
c
c put the nul frequency at the middle of the spectrum
c
        call bruvect(tdata,tdata,sizeimage)
	if (qtype.eq.1) then
	   do i = 2, si1im,4
                call mltvect( tdata(si2im*(i)+1),
     *                tdata(si2im*(i)+1),-1.0,2*si2im)
	   enddo
	endif
	return
c
c negative or nul t2	
c
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
	     	
c error with cpx value (complex or real)
64	call gifaout('Data should be in amplitude(1) or 
     & phase(0) modulation')
	error = 1
	return
		
	end







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

c******************************************************************************
	subroutine lpreconstruct1d (sizedata,rdata,amp,width,
     &			phas,freq,nsignals,specw2,offset)
c IN	: sizedata,amp,width,phas,freq,nsignals,specw2,offset
c OUT	: rdata
c
c Calculate the 1D data (FID) from parameter peaks
c
	implicit none
#include "constant.inc"
	integer sizedata,i,j,nsignals
	complex rdata(sizedata),z
	real freq(nsignals),amp(nsignals),phas(nsignals)
	real width(nsignals),specw2,p,f,w,t,offset,a
        character*256 line
c
c Verify that all width are positive to avoid a crash ; put to zero all
c negative width and send a message to user
c
        do i = 1,nsignals
	    if (width(i).lt.0.) then
		write(line,*)'Be careful : a width is 
     & negative !',width(i),i
		call gifaout(line)
	 	width(i) = 1.0
            endif
	enddo
c
c Verify that the spectral width is not nul
c
	if (specw2.eq.0) then
	   call gifaout('Can''t compute the fid : spectral
     & width is nul')
	   return
	endif
c
c Calculate the new fid
c
        call prpvect(rdata,0.0,sizedata*2)
        do j=1,nsignals
           if (width(j) .gt. 0.0) then         ! negative widths are pointers
             f = ((freq(j)-offset)*(2*pi)/specw2) -pi
             p = phas(j)*pi/180.0
             t = (width(j)*pi)/abs(specw2)
             a = amp(j) * width(j)
c             write(*,*)'amp',amp(j),'width',width(j),'a',a,j
c             write(*,*)'rdata(1)',rdata(1)
             do i=1,sizedata
               z = a*exp(-float(i-1)*t)
               w = f*float(i-1)+p
               rdata(i) = rdata(i) + z*cmplx(cos(w),sin(w))
c               write(*,*)'z',z,'w',w,'i',i,'rdata(i)',rdata(i)
             enddo
           endif
        enddo
        
        call bruvect(rdata,rdata,2*sizedata)
c        rdata(1) = 0.5*rdata(1)
c        rdata(2) = 0.5*rdata(2)

      return
      end




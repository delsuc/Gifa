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

      subroutine extpic(zn,width,freq,amp,phas,
     *		ipvt,order,zz,wk,data,sizdat,a,specw,offset,
     *          error,stdev,delay)
c IN	: zn,order,zz,data,sizdat,a,specw,offset,delay
c OUT	: width,freq,amp,phas,error,stdev
c WORK	: ipvt,wk
c
c Computes line parameters form the data "data" and the set of 
c roots of the autoregresseive polynomial "zn"
c amplitude and phases are computed by a matrix inversion, using
c the Prony's method.
c
      implicit none
#include "constant.inc"
c
      character*256 line
      integer i,j,job,order,sizdat,error
      integer ipvt(order)
      complex data(sizdat),temp
      real delay,specw,stdev,temp1,offset
      real amp(order),width(order),freq(order),phas(order)
c
      real*8 ampz,rcond,cycf,shift,zi,zr

      real tpi
      complex*16 znsum
      complex*16 zz(order,order),wk(order),a(order)
      complex*16 zn(order)
c
      if (order.eq.0) then
	write(*,*)'Warning : order is nul !'
	error = 1
	goto 100
      endif
c
      tpi=2.0*pi
c
      do 700 i=1, order
	if (zn(i).eq.0.d0) then
	  write(line,*)'You have a null root. Remove this root' 
	  call gifaout(line)
          write(line,*)'before caculating the peak''s parameters'
	  call gifaout(line)
          error = 1
          goto 100
	endif
#_f2c        ampz=cdabs(zn(i))
#f2c        ampz=zabs(zn(i))
#_f2c        zr=dreal(zn(i))
#f2c        zr=dble(zn(i))
        zi=dimag(zn(i))
c
c Computation of frequences in spectrometer's scale. DO NOT USE ATAN2!
c If zr.ne.0, no problem, else there are special values for frequences.
c 
	if (zr.ne.0.d0) then
	     temp1 = real(datan(zi/zr))*specw/tpi
c
	     if (zr.lt.0.d0)  freq(i) = temp1 + specw/2
	     if ((zi.gt.0.d0).and.(zr.gt.0.d0)) 
     &	    		 freq(i) = temp1
	     if ((zi.lt.0.d0).and.(zr.gt.0.d0)) 
     &	    		 freq(i) = temp1 + specw
             if (dabs(zi).lt.epsilon .and. (zr.gt.0.d0))
     &                   freq(i) = 0.0
	else
             if (zi.gt.0.0)  freq(i) = specw/4
             if (zi.lt.0.0)  freq(i) = 3*specw/4
	endif
	freq(i) = freq(i) + offset
c
c   width is PI times actual spectral linewidth multiplied by
c   time difference between sampled data points.
c
        width(i)=-real(log(ampz))*abs(specw)/pi
700   continue
c
      do j = 1,order
        do i = 1,order
           znsum=dconjg(zn(i))*zn(j)
           zz(i,j)=(znsum**(sizdat)-1.d0)/(znsum-1.d0)
        enddo
      enddo
c
c  Prony's method for determining amplitudes and phases.
c
      do i=1,order
        znsum=dcmplx(0.d0,0.d0)
        do j=1,sizdat
          znsum=znsum+data(j)*(dconjg(zn(i))**(j-1))
        enddo
        a(i)=znsum
c	write(*,*)'a avant cdgesl',a(i),i
      enddo
c
      call cdgeco(zz,order,order,ipvt,rcond,wk)
      if (1.0d0.eq.(rcond+1.0d0)) goto 3333
      goto 3334
3333  call gifaout(' phase matrix singular!')
      return
c
3334  job=0
      call cdgesl(zz,order,order,ipvt,a,job)
c	do i = 1,order
c	   write(*,*)'a apres cdgesl',a(i),i
c	enddo
c
c Calculation of the difference between initial data and data calculated
c from the obtained parameters 
c
      temp1  = (0.0,0.0)
      do i = 1,sizdat
	 temp  = (0.0,0.0)
	 do j = 1,order
	    temp = temp + (a(j)*(zn(j)**(i-1)))
	 enddo
	 temp1 = temp1 + ((data(i)-temp)*conjg(data(i)-temp))
      enddo
      stdev = sqrt(temp1)/sizdat
c
c Calculation of the amp and phase parameters from coeff. a
c
      do i=1, order
#_f2c         amp(i) = cdabs(a(i))*dexp(dble(-delay)*width(i))
#f2c         amp(i) = zabs(a(i))*dexp(dble(-delay)*width(i))
         amp(i) = amp(i) / width(i)
         shift = dble(delay)*freq(i)
         cycf = shift-dint(shift)
#_f2c         phas(i) = (datan2(dimag(a(i)),dreal(a(i)))-cycf)
#f2c         phas(i) = (datan2(dimag(a(i)),dble(a(i)))-cycf)
         if(phas(i).gt.pi) phas(i) = phas(i) - tpi
         if(phas(i).lt.-pi) phas(i) = phas(i) + tpi
         phas(i) = phas(i)*180.0/pi
      enddo

c in gifa peak table, spectrum amplitude are needed, not voumes,
c thus amp = amp/width
c	do i = 1,order
c	   write(*,*)'amp',amp(i),'width',width(i),'freq',
c     & freq(i),'phase',phas(i),'i',i
c	enddo
            
100   continue
      return
      end





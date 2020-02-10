C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in CBS/Montpellier
C
C     Authors :       Therese Malliavin M.A.Delsuc
C                     C.B.S. Faculte de Pharmacie
C                     34060 Montpellier   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.

***********************************************************************
      subroutine redfield (phas,width,amp,order,redf,specw)
c IN	: phas,width,amp,order
c,specw
c OUT	: redf
c
c part of the build-up curve analysis
c Calculate relaxation rate from the amplitudes and width
c calculated by build-up curves processing. The detected composantes
c should be real.
c real width(order),amp(order),phase(order) : signal parameters calculated
c by linear prediction method.
c real redf : relaxation rate
c integer order : number of exponentials detected.
c real specw : spectral width, should be the same that the used one in
c linear prediction processing
c
      implicit none
#include "constant.inc"

      integer j,order
      real redf,temp,damp,specw,phi
      real width(order),amp(order),phas(order)

      temp = 0.0
      do j = 1,order
        damp = width(j)*pi/specw
        phi = phas(j)*pi/180.0
c l'amplitude du fid est le volume du peak
        temp = temp + (cos(phi)*amp(j)*width(j)*damp)
      enddo
      redf = temp

      return
      end      
***************************************************************************
      subroutine pente (data,sizdat,specw,redf)
c IN	: data,sizdat,specw
c OUT	: redf
c
c part of the build-up curve analysis
c Calculate the initial slope of a build-up curve
c real data(sizdat) build-up curve's data
c real redf : relaxation rate
c integer order : number of exponentials detected.
c integer sizdat : considered size of data, can be lesser then the total size
c real specw : spectral width, should be the same that the used one in
c linear prediction processing
c
      implicit none

      integer i,sizdat,zz
      real redf,specw,temp
      real data(sizdat)

      temp = 0.
      do i = 1,sizdat
	temp = temp + (float(i)*data(i))
      enddo
      zz = sizdat*(sizdat+1)*((2*sizdat)+1)
      redf = -6.0*temp*specw/float(zz)

      return
      end
***************************************************************************
      subroutine rcryst (data,sizdat,specw,offset,phas,width,amp,
     &		freq,order,ki2,rfactor)
c IN	: data,sizdat,specw,offset,phas,width,amp,freq,order,ki2
c OUT	: rfactor
c part of the build-up curve analysis
c
c Calculate a "R factor" to check the quality of multi-exponentials
c analysis.
c real data(sizdat) build-up curve's data
c real width(order),amp(order),phas(order),freq(order) : signal parameters 
c obtained by data-processing.
c integer order : number of exponentials detected.
c integer sizdat : considered size of data, can be lesser then the total size
c real specw : spectral width, should be the same that the used one in
c linear prediction processing
c real rfactor : "R factor" between real part of experimental and 
c calculated data.
c real ki2 : ki2 function between real part of experimental and calculated data.
c
      implicit none
#include "constant.inc"

      integer i,j,sizdat,order
      real rfactor,specw,ki2,offset
      real a,p,f,w,t,z,denominator,numerator
      real width(order),amp(order),phas(order),freq(order)
      complex temp,data(sizdat)
      character*256 line
c
c Calculate the new fid from peak parameters
c
	denominator = 0.0
	numerator = 0.0
	ki2 = 0.0
c
	do i = 1,sizdat
	    temp = cmplx(0.0,0.0)
	    do j = 1,order
	        f = ((freq(j)-offset)*(2*pi)/specw) -pi
		p = phas(j)*pi/180.0
		t = (width(j)*pi)/abs(specw)
		a = amp(j) * width(j)
		z = a*exp(-float(i-1)*t)
                w = f*float(i-1)+p
		temp = temp + (z*cmplx(cos(w),sin(w)))
	    enddo
	    if (mod(i,2).eq.0) temp = -temp
	    numerator = abs(real(temp - data(i))) + numerator
	    denominator = abs(real(data(i))) + denominator
	    ki2 = ((real(temp - data(i)))**2) + ki2
	enddo

	if (denominator.ne.0.0) then
	   rfactor = numerator/denominator
	else
	   write(line,*)'Be careful : all data are nul'
	   call gifaout(line)
	endif

        return
        end
***********************************************************************
      subroutine rtfreq (zn,order,index,alpha)
c IN	: alpha
c INOUT	: zn,order
c WORK	: index
c
c part of the build-up curve analysis
c
c Keep only the roots whose frequence=0, the others roots are put on the real 
c axis with values zr+zi and zr-zi, where zr and zi are real and imaginary 
c the roots with too big imaginary parts are removed
c parts of zn 
c complex*16 zn(order) : roots 
c complex*16 wk(order) : working array 
c integer index(order) : working array 
c integer order : number of roots
c real  alpha : parameter determining the cone extent where the roots
c are pivoted.
c
      implicit none
#include "constant.inc"

      integer order,i,j,k,piv,throw
      integer index(order)
      real*8 zr,zi,a,b,small,dalpha
      real alpha
      complex*16 zn(order)
      character*256 line
	
      dalpha = dble(alpha)
      small = dble(epsilon)
      piv = 0
      throw = 0
c      write(*,*)'order',order
      do i = 1,order
	 zr = dble(zn(i))
	 zi = dimag(zn(i))
c
c Do rtfreq only if imaginqry part of zn is small in relation to his real part
c
	 if ((dabs(zi).ge.small).and.
     &		(dabs(zi).lt.dabs(zr/dalpha))) then
	    a = 0.d0
	    b = 0.d0
	    do j = i+1,order
		if (dabs(dimag(zn(i))+dimag(zn(j))).le.small) then
		   if (dabs(zi+zr).ge.1.d0) then
		      zn(j) = dcmplx(zr,0.d0)
		      a = -2.d0*zi
		      b = -zi
		      piv = piv + 1
		      write(line,*)'Pivoting roots  : (zi+zr).ge.1)'
 		      call gifaout(line)
		   elseif (dabs(zr-zi).ge.1.d0) then
		      zn(j) = dcmplx(zr,0.d0)
		      a = 2.d0*zi
		      b = -zi
		      piv = piv + 1
		      write(line,*)'Pivoting roots  : (zi-zr).ge.1)'
 		      call gifaout(line)
		   else
		      zn(j) = dcmplx(zr+zi,0.d0)
		      a = -zi
		      b = -zi
		      piv = piv + 1
		      write(line,*)'Pivoting roots'
 		      call gifaout(line)
                   endif
		endif
	    enddo
	    zn(i) = dcmplx(zr+a,zi+b)
	 elseif ((dabs(zi).le.small).and.
     &		(dabs(zi).lt.dabs(zr/dalpha))) then
	    zn(i) = dcmplx(zr,0.d0)
	 endif
      enddo
c
c Then remove the roots whose the imaginary parts are too large
c and the real roots which are negative. 
c
      k = 1
      do i = 1,order
	 zr = dble(zn(i))
	 zi = dimag(zn(i))
c	 write(*,*)'zr',zr,'zi',zi,'i',i
c	 write(*,*)'small',small
	 if ((dabs(zi).lt.dabs(zr/dalpha)).and.
     & (zr.gt.0.0d0)) then
	    index(k) = i
c	    write(*,*)'index',index(k),'k',k
	    k = k + 1
	 endif
      enddo
      throw = order - k + 1
      call rselect(zn,index,k-1,order)
      write(*,*)throw,' roots removed, ',piv,' roots pivoted.'
      order = order - throw

20    continue
      return
      end



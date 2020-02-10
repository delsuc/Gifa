C
C     This file is a part of the GIFA program.
C     This software was developed by the NMR Group of the
C     CNRS in GIF/Yvette France.
C
C                     NMR Laboratory
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user has been fully 
C licensed by the above laboratory.
C     This file cannot be copied, transferred, or used for any other
C purpose without the written permission of the authors.
C                                                      
C
      subroutine burgr(data,n,m,pow,ar,wk1,wk2,wk3)
c IN	: data,n,m
c OUT	: pow,ar
c WORK	: wk1,wk2,wk3
C Burg method for finding AR coefficients.
C for real data
C with :
C input:  data(n)   data to be evaluated
C         m order of the AR
C         wk1,wk2,wk3 work areas of n long
C output: ar(m) AR coeff.
C         pow   power of the spectrum
      implicit none
      integer n,m,i,k
      real*8 ar(m),wk1(n),wk2(n),wk3(m)
      real*8 pow,tt1,tt2,tt3
      real data(n),ploc
C
C evaluate power
      call dotvect(ploc,data,data,n)
      pow = dble(ploc)/dfloat(n)
C initialize work area
      call sdcopvect(wk1,data,n-1)
      call sdcopvect(wk2,data(2),n-1)
C start looping on order
      do k=1,m
         call dddotvect(tt3,wk1,wk2,n-k)
         call dddotvect(tt1,wk1,wk1,n-k)
         call dddotvect(tt2,wk2,wk2,n-k)
         ar(k) = 2.0*tt3/(tt1+tt2)
         pow = pow*(1.0-ar(k)**2)
         if (k.ne.1) then
            do i=1,k-1
               ar(i) = wk3(i)-ar(k)*wk3(k-i)
            enddo
         endif
         if (k.eq.m) return
         call ddcopvect(wk3,ar,k)
         call daddvect(wk1,wk1,wk2,-wk3(k),n-k-1)
         call daddvect(wk2,wk2(2),wk1(2),-wk3(k),n-k-1)
      enddo
      end
**************************************************************************
      subroutine burgc (data,n,m,pow1,ar,wk1,wk2,error)
c IN	: data,n,m
c OUT	: pow,ar,error
c WORK	: wk1,wk2
C Burg method for finding AR coefficients.
C for complex data
C with :
C input:  data(n)   data to be evaluated
C         m order of the AR
C         wk1,wk2 work areas of n long
C output: ar(m) AR coeff.
C         pow   power of the spectrum

      implicit none
#include "constant.inc"

      integer j,k,n,m,khalf,kj,error
      complex data(n)
      complex*16 ar(m),wk1(n),wk2(n),num,save1,save2
      real*8 pow1,den,temp
      real pow

c initialize the ar coefficients (BURG command)
      call prdvect (ar,0.d0,2*m)
      error = 0
      call dotvect(pow,data,data,2*n)
      pow1 = dble(pow)
      den = pow1*2.d0
      pow1 = pow1/n
      if (m.eq.0) return
      call sdcopvect(wk1,data,2*n)
      call sdcopvect(wk2,data,2*n)
      temp = 1.d0
      k = 0

100   k = k + 1
      num = (0.d0,0.d0)
      do j = k+1,n
	num = num +wk1(j)*dconjg(wk2(j-1))
      enddo
      den = temp*den - dble(wk1(k))**2 - dimag(wk1(k))**2
     &		- dble(wk2(n))**2 - dimag(wk2(n))**2
      if (den.eq.0.d0) then
	call gifaout ('*** Pb with burg : your data may be null')
        error = 1
	return 
      endif
      save1 = -2.0d0*num/den
      temp = 1.0d0 - dble(save1)**2 - dimag(save1)**2
      if (temp.le.epsilon) then
        if (temp.le.-epsilon) then
          error = 1
          call gifaout ('*** Pb with burg : temp is negative or null')
          return
        else
          temp = epsilon
        endif
      endif
      pow1 = pow1*temp
      ar(k) = save1
      if (k.eq.1) goto 60
      khalf = k/2
      do j = 1,khalf
	kj = k-j
	save2 = ar(j)
	ar(j) = save2 + save1*dconjg(ar(kj))
	if (j.eq.kj) goto 50
	ar(kj) = ar(kj) + save1*dconjg(save2)
      enddo
50    continue
60    if (k.eq.m) return
      do j = n,k+1,-1
	save2 = wk1(j)
	wk1(j) = save2+save1*wk2(j-1)
	wk2(j) = wk2(j-1) + dconjg(save1)*save2
      enddo	
      goto 100

      end





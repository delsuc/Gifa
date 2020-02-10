C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in CBS/Montpellier
C
C     Authors :       M.A.Delsuc Therese Malliavin
C                     C.B.S. Faculte de Pharmacie
C                     34060 Montpellier   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.

      subroutine polyarf (svd,nsignals,data,sizdat,af,order,
     &		nd1,u,v,wk,b,a)
c IN	: svd,nsignals,data,sizdat,order,nd1,u,v
c OUT	: af
c WORK	: wk,b,a
c Calculate the forward prediction error polynome from the svd and u,v
c complex*16 svd(nsignals),u(nd1,nd1),v(order,order) singular values
c and transformation matrices
c complex data(sizdat)
c complex*16 af(order) polynomial coefficients
c complex*16 wk(sizdat),b(sizdat) working areas
c
      implicit none
c
      integer sizdat,order,nsignals,nd1
      integer i,j
c
      complex*16 b(sizdat), wk(sizdat),a(sizdat),
     &      svd(nsignals), v(order,order),u(nd1,nd1),
     &      af(order)
      complex*16 ct, cttt
      complex data(sizdat)
c
c forward, then t(U).b  in wk(), and then divde by s() (SVD)
c
      do i = 1,nd1
C dcmplx is bugged in xlf AIX v2.0 !
#aix    	  b(i) = -dcmplx(dble(real(data(i+order+1))),
#aix     *          dble(imag(data(i+order+1))))
#_aix              b(i) = -dcmplx(data(i+order+1))
      enddo

      do i = 1,nsignals
         cttt = dcmplx(0.0d0,0.0d0)
         ct = cttt
         do j = 1,nd1
            ct = dconjg(u(j,i))*b(j)+ct
	 enddo
         if (dble(svd(i)).ge.1.0D-8) cttt=ct/dble(svd(i))
         wk(i)=cttt
       enddo
c
c  now a() = v()*wk()
c
      do i=1,order
	a(i)=dcmplx(0.d0,0.d0)
	do j=1,nsignals
	   a(i)=a(i)+v(i,j)*wk(j)
	enddo
      enddo
c
c	INVERSE the order of the vector solution of linear equation to obtain
c	the forward autoregressive coefficients
c 	le vecteur solution du systeme lineaire doit etre inverse pour obtenir 
c	coeff AR forward, et il devra etre reinverse a nouveau pour obtenir 
c	les coefficients du polynome de PE.
c
	do i = 1,order
	   af(i) = a(order-i+1)
	enddo
c
      return
      end

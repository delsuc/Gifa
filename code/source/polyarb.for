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

      subroutine polyarb (svd,nsignals,data,sizdat,ab,order,
     &	nd1,u,v,wk,a,b)
c IN	: svd,nsignals,data,sizdat,,order,nd1,u,v
c OUT	: ab
c WORK	: wk,a,b
c Calculate the coefficient of backward prediction error polynomial
c complex*16 svd(nsignals) singular values
c complex*16 u(nd1,nd1) v(order,order) transformation matrices
c complex data(sizdat)
c complex*16 a(sizdat),b(sizdat),wk(sizdat) working area
c ab(order) coefficient of backward prediction error polynome
c
      implicit none
c
      integer sizdat, nsignals,order,nd1
      integer i, j, nmin
c
      complex*16 b(sizdat), wk(sizdat),
     &      svd(nsignals), v(order,order),u(nd1,nd1),
     &      ab(order),a(order)
c
      complex data(sizdat)
c
      complex*16 ct,cttt
c
c	nmin = order car nd>=order : a quoi sert nmin ?
c
      nmin = min(order,nd1)
c
c                      computation of V.L-1.t(U).b
c first recompute the signal vector
c
      do i = 1,nd1
c	b(i) = -dcmplx(conjg(data(i)))
C dcmplx is bugged in xlf AIX v2.0 !
#aix      b(i) = -dcmplx(dble(real(data(i))),dble(imag(data(i))))
#_aix      b(i) = -dcmplx(data(i))
      enddo
c
c then t(U).b  in wk(), and then divide by s() (SVD)
c
      do i = 1,nsignals
	cttt = dcmplx(0.0d0,0.0d0)
	ct = cttt
	do j = 1,nd1
      	   ct = dconjg(u(j,i))*b(j) + ct
	enddo
	if (dble(svd(i)).ge.1.0D-8) cttt = ct/dble(svd(i))
	wk(i) = cttt
      enddo
c
c  now a() = v()*wk()
c
      do i = 1,order
	a(i) = dcmplx(0.d0,0.d0)
	do j = 1,nsignals
	   a(i) = a(i) + v(i,j)*wk(j)
	enddo
      enddo
c
c	le vector solution du systeme lineqr n'a pas besoin d'etre inverse pour
c	obtenir les coeff AR backward, mais il devra etre decale pour obtenir
c	les coefficients du polynome de PE
c
	do i = 1,order
	   ab(i) = a(i)
	enddo
c
      return
      end

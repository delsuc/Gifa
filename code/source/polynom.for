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

      subroutine polynom (root,coeff,order,ar)
c IN	: root,order,ar
c OUT	: coeff
c
c Caculate the coefficients of polynome knowing the roots
c coeff(0:order) : complex*16 PE coefficients
c ar(1:order) : complex*16 AR coefficients
c root(1:order) : complex*16 roots
c order : degree of polynome
c
      implicit none

      integer order,i,j
      complex*16 root(order),coeff(0:order),ar(order)

      coeff(0) = (1.0d0,0.0d0)
      do i = 1,order
	do j = i-1,1,-1
	   coeff(j) = -(coeff(j)*root(i)) + coeff(j-1)
c
c recurrence sur le degre du polynome : 
c P(q-1)(z-root(i)) = P(q) where q and q-1 are the order of the polynomes
c The coefficients of P(q) can be calculated with coefficients of P(q-1)
c C(q,1<k<q-1) = -C(q-1,k)*root(i) + C(q-1,k-1)
c C(q,k) = coeff(k)
c
	enddo
	coeff(0) = -root(i)*coeff(0)
	coeff(i) = (1.0d0,0.0d0)
      enddo

      do i = 1,order
        ar(i) = coeff(order-i)
      enddo

      return
      end

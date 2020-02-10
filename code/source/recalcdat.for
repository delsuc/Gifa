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

      subroutine recalcdat (svd,nsignals,data,sizdat,order,
     &	nd1,u,v,x)
c IN	: svd,nsignals,data,sizdat,order,nd1,u,v
c OUT	: x
c
c Recalculate the Toeplitz data matrix x from the svd and u,v
c complex*16 svd(nsignals),u(nd1,nd1),v(order,order) singular values
c and transformation matrices
c complex data(sizdat)
c
      implicit none
c
      integer sizdat,order,nsignals
      integer nd1,i,j,k,l,s
      complex*16 temp
c
      complex*16 svd(order), v(order,order),u(nd1,nd1),
     &		x(nd1,order)
      complex data(sizdat)
c
c Recalculate matrix x = U svd Vt
c
      do i = 1,nd1
	do j = 1,order
	   temp = (0.d0,0.d0)
	   do k = 1,nsignals
		temp = temp + (u(i,k)*svd(k)*dconjg(v(j,k)))
	   enddo
	   x(i,j) = temp
	enddo
      enddo
c
c now, calculate the new data by mean coefficients of x to obtain a Toeplitz 
c matrix and calculate the new data 
c
      do k = 2,sizdat-1
	temp = (0.d0,0.d0)
	l = max(0,k-order-1)
	s = min(k-1,nd1)
	do i = l+1,s
	   temp = temp + x(i,k-i)
	enddo
	data(k) = temp/(s-l)
      enddo
c
      return
      end

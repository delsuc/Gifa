C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in CBS/Montpellier
C
C     Authors :       M.A.Delsuc T.E.Malliavin
C                     C.B.S. Faculte de Pharmacie
C                     34060 Montpellier   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.

      subroutine fairemat (data,sizdat,order,x,nd1)
c IN	: data,sizdat,order,nd1
c OUT	: x
c
c Prepare the data for calculation of svd 
c data(sizdata) complex
c integer order : number of coeff ar
c integer nd1 : sizdat-order-1 : other dimension of matrix X
c complex*16 x(nd1,order) : matrix whose svd will be calculated
c 
      implicit none
c
      integer i, j,sizdat, order,nd1
c
      complex data(sizdat)
c
      complex*16 x(nd1,order)
c
c construct the signal matrix
c
C x(i,j), i:row,1..nd-1  j:column,1..order  with nd>=order
c
      do i = 1, nd1
	do j = 1, order
C dcmplx is bugged in xlf AIX v2.0 !
#aix	   x(i,j) = dcmplx(dble(real(data(i+j))),dble(imag(data(i+j))))
#_aix       x(i,j) = dcmplx(data(i+j))
	enddo
      enddo
c
      return
      end

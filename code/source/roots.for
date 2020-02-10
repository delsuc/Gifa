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
      subroutine inverse (zno,zni,order)
c IN	: zni,order
c OUT	: zno
c
c part of the LPSVD package
c
c Calculate the inverse conjugates of the roots of polynome
c complex*16 zno(order) : output roots 
c complex*16 zni(order) : input roots 
c integer order : degree of the polynome
c
      implicit none
#include "constant.inc"

      integer order,i
      complex*16 zno(order),zni(order)
      real*8 ampz

      do i = 1,order
#_f2c      ampz = cdabs(zni(i))
#f2c      ampz = zabs(zni(i))
	if (ampz.gt.depsilon) then
	   zno(i) = dconjg(zni(i))/(ampz**2)
	else 
	   call gifaout('Be careful : you have a null root')
	endif
      enddo

      return
      end
***********************************************************************
      subroutine clean (zn,order,new)
c IN	: order,new
c INOUT	: zn
c
c part of the LPSVD package
c
c
c Keep only the roots outside of the unity-circle ; new is the new order
c
      implicit none
   
      integer order,i,k,new
      complex*16 zn(order)
      real*8 ampz

      k = 0
      do i = 1,order
#_f2c      ampz = cdabs(zn(i))
#f2c      ampz = zabs(zn(i))
	if (ampz.gt.1.d0) then
	   k = k + 1
	   zn(k) = zn(i)
	endif
      enddo
      new = k 

      return
      end
***********************************************************************
      subroutine reflect (zn,order)
c IN	: order
c INOUT	: zn
c
c part of the LPSVD package
c
c Calculate the inverse conjugates of the roots which are outside 
c of unity circle
c
      implicit none

      integer order,i
      complex*16 zn(order)
      real*8 ampz
	
      do i = 1,order
#_f2c      ampz = cdabs(zn(i))
#f2c      ampz = zabs(zn(i))
	if (ampz.gt.1.d0) then
	   zn(i) = zn(i)/(ampz**2)
	endif
      enddo

      return
      end
***********************************************************************
      subroutine match (zn1,zn2,order,nsignals,x,zn)
c IN	: zn1,zn2,order,nsignals
c OUT	: zn
c WORK	: x
c
c part of the LPSVD package
c Compare two sets of roots zn1 and zn2 to determine which of them are noise
c complex*16 zn1 forward roots,zn2 backward roots
c integer order : degree of polynome,number of roots
c integer nsignals : number of real signals
c real*8 x : working area
c complex*16 zn : the final roots
c
      implicit none
#include "constant.inc"
c
      integer order, nsignals
      integer i,j,k,imin,jmin
      complex*16 zn1(order),zn2(order),zn(nsignals),x(order,order)
      complex*16 zz
      real*8 t, ttt,ampz,zi, zr 
c
c Check if there is no backward roots nul
c and build a matrix with all the distances
c
      do i=1,order
#_f2c      ampz = cdabs(zn2(i))
#f2c      ampz = zabs(zn2(i))
	if (ampz.lt.depsilon) then
	   call gifaout('Be careful : you have a null backward root')
	   return
	endif
	zz = dconjg(zn2(i))/(ampz**2)
	do j=1,order
#_f2c        x(i,j) = cdabs(zz - zn1(j))**2
#f2c         x(i,j) = zabs(zz - zn1(j))**2
	enddo
      enddo
c
c then pick-out the nsignals first smallest distance,
c  marking the already picked roots with a -1
c
      do i = 1,nsignals
	ttt = 1.0d0
	imin = 1
	jmin = 1
	do j = 1,order
  	   do k = 1,order
    		t = x(j,k)
    		if (t.lt.ttt.and.t.ge.0.0d0) then
       		   ttt = t
       		   imin = j
       		   jmin = k
    		endif
	   enddo
	enddo
#_f2c      ampz = cdabs(zn2(imin))
#f2c      ampz = zabs(zn2(imin))
	zz = dconjg(zn2(imin))/(ampz**2)
	zr = 0.5d0*(dble(zz) + dble(zn1(jmin)))
	zi = 0.5d0*(dimag(zz) + dimag(zn1(jmin)))
  	zn(i) = dcmplx(zr,zi)
	do j = 1,order
 	   x(imin,j) = -1.0d0
  	   x(j,jmin) = -1.0d0
	enddo
      enddo
c
      return
      end
***********************************************************************
      subroutine rselect (zn,index,new,order,szind)
c IN	: index,order
c INOUT	: zn
c OUT	: new
c
c part of the LPSVD package
c
c Keep only the roots whose index are given ; new is new order
c
      implicit none

      integer order,i,new,szind,index(szind)
      complex*16 zn(order)
	
      do i = 1,new
	 zn(i) = zn(index(i))
      enddo

      return
      end

C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Authors :       M.A.Delsuc
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully 
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C
      subroutine phase2d(tdata,size1,size2,p0,p1,direc)
c IN	: size1,size2,p0,p1,direc
c INOUT	: tdata
C  This subroutine applies a NMR-type phase correction to the hypercomplex
C data tdata(i,j) of length si1im,si2im, with p0 as zero-order correction and
C p1 as first-order correction. P0 and P1 are both in degree. If Direc is
C 1 correction is applied in Dim 1 (vertical), if direc is 2, correction
C is applied in dim 2
C
C MAD modified 10 jul 2002 
C     to remove the column move, and write on complex rotations.
C
      Implicit none

      Integer size1, size2, direc
      real tdata(size2,size1)
      real P0, P1
#include "sizeparam.inc"
#include "controlc.inc"
      Integer i,m,szp2
      real z, pi, pp0, pp1
      parameter (pi=3.1415926)

      if (direc.eq.2) then
	call prcvect(work1d,cmplx(1.0,0.0),size2/2)
	call phase(work1d,p0,p1,size2/2)    ! compute phase once
	do i=1,size1                        ! and apply many
	    call cdiavect(tdata(1,i),work1d, tdata(1,i), size2/2)
            if (control.eq.1) return
	enddo
      elseif (direc.eq.1) then
c         do 101,i=1,size2
c            call getcol(work1d,tdata,i,size1,size2)
c            call phase(work1d,p0,p1,size1/2)
c            call putcol(work1d,tdata,i,size1,size2)
c            if (control.eq.1) return
c101      continue

        pp1 = p1*pi/180.0
        pp0 = p0*pi/180.0 - pp1/2.0

	    call powtwo(size2,m)
	    if (m.ne.0) then	! swa / usw work on power of 2 only
		szp2 = (size2+m)
	    else
		szp2 = size2
	    endif
	do i=1,size1,2                  ! phase each row with the varying phase 
C doing this is faster because i) get/put col is slow (specially on 2**n sizes)
C                             ii) there is much less cos/sin computation
	    call copvect(work1d,tdata(1,i),size2)	! get them by pair
	    call copvect(work1d(szp2+1),tdata(1,i+1),size2)
	    call swavect(work1d,2*szp2)			! and swap complexes

	    z = pp0 + pp1*float((i/2-1))/((size1/2)-1)
	    call cmltvect(work1d,work1d, cmplx( cos(z), sin(z) ), size2)	! apply phase rotation

	    call uswvect(work1d,2*szp2)			! unswap and store
	    call copvect(tdata(1,i),work1d,size2)
	    call copvect(tdata(1,i+1),work1d(szp2+1),size2)
	    if (control.eq.1) return
	enddo
      endif
      return
      end


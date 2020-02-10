C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Authors :       M.A.Delsuc, V.Stoven, M.Robin
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully 
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C
      subroutine median1d(array,size,nsm,inx)
c IN	: size,nsm,inx
c INOUT	: array
c
C performs a median filtering on a nsm wide window, and gets the inxth
C point 
C moves array by nsm/2 points

      Implicit None
      integer size,nsm,inx
      real array(size)

#include "sizeparam.inc"       ! used only for the tindex(szindex) buffer
      integer i,j

      If (nsm.lt.2 .or. nsm.gt.size-2 .or.
     *    inx.lt.1 .or. inx.gt.nsm .or. inx.gt.szindex) then
         write(*,*) 'Error in parameters'
         return
      endif

      do i=1,size-nsm
        do j=1,nsm
           tindex(j) = array(i+j-1)
        enddo
        call bsort(tindex,nsm)
        array(i) = tindex(inx)
      enddo
      size = size-nsm
      return
      end

      subroutine median2d(array,size1,size2,nsm1,nsm2,inx)
c IN	: size1,size2,nsm1,nsm2,inx
c INOUT	: array
C performs a median filtering on a nsm1.nsm2 wide window, and gets the inxth
C point 
C array(size2,size1) is real

      Implicit None
      integer size1,nsm1,size2,nsm2,inx,nstot
      real array(size2,size1)

#include "sizeparam.inc"
      integer i1,j1,i2,j2

      If (nsm1.lt.2 .or. nsm1.gt.size1-2 .or.
     *    inx.lt.1 .or. inx.gt.(nsm1*nsm2) .or. inx.gt.szindex .or.
     *    nsm2.lt.2 .or. nsm2.gt.size2-2) then
         write(*,*) 'Error in parameters'
         return
      endif

      do i1=1,size1-nsm1
        do i2=1,size2-nsm2
          do j1=0,nsm1-1
            do j2=0,nsm2-1
              tindex(j1*nsm2+j2+1) = array(i2+j2,i1+j1)
            enddo
          enddo
        nstot = nsm1*nsm2
        call bsort(tindex,nstot)
        array(i2,i1) = tindex(inx)
        enddo
      enddo
      return
      end


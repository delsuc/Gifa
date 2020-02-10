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
      subroutine expmult(array,size,lb,itype)
c IN	: size,lb,itype
c INOUT	: array
c
c multiplies the array with an exponential, which damping is lb.
c if itype .eq. 0 data are real
c if itype .eq. 1 data are complex
c
      implicit none
                   
      integer size,itype
      real array(size),lb

      integer i
      real zz,z

#vms      call Lib$flt_under(0)

      z = lb/4000.0
      if (size.gt.1) then
       if (itype.eq.0) then
         do i=2,size
            zz = exp(-z*(i-1))
            array(i) = array(i)*zz
         enddo
       else
         do i=3,size,2
            zz = exp(-z*(i-1))
            array(i) = array(i)*zz
            array(i+1) = array(i+1)*zz
         enddo
       endif
      endif

      return
      end

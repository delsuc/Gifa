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
      subroutine gausmult(array,size,gb,itype)
c IN	: size,gb,itype
c INOUT	: array
c
c multiplies the array with an gaussian, which damping is gb.
c if itype .eq. 0 data are real
c if itype .eq. 1 data are complex
c

      implicit none
#include "constant.inc"
      integer size,itype
      real array(size),gb

      integer i
      real zz,z

#vms      Call Lib$flt_under(0)

c      z = (gb/(pi*2000.0))**2
      z = (gb/(pi*2000.0) * pi/(4*sqrt(log(2.0))))**2
      if (size.gt.1) then
       if (itype.eq.0) then
         do i=2,size
            zz = exp( -z*(i-1)*(i-1) )
            array(i) = array(i)*zz
         enddo
        else
         do i=3,size,2
            zz = exp( -z*(i-1)*(i-1) )
            array(i) = array(i)*zz
            array(i+1) = array(i+1)*zz
         enddo
        endif
      endif

      return
      end

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
      subroutine dsavect(data,size,shift,factor)
c IN	: size,shift,factor
c INOUT	: data
c
c replaces data(i) by data(i) + factor*data(i+shift)
c
      implicit none
      integer shift,size
      real data(size),factor

      if (shift.lt.1 .or. size.lt.shift) return
      call addvect(data,data,data(shift+1),factor,size-shift)
      call prpvect(data(size-shift+1),0.0,shift)
      return
      end

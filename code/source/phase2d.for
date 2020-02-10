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

      Implicit none

      Integer size1, size2, direc
      real tdata(size2,size1)
      real P0, P1
#include "sizeparam.inc"
#include "controlc.inc"
      Integer i

      if (direc.eq.2) then
         do 100,i=1,size1
            call phase(tdata(1,i),p0,p1,size2/2)
            if (control.eq.1) return
100      continue
      elseif (direc.eq.1) then
         do 101,i=1,size2
            call getcol(work1d,tdata,i,size1,size2)
            call phase(work1d,p0,p1,size1/2)
            call putcol(work1d,tdata,i,size1,size2)
            if (control.eq.1) return
101      continue
      endif
      return
      end


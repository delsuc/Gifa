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

      subroutine stockpoint(tablo,si,min,max,nombre,signpic
     *                      ,zoom,zol,zou)

      implicit none

#include "maxentv.inc"
#include "sizebase.inc"
#include "peaksize.inc"
    
      integer si,col,nombre,nopic,signpic,zoom,zol,zou,cd,cf
      real  tablo(si),som,somm1,somp1,min,max
c      real somm2,somp2

      nopic=1
      if (zoom.eq.1) then
          cd = zol
          cf = zou
      else
          cd = 2
          cf = si-1
      endif
      do col=cd,cf
        som=tablo(col)
        if (som.ge.min .and. som.le.max) then
          somm1=tablo(col-1)
c          somm2=tablo(col-2)
          somp1=tablo(col+1)
c          somp2=tablo(col+2)
c          if (som.gt.somm1 .and. som.gt.somp1 .and.   signpic
c     *        som.gt.somm2 .and. som.gt.somp2) then
          if (signpic.eq.1  .and. som.gt.somm1 .and. som.gt.somp1 .or.
     *       signpic.eq.-1 .and. som.lt.somm1 .and. som.lt.somp1) then
            peak1d(nopic,1)=col
            peak1d(nopic,2)=3.0
            peak1d(nopic,3)=som
            peak1d(nopic,4)=0.0
            nopic=nopic+1
            if (nopic.gt.peakmax) then
            call gifaout('too many peaks,change parameters in MINIMAX')
              go to 10
             endif 
         endif
        endif
      enddo               
      nombre=nopic-1                    
10    return
      end


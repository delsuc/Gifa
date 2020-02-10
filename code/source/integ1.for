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
       subroutine integ1(tab,rap,monte,min,
     & taille,zq,signpic,shift,noise)
c IN	: tab,rap,monte,min,taille,zq,signpic,shift,noise
c OUT	: peak1d
c realizes the integration of the 1D peak table

      implicit none
#include "sizeparam.inc"
#include "peaksize.inc"
#include "zoomparamv.inc"

      integer limsup,liminf,col,xx,zz,taille
      integer zq,signpic
      real   rap,monte,min,tab(taille),tabj,tabjp1
      real   incert,noise,shift,amp,volume,zz4
      logical   test
      integer poinpic
      character*256 st

      if (noise.eq.0.0)  
     * call gifaout('Warning: NOISE is 0.0, and the intensity errors will 
     * not be calculated: use EVALN')

      call gifaout ('Peak coordinates in '//unitx)
      write(st,'(2x,A6,A10,A15,A12,A6)')
     * 'index','coord','volume','left/right extent','error'
      call gifaout(st)
      do zz=1,nbpic1d
         test = .false.      
         col = peak1d(zz ,1)   
         tabj = signpic*tab(peak1d(zz,1))
         if (zq.eq.1) tabj = abs(tabj)
         amp = tabj

1        if (col+1.le.taille) then
            tabjp1=signpic*tab(col+1)
            if (zq.eq.1) tabjp1 = abs(tabjp1)
            test=((tabjp1-tabj).gt.monte .or.
     * tabjp1.lt.min .or. tabjp1.lt.(amp/rap))
         else 
            test = .true.
         endif                                 
         if (.not.test) then 
            tabj = tabjp1
            col = col + 1
            go to 1
         endif
         limsup = col
              
         test = .false.
         col = peak1d(zz,1)                      
         tabj = signpic*tab(peak1d(zz,1))
         if (zq.eq.1) tabj = abs(tabj)
         amp = tabj

2        if (col-1.ge.1) then
            tabjp1 = signpic*tab(col-1)
            if (zq.eq.1) tabjp1 = abs(tabjp1)
            test=((tabjp1-tabj).gt.monte .or.
     *        tabjp1.lt.min .or. tabjp1.lt.(amp/rap))
         else 
            test = .true.
         endif                                 
         if (.not.test) then 
            tabj = tabjp1
            col = col - 1
            goto 2
         endif
         liminf = col 

         volume = 0.0
         poinpic = 0                                
         do xx = liminf,limsup
            poinpic = poinpic + 1
            volume = volume + tab(xx)
         enddo

         volume = volume - (signpic*poinpic*shift)
         peak1d(zz,2) = max(1.0,float(limsup-liminf)/2.0)
         peak1d(zz,3) = volume/peak1d(zz,2)
         incert=sqrt(float(poinpic))*noise
         call currunitr(zz4,peak1d(zz,1),1)
         if (volume.ne.0.0) then
            write(st,46) zz,zz4,volume,liminf-int(peak1d(zz,1)),
     & limsup-int(peak1d(zz,1)),100.0*incert/volume
         else
            write(st,46) zz,zz4,volume,liminf-int(peak1d(zz,1)),
     & limsup-int(peak1d(zz,1)),0.0
46          format(I6,F10.3,G15.6,i6,i6,' +/-',f6.1,'%')
         endif
         call gifaout(st)
      enddo
      porig1d = 2

      return
      end

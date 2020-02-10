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
      subroutine bornelig(tablo,si1,si2,no,rap,monte,min,
     *                    lim1,lim2,signpic,zq)
c IN	: tablo,si1,si2,no,rap,monte,min,signpic,zq
c OUT 	: lim1,lim2
c used by 2D INTEG
c returns the limits along lines of peak no as defined in peak2d

      implicit none

#include "sizeparam.inc"      
#include "peaksize.inc"
                                             
      integer no,lim1,lim2,col,si2,si1,lig,k,zq,signpic
      real   rap,monte,min,tablo(si2,si1),tabj,tabjm1,big
      logical   test

      test=.false.      
      col=peak2d(no,1)                      
      lig=peak2d(no,4)
      lim2=col
      lim1=col
      if (peak2d(no,5).gt.0.0) then
        big=signpic*tablo(peak2d(no,1),peak2d(no,4))                      
      else
        k=abs(peak2d(no,5))
        big=signpic*tablo(peak2d(k,1),peak2d(k,4))                      
      endif
      if (zq.eq.1) big = abs(big)

1     if (col+1.le.si2) then
        tabjm1=signpic*tablo(col,lig)
        if (zq.eq.1) tabjm1 = abs(tabjm1)
        col=col+1
        tabj=signpic*tablo(col,lig)
        if (zq.eq.1) tabj = abs(tabj)
        test=((tabj-tabjm1).gt.monte .or.
     *        tabj.lt.min .or. tabj.lt.(big/rap))             
      else 
        test=.true.
      endif                                 
      if (.not.test) then 
        tabjm1=tabj
        lim2=col
        go to 1
      endif
              
      test=.false.
      col=peak2d(no,1)                      
      lig=peak2d(no,4)

2     if (col-1.ge.1) then 
        tabjm1=signpic*tablo(col,lig)
        if (zq.eq.1) tabjm1 = abs(tabjm1)
        col=col-1
        tabj=signpic*tablo(col,lig) 
        if (zq.eq.1) tabj = abs(tabj)
        test=((tabj-tabjm1).gt.monte .or.
     *        tabj.lt.min .or. tabj.lt.(big/rap))             
      else 
        test=.true.
      endif                                 
      if (.not.test) then 
        tabjm1=tabj
        lim1=col
        go to 2
      endif    
                                     
      return
      end
C************************************************************************
      subroutine bornecol(tablo,si1,si2,no,rap,monte,min,
     *          lim1,lim2,signpic,zq)

c IN	: tablo,si1,si2,no,rap,monte,min,signpic,zq
c OUT 	: lim1,lim2
c used by 2D INTEG
c returns the limits along columns of peak no as defined in peak2d

      implicit none

#include "sizebase.inc"      
#include "peaksize.inc"
                                             
      integer no,lim1,lim2,col,si2,si1,lig,k,signpic,zq
      real   rap,monte,min,tablo(si2,si1),tabj,tabjm1,big
      logical   test
                                                
      test=.false.
      col=peak2d(no,1)
      lig=peak2d(no,4)
      lim2=lig
      lim1=lig
      if (peak2d(no,5).gt.0.0) then
        big=signpic*tablo(peak2d(no,1),peak2d(no,4))
      else
        k=abs(peak2d(no,5))
        big=signpic*tablo(peak2d(k,1),peak2d(k,4))
      endif
      if (zq.eq.1) big = abs(big)

1     if (lig+1.le.si1) then
        tabjm1=signpic*tablo(col,lig)
        if (zq.eq.1) tabjm1 = abs(tabjm1)
        lig=lig+1
        tabj=signpic*tablo(col,lig) 
        if (zq.eq.1) tabj = abs(tabj)
        test=((tabj-tabjm1).gt.monte .or.
     *        tabj.lt.min .or. tabj.lt.(big/rap))             
      else 
        test=.true.
      endif                                 
      if (.not.test) then 
        tabjm1=tabj
        lim2=lig
        go to 1
      endif
              
       test=.false.
       col=peak2d(no,1)                      
       lig=peak2d(no,4)

2     if (lig-1.ge.1) then 
        tabjm1=signpic*tablo(col,lig)
        if (zq.eq.1) tabjm1 = abs(tabjm1)
        lig=lig-1
        tabj=signpic*tablo(col,lig) 
        if (zq.eq.1) tabj = abs(tabj)
        test=((tabj-tabjm1).gt.monte .or.
     *        tabj.lt.min .or. tabj.lt.(big/rap))             
      else 
        test=.true.
      endif                                 
      if (.not.test) then 
        tabjm1=tabj
        lim1=lig
        go to 2
      endif
                              
      return
      end
C************************************************************************
      subroutine limlig(tablo,si1,si2,no,rap,monte,min,
     *            lig,colsom,lim1,lim2,limlig1,limlig2,signpic,zq)
     
c IN	: tablo,si1,si2,no,rap,monte,min,lig,colsom,lim1,lim2,signpic,zq
c OUT 	: limlig1,limlig2
c used by 2D INTEG
c returns the limits along lines of peak no as defined in peak2d

      implicit none

#include "sizeparam.inc"      
#include "peaksize.inc"
                                             
      integer lim1,lim2,col,si2,si1,lig,colsom,no,limlig1,
     *    limlig2,k,signpic,zq
      real   rap,monte,min,tablo(si2,si1),tabj,tabjm1,big
      logical   test
                                                   
      test=.false.      
      col=colsom
      limlig1=col 
      limlig2=col 
      if (peak2d(no,5).gt.0.0) then
        big=signpic*tablo(peak2d(no,1),peak2d(no,4))
      else
        k=abs(peak2d(no,5))
        big=signpic*tablo(peak2d(k,1),peak2d(k,4))
      endif
      if (zq.eq.1) big = abs(big)

1     if (col+1.le.lim2) then 
        tabjm1=signpic*tablo(col,lig)
        if (zq.eq.1) tabjm1 = abs(tabjm1)
        col=col+1
        tabj=signpic*tablo(col,lig)
        if (zq.eq.1) tabj = abs(tabj)
        test=((tabj-tabjm1).gt.monte .or.
     *        tabj.lt.min .or. tabj.lt.(big/rap))             
      else 
        test=.true.
      endif                                 
      if (.not.test) then 
        tabjm1=tabj
        limlig2=col 
        go to 1
      endif

      col=colsom            
      test=.false.

2     if (col-1.ge.lim1) then 
        tabjm1=signpic*tablo(col,lig)
        if (zq.eq.1) tabjm1 = abs(tabjm1)
        col=col-1
        tabj=signpic*tablo(col,lig) 
        if (zq.eq.1) tabj = abs(tabj)
        test=((tabj-tabjm1).gt.monte .or.
     *        tabj.lt.min .or. tabj.lt.(big/rap))             
      else 
        test=.true.
      endif                                 
      if (.not.test) then 
        tabjm1=tabj
        limlig1=col 
        go to 2
      endif

      return
      end

C**********************************************************************
       subroutine maxlig(tablo,si1,si2,lig,lim1,lim2,colsom,signpic,zq)

c IN	: tablo,si1,si2,lig,lim1,lim2,signpic,zq
c OUT 	: colsom
c used by 2D INTEG

       implicit none

#include "sizeparam.inc"
 
       integer si1,si2,lig,colsom,col,lim1,lim2,zq,signpic
       real   tablo(si2,si1),val,plugran

       plugran=signpic*tablo(lim1,lig)
       if (zq.eq.1) plugran = abs(plugran)
       colsom=lim1
       do col=lim1+1,lim2
         val=signpic*tablo(col,lig)
         if (zq.eq.1)  val = abs(val)
         if (val.gt.plugran) then
              plugran=val
              colsom=col
         endif
       enddo

       return
       end


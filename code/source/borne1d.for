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
      subroutine borne1d(tablo,si,no,rap,monte,min,lim1,lim2,
     *     signpic,zq)

c IN	: tablo,si,no,rap,monte,min,signpic,zq (??)
c OUT	: lim1,lim2
c used by 1D INTEG
c returns the limits of peak no as defined in peak1d
 
      implicit none

#include "sizeparam.inc"
#include "peaksize.inc"

      integer no,lim1,lim2,col,si,signpic,zq
      real   rap,monte,min,tablo(si),tabj,tabjp1,big
      logical   test

      test = .false.      
      col=peak1d(no,1)                      
c      tabj=signpic*peak1d(no,3)
      big=signpic*tablo(peak1d(no,1))
      if (zq.eq.1) big = abs(big)
      tabj=big

1     if (col+1.le.si) then
        tabjp1=signpic*tablo(col+1) 
        if (zq.eq.1) tabjp1 = abs(tabjp1)
        test=((tabjp1-tabj).gt.monte .or.
     *        tabjp1.lt.min .or. tabjp1.lt.(big/rap))
      else 
          test = .true.
      endif                                 
      if (.not.test) then 
          tabj=tabjp1
          col=col+1
          go to 1
      endif
      lim2=col
              
      test=.false.
      col = peak1d(no,1)                      
c      tabj = signpic*peak1d(no,3)
      big = signpic*tablo(peak1d(no,1))
      if (zq.eq.1) big = abs(big)
      tabj = big

2     if (col-1.ge.1) then
        tabjp1 = signpic*tablo(col-1) 
        if (zq.eq.1) tabjp1 = abs(tabjp1)
        test=((tabjp1-tabj).gt.monte .or.
     *        tabjp1.lt.min .or. tabjp1.lt.(big/rap))
      else 
        test = .true.
      endif                                 
      if (.not.test) then 
        tabj=tabjp1
        col=col-1
        go to 2
      endif
      lim1=col 

      return
      end

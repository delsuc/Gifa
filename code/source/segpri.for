C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Author :       A.Rouh
C                    NMR Laboratory,
C                    ICSN - CNRS
C                    91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully 
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C
      subroutine stddev(debug,size)
c part of BCORR 3
      implicit none

      integer debug,size

#include "sizeparam.inc"
#include "linecor2v.inc"         
#include "constant.inc"


      integer i,n
      real s0,s1,us0,vs0



      s0 = maxexp / ldbu 
10    us0 = ldbu * s0
      call copvect(corr,source1,size)
      n = 0
      do 20 i=1,size
         if (abs(source1(i)).gt.us0) then
            corr(i) = 0.0
           n = n + 1
         endif
20    continue


      call dotvect(s1,corr,corr,size)
      s1 = sqrt(s1 / (size-n))   
      if (abs(s1-s0).gt.epsilon) then 
         s0 = s1
         goto 10
      endif
      vs0 = ldbv * s0
      call prpvect(segm,1.0,size)
cvd& nodepchk
      do 30 i=1,size
         if (abs(source1(i)).gt.vs0) segm(i) = 0
30    continue 

      return
       end

c*************************************************************************
      subroutine seuil(debug,size)
c part of BCORR 3

      implicit none

      integer debug,size

#include "sizeparam.inc"
#include "linecor2v.inc"         

      integer i,j
      real a,sdb2,scb2,sds2,scs2,b

      b = size
      call dotvect(a,source1,source1,size)
      a = sqrt(a/b)
      sdb2 = a*sdb
      sds2 = a*sds
      call dotvect(a,source2,source2,size)
      a = sqrt(a/b)
      scb2 = a*scb
      scs2 = a*scs


      j = 0
cvd$ cncall 
cvd$ nodepchk
      do 10 i=1,size
         if ((abs(source1(i)).lt.sdb2).and.
     *(abs(source2(i)).lt.scb2)) then
            segm(i) = 1
            if (j.ne.0) then 
               call prpvect(segm(j),1.0,i-j)
               j = 0
            endif
         else if ((abs(source2(i)).gt.scs2).or.
     *(abs(source1(i)).gt.sds2)) then
            segm(i) = 0
            if (j.ne.0) then
               if (segm(j).eq.1) then
                  call prpvect(segm(j),1.0,i-j)
               else
                  call prpvect(segm(j),0.0,i-j)
               endif
            j = 0
            endif
         else if (j.eq.0) then 
            j=i-1 
         endif
10    continue
      if (j.ne.0) call prpvect(segm(j),1.0,i-j)

      return
      end

c****************************************************************
      subroutine morpho(size,n10,n20)
c part of BCORR 3

      integer size
      real n10,n20

#include "sizeparam.inc"
#include "linecor2v.inc"

      integer nn1,nn2,i,fact,n1,n2

      fact = 1 + (size-1) / blocbase
      n1 = n10*fact
      n2 = n20*fact
      nn1 = 2*n1+1
      nn2 = 2*n2+1

cvd$r cncall
c     errosion ligne de base
      if (n1.gt.0) then
         call copvect(corr,segm,size)
         do 9 i=1,n1
            if (corr(i).eq.0.0) call prpvect(segm,0.0,i+n1)
9        continue
         do 10 i=1+n1,size-n1
            if (corr(i).eq.0.0) call prpvect(segm(i-n1),0.0,nn1)
10       continue
         do 11 i=size-n1,size
            if (corr(i).eq.0.0) call prpvect(segm(i-n1),0.0,size-i+n1+1)
11       continue

c     dilatation ligne d base
         call copvect(corr,segm,size)
         do 19 i=1,n1
            if (corr(i).eq.1.0) call prpvect(segm(i-n1),1.0,i+n1)
19       continue
         do 20 i=1+n1,size-n1
            if (corr(i).eq.1.0) call prpvect(segm(i-n1),1.0,nn1)
20       continue
         do 21 i=size-n1,size
            if (corr(i).eq.1.0) call prpvect(segm(i-n1),1.0,size-i+n1+1)
21       continue
      endif



c     errosion signal
      if (n2.gt.0) then
         call copvect(corr,segm,size)
         do 29 i=1,n2
            if (corr(i).eq.1.0) call prpvect(segm(i-n2),1.0,i+n2)
29       continue
         do 30 i=1+n2,size-n2
            if (corr(i).eq.1.0) call prpvect(segm(i-n2),1.0,nn2)
30       continue
         do 31 i=size-n2,size
            if (corr(i).eq.1.0) call prpvect(segm(i-n2),1.0,size-i+n2+1)
31       continue


c     dilatation du signal
         call copvect(corr,segm,size)
         do 39 i=1,n2
            if (corr(i).eq.0.0) call prpvect(segm(i-n2),0.0,i+n2)
39       continue
         do 40 i=1+n2,size-n2
            if (corr(i).eq.0.0) call prpvect(segm(i-n2),0.0,nn2)
40       continue
         do 41 i=size-n2,size
            if (corr(i).eq.0.0) call prpvect(segm(i-n2),0.0,size-i+n2+1)
41       continue
      endif

      return
      end



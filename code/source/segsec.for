C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Author :       A.Rouh
C                    NMR Laboratory,
C                    ICSN - CNRS
C                    91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully 
C licensed to do so form the above laboratory.ed or used in any other program,
C without written permission from the authors.
C

      subroutine apply_segm(size)
c part of BCORR 3

      implicit none

      integer size

#include "sizeparam.inc"
#include "linecor2v.inc"         
#include "constant.inc"

      integer i,j,i1,i2,fen,n,cpt
      real a,b,ri1,ri2,rj
      
      j = approx / 100
      i = approx / 10 - 10 * j
      if (i.ne.1) then
         call diavect(segm,segm,source,size)
      else
         call copvect(corr,segm,size)
         call copvect(segm,source,size)

         i1 = 1 + (size-1)/blocbase
         fen  = i1*fenlin
         n = fen*2+1

         do 2 i1=1,fen
            cpt = 0
            a = 0
            do 3 i2=1,i1+fen
               if (corr(i2).eq.1) then
                  cpt = cpt + 1
                  a = a + source(i2)
               endif
3           continue
            if (cpt.gt.0) then
               b = cpt
               a = a / b
               segm(i1) = a
            endif
2        continue



         do 4 i1=fen+1,size-fen
            cpt = 0
            a = 0
            do 5 i2=i1-fen,i1-fen+n
               if (corr(i2).eq.1) then
                  cpt = cpt + 1
                  a = a + source(i2)
               endif
5           continue
            if (cpt.gt.0) then
               b = cpt
               a = a / b
               segm(i1) = a
            endif
4        continue



         do 6 i1=size-fen+1,size
            cpt = 0
            a = 0
            do 7 i2=i1-fen,size
               if (corr(i2).eq.1) then
                  cpt = cpt + 1
                  a = a + source(i2)
               endif
7           continue
            if (cpt.gt.0) then
               b = cpt
               a = a / b
               segm(i1) = a
            endif
6        continue


         i1 = 0
         i2 = 0
         i = 0
10       i = i + 1
         if (corr(i).ne.0 .and. i.lt.size) goto 10
         if (i.lt.size) then
            i1 = i
            ri1 = i
20          i = i + 1
            if (corr(i).eq.0 .and. i.lt.size) goto 20
            if (i.lt.size) then
               i2 = i 
               ri2 = i
               if (i1.gt.fen) then
                  a = (segm(i2)-segm(i1))/(ri2-ri1)
                  b = segm(i1) - a * ri1
                  do 30 j=i1,i2
                     rj = j
                     segm(j) = a * rj + b
30                continue
                  i1 = 0
               else
                  call copvect(weigth,segm,n)
                  call movave(segm,weigth,n,fenlin,blocbase)
                  i1 = 1 
                  ri1 = 1
                  a = (segm(i2)-segm(i1))/(ri2-ri1)
                  b = segm(i1) - a * ri1
                  do 32 j=i1,i2
                     rj = j
                     segm(j) = a * rj + b
32                continue
                  i1 = 0
               endif
               goto 10
            else
               if (i1.lt.fen) then
                  call copvect(weigth,segm,n)
                  call movave(segm,weigth,n,fenlin,blocbase)
                  i1 = 1 
                  ri1 = 1
               endif
               if (i1+fen.le.size) then
                  i2 = size - n + 1
                  call copvect(weigth,segm(i2),n)
                  call movave(segm(i2),weigth,n,fenlin,blocbase)
                  i2 = size
                  ri2 = i2
                  a = (segm(i2)-segm(i1))/(ri2-ri1)
                  b = segm(i1) - a * ri1
                  do 34 j=i1,i2
                     rj = j
                     segm(j) = a * rj + b
34                continue
               else
                  fen =  size - i1 + 1
                  call copvect(weigth,segm(i1),n)
                  call movave(segm(i1),weigth,n,fenlin,blocbase)
               endif
            endif
          endif

      endif

      return
      end

c**********************************************************************
      subroutine segm2_inter(size,portion)
c part of BCORR 3

      implicit none

      integer size,portion(*)


#include "sizeparam.inc"
#include "linecor2v.inc"         
#include "constant.inc"
#include "transform.inc"

      integer i,j

      nbportion = 1
      i = 0


c     recherche du premier point de la premiere portion

5     i = i + 1
      if (window(i).eq.1 .and. segm(i).eq.1) then
         j = i+1
6        j = j - 1
         if (segm(j).eq.1 .and. j.gt.1) goto 6
         portion(1) = j
      else
         if (i.lt.size) then
            goto 5
         else
            nbportion = 0
         endif
      endif
      if (nbportion.ne.0) goto 20


c     recherche du premier point de la prochaine portion

10    i = i + 1
      if (window(i).eq.1 .and. segm(i).eq.1) then
         j = i+1
15       j = j - 1
         if (segm(j).eq.1 .and. j.gt.portion(2*(nbportion-1))) goto 15
         if (j.eq.portion(2*(nbportion-1))) then
            nbportion = nbportion -1
         else
            portion(2*(nbportion-1)+1) = j
         endif
      else
         if (i.lt.size) then
            goto 10
         else
            nbportion = nbportion - 1
         endif
      endif

c     recherche du second point de la portion courante

20    i = i + 1
      if (window(i).eq.0 .and. segm(i).eq.0) then
         j = i+1
25       j = j - 1
         if (segm(j).eq.0 .and. j.gt.portion(2*(nbportion-1)+1)) goto 25
         if (j.eq.portion(2*(nbportion-1)+1)) then
             nbportion = nbportion - 1
         else
            portion(2*nbportion) = j
            if (i.lt.size) then
               nbportion = nbportion + 1 
               if (nbportion.le.maxportion) goto 10
               nbportion = nbportion - 1
            endif
         endif
      else
         if (i.lt.size) then
            goto 20
         else
            portion(2*nbportion) = size
         endif
      endif
30    continue
      return
      end

c*********************************************************************
      subroutine segm2_auto(portion,size,seuil)
c part of BCORR 3

      implicit none

      integer portion(*),size
      real seuil

#include "sizeparam.inc"
#include "linecor2v.inc"         
#include "constant.inc"

      real rsize,s0,min,max,vg,vd,sp,sn
      integer i,i1,i2,i3,imin,imax,fen


      rsize = size
      call dotvect(s0,source,source,size)
      s0 = sqrt(s0/rsize)
      sp = seuil * s0
      sn = -1.0 * sp


      nbportion = 0
      
      fen = 1 + fensegm2*(size-1)/blocbase

      i=0
c      i1 = 0
      i1 = 1
      i2 = 0
10    i=i+1
      if (segm(i).eq.0 .and. i.lt.size) goto 10
      if (i.gt.size) goto 40

20    i=i+1
      if (segm(i).eq.1 .and. i.lt.size) goto 20
      if (i.lt.size) then
         i2 = i
      else
         goto 40
      endif


30    i=i+1
      if (segm(i).eq.0 .and. i.lt.size) goto 30
      if (i.lt.size) then
         i3 = i
         call mnxvect(min,max,imin,imax,source(i2),i3-i2+1)
c         if (max.lt.seuil*s0 .or. min.gt.mseuil*s0) then
         rsize = fen
         call sumvect(vg,source(i2-fen),fen)
         vg = vg / rsize
         call sumvect(vd,source(i3),fen)
         vd = vd / rsize
         if (abs(vg-vd).lt.sp .or.
     *       max.lt.sp .or. min.gt.sn) then
            i2 = 0
            goto 20
         else
            nbportion = nbportion + 1
            portion(2*(nbportion-1)+1) = i1
            portion(2*nbportion) = i2
            i1 = i3
            i2 = 0
            if (nbportion.lt.maxportion) goto 20
         endif
      else
         i2 = 0
      endif

40    if (i2.eq.0 .and. i1.ne.0) then
         nbportion = nbportion + 1
         portion(2*(nbportion-1)+1) = i1
         portion(2*nbportion) = size
      endif

c      write(*,*) 'nbportion = ' , nbportion
c      do 50 i=1,2*nbportion
c         write(*,*) 'portion(',i,')=',portion(i)
c50    continue

      return
      end

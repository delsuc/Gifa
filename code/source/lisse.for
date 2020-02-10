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

      subroutine hyste(imaged,images,size,pr)
c used by linecor2


      implicit none

      integer size
      real images(size),imaged(size),pr



      real a,c,p,ds,rsize,c0,cc,dp
      integer i,cpt,maxcpt,sens

      parameter(maxcpt = 5)

      call dotvect(ds,images,images,size)
      rsize = size
      ds = sqrt(ds/rsize)
      p = pr*ds
      dp = p/log(float(maxcpt))

      c = images(1)
      c0 = c
      cc = c
      sens = 0
      imaged(1) = c

      do 10 i=2,size
         a = images(i) - c
         if (abs(a).gt.p) then
            if (a.gt.0) then
               c = images(i) - p
            else
               c = images(i) + p
            endif
         endif
         if (c .eq. c0) then
            imaged(i) = cc
         elseif (c .gt. c0) then
            if (sens .eq. 1) then
               if (cpt .lt. maxcpt) cpt = cpt + 1
            else 
               cpt = 1
            endif 
            sens = 1
            cc = c + dp * log(float(cpt)) 
            imaged(i) = cc
         else
            if (sens .eq. -1) then
               if (cpt .lt. maxcpt) cpt = cpt + 1
            else
               cpt = 1
            endif
            sens = -1
            cc = c - dp * log(float(cpt))
            imaged(i) = cc
         endif
         c0 = c
10    continue  

      return
      end



      subroutine movave(imaged,images,size,p,bb)

c used by linecor2

      implicit none

      integer size,bb
      real images(size),imaged(size),p




      real m,n1
      integer a,n,i


      i = 1 + (size-1)/bb
      a = p*i
      n = 2*a+1
      n1 = n
      n1 = 1 / n1


      if (n.lt.size) then

      do 20 i=1,a
         call sumvect(m,images(1),i+a)
         m = m / (i+a)
         imaged(i) = m
20    continue


cvd$ cncall
      do 30 i = a+1,size-a
         call sumvect(imaged(i),images(i-a),n)
30    continue

      do 35 i = a+1,size-a
         imaged(i) = imaged(i) * n1
35    continue


      do 40 i = size-a+1,size
         call sumvect(m,images(i-a),size-i+1+a)
         m = m / (size-i+1+a)
         imaged(i) = m
40    continue

      else
         call sumvect(m,images,size)
         m = m / size
         call prpvect(imaged,m,size)
      endif
      return
      end


      subroutine movave2(imaged,images,size,win,bb,iter)

c used by linecor2

      implicit none

      integer size,iter,bb
      real images(size),imaged(size),win

      real m,n1
      integer k,n,i,fen


      k = 1 + (size-1) / bb
      fen = win*k
      n = 2*win+1
      n1 = n
      n1 = 1 / n1


cvd$r cncall


      do 10 k=1,iter/2
         do 20 i=1,fen
            call sumvect(m,images(1),i+fen)
            m = m / (i+fen)
            imaged(i) = m
20       continue


         do 30 i = fen+1,size-fen
            call sumvect(imaged(i),images(i-fen),n)
30       continue
         do 35 i = fen+1,size-fen
            imaged(i) = imaged(i) * n1
35       continue


         do 40 i = size-fen+1,size
            call sumvect(m,images(i-fen),size-i+1+fen)
            m = m / (size-i+1+fen)
            imaged(i) = m
40       continue



         do 50 i=1,fen
            call sumvect(m,imaged(1),i+fen)
            m = m / (i+fen)
            images(i) = m
50       continue


         do 60 i = fen+1,size-fen
            call sumvect(images(i),imaged(i-fen),n)
60       continue
         do 65 i = fen+1,size-fen
            images(i) = images(i) * n1
65       continue

         do 70 i = size-fen+1,size
            call sumvect(m,imaged(i-fen),size-i+1+fen)
            m = m / (size-i+1+fen)
            images(i) = m
70       continue

10    continue      
 

      if (iter.ne.(2*(iter/2))) then
         do 80 i=1,fen
            call sumvect(m,images(1),i+fen)
            m = m / (i+fen)
            imaged(i) = m
80       continue
         do 90 i = fen+1,size-fen
            call sumvect(imaged(i),images(i-fen),n)
90       continue
         do 95 i = fen+1,size-fen
            imaged(i) = imaged(i) * n1
95       continue

         do 100 i = size-fen+1,size
            call sumvect(m,images(i-fen),size-i+1+fen)
            m = m / (size-i+1+fen)
            imaged(i) = m
100      continue
      else
         call copvect(imaged,images,size)
      endif


      return
      end



      subroutine movave3(imaged,images,size,fen,bb,iter)

c used by linecor2

      implicit none

      integer size,iter,bb
      real images(size),imaged(size),fen

#include "sizeparam.inc"
#include "linecor2v.inc"

      integer k,i

      call movave(imaged,images,size,fen,bb)


        
      

cvd$ Nosync
      do 10 k=1,iter
      do 20 i=1,size
         if (source(i).gt.imaged(i)) then
            x(i) = imaged(i) 
         else
            x(i) = source(i)
         endif
20    continue
      call movave(imaged,x,size,fen,bb)
10    continue



      return
      end



      subroutine centre(source,size)
c used by linecor2

      implicit none

      integer size
      real source(size)



      real moy,nbpt

      nbpt = size
      call sumvect(moy,source,size)
      moy = moy / nbpt
      call adrvect(source,moy,size)

      return
      end



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

      subroutine deriv(debug,quoi,size)
c IN	: debug,quoi,size
c SIDE	: work3(20*bcmax+1)
c computes derivative and/or curvature of the 1D data held
c in source  ( work3(16*bcmax+1) ) and put it back in source1
c
c if quoi eq 1 derivative
c            2 curvature
c            3 both
c 
      implicit none

      integer debug,quoi,size

#include "sizeparam.inc"
#include "linecor2v.inc"
#include "paramfilev.inc"

      real tampon(bcmax),rp2,rp3,
     *dmin,dmax,cmin,cmax,d,d0,c0
      integer i,j,k,controle,p3,err

      equivalence (work3(20*bcmax+1),tampon(1))

c     calcul de derivee seulement

      if (quoi.eq.1) then

cvd$ nodepchk
         do 10 i=2,size-1
            tampon(i) = source1(i+1)-source1(i-1)
10       continue
         tampon(1) = source1(2)-source1(1)
         tampon(size) = source1(size)-source1(size-1)
         call copvect(source1,tampon,size)


c     calcul de courbure seulement     
      
      elseif (quoi.eq.2) then
cvd$ nosync
         do 20 i=2,size-1
            source2(i) = source1(i+1)-2*source1(i)+source1(i-1)
20       continue
         source2(1)=source2(2)
         source2(size)=source2(size-1)



 
c     calcul de derivee et de courbure     

      elseif (quoi.eq.3) then
cvd$ nodepchk
         do 30 i=2,size-1
            tampon(i) = source1(i+1)-source1(i)
30       continue
cvd$ nosync
         do 31 i=2,size-1
            source1(i) = tampon(i)+tampon(i-1)
31       continue
         source1(1) = tampon(1)
         source1(size) = tampon(size-1)
         do 32 i=2,size-1
            source2(i) = tampon(i)-tampon(i-1)
32       continue
         source2(1)=source2(2)
         source2(size)=source2(size-1)


      endif

      if (debug.ne.1) then
         return
      else
 

c     Debug is now on ... 

      if (dim.eq.1) then
         call message('   1: First derivative')
         call message('   2: Second derivative')
         controle = 1
         call getint2(controle,err) 
         if (controle.eq.1) then
            call copvect(column,source1,size)
         else if (controle.eq.2) then
            call copvect(column,source2,size)
         endif
      else
         p3 = si1im
         rp2 = size
         rp3 = p3
         do 35 i=1,size-1
            tampon(i)=datab(i+1)-datab(i)
35       continue

         do 40 i=2,size-1
            source1(i) = tampon(i)+tampon(i-1)
40       continue
         source1(1) = tampon(1)
         source1(size) = source1(size-1)
         do 50 i=2,size-1
            source2(i) = tampon(i)-tampon(i-1)
50       continue
         source2(1)=source2(2)
         source2(size)=source2(size-1)

         call prpvect(plane2d,0.0,size*p3)

         call mnxvect(dmin,dmax,i,j,source1,size)
         call mnxvect(cmin,cmax,i,j,source2,size)
         d = rp2 / (dmax - dmin)
         dmin = -dmin
         call adrvect(source1,dmin,size)
         call mltvect(source1,source1,d,size)
         d0 = dmin * d
         d = rp3 / (cmax - cmin)
         cmin = -cmin
         call adrvect(source2,cmin,size)
         call mltvect(source2,source2,d,size)
         c0 = cmin * d


c         do 60 k=1,size
c            i = source1(k)
c            j = source2(k)
c            if (i.lt.2) i = 2
c            if (i.gt.size-2) i = size-2
c            if (j.lt.2) j = 2
c            if (j.gt.p3-2) j = p3-2
c            plane2d((j-1)*size+i) = 1
c            plane2d(j*size+i) = 1
c60       continue  
    
         call prpvect(plane2d,0.0,size*p3) 
           
         j = c0 
         if (j.lt.2) j = 2
         if (j.gt.p3-2) j = p3-2
         do 70 i=1,size
            plane2d((j-1)*size+i) = -1
70       continue
         j= c0+1
         do 75 i=1,size
            plane2d((j-1)*size+i) = -1
75       continue
         i = d0
         if (i.lt.2) i = 2
         if (i.gt.size-2) i = size-2
         do 80 j=1,p3
            plane2d((j-1)*size+i) = -1
80       continue  
         i = d0+1
         if (i.lt.2) i = 2
         if (i.gt.size-2) i = size-2
         do 85 j=1,p3
            plane2d((j-1)*size+i) = -1
85       continue 

         do 86 k=1,size
            i = source1(k)
            j = source2(k)
            if (i.lt.2) i = 2
            if (i.gt.size-2) i = size-2
            if (j.lt.2) j = 2
            if (j.gt.p3-2) j = p3-2
            plane2d((j-1)*size+i) = plane2d((j-1)*size+i)+1
            plane2d(j*size+i) = plane2d(j*size+i)+1
            plane2d((j-1)*size+i+1) = plane2d((j-1)*size+i+1)+1
            plane2d(j*size+i+1) = plane2d(j*size+i+1)+1
86       continue  

      endif
      endif

      return
      end



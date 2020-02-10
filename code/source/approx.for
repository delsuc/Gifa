
c This file is a part of the GIFA program
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

      subroutine init_approx(size)

c IN	: size
c SIDE	: base
c initialize buffer for polynomial base line correction
c polynomial base depends on global var approx
      
      implicit none

      integer size


#include "sizeparam.inc"
#include "linecor2v.inc"         
#include "constant.inc"



      integer i,j,type
      real rsize,a,b,ri,rj


      rsize = size

      i = approx / 10
      type = approx - 10 * i


c     base de legendre

      if (type.eq.1) then

      a = 2 / (rsize - 1)
      b = - (rsize + 1) / (rsize - 1)
      do 10 i=1,size
         x(i) = i
10    continue
      call mltvect(x,x,a,size)
      call adrvect(x,b,size)
      call prpvect(base(1,0),1.0,size)
      call copvect(base(1,1),x,size)
      do 20 i=2,degre
         ri = i
         a = (2 * ri - 1) / ri
         b = (1 - ri) / ri
         call diavect(base(1,i),base(1,i-1),x,size)
         call mltvect(base(1,i),base(1,i),a,size)
         call addvect(base(1,i),base(1,i),base(1,i-2),b,size)
20    continue

c     base de tchebytchev

      else if (type.eq.2) then

      a = 2 / (rsize - 1)
      b = - (rsize + 1) / (rsize - 1)
      do 30 i=1,size
         x(i) = i
30    continue
      call mltvect(x,x,a,size)
      call adrvect(x,b,size)
      do 40 i=1,size
         weigth(i) = sqrt(1/(1.0-x(i)*x(i)+epsilon))
c      write(*,*) 'weigth(',i,')=',weigth(i)
40    continue
      call prpvect(base(1,0),1.0,size)
      call copvect(base(1,1),x,size)
      do 50 i=2,degre
         a = 2
         b = -1.0
         call diavect(base(1,i),base(1,i-1),x,size)
         call mltvect(base(1,i),base(1,i),a,size)
         call addvect(base(1,i),base(1,i),base(1,i-2),b,size)
50    continue
      
c     base de fourier

      else if (type.eq.3) then

      do 60 i=1,degre/2
         ri = i
         b = 2 * ri * pi / rsize
         do 70 j=1,size
           rj = j
           base(j,(2*i-1)) = cos(b*rj)
           base(j,(2*i)) = sin(b*rj)
70       continue
60    continue   

      endif

      return
      end







      subroutine approxim(indice,debut,sizeb,size,
     *y10,y11,y12,y20,y21,y22)

c IN	: indice,debut,sizeb,size (??)
c OUT	: y10,y11,y12,y20,y21,y22 (??)
c computes the best polynomial approximation -

	implicit none
      integer sizeb,size,indice(size)
      real y10,y11,y12,y20,y21,y22


#include "sizeparam.inc"
#include "linecor2v.inc"         
#include "constant.inc"

      integer i,j,type,debut, i1, i2


      real rsize,rsizeb,a,b,c1,ri,
     *c(0:degremax),d1(0:degremax),
     *d2(0:degremax),a2,b2,c2,norme(0:degremax)


      i = approx / 10
      type = approx - 10 * i
      rsize = size
      rsizeb = sizeb
      do 10 i=1,size
         indice(i) = (i-1)*rsizeb/rsize+1
10     continue
      

c     approximation de legendre


      if (type.eq.1) then
c     cvd$ cncall
      do 20 i=0,degre
         call i2dotvect(indice,a,base(1,i),base(1,i),size)
         call idotvect(indice,c(i),base(1,i),segm(debut),size)
         c(i) = c(i) / a
         norme(i) = sqrt(a)
20    continue        

      call prpvect(corr(debut),0.0,size)
      do 30 i=0,degre
         call iaddvect(indice,corr(debut),corr(debut),
     *base(1,i),c(i),size)
30    continue
      y10 = corr(debut)
      y20 = corr(debut+size-1)

C     calcul derivee et derivee seconde en i1

      d1(0) = 0.0
      d1(1) = 1.0
      d2(0) = 0.0
      d2(1) = 0.0
      do 50 i=2,degre
         ri = i
         a = (2.0 * ri - 1) / ri 
         b = a * base(indice(1),1)
         c1 = (1.0 - ri) / ri
         b2 = b
         a2 = 2.0 * a  
         c2 = c1
         d1(i) = a * base(indice(1),i-1) + b * d1(i-1) 
     *+ c1 * d1(i-2)
         d2(i) = a2 * d1(i-1) + b2 * d2(i-1) + c2 * d2(i-2) 
50    continue
      y11 = 0.0
      y12 = 0.0 
      do 60 i=0,degre
         y11 = y11 + c(i) * d1(i)
         y12 = y12 + c(i) * d2(i)
60    continue 
         y11 = 2.0 * y11 / (rsize - 1.0)
         y12 = 4.0 * y12 / ((rsize - 1.0) * (rsize -1.0))
      d1(0) = 0.0
      d1(1) = 1.0
      d2(0) = 0.0
      d2(1) = 0.0
      do 70 i=2,degre
         ri = i
         a = (2.0 * ri - 1.0) / ri 
         b = a * base(indice(size),1)
         c1 = (1.0 - ri) / ri
         b2 = b
         a2 = 2.0 * a  
         c2 = c1
         d1(i) = a * base(indice(size),i-1) 
     *+ b * d1(i-1) + c1 * d1(i-2)
         d2(i) = a2 * d1(i-1) + b2 * d2(i-1) + c2 * d2(i-2) 
70    continue
      y21 = 0.0
      y22 = 0.0 
      do 80 i=0,degre
         y21 = y21 + c(i) * d1(i) 
         y22 = y22 + c(i) * d2(i)
80    continue 
      y21 = 2.0 * y21 / (rsize - 1.0)
      y22 = 4.0 * y22 / ((rsize - 1.0) * (rsize -1.0))
     


      else if (type.eq.2) then


C     approximation de tchebytchev



      do 90 i=0,degre
         call idiavect(indice,source1,base(1,i),weigth,size)
         call idotvect(indice,a,base(1,i),source1,size)
         call idotvect(indice,c(i),segm(debut),source1,size)
         c(i) = c(i) / a
         norme(i) = sqrt(a)
90    continue        


      call prpvect(corr(debut),0.0,size)
      do 100 i=0,degre
         call iaddvect(indice,corr(debut),corr(debut),
     *base(1,i),c(i),size)
100    continue

      y10 = corr(i1)
      y20 = corr(i2)

C     calcul derivee et derivee seconde en i1


      a = 2 / (rsize - 1)
      b = - (rsize + 1) / (rsize - 1)
      do 110 i=1,size
         x(i) = i
110    continue
      call mltvect(x,x,a,size)
      call adrvect(x,b,size)
      y10 = corr(i1)
      y20 = corr(i2)


      d1(0) = 0.0
      d1(1) = 1.0
      d2(0) = 0.0
      d2(1) = 0.0
      do 120 i=2,degre
         ri = i
         a = 2.0    
         b = 2.0 * ri
         c1 = -1.0
         d1(i) = a * base(1,i-1) + b * d1(i-1) 
     *+ c1 * d1(i-2)
         d2(i) = a2 * d1(i-1) + b2 * d2(i-1) + c2 * d2(i-2) 
120    continue
      y11 = 0.0
      y12 = 0.0 
      do 130 i=0,degre
         y11 = y11 + c(i) * d1(i) / norme(i) 
         y12 = y12 + c(i) * d2(i) / norme (i) 
130    continue 
         y11 = 2.0 * y11 / (rsize - 1.0)
         y12 = 4.0 * y12 / ((rsize - 1.0) * (rsize -1.0))
      d1(0) = 0.0
      d1(1) = 1.0
      d2(0) = 0.0
      d2(1) = 0.0
      do 140 i=2,degre
         ri = i
         a = 2.0    
         b = 2.0 * ri
         c1 = -1.0
         d1(i) = a * base(size,i-1) 
     *+ b * d1(i-1) + c1 * d1(i-2)
         d2(i) = a2 * d1(i-1) + b2 * d2(i-1) + c2 * d2(i-2) 
140   continue
      y21 = 0.0
      y22 = 0.0 
      do 150 i=0,degre
         y21 = y21 + c(i) * d1(i) / norme (i) 
         y22 = y22 + c(i) * d2(i) / norme (i) 
150    continue 
      y21 = 2.0 * y21 / (rsize - 1.0)
      y22 = 4.0 * y22 / ((rsize - 1.0) * (rsize -1.0))
     



C     decomposition en serie de fourier

      else


      a = 2 / rsize
      do 160 i=1,degre
         call idotvect(indice,c(i),segm(debut),base(1,i),size)
160   continue
      call mltvect(c,c,a,degre)  

      call sumvect(a,segm(debut),size)
      a = a / rsize
      call prpvect(corr,a,size)


      do 170 i=1,degre
         call iaddvect(indice, corr,corr,base(1,i),c(i),size)
170   continue

      a = 2 * pi / rsize
      y10 = corr(1)
      y20 = corr(2)
      y11 = 0
      y21 = 0
      y12 = 0
      y22 = 0
      do 180 i=1,degre/2
         ri = i
         j = 2*i-1
         y11 = y11 + ri * (c(j+1) * base(1,j) - c(j) * base(1,j+1))
         y21 = y21 + ri * (c(j+1) * base(size,j) 
     *- c(j) * base(size,j+1))
180   continue
      y11 = a * y11
      y21 = a * y21

      do 190 i=1,degre/2
         ri = i * i
         j = 2*i-1
         y12 = y12 - ri * (c(j) * base(1,j) + c(j+1) * base(1,j+1))
         y22 = y22 - ri * (c(j) * base(size,j) 
     *+ c(j+1) * base(size,j+1))
190   continue
      y12 = a * a * y12
      y22 = a * a * y22



      endif

      return
      end





      subroutine prolonge(trace,size,i1,y10,y11,i2,y20,y21,y2) 

c IN	: trace,size (??)
c OUT	: y10,y11,i2,y20,y21,y2 (??)
c computes the best polynomial approximation (??)

	implicit none

      integer i1,i2,size,trace
      real y10,y11,y20,y21,y2

#include "sizeparam.inc"
#include "linecor2v.inc"


      integer id,if,i
c      real ri,ri1,ri2,num,den
      real a,b,c,d,
     *n,e,rn,c0,c2,rn2
     

      if (i1.ne.i2) then
      id = i1
      if = i2
      if ((i1.ne.i2).and.(i1.lt.size).and.(i2.lt.size)) then
         if (i1.eq.0) then
            id =1
            if = i2
               a = 0
               b = y2 / 2
               c = y21 - i2 * y2
               d= y20 - i2 * y21 + i2 * i2 * y2 / 2
         elseif (i2.eq.0) then            
            id =i1
            if = size
               a = 0
               b = y2 / 2
               c = y11 - i1 * y2
               d = y10 - i1 * y11 + i1 * i1 * y2 /2
         else

C     resolution systeme n=4 pour continuation c2

c            id = i1
c            if = i2
c            ri1 = i1
c            ri2 = i2
c            num = 0
c            den = 0
            n = i2 - i1
            rn = n
            rn2 = rn * rn
            c0 = 2*y20-rn*(y11+y21)-2*y10
            c2 = 6*y20-2*rn*(2*y11+y21)-6*y10
c            a1 = -1.0 * c0 / (rn2*rn)
c            b1 = c2 / (2*rn2)
c            c1 = y11 - (y20-y10)/rn
c            d1 = 0
c            do 5 i=0,n
c               ri = i
c               c3 = ((ri-2*rn)*ri+rn2)*ri*ri
c               num = num -rm (((a1*ri+b1)*ri+c1)*ri+d1)*c3
c               den = den + c3*c3
c5           continue 
c            a = num / den
            a = 0
            b = -1.0 * (c0 + 2*a*rn2*rn2) / (rn2*rn)
            c = (c2 + 2*a*rn2*rn2) / (2*rn2)
            d = y11
            e = y10
         endif

         do 10 i=0,n
            corr(id+i) = (((a*i+b)*i+c)*i+d)*i+e
10       continue
      endif
      endif

      return
      end
			



C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in CBS/Montpellier
C
C     Authors :       M.A.Delsuc
C                     C.B.S. Faculte de Pharmacie
C                     34060 Montpellier   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.

      subroutine confidence(func1d,p,xi,size,sizedata,up,down,verb)
c IN	: func1d,p,xi,size,verb
c OUT	: up,down

C search around the optimum value p(size) of the given func1d function,
C along the axis xi(size)
C and give the 95% confidence interval.
C func1d : the 1d version of the function to search
C     (usually the 1d version of a nD function, using the conjgrad.inc common)
C p:       the optimum value. Should be an extremum for the function !
C size:    the number of dependent variables in func1d, determines the chi2 law
C     If larger than 1.0, a larger confidence interval will be used.
C up, down: the confidence interval,
C verb:    verbose mode if 1.0
C
C axis should be normalized for results to be correct.

      implicit none
#include "constant.inc"
#include "conjgrad.inc"
#include "controlc.inc"

      integer size,sizedata,verb
      real func1d
      real p(size), xi(size), up, down
      external func1d
      real x, step, step0, pval, y1, y2, conf, accur, len
      real up2, dn2
      integer j

C set-up from the chi2 table

      if (size.eq.1) then
         conf = 2
         accur = 0.1
      else if (size.eq.2) then
         conf = 3
         accur = 0.1
      else if (size.eq.3) then
         conf = 4
         accur = 0.1
      else if (size.eq.4) then
         conf = 5
         accur = 0.1
      else if (size.eq.5) then
         conf = 6
         accur = 0.1
      else if (size.eq.6) then
         conf = 7
         accur = 0.1
      else
         conf = 10
         accur = 0.1
      endif

      do j=1,size
        start(j) = p(j)
        axis(j) = xi(j)
      enddo

      x = 0.0
      pval = func1d(x)
C normalize
      conf = conf*pval/sizedata
      if (verb.eq.1) write(*,*) 'confidence -     :',x,pval,conf

C len is the amplitude of the param along the search axis.
      call dotvect(len,p,xi,size)
      step0 = sqrt(epsilon)*abs(len)

      step = step0
      up = x+step0

C search up first
C inflate
 10   y1 = func1d(up)
      if (control.eq.1) goto 100
      if (verb.eq.1) write(*,*) 'conf - up inflate:',up,y1
      if (abs(pval-y1) .lt. conf) then
         step = 2*step
         up = up+step
         goto 10
      endif

C now we know it lies within up and up-step (up2)
      up2=up-step
 20   if (abs((up-up2)/(up-x)) .gt. accur) then
         if (control.eq.1) goto 100
         y2 = func1d((up+up2)/2)   ! eval in the middle
         if (abs(y2-pval) .gt. conf) then
            up = (up+up2)/2
         else
            up2 = (up+up2)/2
         endif
         if (verb.eq.1) write(*,*) 'conf - up deflate:',(up+up2)/2,y2
         goto 20
      endif

C Now, search down - take up step as starting point
      step  = abs(up-x)
      down = x - step
 30   y1 = func1d(down)
      if (control.eq.1) goto 100
      if (verb.eq.1) write(*,*) 'conf - down inflate:',down,y1
      if (abs(pval-y1) .lt. conf) then
         step = 2*step
         down = down-step
         goto 30
      endif

C now we know it lies between down and down + step
      dn2 = down+step
 40   if (abs((down-dn2)/(down-x)) .gt. accur) then
         if (control.eq.1) goto 100
         y2 = func1d((down+dn2)/2) ! eval in the middle
         if (abs(y2-pval) .gt. conf) then
            down = (down+dn2)/2
         else
            dn2 = (down+dn2)/2
         endif
       if (verb.eq.1) write(*,*) 'conf - down deflate:',(down+dn2)/2,y2
         goto 40
      endif

      if (verb.eq.1) write(*,*) 'confidence :',down,x,up
      y1 = func1d(down)
      y2 = func1d(up)
      if (verb.eq.1) write(*,*) '            ',y1,pval,y2

 100  return
      end


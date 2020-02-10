C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Authors :       M.A.Delsuc
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully 
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C
      subroutine brentmini(left,middle,right,func,tol,niter,
     *                     minimum,funcmini,verb	)

c IN	: left,middle,right,func,tol,niter,verb
c OUT	: minimum,funcmini	
C    This  subroutine isolate the minimum of a 1D function, using the BRENT
C method
C
C   On input:
C- left,middle,right is a triplet bracketing the minimum, such that
C left>middle>right and func(middle)<func(left) and func(middle)<func(right)
C
C- func is the function to minimize  y=func(x)  where x,y are real
C
C- tol is the tolerance to be obtain on the position of the minimum, should
C never be smaller than the machine epsilon. 1e-3 is typical
C
C- niter is the maximum number of iteration to achieve, the programme exit
C whenever one of the condition on tol or on niter is achieved, 20 is typical
C 0 means nearly infinite
C
C   On output:
C- minimum is the abscissa of the located minimum.
C- funcmini is the value of the function at minimum.

      implicit none
      real left,right,middle,tol,minimum,funcmini,func
      external func
      integer niter,verb

      real golden,epsilon
      parameter (golden=.381966, epsilon=1.0E-8)

      integer iter,itt
      real a, b, x, u, v, w, vx, vu, vv, vw, step,
     *     tol1, tol2, r, q, p, steptemp, d, xm

C copy parameters to operationel ones

      A = min(left,right)   ! bracketing
      B = max(left,right)
      x = middle            ! best so far
      vx = func(x)
      if (verb.eq.1) write(*,*) 'Brent x,v(x)   :',x,vx
      w = x                 ! second best
      v = x                 ! previous w
      u = x                 ! most recent evaluation
      vw = vx
      vv = vx
      vu = vx
      step = 0.0    	
      d = 0.0

      If (niter.le.0) then
          itt=100
      else
          itt=niter
      endif

C main loop
      do 100, iter=1,itt
         xm = 0.5*(a+b)
         tol1 = tol*abs(x) + epsilon
         tol2 = 2.0 * tol1
         if (abs(x-xm).le.(tol2-0.5*(b-a))) goto 999   !converged
         
         if (abs(step).gt.tol1) then
            r = (x-w)*(vx-vv)                    ! parabolic fit
            q = (x-v)*(vx-vw)
            p = (x-v)*q - (x-w)*r
            q = 2.0*(q-r)
            if (q.gt.0.0) then
               p = -p
            else
               q = -q
            endif
            steptemp = step
            step = d
            if (((abs(p).ge.abs(0.5*q*steptemp)) .or.
     *                (p.le.q*(a-x)) .or.
     *                (p.ge.q*(b-x)))) goto 10
            if (verb.eq.1) write(*,*) 'Brent: parabolic'
            d = p/q                     ! accept parabolic fit
            u = x+d
            if (u-a.lt.tol2 .or. b-u.lt.tol2) d=sign(tol1,xm-x)
            goto 20
         endif

C reject parabolic fit, do golden search
10       if (x.ge.xm) then
                    step = a-x          ! choose larger step
         else
                    step = b-x          
         endif
         d = golden*step
         if (verb.eq.1) write(*,*) 'Brent: golden'

C   d is the step to take
20       if (abs(d).ge.tol1) then
            u = x+d
         else
            u = x+sign(tol1,d)
         endif
         vu = func(u)
         if (vu.lt.vx) then       ! if better
            if (verb.eq.1) write(*,*) 'Brent: better  :',u,vu
            if (u.ge.x) then
               a = x
            else
               b = x
            endif
            v = w
            vv = vw
            w = x
            vw = vx
            x = u
            vx = vu
         else                ! not better  so rebracket
            if (verb.eq.1)write(*,*) 'Brent: braket  :',u,vu
            if (u.lt.x) then
               a = u
            else
               b = u
            endif
            if (vu.lt.vw .or. w.eq.x) then
               v = w
               vv = vw
               w = u
               vw = vu
            else if (vu.le.vv .or. v.eq.w) then
               v = u
               vv = vu
            endif
         endif
100   continue

999   funcmini = vx
      minimum = x
      if (verb.eq.1) write(*,*) 'Brent: iters   :',iter-1
      return
      end

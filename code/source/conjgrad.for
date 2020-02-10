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

      subroutine conjgrad(func,func1d,dfunc,size,p,iter,miter,
     *                    tol,fmini,verb)
c IN	: func,func1d,dfunc,size,iter,miter,tol,verb
c OUT	: fmini
c INOUT	: p
c
C  Computes the minimum of the real function func, using the conjugate gradient
C method. func(x) where x is a vector size-dimentionnal vector
C - func(x(size)) : function to minimize
C - func1d(y) : real version of func
C - dfunc(x(size),dfret(size)) : derivative of func, returns dfret.
C - size : size of the vectors
C - p(size) : initial value, returns the minimum found in p
C - fmini : on output  minimum found
C - iter : number max of iterations
C - miter : number max of iterations for line minisation
C           if 0.0 do standard steps, fmini is then used on input as initial
C           step
C - tol : epsilon of the minimization.
C - verb : if 1, verbose output

      implicit none

#include "conjgrad.inc"
      integer size,iter,miter,verb
      real p(size),tol,func,func1d,fmini
      external func,func1d,dfunc

      real g(taillemax), h(taillemax), xi(taillemax),
     *    fp, gg, dgg, gama, step
      integer its, j 

      if (size.gt.taillemax) then
           write(*,*) 'Should not have more than',taillemax,
     *                '  variables.'
           return
      endif
      fp = func(p)
      call dfunc(p,xi)
      if (verb.eq.1) then
         write(*,*)'f(start)       :',fp
         write(*,*)'initial f,df   :'
         do j=1,min(size,10)
            write(*,*) 'Param #',j,p(j),xi(j)           
         enddo
      endif
      do j=1,size
         g(j)=-xi(j)
         h(j)=g(j)
         xi(j)=h(j)
      enddo
      if (miter.eq.0) step = fmini
      do its=1,iter
          if (miter.gt.0) then    ! line minimization
               call linemini(func1d,p,xi,size,fmini,miter,tol,verb)
          else                      ! standard step
               if (verb.eq.1) 
     *            write(*,*) '1D step        :',step
               do j=1,size
                  xi(j) = step*xi(j)
                  p(j) = p(j) + xi(j)
               enddo
               fmini = func(p)
C change speed
               if (fmini.lt.fp) then
                  step = 1.2*step
               else
                  step = 0.5*fp*step/fmini
               endif
          endif
          if (2*abs(fmini-fp) .le. tol*(abs(fmini)+abs(fp)+eps))
     *              return       ! exit on tol
          fp = fmini
          call dfunc(p,xi)
          if (verb.eq.1) then
             write(*,*) 'Iteration #    :',its
             write(*,*) 'f(params)      :',fp
             write(*,*) 'current f,df   :'
             do j=1,min(size,10)
                write(*,*) 'Point ',j,p(j),xi(j)           
             enddo
          endif
          gg = 0.0
          dgg = 0.0
          do j=1,size         ! compute gama
             gg = gg+g(j)**2
C            dgg = dgg+xi(j)**2    !Fletcher Reeves
             dgg = dgg+(xi(j)+g(j))*xi(j)      !Polak Ribiere
          enddo
          if (gg.eq.0.0) return      ! if grad.eq.0 we are at minimum point
          gama = dgg/gg
          do j=1,size         ! apply polack-ribiere
             g(j) = -xi(j)
             h(j) = g(j)+gama*h(j)
             xi(j) = h(j)
          enddo
      enddo
      return               ! exit on niter
      end

C******************************************************************
      subroutine linemini(func1d,p,xi,size,fmini,miter,tol,verb)
c IN	: func1d,xi,size,miter,tol,verb
c OUT	: fmini
c INOUT	: p
C performs a 1D minisation of the real function func1d along one axis, using the
C Brent method. The axis used is xi, the starting point is start
C both are put in common for func1d.
C - func1d(y) : function to minimize
C - p(size) : initial value, returns found minimum in p
C - xi(size) : axis for minimization
C - size : size of the vectors
C - fmini : minimum found
C - miter : number max of iterations for line minisation
C - tol : epsilon of the minimization.
C - verb : if 1, verbose output

      implicit none
#include "conjgrad.inc"
      integer size,miter,verb
      real func1d,p(size),xi(size),fmini,tol
      external func1d

      integer j
      real fa,fx,fb,minimum,ax,xx,bx

      do j=1,size
        start(j) = p(j)
        axis(j) = xi(j)
      enddo
      ax = 0.0
      xx = 1.0
      bx = 2.0
      call braket(ax,xx,bx,fa,fx,fb,func1d,verb)
      call brentmini(ax,xx,bx,func1d,tol,miter,minimum,fmini,verb)
      if (verb.eq.1) write(*,*) '1D step        :',minimum
      do j=1,size
         xi(j) = minimum*xi(j)
         p(j) = p(j) + xi(j)
      enddo
      return
      end


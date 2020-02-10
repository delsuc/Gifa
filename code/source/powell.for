C This file is a part of the GIFA program
C
C     Authors :       M.A.Delsuc
C                     C.B.S.
C                     Faculte de Pharmacie
C                     34000 Montpellier
C
C     This software cannot be used unless the user have been fully 
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.


      subroutine powell(func,func1d,size,p,iter,miter,
     *                    tol,fmini,verb,err)
c IN	: func,func1d,size,iter,miter,tol,verb
c OUT	: fmini
c INOUT	: p,err
c
C  Computes the minimum of the real function func, using the conjugate gradient
C method. func(x,size) where x is a vector size-dimentionnal vector
C - func(x(size),size) : function to minimize
C - func1d(y) : real version of func
C     No need of a derivative !
C
C - size : size of the vectors
C - p(size) : initial value, returns the minimum found in p
C - fmini : on output  minimum found
C - iter : number max of iterations
C - miter : number max of iterations for line minisation
C           if 0.0 do standard steps, fmini is then used on input as initial
C           step
C - tol : epsilon of the minimization.
C - err : set to 1 if error
C - verb : if 1, verbose output
c 
c Adapted from Numerical Recipes
C
      implicit none
#include "conjgrad.inc"
#include "controlc.inc"
      integer size,iter,miter,err,verb
      real p(size),tol,func,func1d,fmini
      external func

      real direc(taillemax,taillemax),pt(taillemax),ptt(taillemax)
      real dirit(taillemax)
      real fp,del,fcur,t
      integer i,j,liter,big

C check and build start direc
      if (size.gt.taillemax) then
         err = 1
         return
      endif
      do i=1,size
         do j=1,size
            if (i.eq.j) then
               if (p(i) .ne. 0.0) then
                  direc(i,i) = p(i)
               else
                  direc(i,i) = 1.0
               endif
            else
               direc(i,j) = 0.0
            endif
         enddo
      enddo

C save initial point
      do i = 1, size
         pt(i) = p(i)
      enddo

      fmini = func(p,size)
C do the loop
      do liter=1,iter
         if (control.eq.1) goto 100
         if (verb.eq.1) then
            write(*,*) 'Powell: ---------- Iteration',liter
            write(*,*) '  best so far:',fmini
            write(*,*) '  at :', (p(i),i=1,size)
         endif
         fp = fmini
         big = 0
         del = 0.0
         do i=1,size               ! loop over directions
            do j=1,size
               dirit(j) = direc(j,i)
            enddo
            if (verb.eq.1) then 
               write(*,*) 'Powell: Direction',i
               write(*,*) (dirit(j),j=1,size)
            endif
            fcur = fmini
            call linemini(func1d,p,dirit,size,fmini,miter,tol,verb)
            if (abs(fmini-fcur).gt.del) then ! largest so far
               del = abs(fmini-fcur)
               big = i
            endif
         enddo
C if size==1   not nD search ! only 1 linemini is enough !
         if (size.eq.1) goto 100
         if (abs(fp-fmini) .le. tol*abs(fmini)) goto 100
         do j=1,size              ! do powell trick
            ptt(j)=2*p(j)-pt(j)
            dirit(j)=p(j)-pt(j)
            pt(j)=p(j)
         enddo
C dirit is the new direction (P-PT) == (Pi - Pi-1) at the ith iteration
C ptt is the extrapolated value of func() on the new direction
C
         if (verb.eq.1) write(*,*) 'Powell: Largest Iter',big
         fcur = func(ptt,size)
         if (fcur.ge.fp) goto 10  ! do not change
         t = 2*(fp + fcur - 2*fmini)*(fp-fmini-del)**2
         t = t - del*(fp-fmini)**2    ! powell criterium
         if (verb.eq.1) write (*,*)  'Powell: crit :',t
         if (t.ge.0) goto 10      ! do not change either
         call linemini(func1d,p,dirit,size,fmini,miter,tol,verb)
C change largest decrease with new direction
         do j=1,size
            direc(j,big) = dirit(j)
         enddo
 10   enddo
C end of it
 100  return
      end


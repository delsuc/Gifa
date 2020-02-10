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
C

      subroutine miniinit(st,p,size,err)
C IN:   st,size
C OUT:  p
C
C This one initialize every thing for the generic minimizer (MINIMIZE)
C st : the string to evaluate, should be a function of the global
C      variables $Pi
C size: The number of independent parameters
C p(size) : the fortran array that will hole the counter part of $Pi
C
C initializes the common for powell
C sets p(i) equal to $Pi
C
      implicit none
#include "conjgrad.inc"
#include "minimize.inc"
#include "variable.inc"
      character*(varcontlen) st,loc1
      character*(varnmlen) loc2
      integer size,i,err,k
      real p(size)
      real*8 zz

      actualsize = size
      actualst = st
      err=0
C get (or create) $Pi
      do i =1,size
         write(loc1,'(I6)') i
         call leading(loc1)
         call trailing(loc1,k)
         loc2 = '$P' // loc1(1:k)
         call checkvar(loc2,20,err)
         if (err.eq.0) then              ! does exist
            call getvar(loc2,20,loc1,err)
            call readval(loc1,zz,err)
            if (err.ne.0) goto 100
            p(i)=zz
         else                            ! otherwise
            loc1 = '1.0'
            err = 0
            call assign(loc2,20,loc1,err)
            if (err.ne.0) goto 100
            p(i) = 1.0
         endif
      enddo
C check if the expression is valid
      call eval(actualst,loc1,err)
      if (err.ne.0) goto 100
      call readval(loc1,zz,err)
      if (err.ne.0) goto 100

      return

 100  call gifaout( '*** Error in MINIMIZE/FITGENE initialisation')
      return

      end

C********************************************************************
      subroutine miniend(p,size,verb,err)
C IN:   p,size,verb
C SIDE: copy into user Gifa variable $Pi
C
C size: The number of independent parameters
C p(size) : the fortran array that will hold the counter part of $Pi
C
C copies all running parameters to User variables
C sets $Pi equal to p(i)
C
C verbose if verb > 1
C
      implicit none
#include "conjgrad.inc"
#include "minimize.inc"
#include "variable.inc"
#include "paramfilev.inc"

      integer size,err,verb
      real p(size)

      integer i,k
      character*(varcontlen) loc1
      character*(varnmlen) loc2
      real minimize1d,fitgene1d
      external minimize1d,fitgene1d

      if (verb.gt.0) call gifaout('Parameter Values:')
      do i=1, size
C copy p(i) to $Pi
         write(loc1,'(I6)') i
         call leading(loc1)
         call trailing(loc1,k)
         loc2 = '$P' // loc1(1:k)
         write(loc1,*) p(i)
         call leading(loc1)
         call trailing(loc1,k)
         call assign(loc2,20,loc1,err)
         if (err.ne.0) goto 10
         if (verb.gt.0)  call gifaouti(loc1(1:k),i)
      enddo
      return

 10   call gifaout('*** Error in copying Parameters')
      return
      end

C********************************************************************
      subroutine fitgeneinit(st,p,size,err)
C IN:   st,size
C OUT:  p
C
C This one initialize every thing for the generic minimizer (FITGENE)
C st : the string to evaluate, should be a function of the global
C      variables $Pi and $X
C size: The number of independent parameters
C p(size) : the fortran array that will hole the counter part of $Pi
C
C initializes the common for powell
C sets p(i) equal to $Pi and $X equal to 0.0
C
      implicit none
#include "conjgrad.inc"
#include "minimize.inc"
#include "variable.inc"
      character*(varcontlen) st,loc1
      character*(varnmlen) loc2
      integer size,err
      real p(size)
C set up $X
      loc2 = '$X'
      call checkvar(loc2,20,err)
      if (err.ne.0) then
         err = 0
         loc1 = '1.0'
         call assign(loc2,20,loc1,err)
         if (err.ne.0) goto 100
      endif
      call miniinit(st,p,size,err)
      return
 100  call gifaout('*** Could not create $X in FITGENE')
      return
      end
C********************************************************************
      subroutine fitgend(p,chi2,cov,size,nbpt,verb,err)
C IN:   p,chi2,cov,size,nbpt,verb
C SIDE: copy into user Gifa variable $Pi and $DPi
C
C size: The number of independent parameters
C nbpt: the number of points in the fit
C p(size) : the fortran array that will hold the counter part of $Pi
C cov(size,size) the covariance matrix, of the fit
C chi2: the chi square of the fit
C
C copies all running parameters to User variables
C sets $Pi equal to p(i)
C
C it also computes error bars, using cov() and put them in $DPi
C
C verbose if verb > 1
C
      implicit none
#include "conjgrad.inc"
#include "minimize.inc"
#include "variable.inc"
#include "paramfilev.inc"

      integer size,nbpt,err,verb
      real p(size), chi2, cov(size,size)

      integer i,k
      character*(varcontlen) loc1
      character*(varnmlen) loc2
      real minimize1d,fitgene1d,scale
      external minimize1d,fitgene1d

      call miniend(p,size,verb,err)
      if (err.ne.0) goto 10
      if (chi2 .lt. nbpt/4 .or. chi2 .gt. 4*nbpt) then
         if (chi2 .lt. nbpt/4) then
            call gifaout
     *    ('Warning, Model is overfitting or Noise is overestimated')
         else
            call gifaout
     *    ('Warning, Model is wrong or Noise is underestimated')
         endif
         call gifaout('   reestimating error bars')
         scale = sqrt(chi2/(nbpt-2))
      else
         scale = 1.0
      endif
      do i=1, size
C get error bar
         write(loc1,'(I6)') i
         call leading(loc1)
         call trailing(loc1,k)
         loc2 = '$DP' // loc1(1:k)
         write(loc1,*) (2*scale*sqrt(cov(i,i)))   ! 95 % confidence
         call leading(loc1)
         call trailing(loc1,k)
         call assign(loc2,20,loc1,err)
         if (err.ne.0) goto 10
      enddo
      return

 10   call gifaout('*** Error in copying Parameters after minimisation')
      return
      end

C********************************************************************
      function minimize(p,size)
C
      implicit none
#include "conjgrad.inc"
#include "minimize.inc"
#include "variable.inc"
      character*(varcontlen) loc1
      character*(varnmlen) loc2
       integer size
      real p(size)
      integer i,err,k
      real minimize
      real*8 zz

C copy p() to $Pi
      err = 0
      do i =1,size
         write(loc1,'(I6)') i
         call leading(loc1)
         call trailing(loc1,k)
         loc2 = '$P' // loc1(1:k)
         write(loc1,'(G21.15)') p(i)
         call setvar(loc2,20,loc1,err)
         if (err.ne.0) goto 100
      enddo

C do the eval
      call eval(actualst,loc1,err)
      if (err.ne.0) goto 100
C and make it real
      call readval(loc1,zz,err)
      if (err.ne.0) goto 100
      minimize = zz
      return
 100  write(*,*) '*** Error in MINIMIZE'
      minimize = 0.0
      return
      end

C********************************************************************
       function minimize1d(x)
c IN	: x
C 1d version of minimize
C x a real, parameting along axis, starting from start (in conjgrad.inc)
       implicit none

#include "conjgrad.inc"

       real minimize1d, minimize, x
       real ploc(taillemax)

       call addvect(ploc, start, axis, x, actualsize)
       minimize1d=minimize(ploc,actualsize)

       return
       end
C********************************************************************
      function fitgene(p,size)
C
C compute a CHI2 = szamp(data[i] - computed(tab[i]))^2
C
      implicit none
#include "conjgrad.inc"
#include "minimize.inc"
#include "variable.inc"
#include "paramfilev.inc"
#include "sizeparam.inc"
#include "laplacev.inc"

      character*(varcontlen) loc1
      character*(varnmlen) loc2
       integer size
      real p(size)
      integer i,err,k
      real fitgene
      real*8 zz,chi2

C copy p() to $Pi
      err = 0
      do i =1,size
         write(loc1,'(I6)') i
         call leading(loc1)
         call trailing(loc1,k)
         loc2 = '$P' // loc1(1:k)
         write(loc1,'(G21.15)') p(i)
         call setvar(loc2,20,loc1,err)
         if (err.ne.0) goto 100
      enddo

      chi2 = 0.0
C loop over data points
      do i=1,sizeimage1d
         loc2='$X'
         write(loc1,*) sampling(i)
         call leading(loc1)
         call trailing(loc1,k)
         call assign(loc2,20,loc1,err)
C do the eval
         call eval(actualst,loc1,err)
         if (err.ne.0) goto 100
C and make it real
         call readval(loc1,zz,err)
         if (err.ne.0) goto 100
         chi2 = chi2 + (zz-column(i))**2
      enddo
      fitgene = chi2/sizeimage1d
      return
 100  write(*,*) '*** Error in FITGENE'
      fitgene = chi2/sizeimage1d
      return
      end

C********************************************************************
       function fitgene1d(x)
c IN	: x
C 1d version of fitgene
C x a real, parameting along axis, starting from start (in conjgrad.inc)
       implicit none

#include "conjgrad.inc"

       real fitgene1d, fitgene, x
       real ploc(taillemax)
       external fitgene

       call addvect(ploc, start, axis, x, actualsize)
       fitgene1d=fitgene(ploc,actualsize)

       return
       end

C********************************************************************
      subroutine fitg2(size,p,x,y)
C
C compute  f(x)
C
C where f is defined as a string to be evaluated by eval()
C and x is set to $X
C     p(i)     to $Pi
C
      implicit none
#include "minimize.inc"
#include "variable.inc"

      integer size
      real p(size),x,y
      integer i,err,k
      real*8 zz
      common /fitg/zz
C This common is a kludge, which permits to use the real*8 version of 
C fitg2 when computing the derivative in dfitg2.
      character*(varcontlen) loc1
      character*(varnmlen) loc2

C copy p() to $Pi
      err = 0
      do i =1,size
         write(loc1,'(I6)') i
         call leading(loc1)
         call trailing(loc1,k)
         loc2 = '$P' // loc1(1:k)
         write(loc1,'(G21.15)') p(i)
         call setvar(loc2,20,loc1,err)
         if (err.ne.0) goto 100
      enddo

      loc2='$X'
      write(loc1,*) x
      call leading(loc1)
      call trailing(loc1,k)
      call assign(loc2,20,loc1,err)
C do the eval
      call eval(actualst,loc1,err)
      if (err.ne.0) goto 100
C and make it real
      call readval(loc1,zz,err)
      if (err.ne.0) goto 100
      y = zz
      return
 100  write(*,*) '*** Error in FITGENE'
      Y = 0.0
      return
      end


C********************************************************************
      subroutine dfitg2(size,p,x,dy)
C
C compute  derivative of f(x)
C
C where f is defined as a string to be evaluated by eval()
C and x is set to $X
C     p(i)     to $Pi
C
C derivative is done numerically
C
      implicit none
#include "constant.inc"
#include "sizebase.inc"
#include "levenberg.inc"

      integer size
      real p(size),x,dy(size)
      integer i
      real zloc
      real*8 zu,zl,delta
      real*8 zz
      common /fitg/zz
C see fitg2 for comment
      real pl(sizelev)
#f2c      save pl    ! needed for C array

      delta = 10*epsilon
      do i = 1, size
         call copvect(pl,p,size)
         pl(i) = p(i)*(1.0 + delta)
         call fitg2(size,pl,x,zloc)
         zu = zz        ! see fitg2 for comment
         pl(i) = p(i)*(1.0 - delta)
         call fitg2(size,pl,x,zloc)
         zl = zz
         dy(i) = (zu - zl)/(2.0*p(i)*delta)
      enddo
      return
      end

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

C This file holds the code for the generic Levenberg-Marquardt
C minimiser.
C The function to call is levenberg. Other routines for internal use only.
c uses sizelev as a parameter for maximum size of parameter table

      subroutine levenberg (x,y,wndw,sizdat,sig,prm,npc,
     *                      permut,nactif,chi2,toler,nbiter,
     *                      func,dfunc,covar,verbose,debug,err)
c IN	: x,y,wndw,sizdat,sig,npc, permut,nactif,toler,nbiter,func,dfunc,verbose,debug
c INOUT	: prm
c OUT	: chi2,err,covar
c
c fits the data x(sizdat) / y(sizdat) with the levenberg-marquardt method
c with the 1d function func(size,prm(size),x,y)
c of derivative        dfunc(size,prm(size),x,dy(size))
c parameters are stored in prm(npc).
c only the parameters  prm(permut(1:nactif)) will be modified
c where permut(1:nactif) is a valid partial permutation on 1:npc
c
c covar(npc,npc) should be provided to compute covariance.
c
c standard deviation (sigma) of point x(i)/y(i) is sig/wndw(i)
c i.e. window is a descriptor of the quality of the data.
C typically window(i) holds 1.0 for sigma(i) = sig => stanard point
c                           0.0 for sigma(i) = inf => no good signal there
c
c output is found in prm(), result of the fit is chi2
c 
c exit condition is iter>=nbiter or last move on chi2 < toler
c
C on return, covar(i,j) contains the covariance matrix.
C covar(i,i) is the square of the error on prm(i)
      implicit none
      save
#include "sizebase.inc"
#include "levenberg.inc"
#include "controlc.inc"

      integer  npc,nactif,nbiter,sizdat,verbose,debug,err
      real     prm(npc),chi2,toler,x(sizdat),y(sizdat),sig,
     *         wndw(sizdat)
      integer  permut(npc)
      integer  i,j,iter
      character*256 st
      real     lambda,deltachi2,prevchi2,
     * alpha(sizelev,sizelev),covar(npc,npc),beta(sizelev,1)
      external func,dfunc


      if (npc.gt.sizelev) then
         call gifaouti( 'max variables allowed',sizelev)
         err = 1
         return
      endif

      lambda=-1
      iter=1
      call mrqmin(x,y,sig,wndw,sizdat,prm,npc,permut,
     *       nactif,chi2,covar,alpha,beta,lambda,
     *       func,dfunc,verbose,debug)
      deltachi2 = 2*toler
      if (verbose.ge.1) then
        call gifaout('***************************')
        call gifaout('Initial values :')
        call gifaouti( 'iter : ',iter)
        call gifaoutr( 'chi2 : ',chi2)
        call gifaoutr( 'deltachi2 : ',deltachi2)
        call gifaoutr( 'lambda : ',lambda)
        call gifaout('parameters :')
        do i=1,nactif,3
              write(st,*) (prm(permut(i+j)),j=0,2) 
              call gifaout(st)
        enddo
        call gifaout(' ')
      endif
      dowhile (((lambda.ne.0.0).and.(deltachi2.ge.toler)).and.
     *         (iter.le.nbiter))
          if (control.eq.1) return           ! this emergency exit with ^C
          prevchi2=chi2
          call mrqmin(x,y,sig,wndw,sizdat,prm,npc,permut,
     *      nactif,chi2,covar,alpha,beta,lambda,
     *      func,dfunc,verbose,debug)

          if (chi2.ge.prevchi2) then
            deltachi2 = 2*toler
          else
            deltachi2=abs(prevchi2-chi2)
          endif
          iter=iter+1

          if (verbose.ge.1) then
            call gifaout('***************************')
            call gifaouti( 'iter : ',iter)
            call gifaoutr( 'chi2 : ',chi2)
            call gifaoutr( 'deltachi2 : ',deltachi2)
            call gifaoutr( 'lambda : ',lambda)
            call gifaout('parameters :')
            do i=1,nactif,3
              write(st,*) (prm(permut(i+j)),j=0,2) 
              call gifaout(st)
            enddo
           endif

      enddo
      lambda=0.0
      call mrqmin(x,y,sig,wndw,sizdat,prm,npc,permut,nactif,
     *         chi2,covar,alpha,beta,lambda,func,dfunc,verbose,debug)
      if (verbose .eq.1) then
         call gifaout('square root of covariance matrix elements:')
         do i=1,nactif,3
            write(st,*) (sqrt(covar(permut(i+j),permut(i+j))),j=0,2)
            call gifaout(st)
            call gifaout(' ')
         enddo
      endif
      return
      end



      subroutine mrqmin (x,y,sig,win,sizdt,p,sz,tripar,ajus,
     *              lwerror,cov,alf,bet,lambda,func,dfunc,verbose,debug)
c IN	: x,y,sig,win,sizdt,p,sz,tripar,ajus,func,dfunc,verbose,debug
c INOUT	: lambda
c OUT	: lwerror,cov,alf,bet
c called by levenberg
c
c it's by repeating the calling to mrqmin subroutine that we will
c minimize the error function (err) and find the best suited values
c for the parameters (p) involved in the chosen function (func).

     
      implicit none
      save
#include "sizebase.inc"
#include "levenberg.inc"

      external func,dfunc
      integer  sz,tripar(sz),ajus,sizdt,verbose,debug
      real     p(sz),alf(sz,sz),cov(sz,sz),
     *         erreur,lambda,x(sizdt),y(sizdt),sig,win(sizdt)
      real     lwerror,bet(sizelev),gamma(sizelev),
     *         partry(sizelev)
      integer  i,j,k,indx,job
      real     cond,rz(sizelev)
      integer  ipivot(sizelev)

      if (lambda.lt.0) then
         if (debug.eq.1) call gifaout( 'MRQMIN: init')
         k=ajus+1
c complete the permutation in tripar by putting unused values at the end
         do i=1,sz
            indx=0
            do j=1,ajus
               if (tripar(j).eq.i) then
                  indx=indx+1
               endif
            enddo
            if (indx.eq.0) then
               tripar(k)=i
               k=k+1
            else if (indx.gt.1) then
                 call gifaout( '*** MRQMIN: failure in index')
                 return
            endif
         enddo
         if (k.ne.(sz+1)) then
                 call gifaout( '*** MRQMIN: failure in index - 2')
                 return
         endif

         lambda=1.0
         call mrqcof(x,y,sig,win,sizdt,sz,p,ajus,tripar,
     *               alf,bet,erreur,func,dfunc,debug,verbose)
         lwerror=erreur
         do j=1,sz
            partry(j)=p(j)
         enddo
      endif
      do j=1,ajus
         do k=1,ajus
            cov(j,k)=alf(j,k)
         enddo
         cov(j,j)=alf(j,j)*(1.0+lambda)
         gamma(j)=bet(j)
      enddo
      call SGECO(cov,sz,ajus,ipivot,cond,rz)
      if (debug.eq.1)
     *   call gifaoutr( 'Inversion of Hessian - Rcond : ',cond)
c      if ((1.0+cond) .gt. 1.0) then
      if (cond.ne.0.0) then
         call SGESL(cov,sz,ajus,ipivot,gamma,0)
      else
         call mltvect(gamma,bet,0.01,ajus)
         if (verbose.ge.1 .or. debug.eq.1) then
         call gifaout('Singularity in LEVENBERG/SGECO !')
         endif
      endif
      if (debug.eq.1) then
         call gifaout( 'Step in parameter:  (not in log file)')
         write(*,*) (gamma(j),j=1,ajus)
      endif
C Lambda == 0 forces computation of cov matrix for error calculation
      if (lambda.eq.0) then
         if (cond.ne.0) then
C Let's compute the inverse
           job = 01
           call sgedi(cov,sz,ajus,ipivot,cond,rz,job)
         else
            call gifaout(
     *'curvature matrix is non-inversible, errors will be unavailable')
           do i=1,ajus
              cov(i,i)=0.0
           enddo
         endif
         return
      endif
      do j=1,ajus
         partry(tripar(j))=p(tripar(j))+gamma(j)
      enddo
      call mrqcof(x,y,sig,win,sizdt,sz,partry,ajus,tripar,
     *            cov,gamma,erreur,func,dfunc,debug,verbose)
      if (erreur.lt.lwerror) then
         if (debug.eq.1) call gifaout( 'MRQMIN Progress')
         lambda=lambda*0.1
         lwerror=erreur
         do j=1,ajus
            do k=1,ajus
               alf(j,k)=cov(j,k)
            enddo
            bet(j)=gamma(j)
            p(tripar(j))=partry(tripar(j))
         enddo
      else
         if (debug.eq.1) call gifaout( 'MRQMIN Retry')
         lambda=lambda*10.0
         erreur=lwerror
      endif
      return
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine mrqcof(x,y,sig,w,szd,size,pr,adj,sort,
     *           mata,matb,chisq,func,dfunc,debug,verbose)
c IN	: x,y,sig,w,szd,size,pr,adj,sort,func,dfunc,debug,verbose
c OUT	: mata,matb,chisq
c elementary step of the levenberg module
c called by mrqmin

      implicit none
      save
#include "sizebase.inc"
#include "levenberg.inc"

      external func,dfunc
      integer  size,adj,sort(size),szd,debug,verbose
      real     chisq,pr(size),mata(size,size),z,
     *         matb(size),w(szd),x(szd),y(szd),sig,sqinvsig
      integer  i,j,k
      real     deltay,dydpar(sizelev),fx,tmp

      do i=1,adj
         do j=1,i
            mata(i,j)=0.0
         enddo
         matb(i)=0.0
      enddo
      chisq=0.0
      do i=1,szd
         sqinvsig=(w(i)/sig)**2
         call func(size,pr,x(i),fx)
         call dfunc(size,pr,x(i),dydpar)
         if (debug.eq.1) then
            write (*,*) 'Derivative in',i,':',x(i)
            write (*,*) (dydpar(sort(j)),j=1,adj)
         endif
         deltay=(y(i)-fx)
         do j=1,adj
            tmp=sqinvsig*dydpar(sort(j))
            do k=1,j
                mata(j,k)=mata(j,k)+tmp*dydpar(sort(k))
            enddo
            matb(j)=matb(j)+deltay*tmp
         enddo
         chisq=chisq+deltay*deltay*sqinvsig
      enddo
      do j=2,adj
         do k=1,j-1
            mata(k,j)=mata(j,k)
         enddo
      enddo
      if (debug.eq.1) then
         call gifaout( 'MRQCOF')
         call gifaoutr('Chi2 : ',chisq)
         call gifaout( 'A :  (not in log file) ')
         do j=1,adj
            call gifaouti( 'A',j)
            write(*,*) (mata(j,k),k=1,adj)
         enddo
         call gifaout( 'B :  (not in log file)')
         write(*,*) (matb(k),k=1,adj)
      endif
      if (verbose.ge.1) then
      do i=1,size
      call gifaoutr('matb(i) =',matb(i))
      enddo
      call gifaouti('adj     =',adj)
         call dotvect(z,matb,matb,adj)
         call gifaoutr('norm of the derivative :',sqrt(z))
      endif
      return
      end
 


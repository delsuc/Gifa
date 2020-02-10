C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Authors :       M.A.Delsuc, V.Stoven, M.Robin
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully 
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C
      subroutine itergull(verbose)
c INT	: verbose
c
c realizes a single iteration of the MaxEnt run
      implicit none
      real     gold
      external qfunc
      parameter (gold=1.618034)
      
#include "constant.inc"
#include "sizeparam.inc"
#include "transform.inc"
#include "paramfilev.inc"
#include "paramdatav.inc"
#include "maxentv.inc"


      real tt1, tt2, tt3, max, left, middle, right, tol, q,
     * stepmin, sexp, gg, dgg, gama, esum, lesum, tt4
      integer i, algo1, algo2, algo3, niter,verbose

      real  sumdchi,ax,bx,cx,fa,fb,fc,qfunc
      common/qcom/sumdchi

#vms      call lib$flt_under(0)

      max = log(maxreal)
      stepmin = 1.0E-2
      expa = exp(aa)

C Algo is abc in decimal.  a and b active only if c is 0 or 1.
C                           c:0 is G&D
C                           c:1 is GIFA
C                           c:2 is steepest descent
C                           c:3 is conjugate gradient
C                           b:0 is regular fixed-point
C                           b:1 is line minimization
C                           b:2 is braketing & line minimisation
C                           a:0 ..........   a:1 is Wu correction

      algo1 = mod(algo,10)      !c
      algo2 = mod(algo/10,10)   !b
      algo3 = algo/100          !a
      if (algo1.gt.1) then
          algo3 = 0
      endif
C compute TF(image) in dchi
       call transform(dchi(1),image,dim,memmode)

C CHI=Sum (TF(image)-data)**2)*window

       call addvect(dchi,dchi,datab,-1.0,sizedata)
       if (iter.ne.0) then   ! do window stuff
          tt1=dchi(1)  !keep first point apart
          if (dim.eq.1)
     *       call diavect(dchi,window,dchi,sizedata)
          if (dim.eq.2)
     *      call mult2d(window,dchi,si1dat,si2dat)
          if (sumcons.eq.1) dchi(1) = tt1
       endif
       call dotvect(chi2,dchi,dchi,sizedata)
       chi2=chi2/sigma


C dCHI = 2*tTF(TF(image)-data))/sigma
       
       call mltvect(dent,dchi,(2.0/sigma),sizedata)
       call ttransform(dchi,dent,dim,memmode)
       if (algo2.ne.0) call sumvect(sumdchi,dchi,sizeimage)

C compute entropy
       call sumvect(sum,image,sizeimage)
       esum = escale*sum
       if (iter.ne.0) then
          call entvect(ent,image,esum,sizeimage)
       else
         ent = log(float(sizeimage))
       endif
C compute  derivative of  entropy
       if (iter.ne.0) then
          do i=1,sizeimage
             tt1 = image(i)/(esum)
             if (tt1.gt.0.0) then
                dent(i) = (-ent-log(tt1))/(esum)
             else
                dent(i) = 0.0
             endif
          enddo
       else
          call prpvect(dent,0.0,sizeimage)
       endif

C convergence test dS.dC
       if (iter.ne.0) then
          call dotvect(tt3,dent,dchi,sizeimage)
          call dotvect(tt1,dent,dent,sizeimage)
       else
          tt3 = 0.0
          tt1 = 0.0
       endif
       call dotvect(tt2,dchi,dchi,sizeimage)
       if (tt1*tt2.ne.0.0) then
          conv = tt3/sqrt(tt1*tt2)
       else
          conv=0.0
       endif

C the best lambda could be such as dQ.dS = 0.0 if lambcont.eq.1
C      and if conv.gt.0.0  (dC dS angle larger than pi/2)
C or such as cos(dQ,-dC) = cos(dQ.dS)           if lambcont.eq.2
C tt1=dS.dS tt2=dC.dC tt3=dS.dC
       if (lambcont.ne.0) then
         tt4 = sqrt(tt1/tt2)
         if (iter.eq.0) then
            lambda = (ent/chi2)/sqrt(float(sizeimage))
         else if (tt3.ne.0.0 .and. conv.gt.0.0 
     *            .and. lambcont.eq.1) then
            lambda2 = tt1/tt3
         else if (lambcont.eq.2) then
            lambda2 = (tt4*tt3 + tt1)/(tt4*tt2 + tt3)
         else
            lambda2 = lambda
         endif
       endif
       if (lambda2.lt.0.0) lambda2 = 0.0

C move lambda
       if (iter.ne.0 .and. lambcont.ne.0) then
          lambda = (1.0-inc)*lambda + inc*lambdasp*lambda2
       endif
#d       write (*,*) ' dQ.dS: ',lambda2,' dQ.dC',tt3/tt2,'lambda: ',lambda 
       qu = ent-lambda*chi2
       lesum = lambda*esum

C compute image increment
       if (algo1.eq.1) then
C GIFA equation
          do i=1,sizeimage
            tt1 =  (-ent-lesum*dchi(i))
            tt1 = esum*sexp(tt1)
            dchi(i) = tt1
          enddo
       else if (algo1.eq.0) then
C Gull and Daniel equation
          do i=1,sizeimage
            tt1 =  (-1.0-lesum*dchi(i))
            if (tt1.lt.max) then
               tt1 = exp(tt1)
            else
               tt1 = exp(max)
            endif
            dchi(i) = tt1
          enddo
       else if (algo1.eq.2) then
C Steepest descent
C compute dQ, dQ = dS -lambda.dC into dchi
          call addvect(dchi,dent,dchi,-lambda,sizeimage)
          call addvect(dchi,dchi,image,1.0,sizeimage)
       else if (algo1.eq.3) then
C Conjugate gradient equation.
C Uses Polack-Ribiere algorithm.
C dchi will  holds the direction,
C work2 holds the Gi vector  work3 holds the Hi vector
C first compute dQ, dQ = dS -lambda.dC into dchi
          call addvect(dchi,dent,dchi,-lambda,sizeimage)
          if (iter.eq.0) then
             call mltvect(work2,dchi,1.0,sizeimage)
             call copvect(work3,work2,sizeimage)
          else
             dgg = 0.0
             call dotvect(gg,work2,work2,sizeimage)
             do i=1,sizeimage
                dgg = dgg + (dchi(i) - work2(i))*dchi(i)
             enddo
             if (gg.eq.0.) then     ! highly unlikely
                step = 0.0
                return
             endif
             gama = dgg/gg
#d            write(*,*) gg,dgg,gama
             call copvect(work2,dchi,sizeimage)
             call addvect(work3,work2,work3,gama,sizeimage)
             call copvect(dchi,work3,sizeimage)
          endif
          call addvect(dchi,dchi,image,1.0,sizeimage)
       endif

C if algo3.eq.1 then do Wu correction (Wu, A. and A. 139 p555 (1984) )
       if (algo3.eq.1 .and. iter.gt.0) then !correction if iter>1 
          tt1 = (lambda*sizeimage*(norm**2)) / sigma
          do i=1, sizeimage
             dchi(i) = dchi(i) * (1.0+tt1*image(i))
     *                  / (1.0+tt1*dchi(i))
          enddo
       endif

C if algo2.eq.1 or 2  then do line minimization along dchi
       if (algo2.eq.0 .or. miniter.lt.1) then     ! constant step
           call qaddvect(image,image,dchi,step,sizeimage)
       elseif (algo2.eq.2) then                   ! bracketing first
           ax=0.0
           bx=stepmax
           call braket(ax,bx,cx,fa,fb,fc,qfunc,verbose)
        
c           left = -0.1*stepmax
           left =ax 
           right =cx
C           right = min(stepmax , 10.0*step)
           right = amax1( right , stepmin)
           middle =bx 
           tol = tolerance
           niter = miniter
           call brentmini(left,middle,right,qfunc,tol,niter,
     *         step,q,verbose)
           if (step.eq.0.0)  step=stepmax*1.0E-6
c           if (step.ne.0.0) then
              call qaddvect(image,image,dchi,step,sizeimage)
c           endif
       else                                     ! line minimization
           left=0.0
           right=stepmax
C           right = min(stepmax , 10.0*step)
           right = amax1( right , stepmin)
           middle=0.0
           tol = tolerance
           niter = miniter
           call brentmini(left,middle,right,qfunc,tol,niter,
     *           step,q,verbose)
           if (step.eq.0.0)  step=stepmax*1.0E-6
c           if (step.ne.0.0) then
              call qaddvect(image,image,dchi,step,sizeimage)
c           endif
       endif
       return
       end




C the qfunc function returns the value of -Q for a step x between
C the 2 vectors image and dchi, using dent and work as temporary storage

      function qfunc(x)
c IN	: x
c
c returns the value of the Q MaxEnt function for an offset x
c from current position along the gradient
c
      implicit none
      real qfunc, x, tt1

#include "sizeparam.inc"
#include "transform.inc"
#include "paramfilev.inc"
#include "paramdatav.inc"
#include "maxentv.inc"

      real  sumdchi
      common/qcom/sumdchi

      real qent, qsum, qchi, best

      if (x.eq.0.0) then    
          qfunc = -qu
          best = -qu
      else

          call qaddvect(dent,image,dchi,x,sizeimage) !next Im
          call sumvect(qsum,dent,sizeimage)          
          call entvect(qent,dent,escale*qsum,sizeimage)     !comp. Ent
          call transform(work,dent,dim,memmode)              !and then C
          call addvect(work,work,datab,-1.0,sizedata)
          if (sumcons.eq.1) tt1 = work(1)     !keep first point apart
          if (dim.eq.1)
     *      call diavect(work,window,work,sizedata)
          if (dim.eq.2)
     *      call mult2d(window,work,si1dat,si2dat)
          if (sumcons.eq.1) work(1) = tt1
          call dotvect(qchi,work,work,sizedata)

          qchi = qchi/sigma
          qfunc = lambda*qchi - qent
                 
C this is used to return to controlgull thru itergull, used
C for lambda controlling
          if (qfunc.lt.best) then
            best = qfunc
            ent=qent
            chi2=qchi
          endif
      endif
      return
      end

             
      function sexp(x)
c IN	: x
C this is the Home-made exponential function used to avoid explosive effects
C of the regular exp.
C aa is the point where the exp. is replaced by the tangent line.
      implicit none
      real sexp, x
#include "maxentv.inc"

#vms      call lib$flt_under(0)
      if (x.lt.aa) then
         sexp = exp(x)
      else
         sexp = expa*(x+1.0-aa)
      endif
      return
      end



C This file is a part of the GIFA program
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
      subroutine controlgull(verbose)
c IN	: verbose
c SIDE	: a lot !
c
c Performs initialisation of the MaxEnt run, a realise the loop over 
c iterations

      implicit none

#include "sizeparam.inc"
#include "paramfilev.inc"
#include "paramdatav.inc"
#include "maxentv.inc"
#include "disptypev.inc"
#include "transform.inc"
#include "controlc.inc"
#include "gifabcom.inc"


      real      qu2,chi,tt1,tt2,maxz
      integer   sizedat,sizeim,i
      integer   start,verbose
      character*256 st

      if (dim.eq.1) then 
       sizeim=sizeimage1d
       sizedat=sizedata
      elseif (dim.eq.2) then 
       sizeim=sizeimage
       sizedat=sizedata
      endif

      if (itermax.eq.0) overit = 0

      if (memmode.eq.1) then
         call dofilter          ! compute filter from parameters,

C deconv tells us to use filtering during MaxEnt.
C assume deconv=1 (filtering) except for few cases
        deconv=1
        if (filset.eq.0)                          deconv=0
        if (filset.eq.1) then
          if (lb.eq.0.0 .and. gb.eq.0.0 
     *        .and. j2.eq.0.0) then
             if (dim.eq.1)                      deconv=0
             if (lb.eq.0.0 .and. lb1.eq.0.0 
     *        .and. gb.eq.0.0 .and. gb1.eq.0.0
     *        .and. j1.eq.0.0 .and. j2.eq.0.0
     *        .and. dim.eq.2)                   deconv=0
           endif
        endif

C compute norm of the filter
        if (deconv.eq.1) then
           if (dim.eq.1) then
              call dotvect(tt1,filter,filter,sizedat*nchannel)
              norm = sqrt( tt1/sizedat)
           elseif (dim.eq.2) then
              norm = 0.0
              do i=1,nchannel
                 start = 1+ (i-1)*(si1im+si2im)
                 call dotvect(tt1,filter(start),filter(start),si2dat)
                 call dotvect(tt2,filter(start+si2im),
     *                filter(start+si2im),si1dat)
                 norm = norm + sqrt( (tt1*tt2)/(si1dat*si2dat) )
              enddo
              norm = norm/nchannel
           endif
           write(*,*) 'Norme: ',norm
        else
           norm= 1.0
        endif
      else
         norm = 1.0
      endif
      if (iter.eq.0) call initgull

C Clear screen if disp active, nndisp.ne.0, and clear mode on
      if (on_x_win .eq. 1 .and. nndisp.ne.0 .and. clear.eq.1) then
        if (dim.eq.1) call win_erase(vd_id(1))
        if (dim.eq.2) call win_erase(vd_id(2))
      endif

C start of the BIG loop
                                                  
1     call itergull(verbose)

C store current carac.
      if (iter.lt.nshow) then
        sent(iter+1) = ent/log(float(sizeim))
        schi(iter+1) = chi2/sizedat
        sconv(iter+1) = 1.0-conv
        slamb(iter+1) = lambda
        sstep(iter+1) = step
        ssum(iter+1) = sum
      endif
      qu2 = ent-lambda*chi2

C if step.eq.0  DO something - this should never happen any more (14-fev-1997)
      if (step.eq.0.0) then
           write(*,*) 'concentring 0.95'
           call mltvect(image,image,0.95,sizeim)
      endif

C Then show it

       if (nndisp.ne.0 .and. iter.ne.0) then
         if (mod(iter,nndisp).eq.0) then
           maxz=0.0
           if (dim.eq.1 .and. disp1d.eq.1) then
             call display1d(vd_id(1),sizeimage1d,image,maxz,
     *              1.0,0,0.0,0.1,1)
           elseif (dim.eq.2 .and. disp2d.eq.2) then
             call display2d(vd_id(2),si2im,si1im,image,maxz,
     *               1.0,0,1,0,1,1,1,1)
           endif
           write(st,10) iter,chi2/sizedat,
     *             ent/log(float(sizeim)),
     *             (1.0-conv),
     *             qu2,lambda,step,sum
           call gifaout(st)
           write(*,*)
         endif
       endif
       if (output.eq.1)
     *       write(*,11) (image(i),i=1,min(sizeim,1024))
       chi = chi2
       iter = iter+1


C test for end of iteration
      if (control.eq.1) goto 2
      if (iter.gt.itermax .and. itermax.ne.0) goto 2
      if (overit.eq.1 .or. (chi/sizedat).ge.1.0) goto 1

2     continue
      leastsq = chi2/sizedat
      iterdone = iter-1

      return

10     format(' iter=',I4,
     *  '  chi2=',g12.5,
     *  '  entropy=',f8.5,
     *  '  conv=',f8.5,
     *  '  Q=',g12.5,
     *  '  lambda=',g12.5,
     *  '  step=',f12.5,
     *  '  sum=',g12.5)
11    format ('+',F8.2,$)
      end

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
      subroutine dofilter
c
C this subroutine, compute the filter from the parameters lb, gb, J1 and
C actualize the filter buffer. Set it to 1.0 if no filter used.
c part of the MaxEnt package

      implicit none

#include "sizeparam.inc"
#include "paramfilev.inc"
#include "paramdatav.inc"
#include "maxentv.inc"
#include "transform.inc"

      real tt1
      integer i,sizef,sizedatad

C compute size of the filter buffer
      if (dim.eq.1) then
         sizef = sizeimage1d
      else
         sizef = si1im+si2im
      endif
C Filset tells which kind of filter we're gonna use.
      if (filset.eq.0) then   ! 0 is no filter, put 1.0  everywhere
          call prpvect(filter,1.0, sizef)
      elseif (filset.eq.1) then   ! 1 is automatic filter
        call prpvect(filter,1.0, sizef)
        if (dim.eq.1) then      !case of 1d spectra
          if (lb.ne.0.0) call expmult(filter, sizeimage1d,lb,1)
          if (gb.ne.0.0) call gausmult(filter, sizeimage1d,gb,1)
          if (j2.ne.0.0) call jmult(filter,sizeimage1d,j2,cc,1)
        else                    !case of 2d spectra
          if (lb.ne.0.0) call expmult(filter, si2im,lb,1)
          if (gb.ne.0.0) call gausmult(filter, si2im,gb,1)
          if (lb1.ne.0.0) call expmult(filter(si2im+1), si1im,lb1,1)
          if (gb1.ne.0.0) call gausmult(filter(si2im+1), si1im,gb1,1)
          if (j1.ne.0.0)  call jmult(filter(si2im+1), si1im,j1,cc,1)
          if (j2.ne.0.0)  call jmult(filter,si2im,j2,cc,1)
        endif
      elseif (filset.eq.2) then  ! 2 is home-made filter
C   If home-made filter, normalize it first to sizedata.
       sizedatad=sizedata
       do i=0,nchannel-1
         call dotvect(tt1,filter(i*sizedatad+1),
     *              filter(i*sizedatad+1),sizedatad) 
         if (tt1.lt.0.0) then
            write(*,*) 'Channel',i+1,' is wrong!'
            tt1 = float(sizedatad)
         endif
         call mltvect(filter(i*sizedatad+1),
     *                filter(i*sizedatad+1),
     *                sqrt(float(sizedatad)/tt1),sizedatad)
       enddo
      endif
      return
      end
         

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
      subroutine trans2(ser,smx)
c IN	: timage
c OUT	: tdata
c
c the MaxEnt transform in 2D
c
  
      implicit none

#include "sizeparam.inc"
#include "transform.inc"      
#include "paramfilev.inc"
#include "paramdatav.inc"
      real  ser(smxmax),smx(smxmax),zz


      call copvect(ser,smx,si1im*si2im)
      call tdftreal(ser,si1im,si2im,-1,1,3)
c  calculates ixfb (sign=-1 et mode=3) from real to complex (direc=1)

      if (sumcons.eq.1) zz = ser(1)
      if (deconv.eq.1) call mult2d(filter,ser,si1im,si2im)
      if (sumcons.eq.1) ser(1) = zz

      call zerofill(ser,si1dat,si2dat,si1im,si2im)
c  reajust to the dimensions of the data (fid)
                                              
      return
      end

c*************************************************************
      subroutine ttrans2(smx,ser)
c IN	: tdata 
c OUT	: timage
c
c the MaxEnt transform in 2D
c

      implicit none

#include "sizeparam.inc"
#include "paramfilev.inc"
#include "paramdatav.inc"
#include "transform.inc"

      real  ser(smxmax),smx(smxmax),zz

c     copy the data (ser) in the output matrix (smx)
      call copvect(smx,ser,si1dat*si2dat)
c   possibility of zero-filling or troncature
      call zerofill(smx,si1im,si2im,si1dat,si2dat)      
                                   
      if (sumcons.eq.1) zz = smx(1)
      if (deconv.eq.1) call mult2d(filter,smx,si1dat,si2dat)
      if (sumcons.eq.1) smx(1) = zz

c  calculates direct xfb (signe=1 mode=3) from complex to real (direc=-1)
      call tdftreal(smx,si1im,si2im,1,-1,3)

      return
      end


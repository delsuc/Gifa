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
C transform   data  <-- image
C ttransform  image <-- data
C image is always a real buffer

C This is the Vector version (arrays are in memory)

      subroutine trans1(tdata,timage)
c IN	: timage
c OUT	: tdata
c
c the MaxEnt transform in 1D
c
      implicit none
#include "sizeparam.inc"
#include "transform.inc"
#include "paramfilev.inc"
#include "paramdatav.inc"

      real tdata(*),timage(*),zz         
      integer i, size, start, startd

c sizedata is the total number of points in the fid (real+imaginary)
c size image is the number of points in spectrum, depends also on nchannel

      call copvect(tdata,timage,sizeimage)
      size = sizeimage1d/nchannel
C loops on channels
      do i=1,nchannel
         start = (i-1)*size +1
         startd = (i-1)*sizedata +1
         call fftreal(tdata(start),size,-1,1)
c   sign=-1 car ift et direc=1 car reel vers complexe
         if (sumcons.eq.1) zz = tdata(start)
         if (deconv.eq.1)
     *      call diavect(tdata(start),tdata(start),
     *                   filter(startd),sizedata)
         if (sumcons.eq.1) tdata(start) = zz
      enddo
C then adds the channels altogether
      if (nchannel.gt.1) then
        do i=2,nchannel
           start = (i-1)*size +1
           call addvect(tdata(1),tdata(1),tdata(start),1.0,sizedata)
        enddo
      endif

      if (sizedata.gt.size)
     *    call prpvect(tdata(sizedata+1), 0.0, sizedata-size)
c    si on a fait du zero-filling on ne comparera aux vraies donnees
c    que sur les sizedata premiers points
      return
      end

C*********************************************************
 
      subroutine ttrans1(timage,tdata)
c IN	: tdata 
c OUT	: timage
c
c the MaxEnt transform in 1D
c

      implicit none
#include "sizeparam.inc"
#include "transform.inc"
#include "paramfilev.inc"
#include "paramdatav.inc"

      real timage(*),tdata(*),zz
      integer i,size,start,startd       

c     image has sizeimage real points and 2*sizeimage
c     points when zeros are added in the imaginary part
c     data (the fid) a sizedata/2 points complexes     

      size = sizeimage1d/nchannel
      do i=1,nchannel
         start = 1+(i-1)*size
         startd = 1+(i-1)*sizedata
         call cp2vect(timage(start),tdata,size,sizedata)
c eventualy zero-filling or truncature
         if (sumcons.eq.1) zz = timage(start)
         if (deconv.eq.1)
     *        call diavect(timage(start),timage(start),
     *                     filter(startd),sizedata)
         if (sumcons.eq.1) timage(start) = zz
         call fftreal(timage(start),size,1,-1)
      enddo
      return                                             
      end


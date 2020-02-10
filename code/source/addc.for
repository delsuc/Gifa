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
C

C
C addc1d(id,err)
c addc2d(id,err)
c addc3d(id,err)
c
      subroutine addc1d(idl,err)
c IN	: idl
c OUT	: err
c err=1 means no room for operation
c 
c add the contents of the 1D buffer with the file pointed by idl 
c
c assume that most tests have been done by caller
c

      implicit none
      integer idl, err
#include "cachev.inc"
#include "paramfilev.inc"
#include "sizeparam.inc"

      logical bool

      if (sizeimage1d.le.sizemax) then
            call ch_ac1darea(work1d, idl, 1, sizeimage1d, CB_READ, err)
            call addvect(column,column,work1d,1.0,sizeimage1d)
      elseif (sizeimage1d.gt.4*smxmax) then
            err = 1
            return
      else
            call checkoverflow(smxbig-sizeimage1d,bool,1)
            if (bool) then
               err = 1
               return
            endif
            call ch_ac1darea(image(smxbig-sizeimage1d),
     *                       idl, 1, sizeimage1d, CB_READ, err)
            call addvect(column,column,
     *               image(smxbig-sizeimage1d),1.0,sizeimage1d)
       endif
       return
       end
       
c=================================================================
       subroutine addc2d(idl,err)
c IN	: idl
c OUT	: err
c err=1 means no room for operation
c 
c add the contents of the 2D buffer with the file pointed by idl 
c
c assume that most tests have been done by caller
c

      implicit none

      integer idl, err
#include "cachev.inc"
#include "paramfilev.inc"
#include "sizeparam.inc"

      logical bool
      integer i
           
      if (si2im.le.sizemax) then
            do i=1,si1im
               call ch_ac2darea(work1d,idl,i,1,i,si2im,CB_READ,err)
               call addvect(plane2d((i-1)*si2im+1),
     *                      plane2d((i-1)*si2im+1),
     *                      work1d,1.0,si2im)
            enddo
      elseif (si2im .gt. smxbig-si1im*si2im) then
            err = 1
            return
      else
            call checkoverflow(smxbig-si2im,bool,1)
            if (bool) then
               err = 1
               return
            endif
            do i=1,si1im
               call ch_ac2darea(image(smxbig-si2im),idl,i,1,i,si2im,
     *                        CB_READ,err)
               call addvect(plane2d((i-1)*si2im+1),
     *                      plane2d((i-1)*si2im+1),
     *                      image(smxbig-si2im),1.0,si2im)
            enddo

       endif
       return
       end

c=================================================================
       subroutine addc3d(idl,err)
c IN	: idl
c OUT	: err
c err=1 means no room for operation
c 
c add the contents of the 3D buffer with the file pointed by idl 
c
c assume that most tests have been done by caller
c

      implicit none

      integer idl, err
#include "cachev.inc"
#include "paramfilev.inc"
#include "sizeparam.inc"

      integer i
           
      if (si3d3*si3d2.le.sizemax) then
            do i=1,si3d1
               call ch_ac3darea(work1d,idl,i,1,1,i,si3d2,si3d3,
     *              CB_READ,err)
               call addvect(image((i-1)*si3d3*si3d2+1),
     *                      image((i-1)*si3d3*si3d2+1),
     *                      work1d,1.0, si3d3*si3d2)
            enddo
      elseif (si3d3*si3d2 .gt. smxmax) then
            err = 1
            return
      else
            do i=1,si3d1
               call ch_ac3darea(plane2d,idl,i,1,1,i,si3d2,si3d3,
     *            CB_READ,err)
               call addvect(image((i-1)*si3d3*si3d2+1),
     *                      image((i-1)*si3d3*si3d2+1),
     *                      plane2d, 1.0, si3d3*si3d2)
            enddo

       endif
       return
       end

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

C***************************************************************

      subroutine real3d(image,size1,size2,size3,mode)
c IN	: mode
c INOUT	: image,size1,size2,size3
c
C real3D extract the RRR part of the 3D 3complex array image, and put it
C back in image changed to 3D real, size1 size2 and size3 then refer to real.
c

      implicit none
      integer size1,size2,size3,mode
      real image(size3,size2,size1)
      integer si

      if (mode.eq.1) then
         si = size2*size3
         call real2d(image,size1,si,1)
      else if(mode .eq.2) then
         si = size1*size2
         call real2d(image,si,size3,1)
         size2 = size2/2
      else if (mode.eq.4) then
         si = size1*size2
         call real2d(image,si,size3,2)
      else if (mode.eq.3) then
         si = size2*size3
         call real2d(image,size1,si,1)
         si = size1*size2
         call real2d(image,si,size3,1)
         size2 = size2/2
      else if (mode.eq.5) then
         si = size2*size3
         call real2d(image,size1,si,3)
         size3 = size3/2
      else if (mode.eq.6) then
         si = size1*size2
         call real2d(image,si,size3,3)
         size2 = size2/2
      else if (mode.eq.7) then
         si = size1*size2
         call real2d(image,si,size3,3)
         size2 = size2/2
         si = size2*size3
         call real2d(image,size1,si,1)
      endif
      return
      end

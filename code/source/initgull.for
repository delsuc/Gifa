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
      subroutine initgull
c initialises the parameters and memory for MaxEnt run

      implicit none

#include "sizeparam.inc"
#include "paramfilev.inc"
#include "maxentv.inc"
#include "paramdatav.inc"

      integer imax,imin
      real    max,min      

C set up the memory for iteration
      sigma = noise*noise
      if (memmode.eq.1) then
         call ttransform(image,datab,dim,memmode)
         if (sigma.eq.0.0) sigma=1.0  
         call mnxvect(min,max,imin,imax,image,sizeimage)
      else
         call mnxvect(min,max,imin,imax,datab,sizedata)
      endif
      max=max*ivalue/sizeimage
      call prpvect(image,max,sizeimage)
      return
      end


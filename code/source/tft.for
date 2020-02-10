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
      subroutine transform(tdata,timage,dim,mode)
c IN	: timage,dim.mode
c OUT	: tdata
c
c implements the transform procedure for maxent
C if mode = 1 -> FT
C    mode = 2 -> laplace over regular values
C    mode = 3 -> laplace over tabulated values
c  dim=1 if 1d nmr spectrum,dim=2 if 2d nmr specrum
c
      implicit none

      integer dim,mode
      real    tdata(*),timage(*)

      if (mode.eq.1) then
         if (dim.eq.1)  then
            call trans1(tdata,timage)
         else
            call trans2(tdata,timage)
         endif
      elseif (mode.eq.2) then
         call translap(tdata,timage)
      elseif (mode.eq.3) then
         call transtlap(tdata,timage)
      endif

      return
      end

c*************************************************************
      subroutine ttransform(timage,tdata,dim,mode)
c IN	: tdata,dim.mode
c OUT	: timage 
c
c implements the ttransform procedure for maxent
c  dim=1 if 1d nmr spectrum,dim=2 if 2d nmr specrum
c

      implicit none

      integer dim,mode
      real    tdata(*),timage(*)
      
      if (mode.eq.1) then
         if (dim.eq.1) then
            call ttrans1(timage,tdata)
         else
            call ttrans2(timage,tdata)
         endif
      elseif (mode.eq.2) then
         call ttranslap(timage,tdata)
      elseif (mode.eq.3) then
         call ttranstlap(timage,tdata)
      endif
      return
      end


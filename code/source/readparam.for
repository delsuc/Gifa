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
       subroutine readparam1d(file)
C this subroutine reads-in paramater block of the file, (if any)

       implicit none
#include "paramfilev.inc"

       integer file

c       read(file,err=100,end=100) sp1d,of1d,it1d
c       if (itype.lt.0 .or. itype.gt.1) goto 100
c       return

C if no parameter block available...
c100    write(*,*) 'no parameter block stored with that file'
c       write(*,*) 'Setting ITYPE to 0 (real)'
       it1d = 0
       write(*,*) 'Setting Spectral width to 2000*pi'
       sp1d = 2000*3.1415926
       of1d = 0.0
       return
       end

C**************************************************************
       subroutine readparam2d(file)
C this subroutine reads-in paramater block of the file, (if any)

       implicit none
#include "paramfilev.inc"

       integer file

c       read(file,err=100,end=100) specw1,offset1,specw2,offset2,itype
c       if (itype.lt.0 .or. itype.gt.3) goto 100
c       return

C if no parameter block available...
c100    write(*,*) 'no parameter block stored with that file'
c       write(*,*) 'Setting ITYPE to 0 (real in both dimensions)'
       itype = 0
       write(*,*) 'Setting Spectral width to 2000*pi'
       specw1 = 2000*3.1415926
       specw2 = 2000*3.1415926
       offset1 = 0.0
       offset2 = 0.0
       return
       end







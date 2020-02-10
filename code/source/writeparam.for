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
       subroutine writeparam(file)
C this subroutine write paramater block of the file

       implicit none

#include "paramfilev.inc"
       integer file

       write(file,err=100) sp1d,of1d,it1d

       return
100    write(*,*) 'Error in writing'
       return
       end

C*******************************************************
       subroutine writeparam2d(file)
C this subroutine write paramater block of the file

       implicit none

#include "paramfilev.inc"
       integer file

       write(file,err=100) specw1,offset1,specw2,offset2,itype

       return
100    write(*,*) 'Error in writing'
       return
       end

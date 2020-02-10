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
       subroutine writeparam3d(file)
C this subroutine write paramater block of the file

       implicit none

#include "paramfilev.inc"

       integer file

       write(file,err=100) sp3d1,of3d1,sp3d2,of3d2,it3d,
     *  si3d1,sp3d3,of3d3

       return
100    call gifaout( 'Error in writing')
       return
       end

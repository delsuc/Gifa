C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Authors :       M.A.Delsuc,
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully 
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C
       subroutine readparam3d(file)
C this subroutine reads-in paramater block of the file, (if any)

       implicit none

#include "paramfilev.inc"

       integer file,err,sitot

       read(file,end=100,err=100) sp3d1,of3d1,sp3d2,of3d2,
     *    it3d,si3d1,sp3d3, of3d3
       write(*,*) 'si1 = ',si3d1

       if (it3d.lt.0 .or. it3d.gt.7) then
          goto 100
       else
          si3d2 = si3d2/si3d1
       endif
       return
              
C if no parameter block available...
100    call message( 'no parameter block stored with that file')
       call message( 'Please enter IT3D')
       it3d = 0
       call getint2(it3d,err)
       sitot = si3d2
50     call message('Please enter SIZE in dim 1')
       si3d1 = int(sqrt(float(sitot)))
       call getint2(si3d1,err)
       si3d2 = sitot/si3d1 
       if ((si3d2*si3d1).ne.sitot) then
          call gifaout('dim1 not valid, try again')
          goto 50       
       endif                                                   
       write(*,*) 'size is si1 x si2 x si3:' ,si3d1,si3d2,si3d3
       write(*,*) 'Setting Spectral widths to 2000*pi'
       sp3d1 = 2000*3.1415926
       sp3d2 = 2000*3.1415926
       sp3d3 = 2000*3.1415926
       write(*,*) '...and offsets to 0'
       of3d1 = 0.0
       of3d2 = 0.0
       of3d3 = 0.0
       return
       end

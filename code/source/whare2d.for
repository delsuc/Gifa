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
       subroutine whare2d(spec,size1,size2,name)
c IN	: spec,size1,size2,name
c
c writes the 2D NMR data-set spec(size2,size1) as a FT-NMR compatible file
c of name "name"
c If name.eq.' '  then will prompt for a file-name
c
       implicit none                 

#include "controlc.inc"
       integer size1,size2
       real spec(size1*size2)
       character*256 name,fichier
       integer error,i,j
        
       if (name.eq.' ') then
          call gifaout(' filename: ')
          call getstring(fichier)
       else
          fichier = name
       endif
       open(unit=10,file=fichier,iostat=error,
     *     status='new',form='unformatted')
       if (error.ne.0) then
           call gifaout('*** Unable to open the file! ' // fichier)
           return
       endif

       do 30 j=1,size1
         if (mod(j,16).eq.0)write(*,100) J,size2
100      format ('+',2i6)
         write(10,iostat=error,err=40) size2/2,
     *   (spec(i+(j-1)*size2),i=1,size2) 
         if (control.eq.1) goto 40
30     continue
c       call writeparam(10)
       close(10)
       return

40     call gifaout('*** Error in writing the file ' //fichier)
       close(10)
       return
       end

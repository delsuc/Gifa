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
       subroutine whare(spec,sizes,name)

       implicit none

       integer sizes
       real spec(sizes)
       character*256 fichier,name
       integer error,i,sz
        
       sz = sizes
       if (name.eq.' ') then
          write(*,20)
          call getstring(fichier)
20        format (' filename: ')
       else
          fichier = name
       endif
       open(unit=10,file=fichier,iostat=error,
     *     status='new',form='unformatted')
       if (error.ne.0) then
          write(*,*) 'Unable to open the file!'
          return
       endif
       write(10,iostat=error,err=40) sz/2,(spec(i),i=1,sz)
c       call writeparam(10)
       close(10)
       return

40     write(*,*) 'Error in writing the file'
       close(10)
       return
       end

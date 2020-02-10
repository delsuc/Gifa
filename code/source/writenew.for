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
C to write gifa files with parameters in the beginning of the file

       subroutine writenew(spec,size1,size2,size3,name)

       implicit none                 

#include "controlc.inc"
#include "paramfilev.inc"

       integer size1,size2,size3
       integer error,i,j
       real spec(size1*size2*size3)
       character*256 fichier,name

       do i=1,10
         empty_array1(i)=0
       enddo
       do i=1,size_empty
         empty_array2(i)=0
       enddo
       
       if (name.eq.' ') then
          call getstring(fichier)
          call gifaout('filename:')
       else
          fichier = name
       endif

       open(unit=10,file=fichier,iostat=error,
     *     status='new',form='unformatted')
       if (error.ne.0) then
        call gifaout('Unable to open the file:'//name)
        return
       endif

       call gifaout ('frequency of the spectrometer:')
       call getreal(frequency)
       call gifaout ('date (dd/mm/yy):')
       call getstring(date)
       write(*,*)

       write(unit=10,iostat=error,err=50) 
     *    (tablo(i),i=1,size_tot)
 
       if (control.eq.1) then
         call gifaout('writing aborted by user')
         close(10)
         return
       endif

       do j=1,size1*size2
        if (mod(j,16).eq.0) write(*,100) j,size3
100     format ('+',2i6)
        write(unit=10,iostat=error,err=40)
     *   (spec(i+(j-1)*size3),i=1,size3) 

        if (control.eq.1) then
         call gifaout('writing aborted by user')
         close(10)
         return
        endif
       enddo
       call gifaout('sucessfull writing of the file '//name)
       close(10)
       return

40     call gifaout ('Error in writing the file')
       close(10)
       return
50     call gifaout ('Error in writing the parameters')
       close(10)
       return
       end

C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in CBS/Montpellier
C
C     Authors :       M.A.Delsuc
C                     C.B.S. Faculte de Pharmacie
C                     34060 Montpellier   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C

C this subroutine writes gifa data in text format
C**************************************************
       subroutine writet3d(spec,size1,size2,size3,name)

        implicit none
     
#include "controlc.inc"
#include "paramfilev.inc"

        integer      size1,size2,size3
        real         spec(size1*size2*size3)
        character*256 fichier,name
        integer      error
     
        integer      i,j

        if (name.eq.' ') then
         write(*,20)
         call getstring(fichier)
20       format(' filename: ')
        else
         fichier = name
        endif

       open(unit=10,file=fichier,iostat=error,
     *     status='new',form='formatted')
       if (error.ne.0) then
        call gifaout('unable to open file!'//name)
        return
       endif
   
       write(*,*) 'writing standard file'
  
       write(unit=10,fmt=85,iostat=error,err=40)
     *    dim
85     format(x,'dim='/i2)
       write(unit=10,fmt=80,iostat=error,err=40)
     *    fichier
80     format(x,'nom='/A256)

       if (dim.eq.3) then
        write(unit=10,fmt=90,iostat=error,err=40)
     *    size1,size2,size3,it3d
90      format(x,'size1='/i5/x,'size2='/i5/
     *    x,'size3='/i5/x,'itype='/i5)
        write(unit=10,fmt=91,iostat=error,err=40)
     *    sp3d1,sp3d2,sp3d3,of3d1,of3d2,of3d3
91      format(x,'specw1='/G15.8/x,'specw2='/G15.8/
     *    x,'specw3='/G15.8/x,'offset1='/G15.8/
     *    x,'offset2='/G15.8/x,'offset3='/G15.8)

       elseif (dim.eq.2) then
        write(unit=10,fmt=92,iostat=error,err=40)
     *    size2,size3,itype
92      format(x,'size1='/i5/x,'size2='/i5/
     *    x,'itype='/i5)
        write(unit=10,fmt=93,iostat=error,err=40)
     *    specw1,specw2,offset1,offset2
93      format(x,'specw1='/G15.8/x,'specw2='/G15.8/
     *    x,'offset1='/G15.8/x,'offset2='/G15.8)

       else
        write(unit=10,fmt=94,iostat=error,err=40)
     *    size3,it1d
94      format(x,'size1='/i5/x,'itype='/i5)
        write(unit=10,fmt=95,iostat=error,err=40)
     *    sp1d,of1d
95      format(x,'specw1='/G15.8/x,'offset1='/G15.8)
       endif

       write(unit=10,fmt=100,iostat=error,err=40)
100    format(x,'data:')

       do 30 j=1,size1*size2            
        if (mod(j,16).eq.0)write(*,210) j,size3
210     format (2i6)

        write(unit=10,fmt=120,iostat=error,err=40)
     *  (spec(i+(j-1)*size3),i=1,size3)
        if (control.eq.1) goto 40
120     format( G15.8)

30     continue
       close(10)
       return

40     call gifaout(' error in writing the file')
       close(10)
       return
       end


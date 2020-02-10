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
       subroutine readnew(spec,name,error)

       implicit none

       character*256   fichier,name
       integer        j,i,error,toobig,
     *                size1,size2,size3,sizeim
       real           spec(*)

#include "sizeparam.inc"
#include "paramfilev.inc"
#include "controlc.inc"

       data fichier/'noname'/
        
       if (name.eq.' ') then
          call getstring(fichier)
          call gifaout('filename:')
       else
          fichier = name
       endif

       toobig=0

       do i=1,10
         empty_array1(i)=0
       enddo
       do i=1,size_empty
         empty_array2(i)=0
       enddo

       open(unit=10,file=fichier,iostat=error,
#vms     *     readonly,
     *     status='old',form='unformatted')

       read(unit=10,iostat=error,end=40)
     *    (tablo(i),i=1,size_tot)

       call gifaout('reading parameters')

       if (control.eq.1) then
         call gifaout('writing aborted by user')
         error=1
         close(10)
         return
       endif

       if (dim.eq.0) then
10      if (control.eq.0) then
         call gifaout('reading aborted by user')
         error=1
         close(10)
         return
        else
         call getint(dim)
         call gifaouti('enter dimension of data to read (1,2,or 3):'
     *           ,dim)
         if ((dim.ne.1).and.(dim.ne.2).and.(dim.ne.3)) goto 10
        endif
       else
        goto 20
       endif

20     call gifaouti('dimension of the data to read:',dim)
       call gifaout('date: ')   

       if (dim.eq.1) then	       
        size3=sizeimage1d
        size2=1
        size1=1
        sizeim=sizeimage1d
        call gifaouti('size of the data:',sizeimage1d)   
        call gifaouti('itype:',it1d)   
        call gifaoutr('spectral width:',sp1d)   
        call gifaoutr('offset:',of1d)  
        call gifaouti('data along dim',axis1d)
        call gifaouti('first coordinate',n1d1) 
        call gifaouti('second coordinate',n1d2)
        call gifaoutr('maximum of data:',max1d)
        max0=max1d
       elseif (dim.eq.2) then   
        size3=si2im
        size2=si1im
        size1=1
        sizeim=sizeimage
        call gifaouti('size of the data:',sizeimage)
        call gifaouti('size of the data in dim1:',si1im)
        call gifaouti('size of the data in dim2:',si2im)   
        call gifaouti('itype:',itype)   
        call gifaoutr('spectral width in dim 1:',specw1)   
        call gifaoutr('spectral width in dim 2:',specw2)   
        call gifaoutr('offset in dim 1:',offset1)   
        call gifaoutr('offset in dim 2:',offset2)
        call gifaouti('plane perpendicular to dim',axis2d)
        call gifaouti('plane number',n2d)
        call gifaoutr('maximum of data:',max2d)
        max0=max2d
       elseif (dim.eq.3) then
        size3=si3d3
        size2=si3d2
        size1=si3d1
        sizeim=sizeimage3d
        call gifaouti('size of the data:',sizeimage3d)
        call gifaouti('size of the data in dim1:',si3d1)
        call gifaouti('size of the data in dim2:',si3d2)   
        call gifaouti('size of the data in dim3:',si3d3)   
        call gifaouti('itype:',it3d)   
        call gifaoutr('spectral width in dim 1:',sp3d1)   
        call gifaoutr('spectral width in dim 2:',sp3d2)   
        call gifaoutr('spectral width in dim 3:',sp3d3)   
        call gifaoutr('offset in dim 1:',of3d1)   
        call gifaoutr('offset in dim 2:',of3d2)
        call gifaoutr('offset in dim 3:',of3d3)
        call gifaoutr('maximum of data:',max3d)
        max0=max3d
       else
        call gifaout('invalid dimension of the data')
        close(10)
        return
       endif

       if (sizeim.gt.smxbig) goto 35

       call gifaout('reading the data')
       write(*,*)

       do j=1,size1*size2
        if (mod(j,16).eq.0)then
         write(*,120) j,size3
120      format('+',2i6)
        endif      
        read(unit=10,iostat=error,end=40)
     *   (spec(i+(j-1)*size3),i=1,size3) 

        if (error.ne.0) return
        if (control.eq.1) goto 40
       enddo
       call gifaout('successful reading of data '//name)
       close(10)
       return

35     call gifaout('data are too big!')
       error=1
       close(10)
       return
40     call gifaout('reading aborted by user')
       error=1
       close(10)
       return
       end

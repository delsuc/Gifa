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
       subroutine lhare2d(tdata,dim1,dim2,name,error,noise)
c INOUT	: name
c OUT	: tdata,dim1,dim2,noise,error
c
c reads a file called "name" in 2D D.Hare format

       implicit none

       character*256   fichier,name
       integer     j,i,error,dim1,dim2,zz,toobig
       real        tdata(*),noise,shift
#include "sizeparam.inc"
#include "controlc.inc"

       data fichier/'noname'/
        
       if (name.eq.' ') then
          write(*,20)
          call getstring(fichier)
20        format (' filename: ')
       else
          fichier = name
       endif
       toobig=0
       open(unit=10,file=fichier,iostat=error,
#vms     *     readonly,
     *     status='old',form='unformatted')
       if (error.ne.0) return
       j=1        
101    read(10,iostat=error,end=40)
     *      dim2,(tdata(i),i=1,min(dim2*2,smxbig)) ! read first one
       if (error.ne.0) return
c       if (dim2.lt.0) goto 101   ! modif MAD 5-mai-1995
       if (dim2.lt.0) then
           write (*,*) '*** Wrong size '
           error = 1
           return
       endif
       if (dim2.lt.0) goto 101
       if (dim2*2.gt.smxbig) then
           write(*,*) '*** File is too big'
           error=1
           return
       endif
       j=j+1
30     read(10,iostat=error,end=40,err=40)
     *      zz,(tdata(i+2*(j-1)*dim2),i=1,dim2*2)
            if (zz.ne.dim2) goto 40
            if (mod(j,16).eq.0)write(*,100) J,2*dim2
100         format ('+',2i6)
            j=j+1
            if (2*j*dim2.gt.smxbig) goto 35
            if (error.ne.0) return
            if (control.eq.1) goto 40
       go to 30
35     toobig=1
40     continue
       error = 0
       dim1=(j-1)
       dim2=dim2*2
       write (*,*) 'size of data in dim 1 and 2:',
     * dim1,' ',dim2
       if (control.eq.1) then
         close(10)
         return
       endif
C backspace / param removed for speed -> use cache file format
c       if (toobig.ne.1) backspace(10)
c       call readparam2d(10)
       if (toobig.ne.1) 
     *  call gifaout('Warning, file got truncated in F1')
       close(10)
 
C evaluate noise 
       i=dim2*9/10
       call evaln(tdata(dim2*(dim1-1)+i),dim2-i+1,noise,shift)
       return
       end



C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in CBS/Montpellier
C
C     Authors :       Therese Malliavin
C                     C.B.S. Faculte de Pharmacie
C                     34060 Montpellier   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.

C**********************************************************
       subroutine readt3d(spec,name,error)
c IN	: name
c OUT	: error,spec
c SIDE	: in dataset parameters
c
c reads in a file in text format
c
       implicit none

       character*256  name
       integer       size1,size2,size3,error,itp
       real          sp1,sp2,sp3,off1,off2,off3
       real          spec(*)
       character*256  tline
       character*256  char,names

#include "paramfilev.inc" 
#include "sizeparam.inc"
#include "controlc.inc"

       size1=1
       size2=1
       size3=0

       open(unit=10,file=name,iostat=error
#vms     *        ,readonly
     *        ,status='old',form='formatted')

       if (error.ne.0) then
          call gifaout('Error in opening file '//name)
          goto 999
       endif
       
10     read(unit=10,fmt=100,iostat=error,end=999)
     *     char
100    format(A256)
       if (error.ne.0) then
          call gifaout('Error in reading file '//name)
          return
       endif
    
       call leading(char)
       if (char.eq.'dim=') then
        read(unit=10,fmt=90,iostat=error,end=999)
     *     dim
        write(tline,91) dim
        call gifaout(tline)
       elseif (char.eq.'nom=') then
        read(unit=10,fmt=101,iostat=error,end=999)
     *     names
        write(tline,102) names
        call gifaout(tline)
       elseif(char.eq.'size1=') then
        read(unit=10,fmt=103,iostat=error,end=999)
     *     size1
        write(tline,104) size1
        call gifaout(tline)
       elseif(char.eq.'size2=') then
        read(unit=10,fmt=103,iostat=error,end=999)
     *     size2
        write(tline,105) size2
        call gifaout(tline)
       elseif(char.eq.'size3=') then
        read(unit=10,fmt=103,iostat=error,end=999)
     *     size3
        write(tline,115) size3
        call gifaout(tline)
       elseif(char.eq.'itype=') then
        read(unit=10,fmt=103,iostat=error,end=999)
     *     itp
        write(tline,106) itp
        call gifaout(tline)
       elseif(char.eq.'specw1=') then
        read(unit=10,fmt=107,iostat=error,end=999)
     *     sp1
        write(tline,108)sp1
        call gifaout(tline)
       elseif(char.eq.'specw2=') then
        read(unit=10,fmt=107,iostat=error,end=999)
     *     sp2
        write(tline,109)sp2
        call gifaout(tline)
       elseif(char.eq.'specw3=') then
        read(unit=10,fmt=107,iostat=error,end=999)
     *     sp3
        write(tline,116)sp3
        call gifaout(tline)
       elseif(char.eq.'offset1=') then
        read(unit=10,fmt=107,iostat=error,end=999)
     *     off1
        write(tline,110) off1
        call gifaout(tline)
       elseif(char.eq.'offset2=') then
        read(unit=10,fmt=107,iostat=error,end=999)
     *     off2
        write(tline,111) off2
        call gifaout(tline)
       elseif(char.eq.'offset3=') then
        read(unit=10,fmt=107,iostat=error,end=999)
     *     off3
        write(tline,117) off3
        call gifaout(tline)
       elseif(char.eq.'data:') then 
        write(*,*)'reading data'
        write(*,*)
        call readsdata(spec,size1,size2,size3,error)
        goto 40
       else
         call gifaout('data item not found : '//char)
         goto 10
       endif
       if (error.ne.0) then
          if (char.eq.'data:') then
             call gifaout('Error in reading data')
          else
            call gifaout('Error in reading item '//char)
          endif
          goto 999
       endif
       goto 10

90     format(i2)
91     format(' dimension of data:',i2)
101    format(A256)
102    format(1x,'name of the standart file:',A40)
103    format(i6)
104    format(' size in dim 1:',i6)
105    format(' size in dim 2:',i6)
106    format(' itype=',i6)
107    format(G15.8)
108    format(' spectral width in dim1:',G15.8)
109    format(' spectral width in dim2:',G15.8)
110    format(' offset in dim1:',G15.8)
111    format(' offset in dim2:',G15.8)
113    format(G22.15)
115    format(' size in dim 3:',i6)
116    format(' spectral width in dim3:',G15.8)
117    format(' offset in dim3:',G15.8)


40     continue    ! succesfull read
       error=0 
       close(10)

       if (dim.eq.1) then
          sizeimage1d=size1
          it1d=itp
          sp1d=sp1
          of1d=off1
          max1d=max0
       elseif (dim.eq.2) then
          si2im=size2
          si1im=size1
          itype = itp
          specw2 = sp2
          specw1 = sp1
          offset1 = off1
          offset2 = off2
          sizeimage=si1im*si2im
          max2d=max0
       else
          it3d=itp
          sp3d1=sp1
          sp3d2=sp2
          sp3d3=sp3
          of3d1=off1
          of3d2=off2
          of3d3=off3
          si3d1 = size1
          si3d2 = size2
          si3d3 = size3
          sizeimage3d=si3d1*si3d2*si3d3
          max3d=max0
        endif
       return

999    call gifaout('Error in data or early eof')
       error=1
       close(10)
       return
       end
C*****************************************************************
C*****************************************************************
       subroutine readsdata(spec,si1,si2,si3,error)
c IN	: si1,si2,si3
c OUT	: spec,error
c
c reads in a text format file
c used by readt3d

       implicit none

       integer    error,si1,si2,si3
       integer    i,j,size1,size2,size3
       real       spec(si1*si2*si3)

#include "paramfilev.inc"
#include "sizeparam.inc"
#include "controlc.inc"
       if (dim.eq.1) then
          size3=si1
          size1=1
          size2=1
       elseif (dim.eq.2) then
          size3=si2
          size2=si1
          size1=1
       else
          size1=si1
          size2=si2
          size3=si3
        endif


       if (size3.eq.0) goto 32
       if (size1*size2*size3.gt.smxbig) goto 30
       do 25 j=1,size1*size2
        if (mod(j,16).eq.0) write(*,120) j,size3
120     format( '+',2i6)
          
         read(unit=10,fmt=121,iostat=error,err=35)
     *  (spec(i+(j-1)*size3),i=1,size3)
c         write(*,*)(spec(i+(j-1)*size3),i=1,size3)
        if (control.eq.1) goto 35
121     format( G25.12)

        if (control.eq.1) goto 35
        
25     continue
       return

30     call gifaout('the file is too big!!!')
       error=1
       return

32     call gifaout('size of the aquisition dimension is zero!!!')
       error=1
       return

35     call gifaout('Error in reading the data')
       error=1
       return
       end

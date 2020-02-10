C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Authors :       M.A.Delsuc, V.Stoven
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C

C this subroutine reads text data compacted in base 64.
C**********************************************************
       subroutine readtc3d(name,error)


       implicit none

       character*256  name
       integer       size1,size2,size3,error
       real          sp1,sp2,sp3,of1,of2,of3
       real*8        const
       character*256  tline
       character*30  char,names

#include "paramfilev.inc"
#include "sizeparam.inc"
#include "controlc.inc"

       size1=1
       size2=1
       size3=0
       const=2.0d0**28
       max0=10000.0

       open(unit=10,file=name,iostat=error
#vms     *        ,readonly
     *        ,status='old',form='formatted')

       if (error.ne.0) then
          call gifaout('Error in opening file '//name)
          return
       endif
       
10     read(unit=10,fmt=80,iostat=error,end=40)
     *     char
80     format(A)
       if (error.ne.0) then
          call gifaout('Error in reading file '//name)
          return
       endif

       if (char.eq.' ') goto 10
       call leading(char)
        if (char.eq.'dim=') then
         read(unit=10,fmt=90,iostat=error,end=40)
     *     dim
         write(tline,91) dim
         call gifaout(tline)
        elseif (char.eq.'name=') then
         read(unit=10,fmt=101,iostat=error,end=40)
     *     names
         write(tline,102) names
         call gifaout(tline)
        elseif(char.eq.'size1=') then
         read(unit=10,fmt=103,iostat=error,end=40)
     *     size1
         write(tline,104) size1
         call gifaout(tline)
        elseif(char.eq.'size2=') then
         read(unit=10,fmt=103,iostat=error,end=40)
     *     size2
         write(tline,105) size2
         call gifaout(tline)
        elseif(char.eq.'size3=') then
         read(unit=10,fmt=103,iostat=error,end=40)
     *     size3
         write(tline,115) size3
         call gifaout(tline)
        elseif(char.eq.'itype=') then
         read(unit=10,fmt=103,iostat=error,end=40)
     *     itype
         write(tline,106) itype
         call gifaout(tline)
        elseif(char.eq.'specw1=') then
         read(unit=10,fmt=107,iostat=error,end=40)
     *     sp1
         write(tline,108)sp1
         call gifaout(tline)
        elseif(char.eq.'specw2=') then
         read(unit=10,fmt=107,iostat=error,end=40)
     *     sp2
         write(tline,109)sp2
         call gifaout(tline)
        elseif(char.eq.'specw3=') then
         read(unit=10,fmt=107,iostat=error,end=40)
     *     sp3
         write(tline,116)sp3
         call gifaout(tline)
        elseif(char.eq.'offset1=') then
         read(unit=10,fmt=107,iostat=error,end=40)
     *      of1
         write(tline,110) of1
         call gifaout(tline)
        elseif(char.eq.'offset2=') then
         read(unit=10,fmt=107,iostat=error,end=40)
     *      of2
         write(tline,111) of2
         call gifaout(tline)
        elseif(char.eq.'offset3=') then
         read(unit=10,fmt=107,iostat=error,end=40)
     *      of3
         write(tline,117) of3
         call gifaout(tline)
        elseif(char.eq.'max0=') then
         read(unit=10,fmt=107,iostat=error,end=40)
     *      max0
         write(tline,112) max0
         call gifaout(tline)
        elseif(char.eq.'normalisation=') then
         read(unit=10,fmt=113,iostat=error,end=40)
     *     const
         write(tline,114) const
         call gifaout(tline)
        elseif(char.eq.'data:') then 
         write(*,*)'reading data'
         write(*,*)
         if (dim.eq.1) then
          size3=size1
          size1=1
          size2=1
          sizeimage1d=size3
          it1d=itype
          sp1d=sp1
          of1d=of1
          max1d=max0
         elseif (dim.eq.2) then
          si1im=size1
          si2im=size2
          sizeimage=si1im*si2im
          size3=size2
          size2=size1
          size1=1
          specw2 = sp2
          specw1 = sp1
          offset2 = of2
          offset1 = of1
          max2d=max0
         else
          si3d1=size1
          si3d2=size2
          si3d3=size3
          it3d=itype
          sp3d1=sp1
          sp3d2=sp2
          sp3d3=sp3
          of3d1=of1
          of3d2=of2 
          of3d3=of3 
          sizeimage3d=si3d1*si3d2*si3d3
          max3d=max0
         endif
         if (size3.eq.0) goto 32 
         if (max0.eq.0.0) goto 31
         if (size1*size2*size3.gt.smxbig) goto 30
         if (dim.eq.1) then
            call readscdata(column,size1,size2,size3,
     *               const,error)
         elseif (dim.eq.2) then
            call readscdata(plane2d,size1,size2,size3,
     *               const,error)
         elseif (dim.eq.3) then
            call readscdata(image,size1,size2,size3,
     *               const,error)
         endif
        else
         goto 20
        endif
       if (error.ne.0) then
          if (char.eq.'data:') then
             call gifaout('Error in reading data')
          else
            call gifaout('Error in reading item '//char)
          endif
          close(10)
          return
       endif
       goto 10

90     format(i2)
91     format(' dimension of data:',i2)
101    format(A256)
102    format(' name of the standart file:',A40)
103    format(i6)
104    format(' size in dim 1:',i6)
105    format(' size in dim 2:',i6)
106    format(' itype=',i6)
107    format(G15.8)
108    format(' spectral width in dim1:',G15.8)
109    format(' spectral width in dim2:',G15.8)
110    format(' offset in dim1:',G15.8)
111    format(' offset in dim2:',G15.8)
112    format(' maximum of spectral data:max0=',G15.8)
113    format(G22.15)
114    format(' normalisation constant:',G22.15)
115    format(' size in dim 3:',i6)
116    format(' spectral width in dim3:',G15.8)
117    format(' offset in dim3:',G15.8)

20     call gifaout('data item not found : '//char)
       goto 10

40     continue
       error=0 
       close(10)
       return

30     write(*,*) ' the file is too big!!!'
       error=1
       return

31     write(*,*) ' Error in data: max0=0'
       error=1
       return

32     write(*,*) ' size of the aquisition dimension is zero!!!'
       error=1
       return
       end
C*****************************************************************
C*****************************************************************
       subroutine readscdata(spec,size1,size2,size3,
     *                const,error)

C this subroutine reads the data which are characters (reals 
C writen in base 64) and return the correspondant 
C reals.

       implicit none

       integer        error,size1,size2,size3
       real*8         fact,const
       real           spec(size3,size1*size2)

       integer        i,j
       character*80   line

#include "paramfilev.inc"
#include "sizeparam.inc"
#include "controlc.inc"

       fact=max0/const

       call initinprog(size1*size2)     ! to show how it is going
       do j=1,size1*size2

        i=0
        do while (i.lt.size3)
         read(unit=10,fmt=121,iostat=error,err=35)
     *    line
121      format(A80)
         call leading(line)
         call getnumber(line,spec,i,j,size1,size2,size3,fact,error)
        enddo        
        if (control.eq.1) goto 35
        call inprog(j)
       enddo
       return

35     call gifaout('Error in reading the file line')
       error=1
       return
       end


C************************************************************
C************************************************************
       subroutine getnumber(line,spec,i,j,size1,size2,size3,
     *                  fact,error)

       implicit none

       integer      i,j,size1,size2,size3,error
       character*80 line,tline
       real*8       fact
       real         spec(size3,size1*size2)
       integer      espec(6)
       integer      p,q,csi
       character*1  sign

       p=1
       do while (line(p:p).ne.' ')
        i=i+1
        csi=0
        q=1
        do while ((line(p:p).ne.'+')
     *    .and.(line(p:p).ne.'-'))

         call b10(line(p:p),espec(q),error)
         q=q+1
         p=p+1
        enddo
        if (error.ne.0) then
            write(tline,*)'Error in reading value i, j =',i,j
            call gifaout(tline)
            return
        endif 
        sign=line(p:p)
        csi=q-1
        call base10(spec(i,j),fact,sign,espec,csi,error)
        p=p+1
        if (i.gt.size3) return
       enddo
       return 
       end

C************************************************************
C************************************************************
       subroutine b10(nb,n,err)

       implicit none

       integer n,err
       character*1 nb,char0,chara,charca
       data char0/'0'/,chara/'a'/,charca/'A'/

       if ((nb.ge.'0').and.(nb.le.'9')) then
        n=ichar(nb)-ichar(char0)
       elseif ((nb.ge.'A').and.(nb.le.'Z')) then
        n=ichar(nb)-ichar(charca)+10
       elseif ((nb.ge.'a').and.(nb.le.'z')) then
        n=ichar(nb)-ichar(chara)+36
       elseif (nb.eq.',') then
        n=62
       elseif (nb.eq.'.') then
        n=63
       else
         call gifaout('error in data')
         err=1
       endif
       return
       end

C************************************************************
C************************************************************
       subroutine base10(spec1,fact,sign1,espec1,csi1,err)
       implicit none

       integer     csi1,err
       integer     espec1(6)
       real        spec1
       real*8      fact
       character*1 sign1

       integer     m
       real        temp

       temp=0
       do m=csi1,1,-1
        temp=temp*64+ espec1(m)
       enddo

       if(sign1.eq.'-') then
        spec1=-fact*temp
       else
        spec1=fact*temp
       endif
       return
       end


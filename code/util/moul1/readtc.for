C this subroutine reads text data compacted in base 64.
C**********************************************************
       subroutine readtc(spec,name,size1,size2,size3,
     *      dim,max0,error)


       implicit none

       character*80  name
       integer       size1,size2,size3,dim,error
       real          spec(*)
       real          max0
       real*8        const
       character*80  tline
       character*30  char,names

       include 'paramfile.inc'
       include 'sizeparam.inc'
C       include 'controlc.inc'

       size1=1
       size2=1
       size3=0
       const=2.0d0**28
       max0=10000.0

       open(unit=10,file=name,iostat=error
cvms     *        ,readonly
     *        ,status='old',form='formatted')

       if (error.ne.0) then
          write(*,70) name
70        format(' Error in opening file ',A80)
          return
       endif
       
10     read(unit=10,fmt=80,iostat=error,end=40)
     *     char
80     format(A)
       if (error.ne.0) then
          write(*,81) name
81        format(' Error in reading file ',A80)
          return
       endif

        if (char.eq.' ') goto 10
        call leading(char)
        if (char.eq.'dim=') then
         read(unit=10,fmt=90,iostat=error,end=40)
     *     dim
         write(*,91) dim
        elseif (char.eq.'name=') then
         read(unit=10,fmt=101,iostat=error,end=40)
     *     names
         write(*,102) names
        elseif(char.eq.'size1=') then
         read(unit=10,fmt=103,iostat=error,end=40)
     *     size1
         write(*,104) size1
        elseif(char.eq.'size2=') then
         read(unit=10,fmt=103,iostat=error,end=40)
     *     size2
         write(*,105) size2
        elseif(char.eq.'size3=') then
         read(unit=10,fmt=103,iostat=error,end=40)
     *     size3
         write(*,115) size3
        elseif(char.eq.'itype=') then
         read(unit=10,fmt=103,iostat=error,end=40)
     *     itype
         write(*,106) itype
        elseif(char.eq.'specw1=') then
         read(unit=10,fmt=107,iostat=error,end=40)
     *     specw1
         write(*,108)specw1
        elseif(char.eq.'specw2=') then
         read(unit=10,fmt=107,iostat=error,end=40)
     *     specw2
         write(*,109)specw2
        elseif(char.eq.'specw3=') then
         read(unit=10,fmt=107,iostat=error,end=40)
     *     specw3
         write(*,116)specw3
        elseif(char.eq.'offset1=') then
         read(unit=10,fmt=107,iostat=error,end=40)
     *      offset1
         write(*,110) offset1
        elseif(char.eq.'offset2=') then
         read(unit=10,fmt=107,iostat=error,end=40)
     *      offset2
         write(*,111) offset2
        elseif(char.eq.'offset3=') then
         read(unit=10,fmt=107,iostat=error,end=40)
     *      offset3
         write(*,117) offset3
        elseif(char.eq.'max0=') then
         read(unit=10,fmt=107,iostat=error,end=40)
     *      max0
         write(*,112) max0
        elseif(char.eq.'normalisation=') then
         read(unit=10,fmt=113,iostat=error,end=40)
     *     const
         write(*,114) const
        elseif(char.eq.'data:') then 
         write(*,*)'reading data'
         write(*,*)
         if (dim.eq.1) then
          size3=size1
          size1=1
          size2=1
         elseif (dim.eq.2) then
          size3=size2
          size2=size1
          size1=1
         endif

         if (size3.eq.0) goto 32 
         if (max0.eq.0.0) goto 31
         if (size1*size2*size3.gt.smxbig) goto 30
         call readscdata(spec,size1,size2,size3,
     *               dim,max0,const,error)
        else
         goto 20
        endif
       if (error.ne.0) then
          if (char.eq.'data:') then
            write(*,*) 'Error in reading data'
          else
            write(*,120) char
120         format('Error in reading item ',A30)
          endif
          close(10)
          return
       endif
       goto 10

90     format(i2)
91     format(' dimension of data:',i2)
101    format(A80)
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

20     write(*,121) char
121    format('data item not found : ',A30)
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
     *                dim,max0,const,error)

C this subroutine reads the data which are characters (reals 
C writen in base 64) and return the correspondant 
C reals.

       implicit none

       integer        error,size1,size2,size3,dim
       real           max0
       real*8         fact,const
       real           spec(size3,size1*size2)

       integer        i,j,p,q
       character*80   line

       include 'paramfile.inc'
       include 'sizeparam.inc'

       fact=max0/const

       do j=1,size1*size2
        if (mod(j,16).eq.0)then
         write(*,120) j,size3
120      format('+',2i6)
        endif

        i=0
        do while (i.lt.size3)
10       read(unit=10,fmt=121,iostat=error,err=35)
     *    line
121      format(A80)
         if (line.eq.' ') goto 10
         call leading(line)
         call getnumber(line,spec,i,j,size1,size2,size3,fact,error)
        enddo        
       enddo
       return

35     write(*,*) ' Error in reading the file line'
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
            write(*,100) i,j
100         format('Error in reading value i, j =',2i6)
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
       character*1 nb

       if ((nb.ge.'0').and.(nb.le.'9')) then
        n=ichar(nb)-ichar('0')
       elseif ((nb.ge.'A').and.(nb.le.'Z')) then
        n=ichar(nb)-ichar('A')+10
       elseif ((nb.ge.'a').and.(nb.le.'z')) then
        n=ichar(nb)-ichar('a')+36
       elseif (nb.eq.',') then
        n=62
       elseif (nb.eq.'.') then
        n=63
       else
         write(*,*) ' error in data'
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

C************************************************************
C************************************************************
C       subroutine puis(b,a,n)
C
C       implicit none
C
C       integer a,n,b
C
C       integer l
C
C
C       if (a.eq.0) then
C        b=0
C       elseif ((a.eq.1).or.(n.eq.0)) then
C        b=1
C       elseif (n.eq.1) then
C        b=a
C       else 
C        b=a
C        do l=2,n
C           b=a*b
C        enddo
C       endif
C       return 
C       end

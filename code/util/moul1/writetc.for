C this subroutine writes gifa data in compacted (base 64)
C text format.
C********************************************************
       subroutine writetc(spec,size1,size2,size3,
     *       dim,max0,name)
     
        implicit none
     
        integer      size1,size2,size3
        real         max0
        real         spec(size1*size2*size3)
        character*80 fichier,name
        integer      error,dim
     
        integer      i,j,k,m
        integer      espec(10)
        integer      csi(10)
        real         si1,si2,si3
        real*8       fact,const
        character*6  cspec(10)
        character*80 tline

C        include 'controlc.inc'
        include 'paramfile.inc'
     
        if (name.eq.' ') then
         write(*,20)
         call getstring(fichier)
20       format(' filename: ')
        else
         fichier = name
        endif
     
        do i=1,size1*size2*size3
         max0=max(abs(spec(i)),max0)
        enddo
        if (max0.eq.0.0) then
         write(*,*) ' the file is empty!!!'
         return
        endif
        const = 2.0d0**28
        fact= const/max0


       open(unit=10,file=fichier,iostat=error,
     *     status='new',form='formatted')
       if (error.ne.0) then
        write(*,200) name
200     format('unable to open file!',A80)
        return
       endif
   
       write(*,*) 'writing standard file'
  
       write(unit=10,fmt=85,iostat=error,err=40)
     *    dim
85     format(1x,'dim='/i2)
       call trailing(fichier,k)
       write(unit=10,fmt=80,iostat=error,err=40)
     *    fichier(1:k)
80     format(1x,'name='/A)

       if (dim.eq.3) then
        write(unit=10,fmt=90,iostat=error,err=40)
     *    size1,size2,size3,itype
90      format(1x,'size1='/i5/1x,'size2='/i5/
     *    1x,'size3='/i5/1x,'itype='/i5)
        write(unit=10,fmt=91,iostat=error,err=40)
     *    specw1,specw2,specw3,offset1,offset2,offset3
91      format(1x,'specw1='/G15.8/1x,'specw2='/G15.8/
     *    1x,'specw3='/G15.8/1x,'offset1='/G15.8/
     *    1x,'offset2='/G15.8/1x,'offset3='/G15.8)

       elseif (dim.eq.2) then
        write(unit=10,fmt=92,iostat=error,err=40)
     *    size2,size3,itype
92      format(1x,'size1='/i5/1x,'size2='/i5/
     *    1x,'itype='/i5)
        write(unit=10,fmt=93,iostat=error,err=40)
     *    specw1,specw2,offset1,offset2
93      format(1x,'specw1='/G15.8/1x,'specw2='/G15.8/
     *    1x,'offset1='/G15.8/1x,'offset2='/G15.8)

       else
        write(unit=10,fmt=94,iostat=error,err=40)
     *    size3,itype
94      format(1x,'size1='/i5/1x,'itype='/i5)
        write(unit=10,fmt=95,iostat=error,err=40)
     *    specw1,offset1
95      format(1x,'specw1='/G15.8/1x,'offset1='/G15.8)

       endif

       write(unit=10,fmt=100,iostat=error,err=40)
     *    max0,const
100    format(1x,'max0='/G15.8/1x,'normalisation='/G22.15)
       write(*,201) max0
       write(*,*)
201    format(' maximum of spectral data:max0=',G15.8)
       write(unit=10,fmt=101,iostat=error,err=40)
101    format(1x,'data:')
   
       do j=1,size1*size2 
           
        if (mod(j,16).eq.0)write(*,210) j,size3
210     format ('+',2i6)

        i=0
        m=mod(size3,10)

        do while(i.lt.size3-m)

C convertit sspec en entier espec:
          call entier(espec,spec((i+1)+((j-1)*size3)),10,fact)

C convertit chaque entier espec(k) en base 64
C et teste le signe de espec(k).on obtient une chaine
C de caracteres cspec(k) de longueur csi(k):

         do k=1,10
          cspec(k)='      '
          call compac64(cspec(k),espec(k),csi(k))
          call signtest(cspec(k),espec(k),csi(k))
         enddo

C ecrit les dix chaines de caracteres cspec(k):

         write(unit=10,fmt=111,iostat=error,err=40)
     *        (cspec(k)(1:csi(k)),k=1,10)

C         if (control.eq.1) goto 40

         i=i+10
        enddo

        i=size3-m
         call entier(espec,spec((i+1)+((j-1)*size3)),m,fact)
         do k=1,m
          call compac64(cspec(k),espec(k),csi(k))
          call signtest(cspec(k),espec(k),csi(k))
         enddo
         write(unit=10,fmt=112,iostat=error,err=40)
     *        (cspec(k)(1:csi(k)),k=1,m)
 
111      format(1x,10A)
112      format(1x,10A)
   
       enddo
       close(10)
       return
     
40     write(*,*) ' error in writing the file' 
       close(10)
       return
       end
C********************************************************
C********************************************************
       subroutine entier(espec1,rspec1,size,fact)
 
C   This subroutine, given real data, returns
C the corresponding integer data whith the
C maximum of the data at const (2**27) .
     
     
       implicit none
 
       integer size
       integer espec1(size)
       real    rspec1(size)
       real*8  fact
     
       integer j
                  
       do j=1,size
        espec1(j) = nint(rspec1(j)*fact)
       enddo
       return
       end
C**********************************************************
C**********************************************************
       subroutine compac64(cspeci1,especi1,csi1)
     
       implicit none
 
       integer     especi1,csi1
       character*6 cspeci1

       integer     j,p,n
       character   nb
     
       p=1
       j=abs(especi1)
       do while (j.ge.64)
        n=mod(j,64)
        call base64(nb,n)
        cspeci1(p:p)=nb
        j=j/64
        p=p+1
       enddo
       n=j
       call base64(nb,n)
       cspeci1(p:p)=nb
       csi1=p
       return
       end
C**********************************************************
C**********************************************************
       subroutine base64(ib,i)
     
       implicit none
 
       integer   i
       character ib
     
       if (i.le.9) then 
        ib=char(ichar('0')+i)
       elseif (i.le.35) then
        ib=char(ichar('A')+(i-10))
       elseif (i.le.61) then
        ib=char(ichar('a')+(i-36))
       elseif (i.eq.62) then
        ib=','
       else
        ib='.'
       endif
       return
       end
C**********************************************************
C**********************************************************
       subroutine signtest(cspeci2,especi2,csi2)
 
       implicit none

       integer     csi2
       integer     especi2
       character*6 cspeci2
     
       csi2=csi2+1
     
       if (especi2.eq.0) then
          cspeci2='0+'
       else
        if (especi2.gt.0) then
          cspeci2(csi2:csi2)='+'
        else
          cspeci2(csi2:csi2)='-'
        endif
       endif
       return
       end    

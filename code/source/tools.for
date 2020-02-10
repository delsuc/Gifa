C This file is a part of the GIFA program
C
C     Authors :       M.A.Delsuc, and others
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C     then
C                     C.B.S. Fac de Pharmacy
C                     34000 Montpellier
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C
C this file contains a serie of  tools for string and I/O manipulations
C
c      subroutine getstring(s)
c      subroutine getreal(x)
c      subroutine getint(x)
c      function   alphanum(s)
c      subroutine lowercase(s,n)
c      subroutine uppercase(s,n)
c      subroutine powtwo(n,m)
c      subroutine trailing(st,l)
c      subroutine leading(st)
c      subroutine parsest(st,val,loc,job,err)
c      subroutine opengif(name,unit,err)
c      subroutine itohr - itopr - htoir - ptoir
c      subroutine currunitr(val,i,dd)
c   and real integer versions
c      subroutine t3dto2d(axis)
c      subroutine t3dto1d(axis)
c      subroutine t2dto1d(axis)
c      subroutine ttodata(dim)
c      subroutine tfromdata(dim)
c      subroutine checkoverflow
c      subroutine printit(file_name)
c      subroutine file_exist(name,err)
c      function closest(nbre)




      subroutine getstring(s)
c INOUT	: s
c
C   get a string from input
c basic I/O routine, prompts the user with s, no change if return
c
      implicit none
      character*256 st
      character*(*) s
      integer ll

      call trailing(s,ll)
      write(*,30) s(1:ll)
      read (*,20,err=10,end=5) st
      call cleanst(st,256)
      if (st.ne.' ') s=st
5     return
10    call gifaout('error in reading')
      st = ' '
      return
20    format(a256)
30    format(' ',a,'   ',$)
      end
c************************************************************
      subroutine getreal(x)
c INOUT	: x
c
C   get a real from input
c basic I/O routine, prompts the user with x, no change if return
c
      implicit none
      real x,xx
      character*40 st
      write(*,50) x
      read (*,40) st
      if (st.eq.' ') return
      read (st,*,err=10,end=20) xx
      x = xx
20    return
10    write(*,*) 'error in reading'
      x = 0.0
      return
30    format (bn,G20.8)
40    format (a40)
50    format (' ',g20.5,'   ',$)
      end

c************************************************************
      subroutine getint(x)
c INOUT	: x
c
C   get a int from input
c basic I/O routine, prompts the user with x, no change if return
c
C    accepts input such as 16k

      implicit none
      integer x,xx,i
      character*40 st
      write(*,50) x
      read (*,40) st
      if (st.eq.' ') return
      call uppercase(st,40)
      i = index(st,'K')
      if (i.ne.0) st(i:i) = ' '
      read (st,*,err=10,end=20) xx
      if (i.ne.0) xx = xx*1024
      x = xx
20    return
10    write(*,*) 'error in reading'
      x = 0
      return
30    format (bn,I20)
40    format (a40)
50    format (' ',i20,'   ',$)
      end

c************************************************************
      function alphanum(cc)
c IN	: cc
C    true if "cc" is alpha numeral or _
c
      implicit none
      logical alphanum
      character*1 cc

      alphanum = ( (lge(cc,'0') .and. lle(cc,'9')) .or.
     *             (lge(cc,'a') .and. lle(cc,'z')) .or.
     *             (lge(cc,'A') .and. lle(cc,'Z')) .or.
     *             ( cc .eq. '_')    )
      return
      end

c************************************************************
      function isalphanum(st,n)
c IN	: st,n
C    true if st is a alpha numeral or _
c
      implicit none
      logical isalphanum
      integer n
      character*(*) st
      integer i
      logical bool, alphanum
      external alphanum

      do i = 1,n
        bool = (bool .and. alphanum(st(i:i)))
      enddo
      isalphanum = bool
      end

c************************************************************
      function isnumeral(st,n)
c IN	: st,n
C    true if st is a numeral value
c
      implicit none
      logical isnumeral
      integer n
      character*(*) st

C nouvelle
      real*8 zz
      integer err

      err = 0
      call readval(st(1:n),zz,err)
      if (err.ne.0) then
         isnumeral = .false.
      else
         isnumeral = .true.
      endif
      return
c vieille version - a enlever !
c      character*256 stl
c      character*(1) cc
c      integer i,idot,ie,ip
c      logical bool
c
c      idot = 0
c      ie = 0
c      stl = st
c      call leading(stl)
cC remove leading +/-
c      cc = stl(1:1)
c      if (cc .eq. '+') then
c         stl = stl(2:n)
c         n = n-1
c      elseif (cc .eq. '-') then
c         stl = stl(2:n)
c         n = n-1
c      endif
c      i = 1
c      do while (i.le.n)
c        cc = stl(i:i)
c        write(*,*) cc
c        if (lge(cc,'0') .and. lle(cc,'9')) then
c          bool = .true.
c        elseif (cc .eq. '.') then
c          if (idot .eq. 0) then   ! only one dot
c             idot = 1
c          else
c             goto 100
c          endif
c        elseif (cc .eq. 'e' .or. cc .eq. 'E') then
c          if (idot .eq. 0) then   ! only one e/E
c             if (i.eq.n)   goto 100   ! cannot end with an E
c             idot = 1
c             cc = stl(i+1:i+1)
c             if (cc.eq.'+' .or. cc.eq.'-') i = i+1
c          else
c             goto 100
c          endif
c        endif
c        i = i + 1
c      enddo
c      isnumeral = .true.
c      return
c 100  isnumeral = .false. 
c      return
      end

c************************************************************
      subroutine lowercase(s,n)
c IN	: n
c INOUT	: s
C    put the string of char "s(n)" in lowercase
c
      implicit none
      integer n
      character*(*) s
      integer i
      character*1 chara,charca
      data chara/'a'/,charca/'A'/
      do i=1,n
         if ( LGE(s(i:i),'A') .and. LLE(s(i:i),'Z') )
     *       s(i:i) = char( ichar(s(i:i)) - ichar(charca)+ichar(chara) )
         if (LLT(s(i:i),' '))
     *       s(i:i) = ' '
      enddo
      return
      end
c************************************************************
      subroutine uppercase(s,n)
c IN	: n
c INOUT	: s
C    put the string of char "s(n)" in uppercase
c
      implicit none
      integer n
      character*(*) s
      integer i,is
      character*1 chara,charca,charz
      data chara/'a'/,charca/'A'/,charz/'z'/
      do i=1,n
         is = ichar(s(i:i))
         if ( is.ge.ichar(chara) .and. (is.le.ichar(charz)) ) then
            s(i:i) = char( is - ichar(chara)+ichar(charca) )
c         else if (is.lt.ichar(' ')) then
c            s(i:i) = ' '
         endif
c         if ( LGE(s(i:i),'a') .and. LLE(s(i:i),'z') ) then
c            s(i:i) = char( ichar(s(i:i)) - ichar('a')+ichar('A') )
c         else if (LLT(s(i:i),' ')) then
c            s(i:i) = ' '
c         endif
      enddo
      return
      end

c************************************************************
      subroutine cleanst(s,n)
c IN	: n
c INOUT	: s
c
C    clean the string  "s(n)" for non-printable char

      implicit none
      integer n
      character*(*) s
      integer i
      do i=1,n
        if (ichar(s(i:i)).lt.32) s(i:i) = ' '
        if (ichar(s(i:i)).eq.127) s(i:i) = ' '
      enddo
      return
      end



c************************************************************
      subroutine powtwo(n,m)
c IN	: n
c OUT	: m
c
C    Verify that n is power of two
C     m is the difference between n and next power of two
c i.e.  n=2**P+m with   0 <= m   and m minimum

      Implicit none
      integer n,m,j
      real nn

      nn = float(abs(n))
      j = 0
10    if (nn.gt.1.0) then
         nn = nn/2.0
         j = j+1
         goto 10
      endif
      m = 2**j - n
      return
      end
c************************************************************
      subroutine trailing(st,l)
c INOUT	: l
c IN   	: st
c
C  Removes trailing blanks at the end of string st of length l
C
      Implicit none
      integer l,i
      character*(*) st
      if (st.eq.' ') then
         i = 1
      else
         I = Len(St)
         do while (i.gt.1 .and. st(i:i).eq.' ')
            i = i-1
         enddo
      endif
20    l = i
      return
      end
c************************************************************
      subroutine leading(st)
c INOUT	: st
c
C  Removes leading blanks from the string st
C
      Implicit none
      integer l,i
      character*(*) st

      if (st.eq.' ') return
      l = len(st)
      i = 1
      do while(i.lt.l .and. st(i:i).eq.' ')
        i = i+1
      enddo
      if (i.ne.1) st = st(i:l)

      return
      end
c************************************************************
      subroutine bsort(x,size)
c INOUT	: x,size
c
C Performs a buble sort on a integer array of length size
C Removes duplicates (and changes size if so)
c
      implicit none
      integer size,x(size)
      integer i,j,a
      logical finished

1     finished = .true.
      do i=1,size-1
         if (x(i).eq.x(i+1)) then    ! removes duplicates here
            do j=i+1,size-1
              x(j) = x(j+1)
            enddo
            size = size-1
            goto 1    ! exit the loop
         elseif (x(i).gt.x(i+1)) then    ! sorts here
            a = x(i)
            x(i) = x(i+1)
            x(i+1) = a
            finished = .false.
         endif
      enddo
      if (.not.finished) goto 1
      return
      end

c------------------------------------------------------------
	subroutine file_exist(namep,error)
c
c  file_exist - check file.  Returns error=0 if file exists
c IN    : namep
c OUT   : error
      implicit none
      character*(*) namep     ! this is the file name
      integer       error,erl

	
        open(unit=96,file=namep,
#hp     *        readonly,
     *        iostat=erl,status='old')

      if (erl.eq.0) then 
	  error = 0	! found it
      else
          error = 1   	! not found
      endif
      close(unit=96)
      return
      end




c************************************************************
      subroutine opengif(namep,unumb,error)
c IN	: namep,unumb
c OUT	: error
c
c Opens the file called "namep" and connects it to "unumb"
c
C This subroutine is used to open the macro file.
C
C if the global variable PATH is empty, the old set-up is used :
C   First search the file in working directory,
C
C   then in $HOME/macro    (sys$login:[MACRO] in vms)
C
C   then in the standard place for gif files :
C    gifa$gif            on VMS
C    /usr/local/gifa/macro  on Unix
C
C else
C   search in all entries in PATH, (blank separated)
C
c in any case, do nothing if the file name starts with a / (UNIX)
C Author : Marc A.Delsuc
C rev date 6-dec-95
c************************************************************
      implicit none
      character*(*) namep     ! this is the file name
      integer      unumb,    ! unit to open
     *             error     ! non-zero on error condition.

#include "gifshellv.inc" 
      integer erl,i,j
      character*256 name,nameloc,nl,ploc

C first try here

      name = namep
C   VMS adds a .g extension if no extension present
#vms      if (index(name,'.').eq.0) then
#vms         call trailing(name,i)
#vms         name = namep(1:i) // '.g'
#vms      endif


      open(unit=unumb,file=name,
#vms     *        readonly,
#hp     *        readonly,
     *        iostat=erl,status='old')

      if (erl.eq.0) goto 100  ! found it

      if (path .eq. ' ') then        ! if no PATH, 
C then on HOME place
#_vms        call gethome(nl)
#_vms        call trailing(nl,i)
#_vms        nameloc = nl(1:i) // '/macro/' // name
#vms         nameloc = 'SYS$LOGIN:[macro]' // name

        call trailing(nameloc,i)
        open(unit=unumb,file=nameloc(1:i),
#vms     *        readonly,
#hp     *        readonly,
     *        iostat=erl,status='old')

        if (erl.eq.0) goto 100  ! found it

C then on standard place.
#vms        nameloc='gifa$macro:'//name
#_vms        nameloc='/usr/local/gifa/macro/'//name
      
        call trailing(nameloc,i)
        open(unit=unumb,file=nameloc(1:i),
#vms     *        readonly,
#hp     *        readonly,
     *        iostat=erl,status='old')

        if (erl.eq.0) goto 100  ! found it

      else                ! path != ' '
        ploc = path
        call leading(ploc) 
        dowhile (ploc .ne. ' ')
           j = index(ploc,' ')
           nameloc = ploc(1:j-1) // '/' // name
           call trailing(nameloc,i)
c         write (*,*) nameloc(1:i)
           open(unit=unumb,file=nameloc(1:i),
#vms     *        readonly,
#hp     *        readonly,
     *        iostat=erl,status='old')
           if (erl.eq.0) goto 100  ! found it
           ploc = ploc(j+1:256)
           call leading(ploc) 
        enddo
      endif


      if (erl.eq.0) goto 100  ! found it

      error = 1    ! not found
      return

100   error = 0
      return
      end

c************************************************************
      subroutine t3dto2d(axis)
c IN	: axis
c
c transfers the descriptors (held into paramfilev) from 3D
c data to 2D data, depending on axis (1, 2 or 3)
c

      implicit none

#include "paramfilev.inc"
#include "zoomparamv.inc"
      integer axis
           if (axis.eq.1) then
                 itype = mod(it3d,4)
                 specw1 = sp3d2
                 specw2 = sp3d3
                 offset1 = of3d2
                 offset2 = of3d3
                 zolf2 = zo3df3l
                 zour2 = zo3df3u
                 zolf1 = zo3df2l
                 zour1 = zo3df2u
                 freq1 = freq3d2
                 freq2 = freq3d3
           else if (axis.eq.2) then
                 itype = 2*(it3d/4) + mod(it3d,2)
                 specw1 = sp3d1
                 specw2 = sp3d3
                 offset1 = of3d1
                 offset2 = of3d3
                 zolf2 = zo3df3l
                 zour2 = zo3df3u
                 zolf1 = zo3df1l
                 zour1 = zo3df1u
                 freq1 = freq3d1
                 freq2 = freq3d3
           else if (axis.eq.3) then
                 itype = it3d/2
                 specw1 = sp3d1
                 specw2 = sp3d2
                 offset1 = of3d1
                 offset2 = of3d2
                 zolf2 = zo3df2l
                 zour2 = zo3df2u
                 zolf1 = zo3df1l
                 zour1 = zo3df1u
                 freq1 = freq3d1
                 freq2 = freq3d2
           endif
      return
      end
c************************************************************
      subroutine t3dto1d(axis)
c IN	: axis
c
c transfers the descriptors (held into paramfilev) from 3D
c data to 1D data, depending on axis (1, 2 or 3)

      implicit none

#include "zoomparamv.inc"
#include "paramfilev.inc"
#include "unitextern.inc"
      integer axis

          if (axis.eq.1) then
             if (checkdim(1,it3d,3)) then
                it1d = 1
             else
                it1d = 0
             endif
             sp1d = sp3d1
             of1d = of3d1
             zo1dl = zo3df1l
             zo1du = zo3df1u
             freq1d = freq3d1
          elseif (axis.eq.2) then
             if (checkdim(2,it3d,3)) then
                it1d = 1
             else
                it1d = 0
             endif

             sp1d = sp3d2
             of1d = of3d2
             zo1dl = zo3df2l
             zo1du = zo3df2u
             freq1d = freq3d2
          elseif (axis.eq.3) then
             if (checkdim(4,it3d,3)) then
                it1d = 1
             else
                it1d = 0
             endif
             sp1d = sp3d3
             of1d = of3d3
             zo1dl = zo3df3l
             zo1du = zo3df3u
             freq1d = freq3d3
          endif
      return
      end
      
c************************************************************
      subroutine t2dto1d(axis)
c IN	: axis
c
c transfers the descriptors (held into paramfilev) from 2D
c data to 1D data, depending on axis (1, or 2)

      implicit none
#include "zoomparamv.inc"
#include "paramfilev.inc"
      integer axis

      if (axis.eq.2) then
           it1d = mod(itype,2)
           sp1d = specw2
           of1d = offset2                   
           zo1dl = zolf2
           zo1du = zour2
           freq1d=freq2
      elseif(axis.eq.1)   then         
           it1d = itype/2
           sp1d = specw1
           of1d = offset1
           zo1dl = zolf1
           zo1du = zour1
           freq1d=freq1
      endif
      return
      end
      
c************************************************************
      subroutine ttodata(dimd)
c IN	: dimd
c SIDE  : on data parameters
c
c transfers the descriptors (held into paramfilev) from the spectral
c data (described by dimd) to the data buffer

      implicit none
#include "zoomparamv.inc"
#include "paramfilev.inc"
#include "paramdatav.inc"
      integer dimd

      freqdat = frequency
      dimdat = dimd
      emptydata = .false.
      if (dimd.eq.1) then
           sizedata = sizeimage1d
           si1dat =  sizeimage1d
           itdata = it1d
           sp1dat=sp1d
           of1dat=of1d
           freq1dat=freq1d
      elseif(dimd.eq.2)   then         
           sizedata = si1im*si2im
           si1dat = si1im
           si2dat = si2im
           itdata=itype
           sp1dat=specw1
           of1dat=offset1
           freq1dat=freq1
           sp2dat=specw2
           of2dat=offset2
           freq2dat=freq2
      elseif(dimd.eq.3)   then
           sizedata = si3d1*si3d2*si3d3
           si1dat = si3d1
           si2dat = si3d2
           si3dat = si3d3
           itdata = it3d    
           freq1dat = freq3d1
           freq2dat = freq3d2
           freq3dat = freq3d3
           sp1dat = sp3d1
           sp2dat = sp3d2
           sp3dat = sp3d3
           of1dat = of3d1
           of2dat = of3d2
           of3dat = of3d3
      endif
      return
      end
c************************************************************
      subroutine tfromdata(dimd)
c IN	: dimd
c SIDE  : on data parameters
c
c transfers the descriptors (held into paramfilev) from the spectral
c data (described by dimd) to the data buffer

      implicit none
#include "zoomparamv.inc"
#include "paramfilev.inc"
#include "paramdatav.inc"
      integer dimd

      if (dimd.eq.1) then
           sizeimage1d = si1dat
           it1d = itdata
           sp1d = sp1dat
           of1d = of1dat
           freq1d = freq1dat
      elseif(dimd.eq.2)   then         
           si1im = si1dat
           si2im = si2dat
           itype = itdata
           specw1 = sp1dat
           offset1 = of1dat
           freq1 = freq1dat
           specw2 = sp2dat
           offset2 = of2dat
           freq2 = freq2dat
      elseif(dimd.eq.3)   then
           si3d1 = si1dat
           si3d2 = si2dat
           si3d3 = si3dat
           it3d = itdata
           freq3d1 = freq1dat
           freq3d2 = freq2dat
           freq3d3 = freq3dat
           sp3d1 = sp1dat
           sp3d2 = sp2dat
           sp3d3 = sp3dat
           of3d1 = of1dat
           of3d2 = of2dat
           of3d3 = of3dat
      endif
      return
      end

c************************************************************
      
      subroutine parsest(st,val,loc,job,err)
c IN	: st,job
c OUT	: val,loc,err
c
c the string parser
c
C parsest will parse a string for :
C o isolated words
C o quotted string ('xxx' or "xxx")
C o GIFA variables
C o (expressions)
C
C val will return the value, loc the position of the next char in st
C
C if job = 1 will only locate next word
C if job = 2 will parse and return in val
c************************************************************
      implicit none
#include "gifshellv.inc"

      character*(*) st,val
      integer loc,job,err

      character*(256) st2parse ,var,st1,st2
      integer i,j,k,para,off

      if (job.ne.1 .and. job.ne.2) goto 130
      st2parse = st
      call leading(st2parse)
      off = index(st,st2parse(1:1)) - 1
C first variables
      if (st2parse(1:1).eq.'$') then
         i = index(st2parse,' ')
         if (job.eq.2) then
            var = st2parse(1:i-1)
            call uppercase(var,i-1)
            j = index(var,'[')
            if (j.ne.0) then			! array
              k = index(var,']')
              if (j+1.gt.k-1) goto 100
              st2 = var(j+1:k-1)
              call eval(st2,st1,err)
              st2 = var(1:j) // st1
              call trailing(st2,j)
              var = st2(1:j) // ']'
            endif
            call getvar(var,vcontext,val,err)
            if (err.ne.0) goto 100
         endif
C expression
      elseif (st2parse(1:1).eq.'(') then
         para = 1
         i = 1
10         i = i +1			! count parentheses
           if (st2parse(i:i).eq.'(' .or. st2parse(i:i).eq.'[') then
             para = para + 1
           elseif (st2parse(i:i).eq.')' .or. st2parse(i:i).eq.']') then
             para = para - 1
           elseif (st2parse(i:i).eq.'''') then	! careful with quotted
               j = index(st2parse(i+1:256),'''')	! within ( )
               if (j.lt.1) goto 120
               i = j+i
           elseif (st2parse(i:i).eq.'"') then
               j = index(st2parse(i+1:256),'"')
               if (j.lt.1) goto 120
               i = j+i
           endif
           if (para.ne.0 .and. i.lt.256) goto 10
         if (para.ne.0) goto 110
         if (job.eq.2) then
             val = st2parse(1:i)
C this call would need fortran to be recursive
C            call eval(val,val,err)
C            if (err.ne.0) goto 100
          endif
          i = i+1
C quotted string
      elseif (st2parse(1:1).eq.'''') then
          i=index(st2parse(2:256),'''')
          if (i.lt.2) goto 120
          if (job.eq.2) val = st2parse(2:i)
          i=i+2
      elseif (st2parse(1:1).eq.'"') then
          i=index(st2parse(2:256),'"')
          if (i.lt.2) goto 120
          if (job.eq.2) val = st2parse(2:i)
          i=i+2
C all other cases (isolated word)
      else
c           i=index(st2parse,' ')
c           j=index(st2parse,char(9))
c           if (j.ne.0)  i = min(i,j)
           i=1
           do while (st2parse(i:i).ne.' ' .and.
     *               st2parse(i:i).ne.char(9))
              i = i+1
           enddo
          if (job.eq.2) val = st2parse(1:i-1)
      endif
      loc = i+off
      return

100   call gifaout('Variable not found '//var)
      err = 1
      return
110   call gifaout('Error in getting expression')
      err=1
      return
120   call gifaout('Error in getting quotted string')
      err=1
      return
130   call gifaout('Error in parsest()')
      err=1
      return
      end

c************************************************************
      subroutine checkoverflow(size,not_ok,fdata)
c IN	: size,fdata
c OUT	: not_ok
c
C check if, when using the upper memory, starting at size (counted from
C starting point of image), any buffer will be overflowed
C if fdata=1, datab will also be checked
C not_ok will be true if not Ok.
C If Ok, data will be clipped
c 
      implicit none

      logical not_ok
      integer size,err,fdata
#include "sizeparam.inc"
#include "paramfilev.inc"
#include "paramdatav.inc"
#include "gifshellv.inc"
      character*(10) st
      logical bool1

      not_ok = .false.
C first check
      bool1 = .false.
      if (sizeimage3d.gt.size) then
          call gifaout('This overflows the 3D area')
          bool1 = .true.
      endif
      if (sizeimage.gt.size+smxmax) then
          call gifaout('This overflows the 2D area')
          bool1 = .true.
      endif
      if (sizeimage1d.gt.sizemax+size+smxmax) then
          call gifaout('This overflows the 1D area')
          bool1 = .true.
      endif
      if (sizedata.gt.size-4*smxmax .and.fdata.eq.1 
     *    .and. (.not.emptydata)) then
          call gifaout('This overflows the "DATA" area')
          bool1 = .true.
      endif
      if (bool1 .and. input.eq.20) then
          call message('=> Ok to overflow ?')
          st = 'yes'
          call getbool2(st,'memory',err)
          call uppercase(st,len(st))
          not_ok = (st.ne.'YES')
      endif
C then clip
      if (bool1 .and. .not.not_ok) then
         if (sizeimage3d.gt.size) 
     *       si3d1 = size/(si3d2*si3d3)
         if (sizeimage.gt.size+smxmax)
     *       si2im = (size+smxmax)/si2im
         if (sizeimage1d.gt.sizemax+size+smxmax)
     *       sizeimage1d = sizemax+size+smxmax
         if (sizedata.gt.size-4*smxmax .and.fdata.eq.1) then
             if (dimdat.eq.1) then
                sizedata = size-4*smxmax
             elseif (dimdat.eq.2) then
                si1dat = (size-4*smxmax)/si2dat
                sizedata = si1dat*si2dat
             elseif (dimdat.eq.3) then
                si1dat = (size-4*smxmax)/(si2dat*si3dat)
                sizedata = si1dat*si2dat*si3dat
             endif
         endif
      endif
      return
      end

c************************************************************
      subroutine checkunderflow(size,not_ok)
c IN	: size
c OUT	: not_ok
c
C check if, when increasing the memory of the current data-set, up to size
C any buffer will be overflowed
C if fdata=1, datab will also be checked
C not_ok will be true if not Ok.
C If Ok, data will be clipped
c
c will be trigged only if data size is increasing!
c
      implicit none

      logical not_ok
      integer size,err
#include "sizeparam.inc"
#include "paramfilev.inc"
#include "paramdatav.inc"
#include "gifshellv.inc"
      character*(10) st
      logical bool1,booldata

      not_ok = .false.
C first check
      bool1 = .false.
      booldata = .false.
      if (dim.eq.1 .and. size.gt.sizeimage1d) then
        if (size.gt.sizemax) then
          call gifaout('This overflows the 2D area')
          bool1 = .true.
        endif
        if (size.gt.sizemax+smxmax) then
          call gifaout('This overflows the 3D area')
          bool1 = .true.
        endif
        if (size.gt.sizemax+5*smxmax .and. .not. emptydata) then
          call gifaout('This overflows the "DATA" area')
          bool1 = .true.
          booldata = .true.
        endif
      elseif (dim.eq.2 .and. size.gt.si1im*si2im) then
        if (size.gt.smxmax) then
          call gifaout('This overflows the 3D area')
          bool1 = .true.
        endif
        if (size.gt.5*smxmax .and. .not. emptydata) then
          call gifaout('This overflows the "DATA" area')
          bool1 = .true.
          booldata = .true.
        endif
      elseif (dim.eq.3 .and. size.gt.si3d1*si3d2*si3d3) then
        if (size.gt.4*smxmax .and. .not. emptydata) then
          call gifaout('This overflows the "DATA" area')
          bool1 = .true.
          booldata = .true.
        endif
      endif

      if (bool1 .and. input.eq.20) then
          call message('=> Ok to overflow ?')
          st = 'yes'
          call getbool2(st,'memory',err)
          call uppercase(st,len(st))
          not_ok = (st.ne.'YES')
      endif

      if (booldata) emptydata = .true.

      return
      end

c************************************************************
      subroutine printit(file_name)
c IN	: file_name
c
c send the file file_name to the printer through the shell script
c gifaprint
c
      implicit none
      character*(*) file_name
      character*(100) st,name
      integer l

      call trailing(file_name,l)
      name = file_name(1:l)
#_vms      st = 'gifaprint '//name//' &'
#vms      st = 'gifaprint '//name
      call shell(st)
      return
      end

c************************************************************
      function closest(nbre)
c IN	: nbre
C  return the integer greater then 1, and nearest to the real nbre
c
      implicit none
      integer closest,tmp
      real nbre

c      tmp = int(nbre)
c      if (tmp.le.0) then
c         closest = 1
c      elseif (abs(tmp-nbre).le.0.5) then
c         closest = tmp
c      else
c         closest = tmp + 1
c      endif
C modif MAD
      tmp = int(nbre+0.5)
      closest = max(tmp,1)
      return
      end
c************************************************************
      function dclosest(nbre)
c IN	: nbre
C  return the integer greater then 1, and nearest to the real nbre
c
      implicit none
      integer dclosest,tmp
      real*8 nbre

c      tmp = int(nbre)
c      if (tmp.le.0) then
c         closest = 1
c      elseif (abs(tmp-nbre).le.0.5) then
c         closest = tmp
c      else
c         closest = tmp + 1
c      endif
C modif MAD
      tmp = int(nbre+0.5)
      dclosest = max(tmp,1)
      return
      end


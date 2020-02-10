
C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Authors :       M.A.Delsuc
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C
C This files holds the subroutines for reading the input stream.
C getinput   uses showprompt
C getstring2
C  will read either word 'quotted string' "quotted string" $variable
C                   (arithmetique_exp) $_ <filename
C            ( filename can be any of the other syntaxe )
C
C all the others are based on getstring2 :
C getreal2
C getint2
C getbool2
C getcoord permits in index, ppm or hertz
C getout2 getfreq2 getrect getcube
C getdim permits input such as
C F1 F2 F3 F12 F13 F23 F123
C (chkborder message gifaout)
C **************************************************************
      subroutine getinput(ppt,err)
c IN	: ppt
c OUT	: error
c SIDE	: param
c get direct input for gifa, either user (input=20; stdin) or
c in a file (unit>20).
c ppt == 0 prompted
c ppt == 1 if continuation line
c ppt == 2 no prompt
      implicit none
#include "gifshellv.inc"
#include "gifaout.inc"
      integer ppt,err,l
      if (input.eq.20) then

C these lines (no READLINE)
#_readline             call showprompt(ppt)
#_readline#sgi         call system('stty -isig')
#_readline#f2c         read (*,fmt=10,iostat=err,end=199,err=198) param
#_readline#_f2c         read (*,fmt=10,iostat=err) param
#_readline#sgi         call system('stty isig')

C or these one (READLINE)
#readline             call rdline(ppt,param)


      else
         read (unit=input,fmt=10,iostat=err,end=199) param
         currline(input-20) = currline(input-20)+1
      endif
      call cleanst(param,len(param))
c echo in journal file if input = 20 (or if verbose mode)
      call trailing(param,l)
      if (input.eq.20 .or. verbose.ne.0) write(journal,*) param(1:l)
      if (verbose.ne.0) write(*,*) param(1:l)
      return
198   err=1
c csgi      close(5)
c csgi      open(5,file='SYS$INPUT')
      return
199   err = 10
      call trailing(param,l)
      if (input.eq.20 .or. verbose.eq.1) write(journal,*) param(1:l)
      if (verbose.eq.1) write(*,*) param(1:l)         
      return
10    format(A256)
      end


      subroutine ttshowprompt(ppt)
c IN	: ppt
c show the prompt
c
c ppt == 0 show prompt
c ppt == 1 prompt with continuation line
c ppt == 2 no prompt
c
c NOT USED !, use showprompt instead
c
      implicit none
      integer ppt

      if (ppt.eq.0) then
        write(*,11)
      elseif (ppt.eq.1) then
        write(*,12)
      endif
      return
#vms 11    format('$Gifa>')
#aix 11    format('Gifa>',$)
#alliant 11    format(' Gifa>',$)
#adec 11    format(' Gifa>',$)
#sgi 11    format('Gifa>',$)
#hp11    format('Gifa>',$)
#sun11    format('Gifa>',$)
#f2c 11    format('Gifa>',$)
#vms 12    format('$\Gifa>')
#aix 12    format('\\Gifa>',$)
#alliant 12    format(' \Gifa>',$)
#adec 12    format(' \\Gifa>',$)
#sgi 12    format('\\Gifa>',$)
#sun 12    format('\\Gifa>',$)
#hp12    format('\Gifa>',$)
#f2c 12    format('\\Gifa>',$)
      end

C **************************************************************
      subroutine getstring2(st,err)
c INOUT	: st
c OUT	: err
c SIDE	: param
c
C  will read either word 'quotted string' "quotted string" $variable
C                   (arithmetic_exp) $_ <filename  %  %%
C            ( filename can be any of the other syntaxes )
c the user is prompted for the initial value of st, a return will let 
c st unchanged
c Reads in the input stream (either file or interactive)
c if the line is finished on file, redirected to the user
c

      implicit none
#include "gifshellv.inc"
#include "controlc.inc"
#include "gifaout.inc"
#include "gifabcom.inc"

      character*(*) st
      integer i,err,stack,fromfile,un,lm
      logical test
      character*256 st1,parloc(8),stl1
      character*(msgsiz) mmm

      err = 0
      stack = 0
      fromfile = 0

      call leading(param)
 20   if ( param.eq.' ' .or.	               ! empty, prompt the user
     *    (param(1:3).eq.'$_ ' .and. input.eq.20)) then
         if (imsg.eq.0) call message( 'Enter Value :')
         if (on_graph .eq. 1) then
            stl1 = 'Gifa'
            mmm=msg(imsg)
            call trailing(mmm,lm)
            call trailing(st,i)
            call f_saisie(stl1,4,mmm,lm,st,i,err)
         else
           do i =1,imsg
              mmm=msg(i)
              call trailing(mmm,lm)
              call gifaout(mmm(1:lm))
           enddo
           call getstring(st)
           call trailing(st,i)
         endif
         write(journal,*) st(1:i)
         if (err.ne.0) goto 999
         if (param(1:3).eq.'$_ ' .and. input.eq.20)  param = param(3:)
         st1 = st(1:i) // param
         param = st1
      endif
      call leading(param)

C \ or \\ depends on the machine
#sgi      if (param(1:1).eq.'\\') then	! continuation line
#aix      if (param(1:1).eq.'\\') then	! continuation line
#adec      if (param(1:1).eq.'\\') then	! continuation line
#sun      if (param(1:1).eq.'\\') then	! continuation line
#alliant      if (param(1:1).eq.'\') then	! continuation line
#vms      if (param(1:1).eq.'\') then	! continuation line
#hp      if (param(1:1).eq.'\') then       ! continuation line
#f2c      if (param(1:1).eq.'\\') then       ! continuation line
         call getinput(1,err)
         if (err.ne.0) return
         goto 20
      endif

C first special char.

C pop parameter
      if (param(1:3).eq.'$_ ') then
         if (input.gt.20) then
            stack = stack+1
            param = param(3:)
            parloc(stack) = param			! save param
            param = buffer(input-20)
            input = input-1
            vcontext = input
            if (input.eq.20) vcontext = savecontext
            goto 20			! and redo (should be recursive)
         else
            st = ' '
            err = 1
            call gifaerr('Parameters not available')
            goto 999
         endif
      endif

C from file
      if (param(1:1).eq.'<' .and. param(2:2) .ne. '=' 
     *        .and. param(2:2) .ne. ' ' ) then
         fromfile = 1
         param = param(2:256)
c         goto 20
      endif

C then parse
      st1 = st
      call parsest(param,st1,i,2,err)
      if (err.ne.0) goto 100

C then special cases

      if (param(1:1).eq.'(') then
          call eval(st1,st,err)
          if (err.ne.0) goto 100
      elseif (st1(1:2).ne.'%%' .and. st1(1:2).ne.'% ') then
           st = st1
      endif

      if (st1.ne.'%%') param = param(i:256)

C      call leading(st)

      if (stack.gt.0) then      ! if was pop parameter
        do i=stack,1,-1
          input = input+1
          vcontext = input
          buffer(input-20) = param
          param = parloc(i)
        enddo
      endif

      if (fromfile.gt.0) then       ! was <name
         inquire(file=st,opened=test ,number=un)
         if (.not.test) then 
            st1 = st
            call gifaout('*** File not opened : '//st1)
            goto 100
         endif
         read(un,'(a256)',iostat=err,end=110) st
         if (err.ne.0) goto 100
      endif
      goto 999

110   inpeof(un-29) = 1           ! if eof on input
      st = ' '
      if (control.eq.1) then
        err=1                     ! ^C is an error condition
      else
        err=0
      endif
      goto 999

100   param = ' '
      err = 1
      st =  ' '
      if (stack.gt.0) then      ! if was pop parameter
        do i=stack,1,-1
          input = input+1
          vcontext = input
          buffer(input-20) = param
          param = parloc(i)
        enddo
      endif
      goto 999

 999  if (cleanable .eq. 0 ) imsg = 0   ! see message()
      return
      end

C **************************************************************
      subroutine getreal2(x,err)
c INOUT	: x
c OUT	: err
c SIDE	: param
c
c reads a real value, based on getstring2
c
c the user is prompted for the initial value of x, a return will let 
c x unchanged
c Reads in the input stream (either file or interactive)
c if the line is finished on file, redirected to the user
c
      implicit none

      real x
      real*8 xx
      integer i,err
      character*256 st

      write(st,*) x
      call trailing(st,i)
      call leading(st)
      call getstring2(st,err)
      if (err.ne.0) goto 10

      call readval(st,xx,err)
      if (err.ne.0) goto 10

      x = xx
20    return

10    call gifaout('*** error in reading')
      x = 0.0
      return
      end

C **************************************************************
      subroutine getint2(x,err)
c INOUT	: x
c OUT	: err
c SIDE	: param
c
c reads a integer value, based on getstring2
c
c the user is prompted for the initial value of x, a return will let 
c x unchanged
c Reads in the input stream (either file or interactive)
c if the line is finished on file, redirected to the user
c
      implicit none
      integer x
      integer i,err
      real*8 xx
      character*256 st

      write(st,*) x
      call trailing(st,i)
      call leading(st)
      call getstring2(st,err)
      if (err.ne.0) goto 10

      call readval(st,xx,err)
      if (err.ne.0) goto 10

      x = xx
20    return

10    call gifaout('*** error in reading')
      x = 0
      return
      end

C **************************************************************
      subroutine getbool2(b,help_file,err)
c IN	: help_file
c INOUT	: b
c OUT	: err
c SIDE	: param
c
c reads a boolean value, based on getstring2
c
c the user is prompted for the initial value of b, a return will let 
c b unchanged
c legal entries are y (yes) n (no) c (cancel) h (help) the latter will 
c print "help_file"
c Reads in the input stream (either file or interactive)
c if the line is finished on file, redirected to the user
c
      implicit none
      character*(*) b,help_file
      integer err
      character*10 st

c old code  cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c												     c
c	    st = b										     c
c												     c
c   10    call getstring2(st,err)							     c
c	    if (err.ne.0)	goto 20							     c
c	    call uppercase(st,10)							     c
c	    if (st(1:1).eq.'Y')	then							     c
c		 b = 'yes'									     c
c	    elseif (st(1:1).eq.'N') then						     c
c		 b = 'no'									     c
c	    elseif (st(1:1).eq.'C') then						     c
c		 b = 'cancel'								     c
c		 err = 1									     c
c	    else										     c
c		 write(*,*)	'Please answer with y(es) / n(o) / c(ancel) / ?	:'   c
c		 st =	'yes'									     c
c		 goto	10									     c
c	    endif										     c
c												     c
c								 c
c   20    call gifaout('*** error in reading')	 c
c	    b	= 'error'					 c
c	    err = 1						 c
c	    return						 c
c								 c
cccccccccccccccccccccccccccccccccccccccccccccccccc
     
      call getenum2(b,'yes no cancel ?',err)
      return

      end

C **************************************************************
      subroutine getenum2(st,list,err)
c IN	: list
c INOUT	: st
c OUT	: err
c SIDE	: param
c
c reads a string value, which value is chosen in list (blank separated).
c based on getstring2
c
c test are case insensitive - value is returned in uppercase !
c
c limited to 32 values of length 32 !!!
c
      implicit none
      character*(*) st,list
      integer err
      integer i,j,nenum
      character*256 stl,llist
      character*32 enum(32),  enl

      llist = list
      call uppercase(llist,len(llist))
      
      nenum=0
      call leading(llist)
      i = index(llist,' ')
      do while (i.gt.1 .and. nenum .lt. 32)
        nenum=nenum+1
        enum(nenum) = llist(1:min(i,32))
        llist = llist(i:len(llist))
        call leading(llist)
        i = index(llist,' ')
      enddo
      
      stl = enum(1)
      call getstring2(stl,err)
      if (err.ne.0) goto 20
      call uppercase(stl,10)
      call leading(stl)
      i = index(stl,' ')
      do j=1,nenum
         enl = enum(j)
         if (stl(1:i-1) .eq. enl(1:i-1)) goto 30
      enddo
c not found
      llist = 'Please choose value in the list : ( ' // list // ' )'
      call gifaout(llist)
      goto 20

c found
30    st = enum(j)
      return

20    st = 'error'
      err = 1
      return
      end

C **************************************************************
      subroutine getfreq2(x,sw,err)
c IN	: sw
c INOUT	: x
c OUT	: err
c SIDE	: param
c
c reads a frequency value, based on getstring2
c
c the user is prompted for the initial value of x, a return will let 
c x unchanged
c Reads in the input stream (either file or interactive)
c if the line is finished on file, redirected to the user
c

      implicit none
#include "constant.inc"

      real x,xx,sw
      integer err

      if (sw.eq.0.0) then
        call gifaerr( 'Spectral width should not be null')
        goto 10
      endif
      xx = x*sw/(2000.0*pi)
      call getreal2(xx,err)
      if (err.ne.0) goto 10
c test removes. you may want to have    EM -10 !
c      if (xx .lt.0.0) then
c        call gifaerr('Width should not be negative')
c        goto 10
c      endif
      x = xx*(2000.0*pi)/sw
      return
10    x = 2000.0*pi
      err = 1
      return
      end
C **************************************************************
      subroutine getunit2(unit,err)
c INOUT	: unit
c OUT	: err
c SIDE	: param
c
c reads a unit value, based on getstring2
c
c the user is prompted for the initial value of unit, a return will let 
c unit unchanged
c Reads in the input stream (either file or interactive)
c if the line is finished on file, redirected to the user
c legal entries are (h (Hz) p (ppm) i (index) s (seconds)
c
      implicit none
      character*(*) unit
      character*20 st
      integer err

      call message(
     *'Enter Unit : h(z) p(pm) i(ndex) s(econd) d(amping) t(ab)')

      st = unit
      call getstring2(st,err)
      call uppercase(st,20)

      if (st.eq.'I') then
        st = 'INDEX'
      elseif (st.eq.'P') then
        st = 'PPM'
      elseif (st.eq.'H') then
        st = 'HZ'
      elseif (st.eq.'S') then
        st = 'SECOND'
      elseif (st.eq.'D') then
        st = 'DAMPING'
      elseif (st.eq.'T') then
        st = 'TAB'
      endif
      if (     st.ne.'INDEX'   .and. st.ne.'PPM'
     +   .and. st.ne.'HZ'      .and. st.ne.'SECOND'
     +   .and. st.ne.'DAMPING' .and. st.ne.'TAB') then
         call gifaout('*** Error in unit type')
         err = 1
      else
         unit = st
      endif
      return
      end

C **************************************************************
      subroutine getcoord(coordi,dd,err)
c INOUT	: coordi
c IN	: dd
c OUT	: err
c SIDE	: param
c
c getcoor is the integer version of getcoorf
c (i.e. coordi is integer instead of rel)
      implicit none
      integer err,dd
      integer coordi
      real coord

      coord = float(coordi)
      call getcoordf(coord,dd,err)
      coordi = int(coord+0.5)
      return
      end

C **************************************************************
      subroutine getcoordf(coord,dd,err)
c INOUT	: coord
c IN	: dd
c OUT	: err
c SIDE	: param
c
c reads a coordinate on the spectrum value, based on getstring2
c
C coord is the return value in index, dd is the axis to use
C entry of the form
c  123 or 123i for index;   123h 123p 123s for hertz ppm and second

      implicit none
      integer err,dd
      real coord
#include "zoomparamv.inc"
#include "paramfilev.inc"
#include "unitextern.inc"
      character*(256) st,lunit
      real val,specw,off,freq
      real*8 valdp
      integer i,size

C set up for this axis
c      call getreal2(coord,err)
c      return

      if (dim.eq.1) then
        freq = freq1d
        specw = sp1d
        off = of1d
        size = sizeimage1d
      elseif (dim.eq.2) then
        if (dd.eq.1) then
          freq = freq1
          specw = specw1
          off = offset1
          size = si1im
        elseif (dd.eq.2) then
          freq = freq2
          specw = specw2
          off = offset2
          size = si2im
        endif
      else
        if (dd.eq.1) then
          freq = freq3d1
          specw = sp3d1
          off = of3d1
          size = si3d1
        elseif (dd.eq.2) then
          freq = freq3d2
          specw = sp3d2
          off = of3d2
          size = si3d2
        elseif (dd.eq.3) then
          freq = freq3d3
          specw = sp3d3
          off = of3d3
          size = si3d3
        endif
      endif

C uncomment the following to get prompted in 'unit'
C ask the user
C get passed value in current unit
c      if (unit.eq.'INDEX') then
        val = coord
c      elseif (unit.eq.'HZ') then
c        val = itohr(coord,size,specw,off)
c      elseif (unit.eq.'PPM') then
c        val = itopr(coord,size,specw,off,freq)
c      elseif (unit.eq.'SECOND') then
c        val = itosr(coord,specw)
c      else
c        val = 0.0
c      endif

      write(st,*) val
      call trailzero(st)
c      call trailing(st,i)
c      if (unit.ne.'INDEX') st(i+1:i+1) = unit(1:1)
      call getstring2(st,err)
      call uppercase(st,256)
      call trailing(st,i)

C parse the input
      if (st(i:i).eq.'H') then            ! hertz
         call readval(st(1:i-1),valdp,err)
         if (err.ne.0) return
         val = valdp
         coord = htoir(val,size,specw,off)
      elseif (st(i:i).eq.'P') then        ! ppm
         call readval(st(1:i-1),valdp,err)
         if (err.ne.0) return
         val = valdp
         coord = ptoir(val,size,specw,off,freq)
      elseif (st(i:i).eq.'S') then        ! second
         call readval(st(1:i-1),valdp,err)
         if (err.ne.0) return
         val = valdp
         coord = stoir(val,specw)
      elseif (st(i:i).eq.'I') then        ! index
         call readval(st(1:i-1),valdp,err)
         if (err.ne.0) return
         val = valdp
         coord = val
      else
         call readval(st(1:i),valdp,err)
         if (err.ne.0) return
         val = valdp
         coord = val
      endif

c      write(*,*) val, coord

      return
      end        

C **************************************************************
      subroutine getout2(st,err)
c INOUT	: st
c OUT	: err
c SIDE	: param
c
c reads the name of an output device, based on getstring2
c
c legal entries are *S (screen) *PR (printer) *PL (plotter) or file_name
c
      implicit none
      character*256 st,stl
      integer err

#include "gifshellv.inc"

      call message
     *  ('Enter *S(creen), *PR(inter), *PL(otter) or file_name')
      call getstring2(st,err)
      stl = st
      call uppercase(stl,3)
      if (stl(1:2).eq.'*S') then
          st = '*SCREEN'
      else if (stl(1:3).eq.'*PR') then
          st = '*PRINTER'
      else if (stl(1:3).eq.'*PL') then
          st = '*PLOTTER'
      endif
      return
      end

C **************************************************************
      subroutine getdim(dd,err)
c INOUT	: dd
c OUT	: err
c SIDE	: param
c
c reads a dim axis, based on getstring2
c
c legal entries are F1 F2 F3 F12 F13 F23 F123
c
      implicit none

      character*256 st,stl
      integer dd,err

#include "gifshellv.inc"

      if (dd.eq.0) then
        st='NONE'
      elseif (dd.eq.1) then
        st='F1'
      elseif (dd.eq.2) then
        st = 'F2'
      elseif(dd.eq.3) then
        st='F12'
      elseif(dd.eq.4) then
        st='F3'
      elseif(dd.eq.5) then
        st='F13'
      elseif(dd.eq.6) then
        st='F23'
      elseif(dd.eq.7) then
        st='F123'
      else
        st='F1'
      endif
      call getstring2(st,err)
      stl = st
      call uppercase(stl,4)
      if (stl.eq.'NONE') then
            dd = 0
      elseif (stl.eq.'F1  ') then
            dd = 1
      elseif (stl.eq.'F2  ') then
            dd = 2
      elseif (stl.eq.'F12 ' .or. stl.eq.'F21 ') then
            dd = 3
      elseif (stl.eq.'F3  ') then
            dd = 4
      elseif (stl.eq.'F13 ' .or. stl.eq.'F31 ') then
            dd = 5
      elseif (stl.eq.'F23 ' .or. stl.eq.'F32 ') then
            dd = 6
      elseif (stl.eq.'F123' .or. stl.eq.'F132'
     *        .or. stl.eq.'F231'.or. stl.eq.'F213'
     *        .or. stl.eq.'F312' .or. stl.eq.'F321') then
            dd = 7
      else
            call gifaout('*** Error in reading')
            dd = 0
      endif
      return
      end
C **************************************************************
      subroutine getrect(lf2,lf1,ur2,ur1,err)
c INOUT	: lf2,lf1,ur2,ur1
c OUT	: err
C gets coordinates for a rectangle on the data set.
C uses the last clicked data if any available in clickt()
C lf2=lf2 lf1=lf1
      implicit none
#include "sizeparam.inc"
#include "paramfilev.inc"
#include "gifshellv.inc"
#include "zoomparamv.inc"

      integer lf2,ur2,lf1,ur1,it,err
      integer closest
      external closest

      if (dim.eq.1) it=it1d
      if (dim.eq.2) it=itype
      if (dim.eq.3) it=it3d

C   if one uses point, and has clicked on the surface,
C propose the clicked points, found in the clickt table.
      if (nclick.ge.2) then
            lf2 = closest(min(clickt(nclick-1,1),clickt(nclick,1)))
            lf1 = closest(min(clickt(nclick-1,2),clickt(nclick,2)))
            ur2 = closest(max(clickt(nclick-1,1),clickt(nclick,1)))
            ur1 = closest(max(clickt(nclick-1,2),clickt(nclick,2)))
            nclick = 0
C or the passed zoom coordinate
      else
        if (dim.eq.1) then
            lf2 = (min(lf2,sizeimage1d-2))
            ur2 = (min(ur2,sizeimage1d))
        elseif (dim.eq.2 .or. dim.eq.3) then
            lf2 = (min(lf2,si2im-2))
            lf1 = (min(lf1,si1im))
            ur2 = (min(ur2,si2im-2))
            ur1 = (min(ur1,si1im))
        endif
      endif
      if (dim.eq.1) then
        call message( 'enter left point')
        call getcoord(lf2,1,err)
        call message( 'enter right point')
        ur2=min(ur2,sizeimage1d)
        call getcoord(ur2,1,err)

C be careful with complex data sets
        if (it.eq.1 .and. mod(lf2,2).eq.0) lf2 = lf2+1
        if (it.eq.1 .and. mod(ur2,2).eq.1) ur2 = ur2+1
        if (lf2.le.0 .or. lf2.gt.sizeimage1d-1 .or.
     *      ur2.le.1 .or. ur2.gt.sizeimage1d .or.
     *      lf2.ge.ur2) goto 100

C dim.eq.2
      elseif (dim.eq.2 .or. dim.eq.3) then
C lower left point
         call message(
     *     'enter lower left corner coordinates (row,col)')
         call getcoord(lf1,1,err)
         call getcoord(lf2,2,err)
C upper right point
         ur2=min(ur2,si2im)
         ur1=min(ur1,si1im)
         call message(
     *        'enter upper right corner coordinates')
       call getcoord(ur1,1,err)
       call getcoord(ur2,2,err)

C be carefull with complex data
         call chkborder(lf1,lf2,ur1,ur2,it)

         if (lf1.le.0 .or. lf1.gt.si1im-1 .or.
     *       lf2.le.0 .or. lf2.gt.si2im-1 .or.
     *       ur1.le.1 .or. ur1.gt.si1im .or.
     *       ur2.le.1 .or. ur2.gt.si2im .or.
     *       lf1.ge.ur1 .or. lf2.ge.ur2)   goto 100
      endif
      return

100    call gifaout( '*** error with window')
       err = 1
       lf1 = 1
       lf2 = 1
       ur2 = si2im
       ur1 = si1im
       return
       end
C **************************************************************
       subroutine getcube(zz1l,zz2l,zz3l,zz1u,zz2u,zz3u,err)
c INOUT zz1l,zz2l,zz3l,zz1u,zz2u,zz3u,err
c get coordinates for a subcube in a 3D
c
       implicit none
       integer zz1l,zz2l,zz3l,zz1u,zz2u,zz3u,err
#include "paramfilev.inc"
       integer z1l,z2l,z3l,z1u,z2u,z3u
       integer it


       z1l = zz1l
       z2l = zz2l
       z3l = zz3l
       z1u = zz1u
       z2u = zz2u
       z3u = zz3u
       call message('Enter low field point coordinates (F1,F2,F3)')
       call getcoord(z1l,1,err)
       if (err.ne.0 .or. z1l.lt.1 .or. z1l.gt.si3d1) goto 100
       call getcoord(z2l,2,err)
       if (err.ne.0 .or. z2l.lt.1 .or. z2l.gt.si3d2) goto 100
       call getcoord(z3l,3,err)
       if (err.ne.0 .or. z3l.lt.1 .or. z3l.gt.si3d3) goto 100
       call message('Enter high field point coordinates (F1,F2,F3)')
       call getcoord(z1u,1,err)
       if (err.ne.0 .or. z1u.le.z1l .or. z1u.gt.si3d1) goto 100
       call getcoord(z2u,2,err)
       if (err.ne.0 .or. z2u.le.z2l .or. z2u.gt.si3d2) goto 100
       call getcoord(z3u,3,err)
       if (err.ne.0 .or. z3u.le.z3l .or. z3u.gt.si3d3) goto 100
       it = it3d/2
       call chkborder(z1l,z2l,z1u,z2u,it)
       it = mod(it3d,2)
       call chkborder(z1l,z3l,z1u,z3u,it)
       zz1l = z1l
       zz2l = z2l
       zz3l = z3l
       zz1u = z1u
       zz2u = z2u
       zz3u = z3u
 100   return
       end

C **************************************************************
       subroutine chkborder(lf1,lf2,ur1,ur2,it)
c IN	: it
c INOUT	: lf1,lf2,ur1,ur2
c
C this subroutine verify the border of a window on a 2D spectra,
C to be coherent with itype, i.e. begins on a real (odd) address, ends on
C imaginary (even) address (if complex in that direction)
       implicit none

#include "zoomparamv.inc"

       integer lf2,lf1,ur2,ur1,it

       if (it.eq.1 .or. it.eq.3) then
           lf2=(lf2/2)*2+1
           ur2=(ur2/2)*2
       endif
       if (it.eq.2 .or. it.eq.3) then
           lf1=(lf1/2)*2+1
           ur1=(ur1/2)*2
       endif
       return
       end

C **************************************************************
      subroutine message(text)
c IN	: text
c SIDE  : imsg, cleanable, msg(*)
c
c put in a string for reusal by getstring2() for user interface
C used for prompting in MAIN GIFA program...
c
c The message buffer is a set of char(imsgmax)
c the number of string to display in it is kept in imsg
c
c for the MESSAGE command, a strange set-up has been made :
c The buffer can be cleanable or not. (set uncleanable by SET)
c if cleanable ,
c     getstring2 empties the buffer (imsg=0)
c     message adds to the buffer (imsg++)
c if not cleanable (cleanable != 0)
c     getstring2 does nothing
c     message empties the buffer, and makes it cleanable
c
c cleanable is set from outside (for instance the MESSAGE command)


      implicit none

#include "gifshellv.inc"
#include "gifabcom.inc"
      character*(*) text

      if (cleanable .ne. 0) then
         imsg = 0
         cleanable = 0
      endif
      if (imsg .lt. imsgmax) then
         imsg = imsg+1
         msg(imsg)=text
      endif
      end
C **************************************************************
      subroutine jconnect(st,err)
c IN	: st
c OUT	: err
c
c creates a file called st as the new journaling file
c if an alternate journaling is aready opened, it is first closed
c
	  implicit none
      integer err
	  character*(*) st
	  logical bool
	  character*(256) stl
#include "gifaout.inc"

      stl = st
      inquire(3,opened=bool)
      if (bool) close(3)
      open(3,file=st,status='new',err=100)
      journal = 3
      call gifaout('Connecting journal file to '//stl)
      return
100   journal = 2
      err=1
      call gifaout('*** Unable to connect to '//stl)
      return
      end

C **************************************************************
      subroutine disjconnect(err)
c OUT	: err
c
c closes the alternate journaling file currently opened
c and returns to regular journaling
c
	  implicit none
      integer err
	  
#include "gifaout.inc"
      if (journal.ne.3) then
	     call gifaout('*** Not connected yet')
		 err = 1
	  else
	     close(3)
             journal = 2
             call gifaout('Back to gifa.log journal file')
             write(journal,*) 'disconnect'  ! needed to redo gifa.log
	  endif
      return
      end

C **************************************************************
      subroutine gifaout(a)
c IN	: a
c
C this subroutine permits to print a string on the same time on the screen
C and on the log file (unit journal), protect the line with a ";"

      implicit none
      character*(*) a
      integer i

#include "gifaout.inc"


      call trailing(a,i)
#_hp      write(*,*) a(1:i)
#hp       call gifaprint(a,i)
      write(journal,*) ';',a(1:i)
#hp       call gifaflush(fstream(journal))
      haswritten = 1		! used for prompting
      return
      end

C******************************************************************
      subroutine gifaoutr(tline,zz)
c IN	: tline,zz
c
C this subroutine permits to print a string followed by a real
C on the same time on the screen and on the log file (unit journal)

      real         zz
      character*(*) tline
	  character*256 st

      write(st,10) tline,zz
      call gifaout(st)
10    format(A,G15.8)
      return
      end
C******************************************************************
      subroutine gifaouti(tline,i)
c IN	: tline,i
c
C this subroutine permits to print a string followed by an integer
C on the same time on the screen and on the log file (unit journal)

      integer      i
      character*(*) tline
	  character*256 st


      write(st,10) tline,i
      call gifaout(st)
10    format(A,i10)
      return
      end
C******************************************************************
      subroutine gifaoutc(st)
c
c used to 'gifaout' form the C level
c
      implicit none
      character*(256) st		! assume max size
      integer l,i

      i = 1
      do while ( (st(i:i) .ne. char(0)) .and. (i.le.256))
         i = i+1
      enddo
      if (i.gt.1) then
        call trailing( st(1:i-1), l)
        call gifaout(st(1:l))
      else
        call gifaout(' ')
      endif
      return
      end
C******************************************************************
      subroutine gifaerr(st)
c
c used for error msg
c
      implicit none
#include "gifabcom.inc"
#include "gifshellv.inc"
      character*(*) st		! assume max size
c      character*(10) stl
      character*(256) stl2
c      integer err
      integer l,xx

      stl2 = '=ONERROR'
      call checkvar(stl2,vcontext,xx)
      if (xx .ne.0 ) then  ! ne.0 means no onerrorgoto assigned => message
         stl2 = st
         call trailing(stl2,l)
c         if (on_graph .eq. 1) then
c            stl = 'Error'
c            call f_msg_ok(stl,5,stl2,l,err)
c         else
           call gifaout('***'//stl2(1:l))
c         endif
      endif
      return
      end

C******************************************************************
      subroutine writedim(st,dim)
c IN	: dim
c 0UT	: st
c
c converts dim (numeric) to string (F1, F2 ...)
c
      implicit none

      integer dim
      character*(*) st
      if (dim.eq.1) then
        st = 'F1'
      elseif (dim.eq.2) then
        st = 'F2'
      elseif (dim.eq.3) then
        st = 'F12'
      elseif (dim.eq.4) then
        st = 'F3'
      elseif (dim.eq.5) then
        st = 'F13'
      elseif (dim.eq.6) then
        st = 'F23'
      elseif (dim.eq.7) then
        st = 'F123'
      else
        st = 'error'
      endif
      return
      end
C******************************************************************
C
C
      function checkdim(axis,type,dim)
c IN	: axis,type,dim
c
C true if type describes a complex data-set
C in the axis st, with dimentionality dim

      implicit none

      logical checkdim
      integer type,dim,axis

      checkdim = .false.

      if (dim.eq.1) then
         checkdim  = (type.eq.1)
      elseif (dim.eq.2) then
         if (axis.eq.1) then
            checkdim  = (type.ge.2)
         elseif (axis.eq.2) then
            checkdim  = (mod(type,2).eq.1)
         elseif (axis.eq.3) then
            checkdim  = (type.eq.3)
         endif
      elseif (dim.eq.3) then
         if (axis.eq.1) then
            checkdim  = (type.ge.4)
         elseif (axis.eq.2) then
            checkdim  = (mod(type,4).ge.2)
         elseif (axis.eq.3) then
            checkdim  = (type.ge.6)
         elseif (axis.eq.4) then
            checkdim  = (mod(type,2).eq.1)
         elseif (axis.eq.5) then
            checkdim  = (mod(type,4).eq.1)
         elseif (axis.eq.6) then
            checkdim  = (mod(type,4).eq.3)
         elseif (axis.eq.7) then
            checkdim  = (type.eq.7)
         endif
      endif
      return
      end

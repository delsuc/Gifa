C     File: unix_lib.f
C     File contains all unix hardware/os specific routines which are
C      not graphics dependent (those routines are in X_windows.for).
C
C     This file is a part of the GIFA program.
C     This software was developed by the NMR Group of the
C     CNRS in GIF/Yvette France.
C
C                     NMR Laboratory
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user has been fully
C licensed by the above laboratory.
C     This file cannot be copied, transferred, or used for any other
C purpose without the written permission of the authors.
C
C
C Contents:   (All unix specific subroutines...)
c
c subroutine os_config(st)
c subroutine gethome(st)
c subroutine changedir(st)
c subroutine shell(params)
c subroutine init_timer
c subroutine show_timer
c subroutine controlc
c subroutine setupctrlc
c subroutine waitasec(second)
c subroutine text(file,error)
c subroutine os_config(st)
C Identifies Operating system
      subroutine os_config(st)
c OUT	: st
c returns the name of the system
      implicit none
      character*256 st
#alliant      st='UNIX / Alliant concentrix'
#aix      st='UNIX / IBM AIX'
#sgi      st='UNIX / Silicon Graphics IRIS'
#hp       st='UNIX / HP'
#sun      st='UNIX / Sun-Sparc '
#adec      st='UNIX / Digital Ultrix'
#linux      st='LINUX'
#adrwn      st = 'DARWIN'
#spec1      st='UNIX / Spec1'
#f2c      st = st // ' / Compiled with the f2c fortran to C converter'
      return
      end

       subroutine waitasec(second)
c IN 	: second
c
c This subroutine is used to set the process to wait for a given number
C of seconds. (may be smaller than  1.0 !!)
c
c only defined for :   sgi - hp - f2c
       implicit none
       real second

#sgi      integer tick
#sgi      tick = 100.0*second
#sgi      call sginap(tick)

#f2c      call cwaitasec(second)
#hp       call cwaitasec(second)


       return
       end

       subroutine init_timer
c This subroutine is used to access the unix internal clock.
c Init_timer initializes the timer for subsequent use by show_timer.
        implicit none
#sgi        real*4 dtime,tarray(2),x
#sgi        common/timer/tarray
#sgi        x  = dtime(tarray)

#sun        real*4 dtime,tarray(2),x
#sun        common/timer/tarray
#sun        x  = dtime(tarray)

#f2c        real*4 dtime,tarray(2),x
#f2c        common/timer/tarray
#f2c        x  = dtime(tarray)

#adec        real*4 dtime,tarray(2),x
#adec        common/timer/tarray
#adec        x  = dtime(tarray)

#alliant        real*4 dtime,tarray(2),x
#alliant        common/timer/tarray
#alliant        x  = dtime(tarray)

#aix        integer clock, mclock
#aix        common/timer/clock
#aix        clock = mclock()

#hp$HP9000_800 INTRINSICS
#hp$NOSTANDARD INTRINSICS
#hp        integer clock
#hp        real sec
#hp        common/timer/clock,sec
#hp        clock = mclock()
#hp        sec= secnds(0.0)

       end


       subroutine show_timer
c This subroutine is used to display the elapsed time since the call
c  to Init_timer was made.
       implicit none
       character*256 st
       real x
#alliant        real*4 dtime,tarray(2)
#alliant        common/timer/tarray
#alliant        x  = dtime(tarray)
#alliant        write(st,*) 'Total ',x,' User ',tarray(1),' System ',tarray(2)

#f2c        real*4 dtime,tarray(2)
#f2c        common/timer/tarray
#f2c        x  = dtime(tarray)
#f2c        write(st,*) 'Total ',x,' User ',tarray(1),' System ',tarray(2)

#sun        real*4 dtime,tarray(2)
#sun        common/timer/tarray
#sun        x  = dtime(tarray)
#sun        write(st,*) 'Total ',x,' User ',tarray(1),' System ',tarray(2)

#sgi        real*4 dtime,tarray(2)
#sgi        common/timer/tarray
#sgi        x  = dtime(tarray)
#sgi        write(st,*) 'Total ',x,' User ',tarray(1),' System ',tarray(2)

#adec        real*4 dtime,tarray(2)
#adec        common/timer/tarray
#adec        x  = dtime(tarray)
#adec        write(st,*) 'Total ',x,' User ',tarray(1),' System ',tarray(2)

#aix        integer clock,y,mclock
#aix        common/timer/clock
#aix        y = mclock()
#aix        x = (y - clock )/100.0
#aix        clock = y
#aix        write(st,*) 'Total ',x,'sec.'

#hp$HP9000_800 INTRINSICS
#hp$NOSTANDARD INTRINSICS
#hp        integer clock,y
#hp        real x,sec
#hp        common/timer/clock,sec
#hp        y = mclock()
#hp        x = (y - clock )/1000000.0
#hp        clock = y
#hp        sec = secnds(sec)
#hp        write(st,*) 'Elapsed :',sec,' sec.','   CPU :',x,' sec.'

       call gifaout(st)
       end

       subroutine getwdir(st)
c OUT	: st
c This subroutine returns the value of the current workingdirectory
       implicit none
       character*(*) st
       call getenv('PWD',st)
       end

       subroutine gethome(st)
c OUT	: st
c This subroutine returns the value of the $HOME directory
       implicit none
       character*(*) st
       call getenv('HOME',st)
       end

       subroutine changedir(st,err)
c OUT	: st,err
c This subroutine changes the current working directory
       implicit none
       character*(256) st
#aix      character*(256) stl
       integer err,chdir,i
       external chdir

       call trailing(st,i)
#aix         stl = st
#aix         stl(i+1:i+1) = char(0)
#aix         err = chdir(stl)

#f2c        err = chdir(st,i)
#sgi       err = chdir(st(1:i))
#hp       err = chdir(st(1:i))
#sun       err = chdir(st(1:i))
#adec       err = chdir(st(1:i))
       if (err.ne.0) write (*,*) 'Change Directory Failed' 
       end

       subroutine shell(params)
c IN	: params
c This subroutine passes the given string (params) to unix to process
c  as a shell command.
       implicit none
       character*(*) params

        integer status

       if (params.eq.' ') then
          call system('csh')
       else
          call system(params)
       endif
       end


      subroutine setupctrlc
C This subroutine is used to initialize the control_c.
C It sets up a signal linked to control_C which executes the controlc subroutine
C which in turns set the control variable to 1.
C SUN has been set here same as SGI, not tested yet !
      implicit none
#include "controlc.inc"
      integer j,sigvec,isig,isub,keep
      save keep
      external controlc
      integer sigint
      parameter (sigint = 2)

#alliant      external sigvec
#alliant      include '/usr/include/fortran/signal.h'

#aix      include 'fexcp.h' 

#sgi      external signal
#sgi      integer signal

#sun      external signal
#sun      integer signal

#hp      external signal
#hp      integer signal

#f2c      external signal
#f2c      integer signal

      data keep/0/

C  sgi, aix, sun(?) needs to call signal each time, same as VMS
#sgi      goto 10
#sun      goto 10
#aix      goto 10
#hp      goto 10
#f2c      goto 10

      if (keep.ne.0) then
         control = 0
         return
      endif
#alliant      j = sigvec(sigint,sig_call,controlc,isig,isub)
#alliant      if (j.ne.0) then 
#alliant           write(*,*) 'Erreur dans sigvec', j
#alliant           stop
#alliant      endif

#aix10      call signal(sigint,controlc)
#aix      call signal(sigtrap,xl__trce)

#sgi10    j = signal(sigint,controlc,-1)
#sun10    j = signal(sigint,controlc,-1)
#hp10    j = signal(sigint,controlc,-1)
#f2c10    j= signal(sigint,controlc)

      control = 0
      keep = 1
      return
      end

      subroutine controlc(i,j,k)
C This subroutine is called whenever the user hits ^C.
      implicit none
#include "controlc.inc"
      integer i,j,k
      control = 1
c      write(*,*) 'coucou dans control C'
      return
      end

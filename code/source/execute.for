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
C
C
C this will parse the todocomm string and execute it
C
C error .eq. 0 means no error occured
C error .gt. 0 means an error occured
C error .eq.-1 means exiting the prgm is required
c

      subroutine execute(todocomm,tdlen,error)
c IN	: todocomm,tdlen
c OUT	: error
c
c execute the GIFA string of command held into todocomm,tdlen
c
      implicit none
      save
      character*256 todocomm
      integer tdlen,error

#vms#include "memoryparam.inc"
#include "constant.inc"
#include "sizeparam.inc"
#include "peaksize.inc"
#include "paramfilev.inc"
#include "paramdatav.inc"
#include "zoomparamv.inc"
#include "transform.inc"
#include "gifshellv.inc"
#include "maxentv.inc"
#include "linpredv.inc"
#include "disptypev.inc"
#include "disp3dv.inc"
#include "peakintv.inc"
#include "controlc.inc"
#include "plotcomv.inc"
#include "linecor2v.inc"
#include "unitextern.inc"
#include "variable.inc"
#include "gifaout.inc"
#include "cachev.inc"
#include "fitv.inc"
#include "dbmv.inc"
#include "laplacev.inc"
#include "ambv.inc"

#include "gifabcom.inc"
#include "version.inc"
      real z, keepsum, y, 
     *     zz1, zz2, zz3,zz4,zz5,zz6,zz7,
     *     zreal
      real*8 dpz1
      integer zz,xx,yy,empty,i,
     *     win1,win2,win3,win4,pl_id,
     *     win5,win6,ww,
     *     it,it2,it3,
     *     j,k,nest,location,n,
     *     title_length,maxtemp
      logical bool1  ! ,bool2
      character*256 lcommand,st,stp,stp2,stp3,stp4,stp5,ii,win_title
      character*256 stlong
      integer lcom
      parameter (lcom=32)
      character*(lcom) command
c variables used to store integers in string variables
      character*4 st4		! room for one
      character*16 st16		! room for 4	- used by FOR
      character*28 st28     ! room for 7	- used by FOREACH . WITHIN
      integer ist,ist16(4)
      real fst28(7)
      real*4 dtime,tarray(2)
      equivalence (ist,st4)
      equivalence (ist16(1),st16)
      equivalence (fst28(1),st28)

      integer closest
      real brukerphase
      external closest, brukerphase

C clean it and put it in the global string param.
      call cleanst(todocomm,tdlen)
      param = todocomm(1:tdlen)
C copy in jounal in input .eq. 20 (means : from graphic button, or direct input
C 
      if (input.eq.20) write(journal,*) param(1:tdlen)

      if (verbose.ne.0) write(*,*) param(1:tdlen)

C do timer stuff if needed

      if (timer.eq.1 .and. input.eq.20) then
         call init_timer
      endif

C put the wait_cursor

      if (on_x_win.eq.1) then
        do i=1,vd_id_max
          if( vd_id(i).ne.0) call win_set_pointer_pattern(vd_id(i),2)
        enddo
        call wait_cursor_in_all_shell()
      endif

C loop on each command line
 3    continue
C check if empty
      if (param.eq.' ') then
         if (input.eq.20) then
            if (refresh.ne.0) then
               call dorefresh()
               refresh = 0
            endif
C do timer stuff if needed...
            if (timer.eq.1) then
              call show_timer
            endif
C refresh dim pop-up and redisplay windows (might be missing)
            if (on_x_win.eq.1) then
              call update_label_dim(dim)
              call win_refresh()
            endif

            goto 990

         else
            if (refresh.ne.0 .and. refmacro.eq.1) then
               call dorefresh()
               refresh = 0
            endif
            call getinput(2,error)
            if (error.eq.10) goto 199         ! means eof
            if (error.ne.0) goto 93
            goto 3
         endif
       endif
      
C check profiling
       if (input.ne.20 .and. profiler.eq.1) then
           z=dtime(tarray)          ! first fix time
           if (z .gt. 0.0) then          ! z<0 happens !
             stp=currinp(input-20)      ! determine var-name
             call leading(stp)
             call trailing(stp,i)
             write(stp2,*) currline(input-20)-1
             call leading(stp2)
             call trailing(stp2,j)
             stp3 = '$_P[' //  stp(1:i) // '-' // stp2(1:j)  // ']'
             stp5 = '$_P["' //  stp(1:i) // '-' // stp2(1:j)  // '"]'
             call checkvar(stp3,vcontext,xx)      ! set to 0 if unassigned
             if (xx.eq.1) then
               stp4 = '0.0'
               call assign(stp3,20,stp4,error)
              if (error.ne.0) goto 172
             endif
             write(stp,*) 1000*z      ! eval new value
             call leading(stp)
             call trailing(stp,k)
             stp4 = stp(1:k) // '+' // stp5(1:i+j+8)
             call eval(stp4,stp2,error)
             if (error.ne.0) goto 172
             call assign(stp3,20,stp2,error)
             if (error.ne.0) goto 172
             z=dtime(tarray)          ! reset counter
           endif
       endif
C then parse the command line
      call getstring2(lcommand,error)
      if (error.ne.0)  goto 115
      command = lcommand
      call uppercase(command,lcom)
 
C loop on each command
 5    continue
C reset sizeimo (one never knows)
      if (dim.eq.1) then
            sizeimo=sizeimage1d
      elseif (dim.eq.2) then
            sizeimage=si1im*si2im
            sizeimo=sizeimage
      elseif (dim.eq.3) then
            sizeimage3d=si3d1*si3d2*si3d3
            sizeimo=sizeimage3d
      endif

C Then Execute accordingly
C****************************************************************
C       GIFA SHELL COMMANDS
C****************************************************************
#include "gifshellp.inc"
C****************************************************************
C****************************************************************
C        DBM COMMANDS
C****************************************************************
#include "dbmp.inc"
C****************************************************************
C****************************************************************
C       INPUT-OUTPUT COMMANDS
C****************************************************************
#include "inoutp.inc"
C****************************************************************
C****************************************************************
C       DISPLAY COMMANDS
C****************************************************************
#include "disptypep.inc"
C****************************************************************

      else              ! this little trick is needed by certain compiler
        zz = 0
        goto 8          ! typically the SGI one!
      endif
      goto 6
8     if (1 .eq. 2) then
        zz = 1

C****************************************************************
C       3D DISPLAY COMMANDS
C****************************************************************
#include "disp3dp.inc"
C****************************************************************
C****************************************************************
C       PS300-DEPENDENT FUNCTIONS
C****************************************************************
c#vms#include "ps300p.inc"  
C****************************************************************
C****************************************************************
C	GIFA PLOT COMMANDS
C****************************************************************
#include "plotcomp.inc"
C****************************************************************
C****************************************************************
C       INTEGRATION (PARIS) COMMANDS
C****************************************************************
#include "peakintp.inc"
#include "ambp.inc"
C****************************************************************

      else		! this little trick is needed by certain compiler
        zz = 0
        goto 7		! typically the SGI one!
      endif
      goto 6
7     if (1 .eq. 2) then
        zz = 1


C****************************************************************
C	BASELINE CORRECTION COMMANDS
C****************************************************************
#include "linecor2p.inc"
C****************************************************************
C****************************************************************
C       MAXIMUM ENTROPY COMMANDS
C****************************************************************
#include "maxentp.inc"
C****************************************************************
C*************************************************************
C       LINEAR PREDICTION COMMANDS
C*************************************************************
#include "linpredp.inc"
C*************************************************************
C*************************************************************
C       CACHE COMMANDS
C*************************************************************
#include "cachep.inc"
C*************************************************************
C*************************************************************
C       LINE FITTING
C*************************************************************
#include "fitp.inc"
C*************************************************************
C****************************************************************
C       Daniel's stuff
C****************************************************************
#include "danielp.inc"
C****************************************************************
C****************************************************************
C       Motif stuff
C****************************************************************
#include "buttonp.inc"
C****************************************************************
C****************************************************************
C       Motif formulaire
C****************************************************************
#include "formulairep.inc"
C****************************************************************
C****************************************************************
C       Laplace transform and Dosy processing
C****************************************************************
#include "laplacep.inc"
C****************************************************************


C****************************************************************
C Then basic stuff...
      elseif (command.eq.'ITYPE') then
         call message('Please check carefully the documentation
     * before using this command')
         if (dim.eq.1) then
          zz=it1d
          call getint2(it1d,error)        
          if ((it1d.lt.0).or.(it1d.gt.1) .or. error.ne.0) then
            it1d = zz
            goto 98
          endif
          if (zz.ne.it1d) refresh=1
         elseif (dim.eq.2) then
          zz=itype
          call getint2(itype,error)        
          if ((itype.lt.0).or.(itype.gt.3) .or. error.ne.0) then
             itype = zz
             goto 98
          endif
          if (zz.ne.itype) refresh=1
         else                     ! (dim.eq.3)
          zz=it3d
          call getint2(it3d,error)        
          if ((it3d.lt.0).or.(it3d.gt.7) .or. error.ne.0) then
            it3d = zz
            goto 98
          endif
         endif

      else if (command.eq.'ABSMAX') then
         z = max0
         call message('Value of maximum point of the data')
         call getreal2(max0,error)
         if (error.ne.0) goto 93
         if (max0.lt.0.0) then
             max0 = z
             goto 109
         endif
         if ((max0.ne.0.0 .and. z.ne.0.0) .and.
     *      (max0*1.0e8.lt.z .or. z*1.0e8.lt.max0)) then
            max0 = z
            goto 98
         endif
         if (z.ne.max0) then
           refresh=1
         endif

      else if (command.eq.'PLANE') then
C command active in 3D mode only. Gets a plane orthogonal to the given
C direction. This plane is stored in the "plane2d" buffer and can then 
C be plotted or displayed or handled as 2D data (command 'dim') 
           if (dim.ne.3) goto 120
           call message('plane orthogonal to which axis (F1,F2,or F3 ?')
           if (axis2d.eq.3) axis2d=4
           call getdim(axis2d,error)
           if (error.ne.0) goto 93
           if (axis2d .eq. 4) then
                 axis2d = 3
           else if (axis2d.eq.3)then
                goto 98
           endif
           if (axis2d.eq.1) then
              if (si3d2*si3d3.gt.smxmax) goto 103
           elseif (axis2d.eq.2) then
              if (si3d1*si3d3.gt.smxmax) goto 103
           elseif (axis2d.eq.3) then
              if (si3d2*si3d1.gt.smxmax) goto 103
           else
               goto 98
           endif
           call message('which plane : ')
           call getcoord(nplane,axis2d,error)
           if (error.ne.0) goto 93 
           if (nplane.le.0) then                    
               nplane=1
               goto 98
           endif 
           if (axis2d.eq.1) then
                 if (nplane.gt.si3d1) goto 108
                 call getplane(plane2d,image,
     *                     si3d1,si3d2,si3d3,nplane)
                 si1im = si3d2
                 si2im = si3d3
           else if (axis2d.eq.2) then
                 if (nplane.gt.si3d2) goto 108
                 call getplane2(plane2d,image,
     *                             si3d1,si3d2,si3d3,nplane)
                 si1im = si3d1
                 si2im = si3d3
           else if (axis2d.eq.3) then
                 if (nplane.gt.si3d3) goto 108
                 call getplane3(plane2d,
     *                     image,si3d1,si3d2,si3d3,nplane)
                 si1im = si3d1
                 si2im = si3d2
           endif
           call t3dto2d(axis2d)
           n2d = nplane
           max2d = 0.0
           refresh = 1

      else if (command.eq.'DIAG') then                     
        if (dim.eq.2) then
           if (itype.ne.0) goto 96 
           if (max(si1im,si2im).gt.sizemax) goto 103
           sizeimage1d = max(si1im,si2im)
           it1d = 0
           call getdiag(column,sizeimage1d,plane2d,si1im,si2im)
           if ( (disp1d.ne.0) .and.
     *          (input.eq.20 .or. refmacro.eq.1)) then
               call display1d(vd_id(1),sizeimage1d,column,
     *                    max0,scale,it1d,vheight,clear)  
           endif
           max1d = 0.0
           call t2dto1d(2)
        elseif (dim.eq.3) then
           if (it3d.ne.0) goto 96 
           call message('which diagonal plane? F1=F2(F12), F2=F3(F23)
     * or F1=F3(F13) ?')
           zz = 23
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (zz.eq.3) then		! f12
             if (max (si3d1,si3d2)*si3d3.gt.smxmax) goto 103
             si1im = max (si3d1,si3d2)
             si2im = si3d3
             call t3dto2d(2)
           elseif (zz.eq.6) then	! f23
             if (max (si3d2,si3d3)*si3d1.gt.smxmax) goto 103
             si1im = si3d1
             si2im = max(si3d2,si3d3)
             call t3dto2d(3)
           elseif (zz.eq.5) then	! f13
             if (max (si3d1,si3d3)*si3d2.gt.smxmax) goto 103
             si1im = max(si3d1,si3d3)
             si2im = si3d2
             call t3dto2d(3)
           else
              goto 98
           endif
           itype=0
           call getdiag2d(plane2d,si1im*si2im,image,
     *              si3d1,si3d2,si3d3,zz)
           max2d = 0.0
        endif
        refresh = 1

       elseif (command.eq.'WINDOW') then
C Get dimension
       if (dim.eq.3) then
           if ((si3d1+si3d2+si3d3).gt.sizemax) goto 103
           call message( 'window in F1, F2, or F3')
           win3 = 4
           call getdim(win3,error)
           if (error.ne.0) goto 93
           if (win3.eq.1) then
              win4 = si3d1
              zz = si3d3+si3d2
           else if (win3.eq.2) then
              win4 = si3d2
              zz = si3d3
           else if (win3.eq.4) then
              win4 = si3d3
              zz = 0
           else
              goto 98
           endif
       elseif (dim .eq. 2) then
           if ((si1im+si2im).gt.sizemax) goto 103
           call message( 'Window in F1 or F2? ')
           win3 = 2
           call getdim(win3,error)
           if (error.ne.0) goto 93
           if (win3.eq.1) then
              win4 = si1im
              zz = si2im
           elseif (win3.eq.2) then
              win4 = si2im
              zz = 0
           else
              goto 98
           endif
       elseif (dim.eq.1) then
           if (sizeimage1d.gt.sizemax) goto 103
           win3 = 0
           zz = 0
           win4 = sizeimage1d
       endif
C win4 holds the size of the window,  win3 is axis for 2D and 3D
C zz is first point -1
C Now, get window parameters
         win1 = 1
         call message( 'Enter window starting point (0 to reset):  ')
         call getcoord(win1,win3,error)
         if (error.ne.0) goto 93
         win2=min(win1+2,win4)
         if (win1.lt.0 .or. win1.gt.win4) goto  93
         if (win1.ne.0) then
            call message( 'Enter window ending point:  ')
            call getcoord(win2,win3,error)
           if (error.ne.0) goto 93
C compute window
c           win2 = min(win2,win4)
            if (win2.gt.win4 .or. win1.gt.win2) goto 93
            call prpvect(window(zz+win1),0.0,win2-win1+1)
         else
            call prpvect(window(zz+1),1.0,win4)
         endif

c      elseif (command.eq.'CONFIG') then
c         call gifaout('GIFA Version : '//version)
c         call gifaout('Licensed to : '//license(1:ilenc))
c         call gifaouti('Memory available (in kwords) : ', smxbig/1024)
c         call gifaouti('        protected area in 3D : ', 4*smxmax/1024)
c         call gifaouti('        protected area in 2D : ', smxmax/1024)
c         call gifaouti('        protected area in 1D : ', sizemax/1024)
c         call gifaout(' ')
c         call gifaouti('     max number of variables :',varmax)
c         call gifaout(' ')
c         call os_config(st)
c         call gifaout('Operating system : '//st)
c         call win_config(st)
c         call gifaout('Graphic manager : '//st)
c         call pl_config(st)
c         if (on_x_win.eq.1) then
c            call gifaout('Graphic mode is ACTIVE')
c         else
c            call gifaout('Graphic mode is INACTIVE')
c         endif
c         call gifaout('Plotter driver : '//st)

      else if (command.eq.'SETVAL') then
         call message('Enter coordinates')
         call getcoord(win1,1,error)
         if (error.ne.0) goto 93
         if (dim.eq.1) then
            if (win1.lt.1 .or. win1.gt.sizeimage1d) goto 98
            call message('Enter Value')
            call getreal2(z,error)
            if (error.ne.0) goto 93
            column(win1) = z
         elseif (dim.eq.2) then
            call getcoord(win2,2,error)
            if (error.ne.0) goto 93
            if (win1.lt.1 .or. win1.gt.si1im) goto 98
            if (win2.lt.1 .or. win2.gt.si2im) goto 98
            call message('Enter Value')
            call getreal2(z,error)
            if (error.ne.0) goto 93
            plane2d((win1-1)*si2im + win2) = z
         elseif (dim.eq.3) then
            call getcoord(win2,2,error)
            if (error.ne.0) goto 93
            call getcoord(win3,3,error)
            if (error.ne.0) goto 93
            if (win1.lt.1 .or. win1.gt.si3d1) goto 98
            if (win2.lt.1 .or. win2.gt.si3d2) goto 98
            if (win3.lt.1 .or. win3.gt.si3d3) goto 98
            call message('Enter Value')
            call getreal2(z,error)
            if (error.ne.0) goto 93
            image((win1-1)*(si3d3*si3d2) + (win2-1)*si3d2 + win3) = z
         endif
         refresh = 1
         max0 = 0.0


C****************************************************************
C      How to create or modify a file
C****************************************************************

      else if (command.eq.'SIMU') then 
          if (dim.eq.1) then              !case of 1d spectra
           call loaddata(column,sizeimage1d,noise)
           call prpvect(window,1.0,sizemax)
           it1d = 1
          else if (dim.eq.2) then              !case of 2d spectra
            call simu2d(plane2d,si1im,si2im,noise,itype)  
            call prpvect(window,1.0,sizemax)
          else if (dim.eq.3) then              !case of 3d spectra
            call simu3d(image,si3d1,si3d2,si3d3,noise,it3d,error)
            of3d1=0.0
            sp3d1=2000*3.1415926
            of3d2=0.0
            sp3d2=2000*3.1415926
            of3d3=0.0
            sp3d3=2000*3.1415926
          endif           
          max0=0.0
          refresh = 1
          empty = 1

      elseif (command.eq.'SIMUNOE') then
        if (dim.ne.2) goto 97
        call simunoe(plane2d,xx,yy,itype,offset1,
     *               offset2,specw1,specw2,error)
        if (error.ne.0)goto 90
        if ((xx*yy).ne.0) then          ! means real computation
         do zz=2,si1im,4
                call mltvect(plane2d(si2im*(zz)+1),
     *                plane2d(si2im*(zz)+1),-1.0,2*si2im)
         enddo
         si1im = xx
         si2im = yy
         sizeimage = si1im*si2im
         call bruvect(plane2d,plane2d,sizeimage)
         max0=0.0
         refresh=1
         empty=1
        endif

      else if (command.eq.'SIMUN') then
          if (dim.eq.1) then                   !case of 1d spectra
           call addsimu1d (column,unitx,error)
           if (error.ne.0) goto 93
          else if (dim.eq.2) then              !case of 2d spectra
           call addsimu2d (plane2d,unitx,error)
           if (error.ne.0) goto 93
          else if (dim.eq.3) then              !case of 3d spectra
           call addsimu3d(image,unitx,error)
           if (error.ne.0) goto 93
          endif          
          max0=0.0
          refresh = 1
          empty = 1

      else		! this little trick is needed by certain compiler
        zz = 0
        goto 9		! typically the SGI one!
      endif
      goto 6
9     if (1 .eq. 2) then
        zz = 1

      else if (command.eq.'ADDNOISE') then
          if (dim.eq.1) then                   !case of 1d spectra
           call addnoise (column,sizeimage1d,noise,error)
           if (error.ne.0) goto 93
          else if (dim.eq.2) then              !case of 2d spectra
           call addnoise (plane2d,sizeimage,noise,error)
           if (error.ne.0) goto 93
          else if (dim.eq.3) then              !case of 3d spectra
           call addnoise (image,sizeimage3d,noise,error)
           if (error.ne.0) goto 93
          endif          
          max0=0.0
          refresh = 1
          empty = 1

      elseif (command.eq.'CHEMS')then
        call delta(error)
        if (error.ne.0)goto 102

      else if (command.eq.'EXTRACT') then         !for 3d spectra
         if (dim.eq.1) then                    !case of 1d spectra
           if (verbose.ne.0)
     *      call gifaouti('Actual size:  ',sizeimage1d )
C Get extract parameters
           call getrect(win1,win3,win2,win4,error)
           if (error.ne.0) goto 90
           of1d = of1d + sp1d*(sizeimage1d-win2)/sizeimage1d
           sp1d = sp1d*(win2-win1+1)/sizeimage1d
           sizeimage1d = win2-win1+1
           if (verbose.ne.0) call gifaouti('New size:',sizeimage1d)
C then extract  
           call copvect(column,column(win1),sizeimage1d)

         else if (dim.eq.2) then                  !case of 2d spectra
           write(st,*) 'Actual size (F1 x F2):  ',si1im,si2im
           call gifaout(st)
           call getrect(win1,win3,win2,win4,error)
           if (error.ne.0) goto 90
C extract retained data            
           write(st,*) 'Extracting from ',win3,win1,' to ',win4,win2
           call gifaout(st)
           write(st,*) 'New size (F1 x F2):  '
     *       ,win4-win3+1,win2-win1+1 
           call gifaout(st) 
           offset2 = offset2 + specw2*(si2im-win2)/si2im
           offset1 = offset1 + specw1*(si1im-win4)/si1im
           specw2 = specw2*(win2-win1+1)/si2im
           specw1 = specw1*(win4-win3+1)/si1im
           call  extr2d(plane2d,si1im,si2im,win1,win2,win3,win4)

         else if (dim.eq.3) then                 !case of 3d spectra
           write(st,250)  si3d1,si3d2,si3d3
           call gifaout(st)
           call message( 'extract from (F1,F2,F3)')
           call getcoord(win1,1,error)
           if (error.ne.0) goto 93
           call getcoord(win3,2,error)
           if (error.ne.0) goto 93
           call getcoord(win5,3,error)
           if (error.ne.0) goto 93
           win2 = win1+1
           win4 = win3+1
           win6 = win5+1
           call message( 'to')
           call getcoord(win2,1,error)
           if (error.ne.0) goto 93
           call getcoord(win4,2,error)
           if (error.ne.0) goto 93
           call getcoord(win6,3,error)
           if (error.ne.0) goto 93                            
         if (win1.lt.1 .or. win1.gt.si3d1-1 .or. 
     *       win3.lt.1 .or. win3.gt.si3d2-1 .or. 
     *       win5.lt.1 .or. win5.gt.si3d3-1 .or. 
     *       win2.lt.2 .or. win2.gt.si3d1 .or. 
     *       win4.lt.2 .or. win4.gt.si3d2 .or. 
     *       win6.lt.2 .or. win6.gt.si3d3 .or. 
     *       win1+1.gt.win2 .or. 
     *       win3+1.gt.win4 .or. 
     *       win5+1.gt.win6 ) goto 98 
C extract retained data            
           write(st,251) win1,win3,win5,win2,win4,win6
           call gifaout(st)
           write(st,252) win2-win1+1,win4-win3+1,win6-win5+1 
           call gifaout(st)
           of3d3 = of3d3 + sp3d3*(si3d3-win6)/si3d3
           of3d2 = of3d2 + sp3d2*(si3d2-win4)/si3d2
           of3d1 = of3d1 + sp3d1*(si3d1-win2)/si3d1
           sp3d3 = sp3d3*(win6-win5+1)/si3d3
           sp3d2 = sp3d2*(win4-win3+1)/si3d2
           sp3d1 = sp3d1*(win2-win1+1)/si3d1
           call  extr3d(image,si3d1,si3d2,si3d3,
     *                      win1,win2,win3,win4,win5,win6)
         endif
         zoom = 0
         max0=0.0          
         refresh = 1 

       else if (command.eq.'ROW') then    !only for 2d spectra
        if (dim.eq.1) goto 124
        if (si2im.gt.sizemax) goto 103
        call message( 'enter row to select')
c        if (nclick.ge.1) then
c           row = clickt(nclick,2)
c           nclick = nclick-1
c        endif
        call getcoord(row,1,error)
        if (error.ne.0) goto 93  
        if (row.le.si1im .and. row.ge.1) then
           call copvect(column,plane2d((row-1)*si2im + 1),si2im)
           sizeimage1d=si2im
           axis1d = 2
           n1d1 = row
           n1d2 = 1
           call t2dto1d(2)
           if ((disp1d.ne.0) .and.
     *          (input.eq.20 .or. refmacro.eq.1)) then
              if (zoom.eq.0) then
                 call display1d(vd_id(1),sizeimage1d,
     *                    column,max0,scale,it1d,vheight,clear)  
              else
                 call display1d(vd_id(1),zo1du-zo1dl+1,
     *                    column(zo1dl),max0,scale,it1d,vheight,clear)  
              endif
           endif
        else
           goto 108
        endif
        max1d = 0.0

       else if (command.eq.'COL') then  !only for 2d spectra
        if (dim.eq.1) goto 124
        if (si1im.gt.sizemax) goto 103
        call message( 'enter column to select')
c        if (nclick.ge.1) then
c           col = clickt(nclick,1)
c           nclick = nclick-1
c        endif
        call getcoord(col,2,error)
        if (error.ne.0) goto 93  
        if (col.le.si2im .and. col.ge.1) then
           call getcol(column,plane2d,col,si1im,si2im)
           sizeimage1d=si1im     
           axis1d = 1
           n1d1 = col
           n1d2 = 1
           call t2dto1d(1)
           if (disp1d.ne.0 .and.
     *          (input.eq.20 .or. refmacro.eq.1)) then
              if (zoom.eq.0) then
                 call display1d(vd_id(1),sizeimage1d,
     *                    column,max0,scale,it1d,vheight,clear)  
              else
                 call display1d(vd_id(1),zo1du-zo1dl+1,
     *                    column(zo1dl),max0,scale,it1d,vheight,clear)  
              endif
           endif
        else
          goto 108
        endif
        max1d = 0.0

       else if (command.eq.'VERT') then  !only for 3d spectra
          if (dim.ne.3) goto 120
c          if (nclick.ge.1) then
c            colv = clickt(nclick,1)
c            rowv = clickt(nclick,2)
c            nclick = nclick-1
c          endif
          if (axis2d.eq.1) then
            if (si3d1.gt.sizemax) goto 103
            xx = 2
            yy = 3
          elseif (axis2d.eq.2) then
            if (si3d2.gt.sizemax) goto 103
            xx = 1
            yy = 3
          elseif (axis2d.eq.3) then
            if (si3d3.gt.sizemax) goto 103
            xx = 1
            yy = 2
          else
            goto 118
          endif
          write(st,'(i1)') axis2d
          call message ('vertical along axis F'//st)
          call message ('enter coordinates:')
          call message ('along first dim :')
          call getcoord(rowv,xx,error)
          if (error.ne.0) goto 93
          call message ('along second dim :')
          call getcoord(colv,yy,error)
          if (error.ne.0) goto 93
          if (rowv.gt.si1im .or. colv.gt.si2im) goto 108
          if (rowv.lt.1 .or. colv.lt.1) goto 108
          axis1d = axis2d
          n1d1 = rowv
          n1d2 = colv
          if (axis2d.eq.1) then
             call getcol(column,
     *                image,(rowv-1)*si3d3+colv,si3d1,si3d2*si3d3)
             sizeimage1d = si3d1
          elseif (axis2d.eq.2) then
             call getcol(column,
     *                image((rowv-1)*si3d2*si3d3+1),colv,si3d2,si3d3)
             sizeimage1d = si3d2
          elseif (axis2d.eq.3) then
             call copvect(column,
     *             image((rowv-1)*si3d3*si3d2+(colv-1)*si3d3+1),si3d3)
             sizeimage1d = si3d3
          else
            goto 98
          endif
          call t3dto1d(axis2d)
          if (disp1d.ne.0 .and.
     *          (input.eq.20 .or. refmacro.eq.1)) then
             if (zoom.eq.0) then
                call display1d(vd_id(1),sizeimage1d,
     *                    column,max0,scale,it1d,vheight,clear)  
             else
                call display1d(vd_id(1),zo1du-zo1dl+1,
     *                    column(zo1dl),max0,scale,it1d,vheight,clear)  
             endif
          endif
          max1d = 0.0
  
      else if (command.eq.'ZERO') then
         call prpvect(imago(imdim(dim)),0.0,sizeimo)
         max0=0.0
         refresh = 1

      else if (command.eq.'ONE') then
         if (dim.eq.1) then
           if (it1d.eq.0) then
              call prpvect(imago(imdim(dim)),1.0,sizeimo)
           else
              call prcvect(imago(imdim(dim)),
     *                     cmplx(1.0,0.0),sizeimo/2)
           endif
         elseif (dim.eq.2) then
           if (itype.eq.0) then
              call prpvect(imago(imdim(dim)),1.0,sizeimo)
           elseif (itype.eq.1) then
              call prcvect(imago(imdim(dim)),
     *                     cmplx(1.0,0.0),sizeimo/2)
           elseif (itype.eq.2) then
              do i=1,si1im
              if (mod(i,2).eq.1) then
                 call prpvect(plane2d((i-1)*si2im+1),1.0,si2im)
              else
                 call prpvect(plane2d((i-1)*si2im+1),0.0,si2im)
              endif
              enddo
           elseif (itype.eq.3) then
              do i=1,si1im
              if (mod(i,2).eq.1) then
            call prcvect(plane2d((i-1)*si2im+1),cmplx(1.0,0.0),si2im/2)
              else
            call prpvect(plane2d((i-1)*si2im+1),0.0,si2im)
              endif
              enddo
           endif
         elseif (dim.eq.3) then
           if (it3d.eq.0) then
              call prpvect(imago(imdim(dim)),1.0,sizeimo)
           elseif (it3d.eq.1) then
              call prcvect(imago(imdim(dim)),
     *                     cmplx(1.0,0.0),sizeimo/2)
           else
              goto 91
           endif
         endif
         max0=0.0
         refresh = 1
        
      elseif (command.eq.'GET') then
         if (dim.eq.1) zz=sizeimage1d
         if (dim.eq.2) zz=si1im+si2im
         if (dim.eq.3) zz=si3d1+si3d2+si3d3
         call message( 'Get what? ')
         st = 'data'
         call getstring2(st,error)
         if (error.ne.0) goto 93
         call uppercase(st,256)
         if (st.eq.'FILTER') then
           zz = nchannel*zz
           if (zz.gt.sizemax) goto 103
           call copvect(column,filter,zz)
           dim =1
           it1d = 0
         elseif (st.eq.'WINDOW') then
           if (zz.gt.sizemax) goto 103
           call copvect(column,window,zz)
           dim = 1
           it1d = 0
         elseif (st.eq.'STEP') then
           call copvect(column,sstep,iter)
           dim = 1
           it1d = 0
           zz=iter
         elseif (st.eq.'LAMB') then
           call copvect(column,slamb,iter)
           zz=iter
           dim = 1
           it1d = 0
         elseif (st.eq.'CONV') then
           call copvect(column,sconv,iter)
           zz=iter
           dim = 1
           it1d = 0
         elseif (st.eq.'ENT') then
           call copvect(column,sent,iter)
           zz=iter
           dim = 1
           it1d = 0
         elseif (st.eq.'CHI') then
           call copvect(column,schi,iter)
           zz=iter
           dim = 1
           it1d = 0
         elseif (st.eq.'SUM') then
           call copvect(column,ssum,iter)
           zz=iter
           dim = 1
           it1d = 0
         elseif (st.eq.'TAB') then
           call copvect(column,sampling,szsamp)
           zz=szsamp
           dim = 1
           it1d = 0
         elseif (st.eq.'LINEFIT') then
           if (dim.eq.1) then
             if (nbpic1d.lt.1) goto 111
             if (zoom.eq.0) then
               win1=1
               win2=sizeimage1d
             else
               win1=zo1dl
               win2=zo1du
             endif
             call initprm1d(nbpic1d,error)  !param. init.
             if (error.ne.0) goto 91
             do i=win1,win2
               call flogau(4*nbpic1d,prmr1d,float(i),column(i))
               if (control .eq.1) goto 116
             enddo 
             it1d = 0
           else if (dim.eq.2) then
             if (nbpic2d.lt.1) goto 111
	         if (zoom.eq.0) then
	             win1=1
	             win2=1
	             win3=si2im
	             win4=si1im
	         else
	             win1=zolf2
	             win2=zolf1
	             win3=zour2
	             win4=zour1        
	         endif
	         xx = max(si1im+1,si2im+1)
             call initprm2d(nbpic2d,xx,error)  !param. init.
             if (error.ne.0) goto 91
             do i = win1,win3
               do j = win2,win4
                 call flogau2d(6*nbpic2d+1,prmr2d,float(i+xx*j),z)
                 plane2d(i+si2im*(j-1)) = z
               enddo
               if (control .eq.1) goto 116
             enddo
             itype = 0
           else
              goto 124
           endif

         elseif (st.eq.'DATA') then
           if ( emptydata ) goto 95
           if (dimdat.ne.dim) goto 117
           zz=sizedata
           call tfromdata(dim)
           call copvect(imago(imdim(dim)),datab,sizedata)
         elseif (st.eq.'AMOEBA') then  
           if (dim.ne.2) goto 97
           call copvect(plane2d,work2,sizeimage)
           zz=sizeimage
           itype = 0
         elseif (st.eq.'RESIDUE') then
           if ( emptydata ) goto 95
           if (iter.eq.0)  goto 91
           if (dimdat.ne.dim) goto 117
           call checkoverflow(7*smxmax,bool1,1)
           if (bool1) goto 90
           if (sizeimo.gt.smxmax) goto 103
           if (dim.eq.1) then
              if (it1d.ne.0) goto 91
              call powtwo(sizeimage1d/nchannel,zz)
              if (zz.ne.0) goto 94
              if (itdata.ne.1) goto 91
              zz = sizedata
              it1d = itdata
           elseif (dim.eq.2) then
              if (itype.ne.0) goto 91
              call powtwo(si1im/nchannel,zz)
              call powtwo(si2im,yy)
              if (zz.ne.0 .or. yy.ne.0) goto 94
              if (itdata.ne.3) goto 91
              zz=sizedata
              itype = itdata
              si1im = si1dat
              si2im = si2dat
           else
              goto 124
           endif
           call transform(work3,imago(imdim(dim)),dim,1)
           call addvect(imago(imdim(dim)),work3,datab,-1.0,sizedata)
         else
           goto 110
         endif
         if (dim.eq.1) sizeimage1d = zz
         if (dim.eq.2) sizeimage = zz
         if (dim.eq.3) sizeimage3d = zz
         max0=0.0
         refresh = 1

      elseif (command.eq.'PUT') then
         call message( 'Put what ?')
         st = 'data'
         call getstring2(st,error)
         if (error.ne.0) goto 93
         call uppercase(st,256)
         if (st.eq.'DATA') then
           if (sizeimo.gt.datamax) goto 103
           call checkoverflow(4*smxmax,bool1,0)
           if (bool1) goto 90
            call copvect(datab,imago(imdim(dim)),sizeimo)
            call ttodata(dim)
            emptydata = .false.
         elseif (dim.eq.1) then
           if (st.eq.'FILTER') then
             if (nchannel.eq.1) then
               if (sizeimage1d.gt.sizemax) goto 103
               call copvect(filter,column,sizeimage1d)
             else
               win1 = 1
               call message( 'Which channel? ')
               call getint2(win1,error)
               if (error.ne.0) goto 93
               if (win1.lt.0 .or. win1.gt.nchannel) then
                  goto 107
               elseif (win1.eq.0) then
C consider the whole data as a multi filter
                  if (sizeimage1d.gt.sizemax) goto 103
                  call copvect(filter,column,sizeimage1d)
               else
                  if ((nchannel*sizeimage1d).gt.sizemax) goto 103
                  call copvect(filter(sizeimage1d*(win1-1)+1),
     *                         column,sizeimage1d)
               endif
             endif
           elseif (st.eq.'WINDOW') then
             if (sizeimage1d.gt.sizemax) goto 103
             call copvect(window,column,sizeimage1d)
           elseif (st.eq.'TAB') then
              if (sizeimage1d.gt.sizemax) goto 103
              call copvect(sampling,column,sizeimage1d)
              szsamp = sizeimage1d
           else
             goto 110
           endif
         elseif (dim.eq.2) then
           if (st.eq.'ROW') then
             call message( 'Which Index? ')
             call getint2(zz,error)
             if (error.ne.0) goto 93
             if (zz.lt.1 .or. zz.gt.si1im) goto 108
             call copvect(plane2d((zz-1)*si2im+1),column,si2im)
           elseif (st.eq.'COL') then
             call message( 'Which Index? ')
             call getint2(zz,error)
             if (error.ne.0) goto 93
             if (zz.lt.1 .or. zz.gt.si2im) goto 108
             call putcol(column,plane2d,zz,si1im,si2im)
           elseif (st.eq.'AMOEBA') then
             call copvect(work2,plane2d,sizeimage)
           else
             goto 110
           endif
           max0 = 0.0
           refresh = 1
         elseif (dim.eq.3) then
           if (st.eq.'PLANE') then
             call message('plane orthogonal to which axis?')
             if (axis2d.eq.3) axis2d=4
             call getdim(axis2d,error)
             if (error.ne.0) goto 93
             if (axis2d .eq. 4) then
                 axis2d = 3
             else if (axis2d.eq.3)then
                axis2d = 4
             endif
             if (axis2d.lt.1 .and. axis2d.gt.3) goto 98
             call message('n: put the nth plane')
             call getint2(nplane,error)
             if (error.ne.0) goto 93 
             if (axis2d.eq.1) then
                if (si1im.ne.si3d2 .and. si2im.ne.si3d3) goto 117
                if (nplane.lt.1 .or. nplane.gt.si3d1) goto 108
                 call putplane(plane2d,image,
     *                     si3d1,si3d2,si3d3,nplane)
             elseif (axis2d.eq.2) then
                if (si1im.ne.si3d1 .and. si2im.ne.si3d3) goto 117
                if (nplane.lt.1 .or. nplane.gt.si3d2) goto 108
                 call putplane2(plane2d,image,
     *                             si3d1,si3d2,si3d3,nplane)
             elseif (axis2d.eq.3) then
                if (si1im.ne.si3d1 .and. si2im.ne.si3d2) goto 117
                if (nplane.lt.1 .or. nplane.gt.si3d3) goto 108
                 call putplane3(plane2d,image,
     *                             si3d1,si3d2,si3d3,nplane)
             else
              goto 110
             endif
             max0 = 0.0
             refresh = 1
           else if (st.eq.'VERT') then
               if (axis2d.eq.1) then
                 if (si3d1.ne.sizeimage1d) goto 117
                 xx = 2
                 yy = 3
               elseif (axis2d.eq.2) then
                 if (si3d2.ne.sizeimage1d) goto 117
                 xx = 1
                 yy = 3
               elseif (axis2d.eq.3) then
                 if (si3d3.ne.sizeimage1d) goto 117
                 xx = 1
                 yy = 2
               else
                 goto 118
               endif
               write(stp,'(i1)') axis2d
               call message ('vertical along axis F'//stp)
               call message ('enter coordinates:')
               call message ('along first dim :')
               call getcoord(rowv,xx,error)
               if (error.ne.0) goto 93
               call message ('along second dim :')
               call getcoord(colv,yy,error)
               if (error.ne.0) goto 93
               if (rowv.gt.si1im .or. colv.gt.si2im) goto 108
               if (rowv.lt.1 .or. colv.lt.1) goto 108
               if (axis2d.eq.1) then
                 call putcol(column,
     *                    image,(rowv-1)*si3d3+colv,si3d1,si3d2*si3d3)
               elseif (axis2d.eq.2) then
                  call putcol(column,
     *                image((rowv-1)*si3d2*si3d3+1),colv,si3d2,si3d3)
               elseif (axis2d.eq.3) then
                  call copvect(column,
     *             image((rowv-1)*si3d3*si3d2+(colv-1)*si3d3+1),si3d3)
               endif
               max0 = 0.0
               refresh = 1
           endif
         endif

      elseif (command.eq.'SHOW') then
         if (on_x_win.eq.0) goto 91
c         if (disp1d.eq.0 .and. disp2d.eq.0) goto 127
         z = 0.0
         st = 'FILTER'
         call getstring2(st,error)
         if (error.ne.0) goto 93
         call uppercase(st,256)
         if (st.eq.'STEP') then
           if (disp1d.eq.0) goto 127
           call gcolor(vd_id(1),scolor)
           call display1d(vd_id(1),iter,sstep,z,1.0,0,0.0,clear)
           call gcolor(vd_id(1),color)
         elseif (st.eq.'LAMB') then
           if (disp1d.eq.0) goto 127
           call gcolor(vd_id(1),scolor)
           call display1d(vd_id(1),iter,slamb,z,1.0,0,0.0,clear)
           call gcolor(vd_id(1),color)
         elseif (st.eq.'CONV') then
           if (disp1d.eq.0) goto 127
           call gcolor(vd_id(1),scolor)
           call display1d(vd_id(1),iter,sconv,z,1.0,0,0.0,clear)
           call gcolor(vd_id(1),color)
         elseif (st.eq.'ENT') then
           if (disp1d.eq.0) goto 127
           call gcolor(vd_id(1),scolor)
           call display1d(vd_id(1),iter,sent,z,1.0,0,0.0,clear)
           call gcolor(vd_id(1),color)
         elseif (st.eq.'CHI') then
           if (disp1d.eq.0) goto 127
           call gcolor(vd_id(1),scolor)
           call display1d(vd_id(1),iter,schi,z,1.0,0,0.0,clear)
           call gcolor(vd_id(1),color)
         elseif (st.eq.'SUM') then
           if (disp1d.eq.0) goto 127
           call gcolor(vd_id(1),scolor)
           call display1d(vd_id(1),iter,ssum,z,1.0,0,0.0,clear)
           call gcolor(vd_id(1),color)
         elseif (st.eq.'TAB') then
           if (disp1d.eq.0) goto 127
           call gcolor(vd_id(1),scolor)
           call display1d(vd_id(1),szsamp,sampling,z,1.0,0,0.0,clear)
           call gcolor(vd_id(1),color)
         elseif (st.eq.'WINDOW') then
           if (dim.eq.1) then
               win4=sizeimage1d
           elseif (dim.eq.2) then
               win4=si1im+si2im
           else
               win4=si3d1+si3d2+si3d3
           endif
           if (disp1d.eq.0) goto 127
           call gcolor(vd_id(1),scolor)
           call display1d(vd_id(1),win4,window,z,1.0,0,0.1,clear)
           call gcolor(vd_id(1),color)
         elseif (st.eq.'FILTER') then
           if (disp1d.eq.0) goto 127
           if (dim.eq.1) then
               win4=sizeimage1d*nchannel
           elseif (dim.eq.2) then
               win4=(si1im+si2im)*nchannel
           else
               win4=(si3d1+si3d2+si3d3)*nchannel
           endif
           call gcolor(vd_id(1),scolor)
           call display1d(vd_id(1),win4,filter,z,1.0,0,0.0,clear)
           call gcolor(vd_id(1),color)
         elseif (st.eq.'DATA') then
            if (dim .ne. dimdat) goto 117
            if (dim.eq.1) then
               if (disp1d.eq.0) goto 127
               call gcolor(vd_id(1),scolor)
               if (zoom.eq.0) then
                  call display1d(vd_id(1),sizedata,datab,
     *               max0,scale,itdata,vheight,clear)
               else
                  call display1d(vd_id(1),zo1du-zo1dl+1,
     *               datab(zo1dl),max0,scale,itdata,vheight,clear)
               endif
               call gcolor(vd_id(1),color)
            elseif (dim.eq.2) then
               if (disp2d.eq.0) goto 127
               call display2d(vd_id(2),si2dat,si1dat,
     *                 datab,max0,scale,itdata,sign,
     *                 zoom,zolf1,zolf2,zour1,zour2)
            else
               goto 124
            endif
         elseif (st.eq.'CURRENT') then
           if (dim.eq.1) then
             if (disp1d.eq.0) goto 127
             if (zoom.eq.0) then
                call display1d(vd_id(1),sizeimage1d,column,
     *               max0,scale,it1d,vheight,clear)
             else
                call display1d(vd_id(1),zo1du-zo1dl+1,
     *               column(zo1dl),max0,scale,it1d,vheight,clear)
             endif
           elseif (dim.eq.2) then
              if (disp2d.eq.0) goto 127
              call display2d(vd_id(2),si2im,si1im,
     *                 plane2d,max0,scale,itype,sign,
     *                 zoom,zolf1,zolf2,zour1,zour2)
           else
              goto 124
           endif
         elseif (st.eq.'AMOEBA') then
           if (dim.ne.2) goto 97
           if (disp2d.eq.0) goto 127
           if (si1im*si2im.gt.2*smxmax) goto 103
           call checkoverflow(6*smxmax,bool1,1)
           if (bool1) goto 90
              zz1 = 0.0
              call display2d(vd_id(2),si2im,si1im,
     *             work2,zz1,max(float(nbpic2d/32),1.0),itype,sign,
     *             zoom,zolf1,zolf2,zour1,zour2)
         elseif (st.eq.'LINEFIT') then
           if (dim.eq.1) then
             if (disp1d.eq.0) goto 127
             if (nbpic1d.lt.1) goto 111
             if (zoom.eq.0) then
               win1=1
               win2=sizeimage1d
             else
               win1=zo1dl
               win2=zo1du
             endif
             call initprm1d(nbpic1d,error)  !param. init.
             if (error.ne.0) goto 91
             do i = win1,win2
               call flogau(4*nbpic1d,prmr1d,float(i),work1d(i))
               if (control .eq.1) goto 116
             enddo
             call gcolor(vd_id(1),scolor)
             call display1d(vd_id(1),win2-win1+1,work1d(win1),max0,
     *         scale,0,vheight,clear)
             call gcolor(vd_id(1),color)
           else if(dim.eq.2) then
             if (cdisp2d.eq.0) goto 127
             if (nbpic2d.lt.1) goto 111
	         if (zoom.eq.0) then
	             win1=1
	             win2=1
	             win3=si2im
	             win4=si1im
	         else
	             win1=zolf2
	             win2=zolf1
	             win3=zour2
	             win4=zour1        
	         endif
	         xx = max(si1im+1,si2im+1)
	         if ((win4-win2+1)*(win3-win1+1).gt.smxmax) goto 103
             call checkoverflow(7*smxmax,bool1,1)
             if (bool1) goto 90
             call initprm2d(nbpic2d,xx,error)  !param. init.
             if (error.ne.0) goto 91
             zz1 = 0.0
             do i = win1,win3
               do j = win2,win4
                 call flogau2d(6*nbpic2d+1,prmr2d,float(i+xx*j),z)
                 work3(i-win1+1+(win3-win1+1)*(j-win2)) = z
               enddo
               if (control .eq.1) goto 116
             enddo
             call gcolor(vd_id(3),scolor)
             call plot2d(0,vd_id(3), win4-win2+1, win3-win1+1, work3,
     *                   max0,scale,0,cx,cy,level,sign,loga,
     *                   0,0,1,1,win4-win2+1,win3-win1+1,clear)

           else
              goto 124
           endif

         elseif (st.eq.'FT') then
            if (sizeimo.gt.smxmax) goto 103
           call checkoverflow(7*smxmax,bool1,1)
           if (bool1) goto 90
           if (dim.eq.1) then
             if (it1d.ne.1) goto 143
             call powtwo(sizeimage1d,zz)
             if (zz.ne.0) goto 94
             call copvect(work3,column,sizeimage1d)
             call fftreal(work3,sizeimage1d,1,-1)
             if (zoom.eq.0) then
               call display1d(vd_id(1),sizeimage1d,work3,z,
     *                        scale,1,vheight,clear)
             else
               call display1d(vd_id(1),zo1du-zo1dl+1,
     *              work3(zo1dl),z,scale,1,vheight,clear)
             endif
           elseif (dim.eq.2) then
             if (itype.ne.3) goto 143
             call powtwo(si1im,zz)
             if (zz.ne.0) goto 94
             call powtwo(si2im,zz)
             if (zz.ne.0) goto 94
             if (si1im*si2im.gt.256*1024)
     *           write(*,*) 'Computing display...'
             call copvect(work3,plane2d,sizeimage)
             call tdftreal(work3,si1im,si2im,1,-1,3)
             call display2d(vd_id(2),si2im,si1im,
     *                 work3,z,scale,itype,sign,
     *                 zoom,zolf1,zolf2,zour1,zour2)
           elseif (dim.eq.3) then
              goto 124
           endif

         elseif (st.eq.'RESIDUE') then
           if ( emptydata ) goto 95
           if (iter.eq.0)  goto 91
           if (dimdat.ne.dim) goto 117
           call checkoverflow(7*smxmax,bool1,1)
           if (bool1) goto 90
           if (sizeimo.gt.smxmax) goto 103
           if (dim.eq.1) then
              if (it1d.ne.0) goto 91
              call powtwo(sizeimage1d/nchannel,zz)
              if (zz.ne.0) goto 94
              if (itdata.ne.1) goto 91
              zz = sizedata
              it1d = itdata
           elseif (dim.eq.2) then
              if (itype.ne.0) goto 91
              call powtwo(si1im/nchannel,zz)
              call powtwo(si2im,yy)
              if (zz.ne.0 .or. yy.ne.0) goto 94
              if (itdata.ne.3) goto 91
              zz=sizedata
              itype = itdata
              si1im = si1dat
              si2im = si2dat
           else
              goto 124
           endif
           call transform(work3,imago(imdim(dim)),dim,1)
           call addvect(work3,work3,datab,-1.0,sizedata)
           if (dim.eq.1) then
             if (zoom.eq.0) then
               call display1d(vd_id(1),sizeimage1d,work3,z,
     *                        scale,1,vheight,clear)
             else
               call display1d(vd_id(1),zo1du-zo1dl+1,
     *              work3(zo1dl),z,scale,1,vheight,clear)
             endif
           else
              call display2d(vd_id(2),si2im,si1im,
     *                 work3,z,scale,itype,sign,
     *                 zoom,zolf1,zolf2,zour1,zour2)
           endif
         else
           goto 110
         endif


      else if (command.eq.'APPLY') then
         call message( 'apply : FILTER, WINDOW')
         st = 'WINDOW'
         call getstring2(st,error)
         if (error.ne.0) goto 93
         call uppercase(st,256)
         if (st.eq.'WINDOW') then 
           if (dim.eq.1) then
              call diavect(column,column,window,sizeimage1d)
           elseif (dim.eq.2) then
              call mult2d(window,plane2d,si1im,si2im)
           else
              call mult3d(window,image,si3d1,si3d2,si3d3)
           endif
         elseif  (st.eq.'FILTER') then
           call dofilter
           if (dim.eq.1) then
              call diavect(column,column,filter,sizeimage1d)
           elseif (dim.eq.2) then
              call mult2d(filter,plane2d,si1im,si2im)
           else
              call mult3d(filter,image,si3d1,si3d2,si3d3)
           endif
         else
           goto 110
         endif
         max0 =0.0
         refresh = 1
                                                    
      else if (command.eq.'ADDH') then
         if (sizeimo.gt.4*smxmax) goto 103
         call checkoverflow(5*smxmax,bool1,1)
         if (bool1) goto 90
         call message( 'Enter file name:  ')
         call getstring2(name,error)
         if (error.ne.0) goto 93       
         if (dim.eq.1) then
           call addh1d(work,column,sizeimage1d,name,error)
           refresh = 1
         else if (dim.eq.2) then
           call addh2d(work,plane2d,si1im,si2im,name,error)
           max0=0.0
           refresh = 1        ! refresh anyway
         else if (dim.eq.3) then
           call addh2d(work,image,si3d1*si3d2,si3d3,name,error)
           max0=0.0
           refresh = 1        ! refresh anyway
         endif
         if (error.ne.0) goto 93
         max0=0.0

      else if (command.eq.'PROJ') then
        if (dim.eq.2) then
         call message( 'along which axis (F1 or F2)?')
         zz=2
         call getdim(zz,error)
         if (error.ne.0) goto 93                         
         if (zz.eq.1) then
           if (si2im.gt.sizemax) goto 103
           axis1d = 2
           sizeimage1d = si2im
           win1 = si1im
         else if (zz.eq.2) then
           if (si1im.gt.sizemax) goto 103
           axis1d = 1
           sizeimage1d = si1im
           win1 = si2im
         else 
           goto 98
         endif        
         call message( 'Choose Mean or Skyline (M/S)')
         st='S'
         call getstring2(st,error)
         if (error.ne.0) goto 93
         call uppercase(st,256)
         if (st.eq.'M') then
           call projadd(column,plane2d,si1im,si2im,zz,1,win1)
         else if (st.eq.'S') then
           call projmax(column,plane2d,si1im,si2im,zz,1,win1)
         else
           goto 98
         endif
         call t2dto1d(axis1d)
         if (disp1d.ne.0 .and.
     *          (input.eq.20 .or. refmacro.eq.1))  then
             if (zoom.eq.0) then
                call display1d(vd_id(1),sizeimage1d,
     *                    column,max0,scale,it1d,vheight,clear)
             elseif (zz.eq.1) then
                call display1d(vd_id(1),zour2-zolf2+1,
     *                    column(zolf2),max0,scale,it1d,vheight,clear)
             elseif (zz.eq.2) then
                call display1d(vd_id(1),zour1-zolf1+1,
     *                    column(zolf1),max0,scale,it1d,vheight,clear)
             endif
         endif
         max1d = 0.0
        elseif (dim.eq.3) then
         call message( 'along which axis (F1, F2 or F3)?')
         if (axis2d.eq.3) axis2d=4
         call getdim(axis2d,error)
         if (error.ne.0) goto 93 
         if (axis2d.eq.1) then
           if (si3d2*si3d3.gt.smxmax) goto 103
            si1im = si3d2
            si2im = si3d3
            win1 = si3d1
         else if (axis2d.eq.2) then
           if (si3d1*si3d3.gt.smxmax) goto 103
            win1 = si3d2
            si1im = si3d1
            si2im = si3d3
         else if (axis2d.eq.4) then
            if (si3d1*si3d2.gt.smxmax) goto 103
            win1 = si3d3
            si1im = si3d1
            si2im = si3d2
            axis2d = 3
         else 
           goto 98
         endif        
         call t3dto2d(axis2d)
         call message( 'Choose Mean or Skyline (M/S)')
         st='S'
         call getstring2(st,error)
         if (error.ne.0) goto 93
         call uppercase(st,256)
         if (st.eq.'M') then
           call proj3dadd(plane2d,image,si3d1,si3d2,si3d3,axis2d,1,win1)
         else if (st.eq.'S') then
           call proj3dmax(plane2d,image,si3d1,si3d2,si3d3,axis2d,1,win1)
         else
           goto 98
         endif
         if (disp2d.ne.0 .and.
     *          (input.eq.20 .or. refmacro.eq.1))  then
              z = 0.0
              call display2d(vd_id(2),si2im,si1im,
     *                 plane2d,z,scale,itype,sign,
     *                 zoom,zolf1,zolf2,zour1,zour2)
         endif
         max2d = 0.0
        else
         goto 124
        endif

      elseif (command.eq.'SYM') then
         if (dim.ne.2) goto 97
         if (si1im.ne.si2im) goto 129
         if (itype.ne.0) goto 96
         call message( 'Mean value (1) or smallest value (2) ? ')
         zz=2
         call getint2(zz,error)
         if (error.ne.0) goto 93
         call symetrize(plane2d,si2im,zz)
         refresh = 1
         max0 = 0.0

      elseif (command.eq.'ADDDATA') then
         if ( emptydata ) goto 95
         if (dimdat.ne.dim) goto 117
         if (sizedata.ne.sizeimo) goto 117
         if (dim.eq.1 .and.(itdata.ne.it1d)) goto 104
         if (dim.eq.2 .and.(itdata.ne.itype)) goto 104
         if (dim.eq.3 .and.(itdata.ne.it3d)) goto 104
         call addvect(imago(imdim(dim)),imago(imdim(dim)),
     *           datab,1.0,sizeimo)
         max0 = 0.0
         refresh = 1

      elseif (command.eq.'EXCHDATA') then
         if ( emptydata ) goto 95
         if (dimdat.ne.dim) goto 117
         if (sizedata.ne.sizeimo) goto 117
         if (dim.eq.1 .and.(itdata.ne.it1d)) goto 104
         if (dim.eq.2 .and.(itdata.ne.itype)) goto 104
         if (dim.eq.3 .and.(itdata.ne.it3d)) goto 104
         call excvect(imago(imdim(dim)),datab,sizeimo)
         max0 = 0.0
         refresh = 1

      elseif (command.eq.'MINDATA') then
         if ( emptydata ) goto 95
         if (dimdat.ne.dim) goto 117
         if (sizedata.ne.sizeimo) goto 117
         if (dim.eq.1 .and.(itdata.ne.it1d)) goto 104
         if (dim.eq.2 .and.(itdata.ne.itype)) goto 104
         if (dim.eq.3 .and.(itdata.ne.it3d)) goto 104
         call minvect(imago(imdim(dim)),imago(imdim(dim)),
     *           datab,sizeimo)
         max0 = 0.0
         refresh = 1

      elseif (command.eq.'MAXDATA') then
         if ( emptydata ) goto 95
         if (dimdat.ne.dim) goto 117
         if (sizedata.ne.sizeimo) goto 117
         if (dim.eq.1 .and.(itdata.ne.it1d)) goto 104
         if (dim.eq.2 .and.(itdata.ne.itype)) goto 104
         if (dim.eq.3 .and.(itdata.ne.it3d)) goto 104
         call maxvect(imago(imdim(dim)),imago(imdim(dim)),
     *           datab,sizeimo)
         max0 = 0.0
         refresh = 1

      elseif (command.eq.'MULTDATA') then
         if ( emptydata ) goto 95
         if (dimdat.ne.dim) goto 117
         if (sizedata.ne.sizeimo) goto 117
         if (dim.eq.1) then
            if (it1d.eq.0) then
               call diavect(column,column,datab,sizeimage1d)
             else
               call cdiavect(column,column,datab,sizeimage1d/2)
             endif
         else if (dim.eq.2) then
            if (itype.eq.0) then
               call diavect(plane2d,plane2d,datab,si1im*si2im)
            else if (itype.eq.1) then
               call cdiavect(plane2d,plane2d,datab,si1im*si2im/2)
            else if (itype.eq.2) then
               do i=1,si2im
                  call getcol(work1d,plane2d,i,si1im,si2im)
                  call getcol(work1d(si1im),datab,i,si1im,si2im)
                  call cdiavect(work1d,work1d,work1d(si1im),si1im/2)
                  call putcol(work1d,plane2d,i,si1im,si2im)
               enddo
            else
               call hdiavect(plane2d,plane2d,datab,si1im,si2im)
            endif
         else
            goto 91 
         endif
         max0 = 0.0
         refresh = 1

      elseif (command.eq.'GAUSS_CPX') then
         if (dim.eq.1) then
            if (it1d.ne.1) goto 143
            call gauscplx(column,sizeimage1d/2)
         endif
         max0 = 0.0
         refresh = 1

      elseif (command.eq.'TRANSPOSE') then
        if (dim.eq.2) then
          call powtwo(si1im,xx)
          if (xx.ne.0) goto 94
          call powtwo(si2im,xx)
          if (xx.ne.0) goto 94
          call transpose(plane2d,1,si1im,si2im,23)
          zz = si1im
          si1im = si2im
          si2im = zz
          z = specw1
          specw1 = specw2
          specw2 = z
          z = offset1
          offset1 = offset2
          offset2 = z
          z = freq1
          freq1 = freq2
          freq2 = z
          if (itype.eq.1) then
             itype = 2
          elseif (itype.eq.2) then
             itype = 1
          endif
          refresh = 1
        elseif (dim.eq.3) then
          call message( 'Transpose which way? (F12,F13 or F23)')
          zz=3
          call getdim(zz,error)
          if (error.ne.0) goto 93
          if (zz.eq.3) then
             call powtwo(si3d1,xx)
             if (xx.ne.0) goto 94
             call powtwo(si3d2,xx)
             if (xx.ne.0) goto 94
             yy = 12
          elseif (zz.eq.5) then
             call powtwo(si3d1,xx)
             if (xx.ne.0) goto 94
             call powtwo(si3d3,xx)
             if (xx.ne.0) goto 94
             yy = 13
          elseif (zz.eq.6) then
             call powtwo(si3d2,xx)
             if (xx.ne.0) goto 94
             call powtwo(si3d3,xx)
             if (xx.ne.0) goto 94
             yy = 23
          else
             goto 98
          endif
          call transpose(image,si3d1,si3d2,si3d3,yy)
          if (zz.eq.3) then
            zz = si3d1
            si3d1 = si3d2
            si3d2 = zz
            if ((it3d.eq.2) .or. (it3d.eq.3)) then
               it3d = it3d + 2
            elseif ((it3d.eq.4) .or. (it3d.eq.5)) then
               it3d = it3d - 2
            endif
            z = sp3d1
            sp3d2 = sp3d1
            sp3d2 = z
            z = of3d1
            of3d2 = of3d1
            of3d2 = z
            z = freq3d1
            freq3d1 = freq3d2
            freq3d2 = z
          elseif (zz.eq.5) then
            zz = si3d1
            si3d1 = si3d3
            si3d3 = zz
            if ((it3d.eq.1) .or. (it3d.eq.3)) then
               it3d = it3d + 3
            elseif ((it3d.eq.4) .or. (it3d.eq.6)) then
               it3d = it3d - 3
            endif
            z = sp3d1
            sp3d1 = sp3d3
            sp3d3 = z
            z = of3d1
            of3d3 = of3d1
            of3d3 = z
            z = freq3d1
            freq3d1 = freq3d3
            freq3d3 = z
          elseif (zz.eq.6) then
            zz = si3d3
            si3d3 = si3d2
            si3d2 = zz
            if (mod(it3d,4).eq.1) then
               it3d = it3d + 1
            elseif (mod(it3d,4).eq.2) then
               it3d = it3d - 1
            endif
            z = sp3d3
            sp3d3 = sp3d2
            sp3d2 = z
            z = of3d3
            of3d3 = of3d2
            of3d2 = z
            z = freq3d3
            freq3d3 = freq3d2
            freq3d2 = z
          endif
        else
           goto 124
        endif

      else if (command.eq.'MODIFYSIZE') then
           if (dim.eq.1) goto 124
           if (dim.eq.2) then  
             call message( 'Enter new sizes in F1 and F2:  ')
             zz=si1im
             call getint2(zz,error)
             if (error.ne.0) goto 93           
             if (zz.le.0) goto 98
             yy=si2im*si1im/zz
             call getint2(yy,error)
             if (error.ne.0) goto 93
             if (zz*yy.ne.sizeimage) goto 98
             si1im = zz
             si2im = yy
             zoom = 0
             refresh = 1
           elseif (dim.eq.3) then
              call message( 'enter new sizes in F1, F2 and F3:')
              zz=si3d1
              call getint2(zz,error)
              if (error.ne.0) goto 93           
              if (zz.le.0) goto 98
              yy=si3d2*si3d1/zz
              call getint2(yy,error)
              if (error.ne.0) goto 93
              xx=si3d3
              call getint2(xx,error)
              if (error.ne.0) goto 93
              if (zz*yy*xx.ne.sizeimage3d) goto 98
              si3d1 = zz
              si3d2 = yy
              si3d3 = xx
              write(st,*) 'new size are:',
     *          si3d1, si3d2, si3d3
              call gifaout(st)
              refresh = 1
           endif                            

C*******************************************************************
C         Various transforms
C*******************************************************************

      else if (command.eq.'CHSIZE') then
         if (dim.eq.1) then                   !case of 1d spectra
           call message( 'Enter new size :  ')
           zz = sizeimage1d
           call getint2(zz,error)
           if (error.ne.0) goto 93
           if (zz.gt.smxbig+smxmax+sizemax) goto 103
           call checkunderflow(zz,bool1)
           if (bool1) goto 90
           if (zz.lt.1) goto 98
           if (it1d.eq.1 .and. ((zz/2)*2.ne.zz)) goto 92
           call cp2vect(column,column,zz,sizeimage1d)
           sizeimage1d = zz                     
         else if (dim.eq.2) then              !case of 2d spectra
           call message( 'Enter new sizes in F1 and F2:  ')
           zz=si1im
           yy=si2im
           call getint2(zz,error)
           if (error.ne.0) goto 93           
           if (itype.ge.2 .and. ((zz/2)*2.ne.zz)) goto 92
           call getint2(yy,error)
           if (error.ne.0) goto 93
           if (mod(itype,2).eq.1 .and. ((yy/2)*2.ne.yy)) goto 92
           if ((zz.lt.1).or.(yy.lt.1))  goto 98
           if (zz*yy.gt.smxbig)  goto 103
           call checkunderflow(zz*yy,bool1)
           if (bool1) goto 90
           call zerofill(plane2d,zz,yy,si1im,si2im)
           si1im = zz
           si2im = yy
           sizeimage = si1im*si2im
         else if (dim.eq.3) then
           call message( 'Enter new sizes in F1, F2 and F3:')
           zz=si3d1
           yy=si3d2
           xx=si3d3

           call getint2(zz,error)
           if (error.ne.0) goto 93
           if (it3d.ge.4 .and. ((zz/2)*2.ne.zz)) goto 92

           call getint2(yy,error)
           if (error.ne.0) goto 93
           if (mod(it3d,4).ge.2 .and. ((yy/2)*2.ne.yy)) goto 92

           call getint2(xx,error)
           if (error.ne.0) goto 93
           if (mod(it3d,4).eq.1 .and. ((xx/2)*2.ne.xx)) goto 92

           if ((zz.lt.1).or.(yy.lt.1).or.(xx.lt.1))  goto 98
           if (zz*yy*xx.gt.smxbig)  goto 103
           call checkunderflow(zz*yy*xx,bool1)
           if (bool1) goto 90
c first zerofill F3
           call zerofill(image,si3d1*si3d2,xx,si3d1*si3d2,si3d3)
c then F1 & F2
           call zerofill(image,zz,yy*xx,si3d1,si3d2*xx)
           si3d1 = zz
           si3d2 = yy
           si3d3 = xx
           sizeimage3d = si3d1*si3d2*si3d3 
         endif
c         call gifaout('be careful, spectrum width and offset 
c     * have not been changed')
         max0 = 0.0
         refresh = 1

      else if (command.eq.'ABS') then
         if (dim .eq. 1) then
          if (it1d.ne.0) go to 96
         elseif (dim .eq. 2) then
          if (itype.ne.0) go to 96
         elseif (dim .eq. 3) then
          if (it3d.ne.0) go to 96
         endif
       call absvect(imago(imdim(dim)),imago(imdim(dim)),sizeimo) 
       refresh=1

      else if (command.eq.'PLUS') then
         if (dim .eq. 1) then
          if (it1d.ne.0) go to 96
         elseif (dim .eq. 2) then
          if (itype.ne.0) go to 96
         elseif (dim .eq. 3) then
          if (it3d.ne.0) go to 96
         endif
         do i=1,sizeimo
            imago(imdim(dim)+i-1) = max(imago(imdim(dim)+i-1),0.0)
            max0 = 0.0
         enddo
         refresh=1

      else if (command.eq.'MINUS') then
         if (dim .eq. 1) then
          if (it1d.ne.0) go to 96
         elseif (dim .eq. 2) then
          if (itype.ne.0) go to 96
         elseif (dim .eq. 3) then
          if (it3d.ne.0) go to 96
         endif
         do i=1,sizeimo
            imago(imdim(dim)+i-1) = min(imago(imdim(dim)+i-1),0.0)
            max0 = 0.0
         enddo
         refresh=1

      else if (command.eq.'ZEROING') then
          z = noise
          call message('Under threshold value :')
          call getreal2(z,error)
          if (error.ne.0 .or. z.le.0.0) goto 93
          do i=1,sizeimo
            if (abs(imago(imdim(dim)+i-1)).lt.z)
     *          imago(imdim(dim)+i-1) = 0.0
          enddo
          refresh=1

      elseif (command.eq.'REVERSE') then
         if (dim.eq.1) then
           if (it1d.eq.0) then
              call revvect(column,sizeimage1d)
           else
              call crevvect(column,sizeimage1d/2)
           endif
         elseif (dim.eq.2) then
           call message( 'in F1, F2, or F12 ? ')
           zz=3
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (zz.eq.2 .or. zz.eq.3) then		! in F2
             if (.not. checkdim(2,itype,2)) then
               do i=0,si1im-1
                 call revvect(plane2d(i*si2im+1),si2im)
               enddo
             else
               do i=0,si1im-1
                call crevvect(plane2d(i*si2im+1),si2im/2)
               enddo
             endif
           endif
           if (zz.eq.1 .or. zz.eq.3) then		! in F1
             if (.not. checkdim(1,itype,2)) then
                do i=1,si2im
                 call getcol(work1d,plane2d,i,si1im,si2im)
                 call revvect(work1d,si1im)
                 call putcol(work1d,plane2d,i,si1im,si2im)
                enddo
             else
                do i=1,si2im
                 call getcol(work1d,plane2d,i,si1im,si2im)
                 call crevvect(work1d,si1im/2)
                 call putcol(work1d,plane2d,i,si1im,si2im)
                enddo
             endif
           endif
         else                   ! in F3
           call message( 'in F1, F2, F3, or combination? ')
           zz=3
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (zz.ge.4) then				! in F3
             if (.not. checkdim(4,it3d,3)) then
               do i=0,si3d1*si3d2-1
                call revvect(image(i*si3d3+1),si3d3)
               enddo
             else
               do i=0,si3d1*si3d2-1
                call crevvect(image(i*si3d3+1),si3d3/2)
               enddo
             endif
           endif
           if (mod(zz/2,2).eq.1) then			! in F2
             if (.not. checkdim(2,it3d,3)) then
                do i=1,si3d1
                  do j=1,si3d3
                    call getcol(work1d,image((i-1)*si3d3*si3d2+1),
     *                          j,si3d2,si3d3)
                    call revvect(work1d,si3d2)
                    call putcol(work1d,image((i-1)*si3d3*si3d2+1),
     *                          j,si3d2,si3d3)
                  enddo
                enddo
             else
                do i=1,si3d1
                  do j=1,si3d3
                    call getcol(work1d,image((i-1)*si3d3*si3d2+1),
     *                          j,si3d2,si3d3)
                    call crevvect(work1d,si3d2/2)
                    call putcol(work1d,image((i-1)*si3d3*si3d2+1),
     *                          j,si3d2,si3d3)
                  enddo
                enddo
             endif
           endif
           if (mod(zz,2).eq.1) then			! in F1
             if (.not. checkdim(1,it3d,3)) then
               do i=1,si3d2*si3d3
                 call getcol(work1d,image,i,si3d1,si3d2*si3d3)
                 call revvect(work1d,si3d1)
                 call putcol(work1d,image,i,si3d1,si3d2*si3d3)
               enddo
             else
               do i=1,si3d2*si3d3
                 call getcol(work1d,image,i,si3d1,si3d2*si3d3)
                 call crevvect(work1d,si3d1/2)
                 call putcol(work1d,image,i,si3d1,si3d2*si3d3)
               enddo
             endif
           endif  ! on zz
         endif    ! on dim
         refresh=1

      elseif (command.eq.'DSA') then
         call message( 'Enter number of points to shift by:  ')
         zz = 2
         call getint2(zz,error)
         if (error.ne.0) goto 93
         call message( 'Enter factor:  ')
         z = -1.0
         call getreal2(z,error)
         if (error.ne.0) goto 93
         if (zz.lt.1) goto 98
         if (dim.eq.1) then
           if (zz.ge.sizeimage1d) goto 98
           call dsavect(column,sizeimage1d,zz,z)
         elseif (dim.eq.2) then
           if (zz.ge.si2im) goto 98
           do i=1,si1im
             call dsavect(plane2d(si2im*(i-1)+1),si2im,zz,z)
           enddo
         elseif (dim.eq.3) then
           if (zz.ge.si3d3) goto 98
           do i=1,si3d1*si3d2
             call dsavect(image(si3d3*(i-1)+1),si3d3,zz,z)
           enddo
         endif
         max0=0.0
         refresh = 1
 
      else if (command.eq.'INVF') then
         if (dim.eq.1) then
           call bru2vect(column,column,sizeimage1d)
         else if (dim.eq.2)  then
           call message( 'Invf in F1, F2 or F12 ? ')
           zz=2
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (zz.eq.2 .or. zz.eq.3) then
              call bru2vect(plane2d,plane2d,sizeimage)
           endif
           if (zz.eq.1 .or. zz.eq.3) then
              do zz = 1,si1im-1,2
                call mltvect( plane2d(si2im*(zz)+1),
     *                plane2d(si2im*(zz)+1),-1.0,si2im)
              enddo
           endif
         else if (dim.eq.3)  then
           call message('revf in F1, F2, or F3')
           zz=4
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (zz.eq.4) then                      
              call bru2vect(image,image,sizeimage3d)
           else if (zz.eq.2) then
              do zz = 1, si3d1*si3d2-1,2
                call mltvect( image(si3d3*(zz)+1),
     *                image(si3d3*(zz)+1),-1.0,si3d3)
              enddo        
           else if (zz.eq.1) then
              do zz = 1, si3d1-1,2
                call mltvect( image(si3d3*si3d2*(zz)+1),
     *                image(si3d3*si3d2*(zz)+1),-1.0,si3d3*si3d2)
              enddo        
           else
              goto 98      
           endif
         endif
         refresh = 1

      else if (command.eq.'REVF') then
         if (dim.eq.1) then
           call bruvect(column,column,sizeimage1d)
         else if (dim.eq.2)  then
           call message( 'Revf in F1, F2 or F12 ? ')
           zz=2
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (zz.eq.2 .or. zz.eq.3) then
              call bruvect(plane2d,plane2d,sizeimage)
           endif
           if (zz.eq.1 .or. zz.eq.3) then
              do zz = 2, si1im,4
                call mltvect( plane2d(si2im*(zz)+1),
     *                plane2d(si2im*(zz)+1),-1.0,2*si2im)
              enddo
           endif
         else if (dim.eq.3)  then
           call message('revf in F1, F2, or F3')
           zz=4
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (zz.eq.4) then                      
              call bruvect(image,image,sizeimage3d)
           else if (zz.eq.2) then
              do zz = 2, si3d1*si3d2,4
                call mltvect( image(si3d3*(zz)+1),
     *                image(si3d3*(zz)+1),-1.0,2*si3d3)
              enddo        
           else if (zz.eq.1) then
              do zz = 2, si3d1,4
                call mltvect( image(si3d3*si3d2*(zz)+1),
     *                image(si3d3*si3d2*(zz)+1),-1.0,2*si3d3*si3d2)
              enddo        
           else
              goto 98      
           endif
         endif
         refresh = 1

      else if (command.eq.'SWA') then
         if (dim.eq.1) then
           if (it1d.ne.0) goto 104
           call powtwo(sizeimage1d,zz)
           if (zz.ne.0) goto 94
           call swavect(column,sizeimage1d)
           it1d = 1
         else if (dim.eq.2)  then
           call message( 'Swa in F1, F2, or F12 ? ')
           zz=2
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (zz.eq.2 .or. zz.eq.3) then
              if (itype.eq.1 .or. itype.eq.3) goto 104
              call powtwo(si2im,zz)
              if (zz.ne.0) goto 94
              do i=0,si1im-1
                 call swavect(plane2d(si2im*i+1),si2im)
              enddo
              itype = itype+1
           endif
           if (zz.eq.1 .or. zz.eq.3) then
              if (itype.eq.2 .or. itype.eq.3) goto 104
              call powtwo(si1im,zz)
              if (zz.ne.0) goto 94
              do i=1,si2im
                 call getcol(work1d,plane2d,i,si1im,si2im)
                 call swavect(work1d,si1im)
                 call putcol(work1d,plane2d,i,si1im,si2im)
              enddo
              itype = itype+2
           endif
         else if (dim.eq.3)  then
            goto 91
         endif
         refresh = 1


      else if (command.eq.'USWA') then
         if (dim.eq.1) then
           if (it1d.ne.1) goto 104
           call powtwo(sizeimage1d,zz)
           if (zz.ne.0) goto 94
           call uswvect(column,sizeimage1d)
           it1d = 0
         else if (dim.eq.2)  then
           call message( 'USwa in F1, F2, or F12 ? ')
           zz=2
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (zz.eq.2 .or. zz.eq.3) then
              if (itype.eq.0 .or. itype.eq.2) goto 104
              call powtwo(si2im,zz)
              if (zz.ne.0) goto 94
              do i=0,si1im-1
                 call uswvect(plane2d(si2im*i+1),si2im)
              enddo
              itype = itype-1
           endif
           if (zz.eq.1 .or. zz.eq.3) then
              if (itype.eq.0 .or. itype.eq.1) goto 104
              call powtwo(si1im,zz)
              if (zz.ne.0) goto 94
              do i=1,si2im
                 call getcol(work1d,plane2d,i,si1im,si2im)
                 call uswvect(work1d,si1im)
                 call putcol(work1d,plane2d,i,si1im,si2im)
              enddo
              itype = itype-2
           endif
         else if (dim.eq.3)  then
            goto 91
         endif
         refresh = 1

      else if (command.eq.'FLIP') then
        if (dim.eq.1) goto 124
        if (dim.eq.2) then
           if (itype.ne.1) goto 104
           call powtwo(si2im,zz)
           if (zz.ne.0 .or. si2im.lt.2) goto 94
           do i=1,si1im 
              call uswvect(plane2d((i-1)*si2im+1),si2im) 
           enddo
           si2im = si2im/2
           si1im = si1im*2                        
           write (st,*) 'Si1im:',si1im,'  Si2:',si2im  
           call gifaout(st)
           itype = 2
        else if (dim.eq.3) then
           call message('flip F3 with which dim (F1/F2) ? ')
           zz = 2
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (zz.eq.2) then
              if (it3d.ne.1 .and. it3d.ne.5) goto 108
              call powtwo(si3d3,xx)
              if (xx.ne.0 .or. si3d3.lt.2) goto 94
              do i=1,si3d1
               do j=1,si3d2  
                call uswvect(
     *          image((i-1)*si3d2*si3d3+(j-1)*si3d3+1),si3d3) 
               enddo
              enddo
              si3d3 = si3d3/2
              si3d2 = si3d2*2                        
              it3d = it3d + 1
           elseif (zz.eq.1) then
              if (it3d.ne.1 .and. it3d.ne.3) goto 108
              call powtwo(si3d3,xx)
              if (xx.ne.0 .or. si3d3.lt.2) goto 94
              do i=1,si3d1
                call uswvect(image((i-1)*si3d2*si3d3+1),si3d3*si3d2) 
              enddo
              si3d3 = si3d3/2
              si3d1 = si3d1*2                        
              it3d = it3d + 3
           endif 
           write (st,*) 'Si1 :',si3d1,' Si2 : ',
     *            si3d2,' Si3 : ',si3d3
           call gifaout(st)
        endif

      else if (command.eq.'FLOP') then
        if (dim.eq.1) goto 124
        if (dim.eq.2) then
           if (itype.ne.2) goto 104 
           call powtwo(si2im,zz)
           if (zz.ne.0) goto 94  
           if (mod(si1im,2) .ne. 0) goto 92 
           do i=1,si1im/2 
               call swavect(plane2d((i-1)*si2im*2+1),si2im*2) 
           enddo
           si2im = si2im*2
           if (si1im.ge.2) si1im = si1im/2
           write (st,*) 'Si1im:',si1im,'  Si2:',si2im 
           call gifaout(st)
           itype = 1
        elseif (dim.eq.3) then
           call message('flop F3 with which dim (F1/F2) ??')
           zz = 2
           call getdim(zz,error)
           if (error.ne.0) goto 93                  
           if (zz.eq.2) then
              if (it3d.ne.2 .and. it3d.ne.6) goto 104 
              call powtwo(si3d3,xx)
              if (xx.ne.0) goto 94  
              if (mod(si3d2,2) .ne. 0) goto 92 
              do j=1,si3d1 
                do i=1,si3d2/2 
                  call swavect(image((j-1)*si3d2*si3d3+
     *                           (i-1)*si3d3*2+1),si3d3*2) 
                enddo
              enddo
              si3d3 = si3d3*2
              if (si3d2.ge.2) si3d2 = si3d2/2
              it3d = it3d-1
           elseif (zz.eq.1) then
              if (it3d.ne.4 .and. it3d.ne.6) goto 104 
              call powtwo(si3d3,xx)
              if (xx.ne.0) goto 94  
              if (mod(si3d1,2) .ne. 0) goto 92 
              do j=1,si3d1/2 
               call swavect(image((j-1)*si3d2*si3d3*2+1),si3d2*si3d3*2) 
              enddo
              si3d3 = si3d3*2
              if (si3d1.ge.2) si3d1 = si3d1/2
              it3d = it3d-3
           endif
           write (st,*) 'Si1 :',si3d1,' Si2 : ',
     *            si3d2,' Si3 : ',si3d3
           call gifaout(st)
        endif

C******************************************************************
C       FOURIER TRANSFORM
C******************************************************************
      else if (command.eq.'RFT') then !real ft for 3d spectra
         if (dim.eq.1) then                   !case of 1d spectra
           if (it1d.ne.0) goto 104
           call powtwo(sizeimage1d,zz)
           if (zz.ne.0) goto 94
           call fftreal(column,sizeimage1d,1,1)
           it1d=1
         else if (dim.eq.2) then         !case of 2d spectra
           call message( 'rft in F1, F2, or F12 ? ')
           zz=2 
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (zz.eq.1) then
             if (itype.ne.0 .and. itype.ne.1) goto 104
             call powtwo(si1im,xx)
             if (xx.ne.0) goto 94
             call tdftreal(plane2d,si1im,si2im,1,1,2) !calculates rft1
             itype = min(3,itype+2)
           else if (zz.eq.2) then
             if (itype.ne.0 .and. itype.ne.2) goto 104
             call powtwo(si2im,xx)
             if (xx.ne.0) goto 94
             call tdftreal(plane2d,si1im,si2im,1,1,1) !calculates rft2
             itype = min(3,itype+1)
           else if (zz.eq.3) then
             if (itype.ne.0) goto 104
             call powtwo(si1im,xx)
             call powtwo(si2im,yy)
             if (xx.ne.0 .or. yy.ne.0) goto 94
             call tdftreal(plane2d,si1im,si2im,1,1,3) !calculates rftb
             itype = 3
           else
             goto 98
           endif
         else if (dim.eq.3) then        
           call message( 'rft in F1, F2, F3
     * and corresponding combinations')
           zz=4 
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (mod(zz,2).eq.1) then
             if (it3d/4.ne.0) goto 104
             call powtwo(si3d1,xx)
             if (xx.ne.0) goto 94
             call tdftreal(image,si3d1,si3d2*si3d3,
     *                                       1,1,2) !calculates rft1
             it3d = min(7,it3d+4)
           endif
           if (zz/2.eq.1) then
             if (mod(it3d/2,2).ne.0) goto 104
             call powtwo(si3d2,xx)
             if (xx.ne.0) goto 94                 
             do i=1,si3d1
                call tdftreal(image((i-1)*si3d2*si3d3+1),
     *                            si3d2,si3d3,1,1,2) !calculates rft2
             enddo
             it3d = min(7,it3d+2)
           else if (zz/2.eq.2) then
             if (mod(it3d,2).ne.0) goto 104
             call powtwo(si3d3,xx)
             if (xx.ne.0) goto 94
             call tdftreal(image,si3d1*si3d2,si3d3,1,1,1) !calculates rftb
             it3d = min(7,it3d+1)
           else if (zz/2.eq.3) then
             if (mod(it3d,4).ne.0) goto 104
             call powtwo(si3d2,xx)
             call powtwo(si3d3,yy)
             if (xx.ne.0 .or. yy.ne.0) goto 94
             do i=1,si3d1
                call tdftreal(image((i-1)*si3d2*si3d3+1),
     *                            si3d2,si3d3,1,1,3) !calculates rftb
             enddo
             it3d = min(7,it3d+3)
           else if (mod(zz,2).ne.1) then
             goto 98
           endif
         endif
         max0=0.0
         refresh = 1

      else if (command.eq.'IFT') then !ift for 3d spectra
         if (dim.eq.1) then                   !case of 1d spectra
           if (it1d.ne.1) goto 143
           call powtwo(sizeimage1d,zz)
           if (zz.ne.0) goto 94
           call fftcpx(column,sizeimage1d/2,-1)
           call mltvect(column,column,2.0/float(sizeimage1d),
     *                  sizeimage1d)  
         else if (dim.eq.2) then   !case of 2d spectra
           call message( 'ift in F1, F2, or F12 ? ')
           zz=3 
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (zz.eq.1) then
             if (itype.ne.2 .and. itype.ne.3) goto 143
             call powtwo(si1im,xx)
             if (xx.ne.0) goto 94
             call tdftcpx(plane2d,si1im,si2im,-1,2) !calculates ift1
             call mltvect(plane2d,plane2d,2.0/float(si1im),sizeimage)
           else if (zz.eq.2) then
             if (itype.ne.1 .and. itype.ne.3) goto 143
             call powtwo(si2im,xx)
             if (xx.ne.0) goto 94
             call tdftcpx(plane2d,si1im,si2im,-1,1) !calculates ift2
             call mltvect(plane2d,plane2d,2.0/float(si2im),sizeimage)
           else if (zz.eq.3) then
             if (itype.ne.3) goto 143
             call powtwo(si2im,xx)
             call powtwo(si1im,yy)
             if (xx.ne.0 .or. yy.ne.0) goto 94
             call tdftcpx(plane2d,si1im,si2im,-1,3) !calculates iftb
             call mltvect(plane2d,plane2d,
     *                   4.0/float(sizeimage),sizeimage)
           else 
             goto 98
           endif           
         else if (dim.eq.3) then           
           call message( 'ift in F1, F2, F3
     * and corresponding combinations')
           zz=4 
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (mod(zz,2).eq.1) then
             if (it3d/4.ne.1) goto 143
             call powtwo(si3d1,xx)
             if (xx.ne.0) goto 94
             call tdftcpx(image,si3d1,si3d2*si3d3,-1,2) !calculates ift1
             call mltvect(image,image,2.0/float(si3d1),sizeimage3d)
           endif
           if (zz/2.eq.1) then
             if (mod(it3d/2,2).ne.1) goto 143
             call powtwo(si3d2,xx)
             if (xx.ne.0) goto 94                 
             do i=1,si3d1
                call tdftcpx(image((i-1)*si3d2*si3d3+1),  
     *                       si3d2,si3d3,-1,2) !calculates ift2
             enddo
             call mltvect(image,image,2.0/float(si3d2),sizeimage3d)
           else if (zz/2.eq.2) then
             if (mod(it3d,2).ne.1) goto 143
             call powtwo(si3d3,xx)
             if (xx.ne.0) goto 94
             call tdftcpx(image,si3d1*si3d2,si3d3,-1,1) 
             call mltvect(image,image,2.0/float(si3d3),sizeimage3d)
           else if (zz/2.eq.3) then
             if (mod(it3d,4).ne.3) goto 143
             call powtwo(si3d2,xx)
             call powtwo(si3d3,yy)
             if (xx.ne.0 .or. yy.ne.0) goto 94
             do i=1,si3d1
                call tdftcpx(image((i-1)*si3d2*si3d3+1),
     *                                si3d2,si3d3,-1,3) 
             enddo
             call mltvect(image,image,
     *           2.0/float(si3d2*si3d3),sizeimage3d)
           else if (mod(zz,2).ne.1) then
             goto 98
           endif
         endif
         max0=0.0
         refresh = 1

      else if (command.eq.'IRFT') then !real ift for 3d spectra
         if (dim.eq.1) then            !case of 1d spectra
           if (it1d.ne.1) goto 143
           call powtwo(sizeimage1d,zz)
           if (zz.ne.0) goto 94
           call fftreal(column,sizeimage1d,-1,-1)
           call mltvect(column,column,2.0/float(sizeimage1d),
     *                  sizeimage1d)
           it1d = 0
         else if (dim.eq.2) then        !case of 2d spectra
           call message( 'irft in F1, F2, or F12 ? ')
           zz=3 
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (zz.eq.1) then
             if (itype.ne.2 .and. itype.ne.3) goto 143
             call powtwo(si1im,xx)
             if (xx.ne.0) goto 94
             call tdftreal(plane2d,si1im,si2im,-1,-1,2) !calculates irft1
             call mltvect(plane2d,plane2d,2.0/float(si1im),sizeimage)
             itype = max(0,itype-2)
           else if (zz.eq.2) then
             if (itype.ne.1 .and. itype.ne.3) goto 143
             call powtwo(si2im,xx)
             if (xx.ne.0) goto 94
             call tdftreal(plane2d,si1im,si2im,-1,-1,1) !calculates irft2
             call mltvect(plane2d,plane2d,2.0/float(si2im),sizeimage)
             itype = max(0,itype-1)
           else if (zz.eq.3) then
             if (itype.ne.3) goto 143
             call powtwo(si1im,xx)
             call powtwo(si2im,yy)
             if (xx.ne.0 .or. yy.ne.0) goto 94
             call tdftreal(plane2d,si1im,si2im,-1,-1,3) !calculates irftb
             call mltvect(plane2d,plane2d,
     *                   4.0/float(sizeimage),sizeimage)
             itype = 0                  
           else 
             goto 98
           endif           
         else if (dim.eq.3) then            ! case of 3d spectra
           call message( 'irft in F1, F2, F3
     * and corresponding combinations')
           zz=4 
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (mod(zz,2).eq.1) then
             if (it3d/4.ne.1) goto 143
             call powtwo(si3d1,xx)
             if (xx.ne.0) goto 94
             call tdftreal(image,si3d1,si3d2*si3d3,-1,-1,2) !calculates irft1
             call mltvect(image,image,2.0/float(si3d1),sizeimage3d)
             it3d = max(0,it3d-4)
           endif
           if (zz/2.eq.1) then
             if (mod(it3d/2,2).ne.1) goto 143
             call powtwo(si3d2,xx)
             if (xx.ne.0) goto 94                 
             do i=1,si3d1
                call tdftreal(image((i-1)*si3d2*si3d3+1),  
     *                       si3d2,si3d3,-1,-1,2) !calculates irft2
             enddo
             call mltvect(image,image,2.0/float(si3d2),sizeimage3d)
             it3d = max(0,it3d-2)
           else if (zz/2.eq.2) then
             if (mod(it3d,2).ne.1) goto 143
             call powtwo(si3d3,xx)
             if (xx.ne.0) goto 94
             call tdftreal(image,si3d1*si3d2,si3d3,-1,-1,1) !calculates irft3
             call mltvect(image,image,2.0/float(si3d3),sizeimage3d)
             it3d = max(0,it3d-1)
           else if (zz/2.eq.3) then
             if (mod(it3d,4).ne.3) goto 143
             call powtwo(si3d2,xx)
             call powtwo(si3d3,yy)
             if (xx.ne.0 .or. yy.ne.0) goto 94
             do i=1,si3d1
                call tdftreal(image((i-1)*si3d2*si3d3+1),
     *                                si3d2,si3d3,-1,-1,3) 
             enddo
             call mltvect(image,image,
     *                   2.0/float(si3d2*si3d3),sizeimage3d)
             it3d = max(0,it3d-3)
           else if (mod(zz,2).ne.1) then
             goto 98
           endif
         endif
         max0=0.0
         refresh = 1

      else if (command.eq.'FT') then ! ft for 3d spectra
         if (dim.eq.1) then               !case of 1d spectra
           if (it1d.ne.1) goto 143
           call powtwo(sizeimage1d,zz)
           if (zz.ne.0) goto 94
           call fftcpx(column,sizeimage1d/2,1)
         else  if (dim.eq.2) then                !case of 2d spectra
           call message( 'ft in F1, F2, or F12 ? ')
           zz=3 
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (zz.eq.1) then
             if (itype.ne.2 .and. itype.ne.3) goto 143
             call powtwo(si1im,xx)
             if (xx.ne.0) goto 94
             call tdftcpx(plane2d,si1im,si2im,1,2) !calculates ft1
           else if (zz.eq.2) then
             if (itype.ne.1 .and. itype.ne.3) goto 143
             call powtwo(si2im,xx)
             if (xx.ne.0) goto 94
             call tdftcpx(plane2d,si1im,si2im,1,1) !calculates ft2
           else if (zz.eq.3) then
             if (itype.ne.3) goto 143
             call powtwo(si2im,xx)
             call powtwo(si1im,yy)
             if (xx.ne.0 .or. yy.ne.0) goto 94
             call tdftcpx(plane2d,si1im,si2im,1,3) !calculates ftb
           else           
             goto 98
           endif                         
         else if (dim.eq.3) then     ! case of 3d spectra
           call message( 'ft in F1, F2, F3
     * and corresponding combinations')
           zz=4 
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (mod(zz,2).eq.1) then  !in F1
             if (it3d/4.ne.1) goto 143
             call powtwo(si3d1,xx)
             if (xx.ne.0) goto 94
             call tdftcpx(image,si3d1,si3d2*si3d3,1,2) !calculates ft1
           endif
           if (zz/2.eq.1) then       ! in F2
             if (mod(it3d/2,2).ne.1) goto 143
             call powtwo(si3d2,xx)
             if (xx.ne.0) goto 94                 
             do i=1,si3d1
                call tdftcpx(image((i-1)*si3d2*si3d3+1),  
     *                       si3d2,si3d3,1,2) !calculates ft2
             enddo
           else if (zz/2.eq.2) then     !calculates ft3
             if (mod(it3d,2).ne.1) goto 143
             call powtwo(si3d3,xx)
             if (xx.ne.0) goto 94
             call tdftcpx(image,si3d1*si3d2,si3d3,1,1) 
           else if (zz/2.eq.3) then      !in F2 and 3
             if (mod(it3d,4).ne.3) goto 143
             call powtwo(si3d2,xx)
             call powtwo(si3d3,yy)
             if (xx.ne.0 .or. yy.ne.0) goto 94
             do i=1,si3d1
                call tdftcpx(image((i-1)*si3d2*si3d3+1),
     *                                si3d2,si3d3,1,3) 
             enddo
           else if (mod(zz,2).ne.1) then               
             goto 98
           endif
         endif                                        
         max0=0.0
         refresh = 1

      else if (command.eq.'FTBIS') then  !real ft for 1d or 2d spectra
         if (dim.eq.1) then                   !case of 1d spectra
           if (it1d.ne.1) goto 143
           call powtwo(sizeimage1d,zz)
           if (zz.ne.0) goto 94
           call fftreal(column,sizeimage1d,1,-1)
           it1d = 0
         else if (dim.eq.2) then    !case of 2d spectra
           call message( 'ftbis in F1, F2, or F12')
           zz=3 
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (zz.eq.1) then
             if (itype.ne.2 .and. itype.ne.3) goto 143
             call powtwo(si1im,xx)
             if (xx.ne.0) goto 94
             call tdftreal(plane2d,si1im,si2im,1,-1,2) !calculates rft1
             itype = max(0,itype-2)
           else if (zz.eq.2) then
             if (itype.ne.1 .and. itype.ne.3) goto 143
             call powtwo(si2im,xx)
             if (xx.ne.0) goto 94
             call tdftreal(plane2d,si1im,si2im,1,-1,1) !calculates rft2
             itype = max(0,itype-1)
           else if (zz.eq.3) then
             if (itype.ne.3) goto 143
             call powtwo(si2im,xx)
             call powtwo(si1im,yy)
             if (xx.ne.0 .or. yy.ne.0) goto 94
             call tdftreal(plane2d,si1im,si2im,1,-1,3) !calculates rftb
             itype = 0
           else   
             goto 98                    
           endif           
         else if (dim.eq.3) then     ! case of 3d spectra
           call message( 'ft in F1, F2, F3
     * and corresponding combinations')
           zz=4 
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (mod(zz,2).eq.1) then
             if (it3d/4.ne.1) goto 143
             call powtwo(si3d1,xx)
             if (xx.ne.0) goto 94
             call tdftreal(image,si3d1,si3d2*si3d3,1,-1,2) !calculates rft1
             it3d = max(0,it3d-4)
           endif                
           if (zz/2.eq.1) then
             if (mod(it3d/2,2).ne.1) goto 143
             call powtwo(si3d2,xx)
             if (xx.ne.0) goto 94                 
             do i=1,si3d1
                call tdftreal(image((i-1)*si3d2*si3d3+1),  
     *                       si3d2,si3d3,1,-1,2) !calculates rft2
             enddo
             it3d = max(0,it3d-2)
           else if (zz/2.eq.2) then
             if (mod(it3d,2).ne.1) goto 143
             call powtwo(si3d3,xx)
             if (xx.ne.0) goto 94
             call tdftreal(image,si3d1*si3d2,si3d3,1,-1,1) !calculates rft3
             it3d = max(0,it3d-1)
           else if (zz/2.eq.3) then
             if (mod(it3d,4).ne.3) goto 143
             call powtwo(si3d2,xx)
             call powtwo(si3d3,yy)
             if (xx.ne.0 .or. yy.ne.0) goto 94
             do i=1,si3d1
                call tdftreal(image((i-1)*si3d2*si3d3+1),
     *                                si3d2,si3d3,1,-1,3) !calculates rftb
             enddo
             it3d = max(0,it3d-3)
           else if (mod(zz,2).ne.1) then
             goto 98
           endif
         endif            
         max0=0.0
         refresh = 1           
        
      else if (command.eq.'IFTBIS') then  !iftbis for 3d spectra
         if (dim.eq.1) then                      !case of 1d spectra
           if (it1d.ne.0) goto 104
           call powtwo(sizeimage1d,zz)
           if (zz.ne.0) goto 94
           call fftreal(column,sizeimage1d,-1,1)     
           call mltvect(column,column,2.0/float(sizeimage1d),
     *                  sizeimage1d)
           it1d = 1
         else if (dim.eq.2) then !case of 2d spectra
           call message( 'iftbis in F1, F2, or F12 ?')
           zz=3 
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (zz.eq.1) then
             if (itype.ne.0 .and. itype.ne.1) goto 104
             call powtwo(si1im,xx)
             if (xx.ne.0) goto 94
             call tdftreal(plane2d,si1im,si2im,-1,1,2) !calculates irft1
             call mltvect(plane2d,plane2d,2.0/float(si1im),sizeimage)
             itype = min(3,itype+2)
           else if (zz.eq.2) then
             if (itype.ne.0 .and. itype.ne.2) goto 104
             call powtwo(si2im,xx)
             if (xx.ne.0) goto 94
             call tdftreal(plane2d,si1im,si2im,-1,1,1) !calculates irft2
             call mltvect(plane2d,plane2d,2.0/float(si2im),sizeimage)
             itype = min(3,itype+1)
           else if (zz.eq.3) then
             if (itype.ne.0) goto 104
             call powtwo(si2im,xx)
             call powtwo(si1im,yy)
             if (xx.ne.0 .or. yy.ne.0) goto 94
             call tdftreal(plane2d,si1im,si2im,-1,1,3) !calculates irftb
             call mltvect(plane2d,plane2d,
     *                  4.0/float(sizeimage),sizeimage)
             itype = 3
           else
             goto 98
           endif           
         else if (dim.eq.3) then
           call message( 'iftbis in F1, F2, F3
     * and corresponding combinations')
           zz=4 
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (mod(zz,2).eq.1) then
             if (it3d/4.ne.0) goto 104
             call powtwo(si3d1,xx)
             if (xx.ne.0) goto 94
             call tdftreal(image,si3d1,si3d2*si3d3,-1,1,2) !calculates irft1
             call mltvect(image,image,2.0/float(si3d1),sizeimage3d)
             it3d = min(7,it3d+4)
           endif
           if (zz/2.eq.1) then
             if (mod(it3d/2,2).ne.0) goto 104
             call powtwo(si3d2,xx)
             if (xx.ne.0) goto 94                 
             do i=1,si3d1
                call tdftreal(image((i-1)*si3d2*si3d3+1),
     *                            si3d2,si3d3,-1,1,2) !calculates irft2
             enddo
             call mltvect(image,image,2.0/float(si3d2),sizeimage3d)
             it3d = min(7,it3d+2)
           else if (zz/2.eq.2) then
             if (mod(it3d,2).ne.0) goto 104
             call powtwo(si3d3,xx)
             if (xx.ne.0) goto 94
             call tdftreal(image,si3d1*si3d2,si3d3,-1,1,1) !calculates irft3
             call mltvect(image,image,2.0/float(si3d3),sizeimage3d)
             it3d = min(7,it3d+1)
           else if (zz/2.eq.3) then
             if (mod(it3d,4).ne.0) goto 104
             call powtwo(si3d2,xx)
             call powtwo(si3d3,yy)
             if (xx.ne.0 .or. yy.ne.0) goto 94
             do i=1,si3d1
                call tdftreal(image((i-1)*si3d2*si3d3+1),
     *                            si3d2,si3d3,-1,1,3) !calculates irftb
             enddo
             call mltvect(image,image,
     *                  2.0/float(si3d2*si3d3),sizeimage3d)
             it3d = min(7,it3d+3)
           else if (mod(zz,2).ne.1) then
             goto 98
           endif
         endif
         max0=0.0
         refresh = 1
C******************************************************************

      else if (command.eq.'MULT') Then
        call message( 'multiply data-set by a scalar')
        z=1.0              
        call getreal2(z,error)
        if (error.ne.0) goto 93   
        call mltvect(imago(imdim(dim)),imago(imdim(dim)),z,sizeimo) 
        max0 = 0.0
        refresh=1

      elseif (command.eq.'EVALN') then
         if (dim.eq.1) then
c           if (it1d.ne.0) goto 96
           call message( 'Enter points to calculate noise:  ')
           call getrect(win1,win3,win2,win4,error)
           if (error.ne.0) goto 90
           call evaln(column(win1),win2-win1+1,noise,shift)
         else if (dim.eq.2) then
c           if (itype.ne.0) goto 96
           call evaln2d(plane2d,si1im,si2im,noise,shift,error)
         else 
           goto 124
         endif
         if (verbose.ne.0) call gifaoutr( 'Noise value: ',noise)
         if (verbose.ne.0) call gifaoutr( 'Overall offset:',shift )

      else if (command.eq.'ADDBASE') Then
        call message( 'enter vertical offset to substract:')
        z=shift                               
        call getreal2(shift,error)
        if (error.ne.0) goto 93   
        call adrvect(imago(imdim(dim)),-shift,sizeimo) 
        shift=z-shift
        max0=0.0
        refresh=1

      else if (command.eq.'PHASE') then
         call message( 'enter phase correction ph0 and ph1')
         call message('values are in degree')   
         call getreal2(ph0,error)
         if (error.ne.0) goto 93
         call getreal2(ph1,error)
         if (error.ne.0) goto 93
         if (dim.eq.1) then
             if (it1d.ne.1) goto 143
             call phase(column,ph0,ph1,sizeimage1d/2)
         else if (dim.eq.2) then
             call message( 'Phase in F1 or F2 ? ')
             zz = 2
             call getdim(zz,error)
             if (error.ne.0) goto 93
             if (zz.lt.1 .or. zz.gt.2) goto 98
             if (zz.eq.1 .and. itype.lt.2) goto 143
             if (zz.eq.2 .and. (mod(itype,2)).ne.1) goto 143
             call phase2d(plane2d,si1im,si2im,ph0,ph1,zz)
         else if (dim.eq.3) then
             call message( 'phase in F1, F2, or F3')
             zz = 4
             call getdim(zz,error)
             if (error.ne.0) goto 93
             if (zz.eq.1) then
                if (mod(it3d/4,2).ne.1) goto 143
                call phase2d(image,si3d1,si3d2*si3d3,
     *                                            ph0,ph1,1)
             else if (zz.eq.2) then
                if (mod(it3d/2,2).ne.1) goto 143
                do i=1,si3d1
                   call phase2d(image((i-1)*si3d2*si3d3+1),
     *                        si3d2,si3d3,ph0,ph1,1)
                enddo
             else if (zz.eq.4) then 
                if (mod(it3d,2).ne.1) goto 143
                call phase2d(image,si3d1*si3d2,si3d3,ph0,ph1,2)
             else
                goto 98
             endif
         endif
         max0=0.0
         refresh = 1

      else if (command.eq.'BRUKER_CORR') then
C BRUKER_CORR DSPFIRM DSPFVS DECIM
C
C compute and apply the first order phase correction to be executed on a spectrum
C after Fourier trasform, when the digital filter was used on a Bruker spectrometer.
C
C the value actually used is put in the $RETURNED context
         call message('enter DSPFVS')
         call getint2(win1,error)
         call message('enter DSPFIRM')
         call getint2(win2,error)
         if (error.ne.0) goto 93
         call message('enter DECIM')   
         call getint2(win3,error)
         if (error.ne.0) goto 93
         if (dim.eq.1) then
             if (it1d.ne.1) goto 143
             z = brukerphase(win1,win2,win3, sp1d)
             if (z.eq.-1.0) goto 98
             call phase(column,0.0,z,sizeimage1d/2)
         else if (dim.eq.2) then
             if ((mod(itype,2)).ne.1) goto 143
             z = brukerphase(win1,win2,win3, specw2)
             if (z.eq.-1.0) goto 98
             call phase2d(plane2d,si1im,si2im,0.0,z,2)
         else if (dim.eq.3) then
             if (mod(it3d,2).ne.1) goto 143
             z = brukerphase(win1,win2,win3, sp3d3)
             if (z.eq.-1.0) goto 98
             call phase2d(image,si3d1*si3d2,si3d3,0.0,z,2)
         endif
         write(returned,'(G12.8)') z      ! copy value in $returned
         max0=0.0
         refresh = 1

      else if (command.eq.'HPHASE') then
         call message( 'enter phase correction ph0 and ph1')
         call message('values are in degree')   
         call getreal2(ph0,error)
         if (error.ne.0) goto 93
         call getreal2(ph1,error)
         if (error.ne.0) goto 93
         if (dim.eq.1) then
             if (it1d.ne.0) goto 104
             call hphase(column,ph0,ph1,sizeimage1d)
         else if (dim.eq.2) then
             call message( 'Phase in F1 or F2 ? ')
             zz = 2
             call getdim(zz,error)
             if (error.ne.0) goto 93
             if (zz.lt.1 .or. zz.gt.2) goto 98
             if (zz.eq.1 .and. itype.ge.2) goto 104
             if (zz.eq.2 .and. (mod(itype,2)).eq.1) goto 104
             call hphase2d(plane2d,si1im,si2im,ph0,ph1,zz)
         else if (dim.eq.3) then
             call message( 'phase in F1, F2, or F3')
             zz = 4
             call getdim(zz,error)
             if (error.ne.0) goto 93
             if (zz.eq.1) then
                if (mod(it3d/4,2).eq.1) goto 104
                call hphase2d(image,si3d1,si3d2*si3d3,
     *                                            ph0,ph1,1)
             else if (zz.eq.2) then
                if (mod(it3d/2,2).eq.1) goto 104
                do i=1,si3d1
                   call hphase2d(image((i-1)*si3d2*si3d3+1),
     *                        si3d2,si3d3,ph0,ph1,1)
                enddo
             else if (zz.eq.4) then 
                if (mod(it3d,2).eq.1) goto 104
                call hphase2d(image,si3d1*si3d2,si3d3,ph0,ph1,2)
             else
                goto 98
             endif
         endif
         max0=0.0
         refresh = 1

      elseif (command.eq.'AUTOPHASE') then
         if (dim.ne.2) goto 97
         if (itype.ne.3) goto 143
         if (sizeimage.gt.smxbig/2) goto 103
         zz1 = 0.0
         zz2 = 0.0
         zz3 = 0.0
         zz4 = 0.0
         call message
     *     ( 'Autophase on 0 order or both 0 and 1 orders (0/1)? ')
         zz = 1
         call getint2(zz,error)
         if (error.ne.0) goto 93
         call autophase(zz1,zz2,zz3,zz4,itermax,miniter,zz,verbose)
         call gifaout( 'Phase corrections: ')
         write(st,*) '  in F1:  ',zz1,zz2
         call gifaout(st)
         write(st,*) '  in F2:  ',zz3,zz4
         call gifaout(st)
         refresh = 1
         max0 = 0.0

      else if (command.eq.'MODULUS') then
         if (dim.eq.1) then
            if (it1d.ne.1) goto 143
            call module(column,sizeimage1d)
            it1d = 0
         else if (dim.eq.3) then
            if (it3d.ne.7) goto 143
            call modu3d(image,si3d1,si3d2,si3d3)
            it3d = 0
         else if (dim.eq.2) then
            if (itype.eq.3) then
               call modu2d(plane2d,si1im,si2im)
               itype = 0
            else if (itype.eq.1) then
               do i=0,si1im-1
                  zz = si2im
                  call module(plane2d(i*si2im+1),zz)
               enddo
               zz = si2im
               si2im = si2im/2
               call zerofill(plane2d,si1im,si2im,si1im,zz)
               itype = 0
            else
               goto 143
            endif
         endif
         max0=0.0
         refresh = 1

      elseif (command.eq.'REAL') then
         if (dim.eq.1) then         !case of 1d spectra
           if (it1d.ne.1) goto 143
           call real1d(column,sizeimage1d)
           it1d = 0
           zo1dl = (zo1dl+1)/2
           zo1du = (zo1du+1)/2
         else if (dim.eq.2) then       !case of 2d spectra
           call message
     *      ( 'Extract real in F1, in F2, or F12 ? ')
           zz=3
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (zz.eq.1) then
             if (itype.ne.2 .and. itype.ne.3) goto 143
             call real2d(plane2d,si1im,si2im,zz)
             itype = itype-2
             zolf1 = (zolf1+1)/2
             zour1 = (zour1+1)/2
           elseif(zz.eq.2) then
             if (itype.ne.1 .and. itype.ne.3) goto 143
             call real2d(plane2d,si1im,si2im,zz)
             itype = itype-1
             zolf2 = (zolf2+1)/2
             zour2 = (zour2+1)/2
           elseif(zz.eq.3) then
             if (itype.ne.3) goto 143
             call real2d(plane2d,si1im,si2im,zz)
             itype = 0
             zolf2 = (zolf2+1)/2
             zour2 = (zour2+1)/2
             zolf1 = (zolf1+1)/2
             zour1 = (zour1+1)/2
           else
             goto 98
           endif
        else if (dim.eq.3) then
           call message
     * (  'Enter mode F1, F2, F3, and combinations')
           zz = 7
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (zz.eq.1) then
             if (it3d.lt.4) goto 143
             it3d = it3d-4
           elseif (zz.eq.2) then
             if (mod(it3d,4).lt.2) goto 143
             it3d = it3d - 2
           elseif (zz.eq.4) then
             if (mod(it3d,2).ne.1) goto 143
             it3d = it3d - 1
           elseif (zz.eq.3) then
             if (it3d.lt.6) goto 143
             it3d = it3d-6
           elseif (zz.eq.5) then
             if (it3d.ne.5 .and. it3d.ne.7) goto 143
             it3d = it3d - 5
           elseif (zz.eq.6) then
             if (mod(it3d,4).ne.3) goto 143
             it3d = it3d - 3
           elseif (zz.eq.7) then
             if (it3d.ne.7) goto 143
             it3d = 0
           else
             goto 98
           endif                      
           call real3d(image,si3d1,si3d2,si3d3,zz)
        endif
        if (axes.ne.0 .and.
     *      (unitx.eq.'INDEX' .or. unity.eq.'INDEX') ) refresh = 1    ! refresh to display axis

      elseif (command.eq.'SMOOTH') then
         call message( 'Enter number of points for smoothing:  ')
         if (dim.eq.1) then         !case of 1d spectra
          if (it1d.ne.0) go to 96 
           call getint2(zz,error)
           if (error.ne.0) goto 93
           call smooth1d(column,sizeimage1d,zz)
         elseif (dim.eq.2) then    !case of 2d spectra
          if (itype.ne.0) go to 96 
           xx = 3
           yy = 3
           call message( 'In F1:  ')
           call getint2(yy,error)
           if (error.ne.0) goto 93
           call message( 'In F2:  ')
           call getint2(xx,error)
           if (error.ne.0) goto 93
           call smooth2d(plane2d,si1im,si2im,yy,xx)
         elseif (dim.eq.3) then    !case of 3d spectra
          if (it3d.ne.0) go to 96 
           xx = 3
           yy = 3
           zz = 3
           call message( 'In F1:  ')
           call getint2(zz,error)
           if (error.ne.0) goto 93
           call message( 'In F2:  ')
           call getint2(yy,error)
           if (error.ne.0) goto 93
           call message( 'In F3:  ')
           call getint2(xx,error)
           if (error.ne.0) goto 93
           call smooth3d(image,si3d1,si3d2,si3d3,zz,yy,xx)
         endif
         max0=0.0
         refresh = 1

      elseif (command.eq.'MEDIAN') then
         if (dim.eq.3) goto 124
         call message( 'Enter number of points for median filter:  ')
         if (dim.eq.1) then         !case of 1d spectra
          if (it1d.ne.0) go to 96 
           call getint2(yy,error)
           if (error.ne.0) goto 93
           call message( 'Enter index:  ')
           call getint2(zz,error)
           if (error.ne.0) goto 93
           call median1d(column,sizeimage1d,yy,zz)
         else if (dim.eq.2) then    !case of 2d spectra
          if (itype.ne.0) go to 96 
           xx = 3
           yy = 3
           call message( 'In F1:  ')
           call getint2(yy,error)
           if (error.ne.0) goto 93
           call message( 'In F2:  ')
           call getint2(xx,error)
           if (error.ne.0) goto 93
           call message( 'Enter index:  ')
           call getint2(zz,error)
           if (error.ne.0) goto 93
           call median2d(plane2d,si1im,si2im,yy,xx,zz)
         endif
         max0=0.0
         refresh = 1

C*******************************************************************
C             Various filters
C*******************************************************************

      else if (command.eq.'EM') then      !for 1d or 2d spectra
         if (dim.eq.1) then               !case of 1d spectra
           z = lb
           call message( 'Exponential window:  ')
           call getfreq2(z,sp1d,error)
           if (error.ne.0) goto 93
           call expmult(column,sizeimage1d,z,it1d)
           lb = z            
         elseif (dim.eq.2) then           !case of 2d spectra
           if (si1im+si2im.gt.sizemax) goto 103
           y=lb1                          !lb in F1
           z=lb                           !lb in F2
           call message( '2D exponential window.')
           call message('Enter lb in F1 and F2:  ')
           call getfreq2(y,specw1,error)
           if (error.ne.0) goto 93
           call getfreq2(z,specw2,error)
           if (error.ne.0) goto 93
           call exp2d(plane2d,si1im,si2im,y,z,itype)
           lb1=y
           lb=z
        else if (dim.eq.3) then
           if (si3d1+si3d2+si3d3.gt.sizemax) goto 103
           zz1=lb2                          !lb in F1
           zz2=lb1                          !lb in F2 
           zz3=lb                           !lb in F3
           call message( '3d exponential window')
           call message('enter lb in F1, 2 and 3')
           call getfreq2(zz1,sp3d1,error)
           if (error.ne.0) goto 93
           call getfreq2(zz2,sp3d2,error)
           if (error.ne.0) goto 93
           call getfreq2(zz3,sp3d3,error)
           if (error.ne.0) goto 93
           call exp3d(image,si3d1,si3d2,si3d3,zz1,zz2,zz3,it3d)
           lb2=zz1
           lb1=zz2
           lb=zz3
         endif
         max0=0.0
         refresh = 1

      else if (command.eq.'GM') then  !for 1d or 2d spectra
         if (dim.eq.1) then
           z = gb
           call message( 'Gaussian window:  ')
           call getfreq2(z,sp1d,error)
           if (error.ne.0) goto 93
           call gausmult(column,sizeimage1d,z,it1d)
           gb = z
         else if (dim.eq.2) then        !case of 2d spectra
           if (si1im+si2im.gt.sizemax) goto 103
           y=gb1                          !lb in F1
           z=gb                           !lb in F2
           call message( '2D gaussian window:  ')
           call message('Enter gb in F1 and F2:  ')
           call getfreq2(y,specw1,error)
           if (error.ne.0) goto 93
           call getfreq2(z,specw2,error)
           if (error.ne.0) goto 93
           call gaus2d(plane2d,si1im,si2im,y,z,itype)
           gb1=y
           gb=z
        else if (dim.eq.3) then
           if (si3d1+si3d2+si3d3.gt.sizemax) goto 103
           zz1=gb2                          !gb in F1
           zz2=gb1                          !gb in F2 
           zz3=gb                           !gb in F3
           call message( '3d gaussian window')
           call message('enter gb in F1, 2 and 3')
           call getfreq2(zz1,sp3d1,error)
           if (error.ne.0) goto 93
           call getfreq2(zz2,sp3d2,error)
           if (error.ne.0) goto 93
           call getfreq2(zz3,sp3d3,error)
           if (error.ne.0) goto 93
           call gaus3d(image,si3d1,si3d2,si3d3,zz1,zz2,zz3,it3d)
           gb1=zz1
           gb2=y
           gb=z
         endif      
         max0=0.0
         refresh = 1


      else if (command.eq.'TM') then
           call message( 'Trapezoidal window, Enter 2 points :')
           call getint2(tm1,error)
           if (error.ne.0) goto 93
           call getint2(tm2,error)
           if (error.ne.0) goto 93
           if (tm1.lt.1 .or. tm1.gt.tm2) goto 98
         if (dim.eq.1) then
           if (tm2.gt.sizeimage1d) goto 98
           call trapeze(column,sizeimage1d,tm1,tm2,it1d)
         elseif (dim.eq.2) then    !case of 2d spectra
           if (si1im+si2im.gt.sizemax) goto 103
           call message( 'filter in F1, F2, or F12 ?')
           zz=3
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (zz.eq.1) then
              if (tm2.gt.si1im) goto 98
           elseif (zz.eq.2) then
              if (tm2.gt.si2im) goto 98
           elseif (zz.eq.3) then
              if (tm2.gt.min(si2im,si1im)) goto 98
           else  
              goto 98
           endif
           call trap2d(plane2d,si1im,si2im,zz,tm1,tm2,itype)
         elseif (dim.eq.3) then    !case of 3d spectra
           if (si3d1+si3d2+si3d3.gt.sizemax) goto 103
           call message( 'filter in F1, F2, F3 and combinations ?')
           zz=7
           call getdim(zz,error)
           if (error.ne.0) goto 93
           if (zz.lt.1 .or. zz.gt.7)  go to 98

           if (mod(zz,1).eq.1 .and. tm2.gt.si3d1) goto 98
           if (mod(zz/2,2).eq.1 .and. tm2.gt.si3d2) goto 98
           if (zz.ge.4 .and. tm2.gt.si3d3) goto 98
           call trap3d(image,si3d1,si3d2,si3d3,zz,tm1,tm2,it3d)
         endif      
      max0 = 0.0
      refresh = 1

      else if (command.eq.'SIN') then  !sin filter for 1d or 2d spectra
C get phase shift parameter
        call message( 'position of maximum between 0.0 and 0.5')
        call message( '0.0 : cos ; 0.5 : sin ') 
        z=0.0
        call getreal2(z,error)
        if (error.ne.0) goto 93               
        if (z.lt.0.0 .or. z.gt.0.5) goto 98
C do computation
        if (dim.eq.1) then               !case of 1d spectra
          call sinmul(column,sizeimage1d,z,it1d)
        else  if (dim.eq.2) then         !case of 2d spectra
          if (si1im+si2im.gt.sizemax) goto 103
          call message( 'Filter in F1, F2, or F12 ? ')
          zz=3
          call getdim(zz,error)
          if (error.ne.0) goto 93
          if (zz.lt.1 .or. zz.gt.3)  go to 98
          call sin2d(plane2d,si1im,si2im,zz,z,itype) 
        else if (dim.eq.3) then
          if (si3d1+si3d2+si3d3.gt.sizemax) goto 103
          call message( 'filter in F1, F2, F3 and combinations ?')
          zz=7
          call getdim(zz,error)
          if (error.ne.0) goto 93
          if (zz.lt.1 .or. zz.gt.7)  go to 98
          call sin3d(image,si3d1,si3d2,si3d3,zz,z,it3d)
        endif                         
        max0=0.0
        refresh = 1              

      else if (command.eq.'SQSIN') then  !sqsin filter for 1d or 2d spectra
C get phase shift parameter
        if (param.eq.' ') then
          call message( 'position of maximum between 0.0 and 0.5')
          call message( '0.0 : sqcos ; 0.5 :sqsin ') 
        endif       
        z=.5
        call getreal2(z,error)
        if (error.ne.0) goto 93
        if (z.lt.0 .or. z.gt.0.5) goto 98
C do computation
        if (dim.eq.1) then               !case of 1d spectra
          call sin2mul(column,sizeimage1d,z,it1d)
        elseif (dim.eq.2) then           !case of 2d spectra
          if (si1im+si2im.gt.sizemax) goto 103
          call message( 'Filter in F1, F2, or F12 ? ')
          zz=3
          call getdim(zz,error)
          if (error.ne.0) goto 93
          if (zz.lt.1 .or. zz.gt.3)  go to 98
          call sinsq2d(plane2d,si1im,si2im,zz,z,itype) 
        elseif (dim.eq.3) then            !case of 3d spectra
          if (si3d1+si3d2+si3d3.gt.sizemax) goto 103
          call message( 'filter in F1, F2, F3 and combinations ?')
          zz=7
          call getdim(zz,error)
          if (error.ne.0) goto 93
          if (zz.lt.1 .or. zz.gt.7)  go to 98
          call sinsq3d(image,si3d1,si3d2,si3d3,zz,z,it3d)
        endif
        max0=0.0
        refresh = 1


C*******************************************************************

C
      else if (command.eq.'MAX') then 
         call mnxvect(mini,maxi,zz,i,imago(imdim(dim)),sizeimo)
         if (verbose.ne.0) call gifaoutr('value of maximum point:',maxi)
         if (verbose.ne.0) call gifaoutr('value of minimum point:',mini)
      
      else if (command.eq.'DIM') then   
         if (dim.eq.3) then
            max3d = max0
         elseif (dim.eq.2) then
            max2d = max0
         else
            max1d = max0
         endif
         call message('enter dimension of spectrum (1, 2 or 3):')
         zz=dim
         call getint2(dim,error)
         if (error.ne.0) goto 93
         if ( (dim.ne.1) .and. (dim.ne.2) .and. (dim.ne.3)) then
            dim = zz
            goto 98        
         endif
         if (zz.eq.3 .and. dim.eq.2) then
            sizeimage=si1im*si2im
	    if (sizeimage.gt.smxmax)
     *         call gifaout('3D buffer will be overflowed !')
            max0 = max2d 
         elseif (zz.eq.2 .and. dim.eq.3) then
	    if (sizeimage.gt.smxmax)
     *         call gifaout('2D buffer will be overflowed !')
            sizeimage3d=si3d3*si3d2*si3d1
            max0 = max3d
         elseif (zz.eq.2 .and. dim.eq.1) then
	    if (sizeimage1d.gt.sizemax)
     *         call gifaout('2D buffer will be overflowed !')
            max0 = max1d
         elseif (zz.eq.1 .and. dim.eq.2) then
            sizeimage=si2im*si1im
            max0 = max2d
	    if (sizeimage1d.gt.sizemax)
     *         call gifaout('1D buffer will be overflowed !')
         elseif (zz.eq.1 .and. dim.eq.3) then
	    if (sizeimage1d.gt.sizemax+smxmax)
     *         call gifaout('1D buffer will be overflowed !')
            sizeimage3d=si3d3*si3d2*si3d1
            max0 = max3d
         elseif (zz.eq.3 .and. dim.eq.1) then
	    if (sizeimage1d.gt.sizemax+smxmax)
     *         call gifaout('3D buffer will be overflowed !')
            max0 = max1d
         endif             
         if (dim.ne.3 .and. zz.ne.dim) refresh = 1

C*************************************************************
C        Parameters
C*************************************************************

      else if (command.eq.'SPECW') then
         if (dim.eq.1) then
           call message('Enter Spectral width')
           call getreal2(sp1d,error)
           if (error.ne.0) goto 93
         else  if (dim.eq.2) then
           call message('Enter Spectral widths in F1 and F2')
           call getreal2(specw1,error)
           if (error.ne.0) goto 93
           call getreal2(specw2,error)
           if (error.ne.0) goto 93
         else  if (dim.eq.3) then
           call message('Enter Spectral widths in F1, F2, F3')
           call getreal2(sp3d1,error)
           if (error.ne.0) goto 93
           call getreal2(sp3d2,error)
           if (error.ne.0) goto 93
           call getreal2(sp3d3,error)
           if (error.ne.0) goto 93
         endif

      else if (command.eq.'OFFSET') then
         if (dim.eq.1) then
           call message('Enter offset of right-most point')
           call getreal2(of1d,error)
           if (error.ne.0) goto 93
         else  if (dim.eq.2) then
           call message('Enter offset of upperright-most point')
           call message('in F1 and F2')
           call getreal2(offset1,error)
           if (error.ne.0) goto 93
           call getreal2(offset2,error)
           if (error.ne.0) goto 93
         else  if (dim.eq.3) then
           call message('Enter offset of upperright-most point')
           call message('in F1, F2, F3')
           call getreal2(of3d1,error)
           if (error.ne.0) goto 93
           call getreal2(of3d2,error)
           if (error.ne.0) goto 93
           call getreal2(of3d3,error)
           if (error.ne.0) goto 93
         endif

      elseif (command.eq.'FREQ') then
        call message('(All values are in MegaHertz)')
        call message('Enter 1H frequency ')
        call getreal2(frequency,error)
        if (error.ne.0) goto 93
        if (dim.eq.1) then
         call message('Enter nucleus frequency ')
         call getreal2(freq1d,error)
         if (error.ne.0) goto 93
        elseif (dim.eq.2) then
         call message('Enter F1 nucleus frequency')
         call getreal2(freq1,error)
         if (error.ne.0) goto 93
         call message('Enter F2 nucleus frequency')
         call getreal2(freq2,error)
         if (error.ne.0) goto 93
        elseif (dim.eq.3) then
         call message('Enter F1 nucleus frequency')
         call getreal2(freq3d1,error)
         if (error.ne.0) goto 93
         call message('Enter F2 nucleus frequency')
         call getreal2(freq3d2,error)
         if (error.ne.0) goto 93
         call message('Enter F3 nucleus frequency')
         call getreal2(freq3d3,error)
         if (error.ne.0) goto 93
        endif

      elseif (command.eq.'UNIT') then
         call message('Enter Unit value')
         call getunit2(unitx,error)
           if (error.ne.0) goto 93
         unity = unitx
         if (axes.ne.0) refresh = 1

      elseif (command.eq.'UNIT_Y') then
         call message('Enter Unit value for Y axis')
         call getunit2(unity,error)
           if (error.ne.0) goto 93
         if (axes.ne.0) refresh = 1

      elseif (command.eq.'REF') then
         refresh = 1

      elseif (command.eq.'UNREF') then
         refresh = 0

C**************************************************************


      else if (command.eq.'EXIT' .or. command.eq.'BYE' 
     *         .or.command.eq.'QUIT') then
         goto 199

      else                             ! check if it is a macro
         if (input.eq.29) goto 113
         i = input-19
         buffer(i) = param            ! store command line
         if (input.eq.20) savecontext = vcontext
         input=input+1
         currinp(i) = lcommand
         call opengif(lcommand,input,error)   ! try to open as a file
         if (error.ne.0) then
            input = input - 1                 ! if not then error.
            call trailing(lcommand,zz)
            call gifaout(lcommand(1:zz)//' : command unknown')
            goto 90
         endif
         if (debug.ne.0) then
           stp = '....................'
           call gifaout(stp(1:2*(input-20)) // lcommand)
         endif 
         vcontext = input
         param = ' '			! clear input line
         currline(i) = 0
      endif

C End of the command dispatcher
C **************************************************************************
C **************************************************************************
C test for control_c
6     if (control.eq.1) goto 116
      if (error.ne.0) goto 90
C this might happen some time.  just cut the '%%' out.
      if(param(1:2).eq.'%%')  param = param(3:256)
C clear message buffer
      if (command.ne.'MESSAGE') imsg = 0
C parse the remaining of the command line, and jump to the top (3 or 5)
      if (param.ne.' ') then
         call getstring2(lcommand,error)
         command = lcommand
         call uppercase(command,lcom)
         goto 5
      endif
      goto 3


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCC Main loop ends here. CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

999   continue          ! exit point
      call message('Do you want to keep the LOG file? (y/n)')
998   st = 'n'
      call getstring2(st,error)
      if (error.ne.0) goto 93
      call uppercase(st,10)
      if (st(1:1).eq.'N') then
                close(2)   ! close log file
#vms		call shell('del/noconf/nolog SYS$LOGIN:gifa.log;')
#_vms		call shell('rm $HOME/gifa.log')
      else if (st(1:1).eq.'Y') then
                close(2)   ! close log file
      else if (st(1:1).eq.'C') then
		goto 3
      else
	   goto 999
      endif
      if (journal.ne.2) close(journal)
      call ch_flushall(error)
      error = -1                ! flag to callers that ought to exit.

#d      if (collision.gt.0) then
#d         write(*,*) 'Number of variable collisions : ',collision
#d      endif
      goto 990
                  
C error codes are here...
90    continue
      error = 0
C first check for control_C
      if (control.eq.1) goto 901
C first general processing
      if (input.gt.20) then
c  check for on_error_goto case, and break if found
	  stp = '=ONERROR'
        call checkvar(stp,vcontext,xx)
        if (xx.eq.0) then
          call getvar(stp,vcontext,st,error)
          if (error.ne.0) goto 901
          stp2 = '=' // st
	    st = stp2
          call dealloc(stp,vcontext,error)
          if (error.ne.0) goto 901
          goto 997                   ! jumps to GOTO code
        endif
        call gifaout('*** Aborting execution of command file : '
     *                 //currinp(input-20))
        call gifaouti('*** At line :',currline(input-20))
        close(input)
        call contclear(input)
        input = input - 1
        vcontext = input
        if (input.eq.20) vcontext = savecontext
        goto 90
      endif


c 901 is used to force errors when onerrorgoto is set, or when a controlC was issued
901   if (input.gt.20) then
         if (control.ne.1) then
            call gifaout('*** Aborting execution of command file : '
     *                 //currinp(input-20))
            call gifaouti('*** At line :',currline(input-20))
         endif
         close(input)
         call contclear(input)
         input = input - 1
         vcontext = input
         if (input.eq.20) vcontext = savecontext
         goto 901
      endif

      if (control.eq.1) then
          call setupctrlc
      else
         if (debug.ne.0) write(*,*) 'ERROR code :', error
         call gifaout('*** Error in processing command : '//command)
      endif
      comefrom = 0  ! used by FOR / FOREACH to flag looping
      error = 1
      goto 990

C exit point of the routine is here
c
c prompt the user, to tell him that work is finished
990   if (haswritten.eq.1) then
               haswritten = 0

#_readline               if (on_x_win.eq.1) call showprompt(0)

      endif

C del the wait_cursor

      do i=1,vd_id_max
        if( vd_id(i).ne.0) call win_set_pointer_pattern(vd_id(i),0)
      enddo
      call arrow_cursor_in_all_shell()

      return


C then specific one to each error
91    call gifaerr( 'Command not available.')
      goto 90
92    call gifaerr( 'Data size must be even.')
      goto 90              
93    call gifaerr( 'Error in reading.' )
      goto 90
94    call gifaerr( 'data size must be a power of two')
      goto 90 
95    call gifaerr( 'please load data first')
      goto 90
96    call gifaerr( 
     *   'dataset should be real, use REAL to change itype')
      goto 90
97    call gifaerr( 'command available on 2D data only')
      goto 90
98    call gifaerr( 'error with value')
      goto 90
99    call gifaerr( 'command available on 1D data only')
      goto 90
101   call gifaerr( 'unable to open file')
      goto 90
102   call gifaerr( 'error in writing')
      goto 90
103   call gifaerr( 'Size too big for operation')
      goto 90
104   call gifaerr( 'Data-set of wrong itype for operation')
      goto 90
105   call gifaerr( 'Too many points')
      goto 90
106   call gifaerr( 'Output device not supported')
      goto 90
107   call gifaerr( 'Problem with NCHANNEL setting.')
      goto 90
108   call gifaerr( 'Outside data-set limits')
      goto 90
109   call gifaerr( 'error with value: should be positive')
      goto 90
110   call gifaerr( 'Don''t know what to do')
      goto 90
111   call gifaerr( 'Peak table empty !')
      goto 90
112   call gifaerr( 'Command available in zoom mode only')
      goto 90
113   call gifaerr( 'Too many recursive macro calls, (9 maximum).' )
      goto 90
114   call gifaerr( 'Command available in macro only.')
      goto 90
115   call gifaerr( 'Syntax error.')
      goto 90
C This is used by control_c
116   call gifaerr( 'Aborted by user.' )
      control = 1      ! this is needed for error code coming from dialog boxes
      goto 90
117   call gifaerr( 'Size not compatible with current data-set')
      goto 90
118   call gifaerr( 'Choose a plane first')
      goto 90
119   call gifaerr( '3D display is not active')
      goto 90
120   call gifaerr( 'Available in dim 3 only')
      goto 90
121   call gifaerr( 'Not implemented yet')
      goto 90
122   call gifaerr( 'command not available, no image on screen')
      goto 90
123   call gifaerr( 
     *   'command not available, no density map on screen')
      goto 90
124   write(st,*) 'Command not available in dim ',dim
      call gifaerr( st)
      goto 90
125   call gifaerr( 'Available only on a PS300')
      goto 90
126   call gifaerr( 'Outside DATA buffer limits')
      goto 90
127   call gifaerr( 'Display must be on')
      goto 90
128   call gifaerr( 'Unable to open window' )
      goto 90
129   call gifaerr( 'sizeF1 and sizeF2 must be equal' )
      goto 90
C this one is available !!!!
130      goto 90
131   call gifaerr('to many OPENed files' )
      goto 90
132   call gifaerr('file not OPENed' )
      goto 90
133   call gifaerr('NOISE should not be 0.0')
      goto 90
134   call gifaerr('No currently joined data-set')
      goto 90
135   call gifaerr('No peak within zoom window')
      goto 90
136   call gifaerr('Error in Motif Widget creation')
      goto 90
137   call gifaerr('IF without matching ENDIF / ELSE / ELSIF')
      goto 90
138   call gifaerr('WHILE without matching ENDWHILE')
      goto 90
139   call gifaerr('FOR/FOREACH without matching ENDFOR')
      goto 90
140   call gifaerr('Error in Motif Widget deletion')
      goto 90
141   call gifaerr('Wrong file access mode')
      goto 90
142   call gifaerr('Wrong parameter')
      goto 90
143   call gifaerr('Data-set should be Complex')
      goto 90
144   call gifaerr('Too many peaks for operation')
      goto 90
145   call gifaerr('Point stack empty')
      goto 90
146   call gifaerr('Point stack full')
      goto 90
147   call gifaerr('Peak number larger than the size of the peak table')
      goto 90
148   call gifaerr('Zoom window too large for operation')
      goto 90
149   call gifaerr(st(1:xx) // ': Variable name too long')
      goto 90
c errors concerning linear prediction commands: 161-168
161   call gifaerr( 'Wrong value for order, or other LP parameter')
      goto 90
162   call gifaerr( '((size/2)-order-1) should be larger
     & than order')
      goto 90
163   write(st,*) '((size/2)-order-1)*order should be smaller
     & than: ',ldmax*ldmax
      call gifaerr( st)
      goto 90
164   write(st,*)  'Order should be smaller than: ', ldmax
      call gifaerr(st)
      goto 90
165   write(st,*)  '(size/2)-order-1 should be smaller than: ',
     &  ldmax
      call gifaerr(st)
      goto 90
166   write(st,*) 'Order should be smaller than: ', 2*ldmax
      call gifaerr(st)
      goto 90
167   write(St,*) 'Order should be smaller than: ',
     & sizeimage1d/2
      call gifaerr(st)
      goto 90
168   call gifaerr( 'Relaxation rate should be strictly negative')
      goto 90
169   call gifaerr( 'error with window or with coordinates')
      goto 90
170   i=256
      call trailing(view_name,i)
      call gifaerr( 'VIEW :' // view_name(1:i) // ' does not exist')
      view_name = 'view_name'
      goto 90
171   call gifaerr('TIMER and PROFILER are incompatible')
      goto 90
172   call gifaerr('Internal error')
      goto 90
C Label not found 
198   i=256
      call trailing(st,i)
      call gifaerr('Label not found : ' // st(1:i))
      goto 90
C if no more input available or EXIT command, close @file,
C dealloc all variable associated with @file and decement input
c
199   if (input.eq.20) goto 999    ! quit
      close(unit=input)
      call contclear(input)
      input = input-1
      vcontext = input
      if (input.eq.20) vcontext = savecontext
      param = buffer(input-19)     ! restore command line
      if (error.eq.10) error = 0
      goto 6                       ! goto end of dispatcher

200   continue		! assert error
#assert (.false.)
      goto 90

10    format(A256)
12    format ('+',F8.2,$)

250   format('Actual size is (F1xF2xF3)',3i8)
251   format('extracting from',3i8,' to',3i8)
252   format('New size is (F1xF2xF3)',3i8)

17    format(a,2i6,a,2i6)
18    format(a1)
20    format(' ','size :',I7,' spectral width:',f9.2,' offset:',f9.2)
34    format(I5)
      end



C
C     Authors :       M.A.Delsuc
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C
C     This software cannot be used unless the users have been fully 
C licensed to do so from the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C
C
C
C
                        program gifa
c main entry point
      implicit none

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
c #include "unitextern.inc"
#include "variable.inc"
#include "gifaout.inc"
#include "cachev.inc"
c #include "fitv.inc"
#include "laplacev.inc"
#include "ambv.inc"

#include "gifabcom.inc"
#include "version.inc"
      integer error,zz,i
      integer motif_init
      external motif_init

      character*256 todo
#_vms	character*(256) st


      write(*,*) 'Running Gifa version 4'
      write(*,*) 'Check Home page at : http://www.cbs.univ-montp1.fr'

      journal = 2
#vms	name = 'SYS$LOGIN:gifa.log'
#_vms      call gethome(st)
#_vms      call trailing(st,i)
#_vms      name = st(1:i) // '/gifa.log'
      open(journal,file=name,status='new',iostat=error)     ! open log file in HOME.
      if (error.ne.0) then
        write(*,*) 'Cannot open the gifa.log file'
        write(*,*) 'Check  o if the disk is full'
        write(*,*) '       o if a protected gifa.log is already ',
     *           'in your HOME directory'
        write(*,*) '       o other possible error conditions...'
        stop
      endif
1     write(*,*)

      call get_licence(license,ilenc)

      if (license(1:ilenc).eq.'NON-VALID') then
         call gifaout
     * ('=*==*=*=*=*=*=*=*=*=*= WARNING =*=*=*=*=*=*=*=*=*=*=*=*==*=')
         call gifaout
     * ('   This version of GIFA has no valid licence information')
         call gifaout
     * ('        Please register by contacting M.A.Delsuc')
         call gifaout
     *   ('              mad@cbs.univ-montp1.fr')
         call gifaout
     * ('=*==*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*==*=')
         write(*,*)
      endif
      call gifaout( 'Version : ' // version)
      call gifaout( 'Licence : ' // license(1:ilenc))
      call gifaouti( 'Memory available in kwords :', smxbig/1024 )
      call gifaout( ' ')
 
C empty marks if buffers are empty
c      empty = 0    !no data are loaded at the begining

      call setupctrlc

C set static variables to default
      name = 'no_name'
      name1d = 'no_name'
      name2d = 'no_name'
      name3d = 'no_name'
      pkname = 'no_name'
      imdim(1) = 1
      imdim(2) = sizemax + 1
      imdim(3) = sizemax + smxmax + 1
      sizeimage = 512
c      do i=1,3
c         call prpvect(imago(imdim(i)),0.0,sizeimage)
c      enddo
      call prpvect(column,0.0,sizeimage)
      call prpvect(plane2d,0.0,sizeimage)
      call prpvect(image,0.0,sizeimage)
      nchannel = 1
      gb = 0.0
      lb = 0.0           
      gb1 = 0.0
      lb1 = 0.0    
      j1 = 0.0
      j2 = 0.0
      gb2 = 0.0
      lb2 = 0.0
      ph0 = 0.0
      ph1 = 0.0
      do i=1,vd_id_max
         vd_id(i) = 0
      enddo 
      on_graph = 0
      refresh = 0         
      tm1 = 1
      tm2 = 8
      col = 1
      colv = 1
      row = 1
      rowv = 1
      imsg = 0
      cleanable = 0

#include "gifshelli.inc"
#include "paramfilei.inc"
#include "paramdatai.inc"
#include "zoomparami.inc"
#include "disptypei.inc"
#include "disp3di.inc"
#include "plotcomi.inc"
#include "maxenti.inc"
#include "linpredi.inc"
#include "linecor2i.inc"
#include "peakinti.inc"
#include "cachei.inc"
#include "transformi.inc"
#include "laplacei.inc"
#include "ambi.inc"

      error = 0

C try to connect
      on_x_win = motif_init()
      if (on_x_win.eq.0) call gifaout('No graphic possible !')

C execute startup macro
      input=20
      vcontext=20
      call opengif('startup.g',20,error)
      if (error .eq. 0) then
         close(20)
         call gifaout('Executing startup.g ....')
         todo = '@startup.g'
         call trailing(todo,i)
         call execute(todo,i,error)
      else
         error = 0
         call gifaout('*** No startup macro was found ***')
         call gifaout
     *  ('Note that if a file startup.g is present in the Gifa PATH :')
         call gifaout(path)
         call gifaout
     *    ('It will be executed when Gifa is started ')
          if (on_x_win.eq.1) call showprompt(0)
      endif

C enter main loop
      if (on_x_win.eq.0) then	! if no possible X connection

  3     call getinput(0,error)
        todo = param
c equivalent a :
c   prompt>
c   read dans todo
c   clean, et longueur
c   copie dans journal

       error = 0
       call execute(todo,256,error)
       if (error.gt.0) error=0
       if (error.ne.-1) goto 3
      else					! got to event loop if X_window
	call motif_loop
      endif
      call gifa_exit(0)
      stop
      end




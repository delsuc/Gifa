C inoutp.inc
C input and output commands
C
      elseif (command.eq.'WRITEL') then
#vms         if (dim.ne.1) goto 99
#_vms         if (dim.eq.3) goto 124
         call message( 'Enter file name:  ')
         call getstring2(name,error)
#vms         call wlevy(column,sizeimage1d,name,it1d)
#_vms         call message
#_vms     * ('Enter parameter file name (will be used as template)')
#_vms         call getstring2(st,error)
#_vms         call trailing(name,i)
#_vms         call trailing(st,j)
#_vms         if (dim .eq.1) then
#_vms         call wlevy(name,i,st,j,column,1,sizeimage1d,itype,
#_vms     *              zz1,sp1d)
#_vms         elseif (dim .eq.2) then
#_vms         call wlevy(name,i,st,j,plane2d,si1im,si2im,itype,
#_vms     *              specw1,specw2)
#_vms         endif

      elseif (command.eq.'READL') then  !specific 1d spectra
         call message( 'Enter file name:  ')
         call getstring2(name,error)
#vms         if (dim.eq.1) then
#vms           call llevy(column,sizeimage,name,noise,error)
#vms           if (error.eq.0) then
#vms             call prpvect(window,1.0,sizemax)
#vms             itype = 1
#vms             empty = 1
#vms             zoom = 0
#vms             max0=0.0
#vms             refresh = 1
#vms             offset1=0.0
#vms             specw1=2000*3.1415926
#vms             offset2=0.0
#vms             specw2=2000*3.1415926
#vms           else
#vms             empty=0
#vms             goto 93
#vms           endif
#vms         else
#vms           goto 99
#vms         endif
#_vms         call message('Enter parameter file name (will be created)')
#_vms         call getstring2(st,error)
#_vms         call trailing(name,i)
#_vms         call trailing(st,j)
#_vms         call llevy(name,i,st,j,column,plane2d,win1,win2,itype,
#_vms     *              zz1,zz2,zz,error)
#_vms         if (error .ne. 0) goto 93
#_vms         if (zz.eq.1) then		! 1D
#_vms            dim = 1
#_vms            sp1d = zz2
#_vms            sizeimage1d = win2
#_vms         elseif (zz.eq.2) then
#_vms            dim = 2
#_vms            specw1 = zz1
#_vms            specw2 = zz2
#_vms            si1im = win1
#_vms            si2im = win2
#_vms         endif
#_vms         empty = 1
#_vms         zoom = 0
#_vms         max0=0.0
#_vms         refresh = 1

      else if (command.eq.'WRITEH') then           !for 1d or 2d spectra
         call message( 'Enter file-name')
         call getstring2(name,error)
         if (dim.eq.1) then                       !case of 1d spectra
           call whare(column,sizeimage1d,name)
         else if (dim.eq.2) then                  !case of 2d spectra
           call whare2d(plane2d,si1im,si2im,name)
         else                                     !case of 3d spectra
           call whare3d(image,si3d1,si3d2,si3d3,name)
         endif

      else if (command.eq.'READH') then
         call message( 'Enter file-name')
         call getstring2(name,error)
         if (dim.eq.1) then              !case of 1d spectra
           call lhare(column,sizeimage1d,name,noise,error)
           if (error.eq.0) then
             call prpvect(window,1.0,sizemax)
           else
             empty=0
             goto 93
           endif
         else if (dim.eq.2) then       !case of 2d spectra
           call lhare2d(plane2d,si1im,si2im,name,error,noise)
           call prpvect(window,1.0,sizemax)
           sizeimage = si1im*si2im
           if (error.ne.0) then
              empty=0
              goto 93
           endif  
         else if (dim.eq.3) then
           call lhare3d(image,si3d1,si3d2,si3d3,name,error,noise)
           call prpvect(window,1.0,sizemax)
           sizeimage3d = si3d1*si3d2*si3d3
           if (error.ne.0) then
              empty=0
              go to 90
           endif
         endif
         max0 = 0.0
         empty = 1
         refresh = 1

      else if (command.eq.'WRITEX') then           !for 1d or 2d spectra
         call message( 'Enter file-name')
         call getstring2(name,error)
         call message ('Enter EXPNO')
	 i = 1
         call getint2(i,error)
         call message ('Enter PROCNO')
	 j = 1
         call getint2(j,error)
         if (error.ne.0) goto 93
         if (dim.eq.1) then                       !case of 1d spectra
c           call wuxnmr1d(name,i,j,error)
         else if (dim.eq.2) then                  !case of 2d spectra
c           call wuxnmr2d(name,i,j,error)
         else                                     !case of 3d spectra
c           call wuxnmr3d(name,i,j,error)
         endif
         if (error.ne.0) goto 102

      else if (command.eq.'READX') then
         call message( 'Enter file-name')
         call getstring2(name,error)
         call message ('Enter EXPNO')
	 i = 1
         call getint2(i,error)
         call message ('Enter PROCNO')
	 j = 1
         call getint2(j,error)
         if (dim.eq.1) then              !case of 1d spectra
c           call ruxnmr1d(name,i,j,error)
         else if (dim.eq.2) then       !case of 2d spectra
c           call ruxnmr2d(name,i,j,error)
         else if (dim.eq.3) then
c           call ruxnmr3d(name,i,j,error)
         endif
         if (error.ne.0) goto 93
         max0 = 0.0
         empty = 1
         refresh = 1

      elseif (command.eq.'READV') then
          if (dim.eq.3) goto 124
          call message( 'Enter file name:  ')
          call getstring2(name,error)
          call trailing(name,i)
          zz = dim
          call lvarian(column,plane2d,win1,win2,name,i,
     *             zz,itype,error)
          if ((zz.ne.1 .and. zz.ne.2).or. error.ne.0) goto 93
          dim = zz
          if (error .ne.0) goto 93
          if (dim.eq.2) then
             si1im = win1
             si2im = win2
             sizeimage = si1im*si2im
             offset1=0.0
             specw1=2000*3.1415926
             offset2=0.0
             specw2=2000*3.1415926
          else
             sizeimage1d = win2
             of1d = 0.0
             sp1d = 2000*3.1415926
          endif
c          if (dim.eq.2) then
c              zz = sizeimage*0.9
c              call evaln(plane2d(zz),sizeimage-zz,noise,shift)
c          else
c              zz = sizeimage1d*0.9
c              call evaln(column(zz),sizeimage1d-zz,noise,shift)
c          endif
          call prpvect(window,1.0,sizemax)
          empty = 1
          zoom = 0
          max0=0.0
          refresh = 1

      elseif (command.eq.'WRITET') then
         call message( 'Enter file name:  ')
         call getstring2(name,error)
         if (dim.eq.1) then              !case of 1d spectra
           call writet3d(column,1,1,sizeimage1d,name)
         elseif (dim.eq.2) then          !case of 2d spectra
           call writet3d(plane2d,1,si1im,si2im,name)
         elseif (dim.eq.3) then          !case of 2d spectra
           call writet3d(image,si3d1,si3d2,si3d3,name)
         endif

      elseif (command.eq.'READT') then
         call message( 'Enter file name:  ')
         call getstring2(name,error)
         call readt3d(imago(imdim(dim)),name,error)
         if (error.eq.0) then
           call prpvect(window,1.0,sizemax)
         else
           empty=0
           goto 93
         endif
         empty = 1
         max0=0.0
         zoom = 0
         refresh = 1

      elseif (command.eq.'WRITES') then
         call message( 'Enter file name:  ')
         call getstring2(name,error)
         if (dim.eq.1) then              !case of 1d spectra
           call writetc3d(column,1,1,sizeimage1d,name)
         elseif (dim.eq.2) then           !case of 2d spectra
           call writetc3d(plane2d,1,si1im,si2im,name)
         else                              !case of 3d spectra
           call writetc3d(image,si3d1,si3d2,si3d3,name)
         endif

      elseif (command.eq.'READS') then
         call message( 'Enter file name:  ')
         call getstring2(name,error)
         call readtc3d(name,error)
         if (error.eq.0) then
           call prpvect(window,1.0,sizemax)
         else
           empty=0
           goto 93
         endif
         empty = 1
         zoom = 0
         refresh = 1

      else if (command.eq.'CONCAT') then    
        if (dim.eq.1) goto 124
        call message( 'File to concatenate:')
        call getstring2(name,error)
        if (dim.eq.3) then
          call lhare2d(image(si3d1*si3d2*si3d3+1),
     *                         yy,xx,name,error,noise)
          if (error.ne.0) goto 93
          if (xx.ne.si3d3) goto 117
          xx = yy/si3d2   
          if (xx*si3d2.ne.yy) goto 117
          si3d1 = si3d1+xx
          write(st,*) 'size is si1 x si2 x si3 :' ,si3d1,si3d2,si3d3
          call gifaout(st)
        elseif (dim.eq.2) then
          call lhare2d(plane2d(sizeimage+1),
     *                         yy,xx,name,error,noise)
          if (error.ne.0) goto 93
          if (xx.ne.si2im) goto 117
          si1im = si1im+yy
          write(st,*) 'size is si1 x si2 :' ,si1im,si2im
          call gifaout(st)
        endif
        refresh = 1
        max0 = 0


C matlab.inc
C input and output to matlab -ascii format
C
      else if (command.eq.'WRITEM') then           !for 1d or 2d spectra
#aix        goto 91
         if (dim.eq.3) goto 124
         call message( 'Enter file-name')
         call getstring2(name,error)
         if (dim.eq.1) then                       !case of 1d spectra
           call save_matlab(name, column, 1, sizeimage1d, error)
         else if (dim.eq.2) then                  !case of 2d spectra
           call save_matlab(name, plane2d, si1im, si2im, error)
         endif
         if (error.ne.0) goto 101

      else if (command.eq.'READM') then
#aix        goto 91
         if (dim.eq.3) goto 124
         call message( 'Enter file-name')
         call getstring2(name,error)
         if (dim.eq.1) then              !case of 1d spectra
           call load_matlab(name, smxbig, column, xx,
     *                  sizeimage1d, error)
           if (xx.ne.1) goto 99
           if (error.eq.0) then
             call prpvect(window,1.0,sizemax)
           else
             empty=0
             goto 93
           endif
         else if (dim.eq.2) then       !case of 2d spectra
           call load_matlab(name, smxbig, plane2d, si1im, si2im, error)
           call prpvect(window,1.0,sizemax)
           sizeimage = si1im*si2im
           if (error.ne.0) then
              empty=0
              goto 93
           endif  
         endif
         max0 = 0.0
         empty = 1
         refresh = 1


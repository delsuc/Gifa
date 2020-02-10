      elseif (command.eq.'CHDUMP') then
         call ch_dump

      elseif (command.eq.'DISJOIN') then
         if (id_cache.eq.0) goto 134
         write(st,*) id_cache
         call ch_close(id_cache,error)
         if (error.ne.0) goto 102
         call leading(st)
         call trailing(st,i)
         stp = '$CACHE[' // st(1:i) // ']' 
         call dealloc(stp,20,error)
         if (error.ne.0) goto 90
c         if (error.ne.0) call gifaoutc(ch_errlist(error))
         id_cache = 0
         c_name = ' '

c******************************************************
      elseif (command.eq.'JOIN') then
         call message('enter file-name')
         call getstring2(st,error)
         if (error.ne.0) goto 93
         call trailing(st,i)
         id_cache = 0
         call ch_join(id_cache,st,i, CF_READ_WRITE,error)
         if (error .eq. C_WRONG_MODE) then
             error = 0
             call ch_join(id_cache,st,i,CF_READ,error)
             if (error.eq.0) then
                if (input.eq.20) 
     *              call gifaout('WARNING, access in Read Only mode')
             elseif (error .eq. C_WRONG_MODE) then
                error = 0
                call ch_join(id_cache,st,i,CF_WRITE,error)
                if (error.eq.0) then
                  if (input.eq.20) 
     *              call gifaout('WARNING, access in Write_Only mode')
                else
                  goto 101
                endif
             else
                goto 101
             endif
         else if (error .eq. C_NOT_OPENED) then
            error = 0
            call ch_open(id_cache,st,i,CF_READ_WRITE,error)
            if (error .eq. C_WRONG_MODE) then
               error = 0
               call ch_open(id_cache,st,i,CF_READ,error)
               if (error.eq.0) then
               if (input.eq.20) 
     *          call gifaout('WARNING, access in Read Only mode')
               else
                  goto 101
               endif
            elseif (error.ne.0) then
               goto 101
            endif
         else if (error .ne. 0) then
            goto 101
         endif
         c_name = st
         call ch_gtparbas(id_cache,c_dimmanip,c_sizef1,c_sizef2
     *      ,c_sizef3,c_type,c_specwf1,c_specwf2,c_specwf3
     *      ,c_freq,c_freq1,c_freq2,c_freq3
     *      ,c_offsf1,c_offsf2,c_offsf3
     *      ,c_dmin,c_dmax,c_dfactor,c_max0)
         write(st,*) id_cache
         call leading(st)
         call trailing(st,i)
         stp = '$CACHE[' // st(1:i) // ']' 
         call assign(stp,20,c_name,error)
         if (error.ne.0) goto 128

c******************************************************
c      elseif (command.eq.'LISTFILEC') then
c         call ch_printfiles

c******************************************************
      elseif (command.eq.'FLUSHCACHE') then
          call ch_flushall(error)

c******************************************************
      elseif (command.eq.'FLUSH') then
          if (id_cache.eq.0) goto 134
          call ch_flush(id_cache, error)

c******************************************************
      elseif (command.eq.'READC' .or. command.eq.'READ') then
         call message('enter file-name')
	 if (dim.eq.1) then
           st = name1d
         elseif (dim.eq.2) then
           st = name2d
         elseif (dim.eq.3) then
           st = name3d
         endif
	 call getstring2(st,error)
         if (error.ne.0) goto 93
         name = st
c         call getstring2(name,error)
c         if (error.ne.0) goto 93
         call trailing(name,j)
         call ch_open(i,name,j,CF_READ,error)
         if (error .ne. 0) goto 101
         call ch_gtparbas(i,c_dimmanip,c_sizef1,c_sizef2
     *      ,c_sizef3,c_type,c_specwf1,c_specwf2,c_specwf3
     *      ,c_freq,c_freq1,c_freq2,c_freq3
     *      ,c_offsf1,c_offsf2,c_offsf3
     *      ,dmin,dmax,dfactor,max0)
      
         if (c_dimmanip.eq.1) then
             name1d = name
             dim = 1
             if (c_sizef1.gt.smxbig)  goto 103
             call checkunderflow(c_sizef1,bool1)
             if (bool1) goto 90
             sizeimage1d = c_sizef1
             it1d = c_type
             frequency = c_freq
             freq1d = c_freq1
             sp1d = c_specwf1
             of1d = c_offsf1
             axis1d = 1
             n1d1 = 1
             n1d2 = 1
             call ch_ac1darea(column, i, 1, c_sizef1, CB_READ, error)
             if (error.ne.0) goto 93
         elseif(c_dimmanip.eq.2) then
             name2d = name
             dim = 2
             if (c_sizef1*c_sizef2.gt.smxbig)  goto 103
             call checkunderflow(c_sizef1*c_sizef2,bool1)
             if (bool1) goto 90
             si1im = c_sizef1
             si2im = c_sizef2
             itype = c_type
             frequency = c_freq
             freq1 = c_freq1
             freq2 = c_freq2
             specw1 = c_specwf1
             specw2 = c_specwf2
             offset1 = c_offsf1
             offset2 = c_offsf2
             axis2d = 1
             n2d = 1
             call ch_ac2darea(plane2d, i,
     *          1, 1,  c_sizef1, c_sizef2, CB_READ, error)
             if (error.ne.0) goto 93
         elseif(c_dimmanip.eq.3) then
	     name3d = name
             dim = 3
             if (c_sizef1*c_sizef2*c_sizef3.gt.smxbig)  goto 103
             call checkunderflow(c_sizef1*c_sizef2*c_sizef3,bool1)
             if (bool1) goto 90
             si3d1 = c_sizef1
             si3d2 = c_sizef2
             si3d3 = c_sizef3
             it3d = c_type
             dim = c_dimmanip
             frequency = c_freq
             freq3d1 = c_freq1
             freq3d2 = c_freq2
             freq3d3 = c_freq3
             sp3d1 = c_specwf1
             sp3d2 =  c_specwf2
             sp3d3 =  c_specwf3
             of3d1 = c_offsf1
             of3d2 = c_offsf2
             of3d3 = c_offsf3
             call ch_ac3darea(image, i,
     *          1, 1, 1, c_sizef1, c_sizef2, c_sizef3, CB_READ, error)
             if (error.ne.0) goto 93
         endif
         call ch_close(i,error)
C Dont forget to reload dataset param if currently joined.
         if (id_cache.ne.0)
     *     call ch_gtparbas(id_cache,c_dimmanip,c_sizef1,c_sizef2
     *      ,c_sizef3,c_type,c_specwf1,c_specwf2,c_specwf3
     *      ,c_freq,c_freq1,c_freq2,c_freq3
     *      ,c_offsf1,c_offsf2,c_offsf3
     *      ,c_dmin,c_dmax,c_dfactor,c_max0)
         if (error.ne.0) goto 93
c         max0 = 0.0
         if (verbose.ne.0) call gifaouti('New data-set, dim ',dim)
         empty = 1
         refresh = 1
         if ((dim.eq.1 .and. zo1du.gt.sizeimage1d)   .or.
     *       (dim.eq.2 .and. (zour2.gt.si2im .or. zour1.gt.si1im)) .or.
     *       (dim.eq.3 .and. (zo3df1u.gt.si3d1
     *             .or. zo3df2u.gt.si3d2 .or. zo3df3u.gt.si3d3)))
     *      zoom = 0

c******************************************************
      elseif (command.eq.'ADDC' .or. command.eq.'ADD') then
         call message('enter file-name')
	 if (dim.eq.1) then
           st = name1d
         elseif (dim.eq.2) then
           st = name2d
         elseif (dim.eq.3) then
           st = name3d
         endif
	 call getstring2(st,error)
         if (error.ne.0) goto 93
         name = st
c         call getstring2(name,error)
c         if (error.ne.0) goto 93
         call trailing(name,j)
         call ch_open(i,name,j,CF_READ,error)
         if (i.eq.0 .or. error .ne.0) goto 101
         call ch_gtparbas(i,ww,xx,yy,zz,j,z,z,z,z,z,z,z,z,z,z,z,z,z,z)
         if (ww.eq.1) then
            if (dim.ne.1 .or. it1d.ne.j .or.
     *          sizeimage1d.ne.xx ) goto 117
            name1d = name
            call addc1d(i,error)
            if (error.eq.1) goto 103
         elseif(ww.eq.2) then
            if (dim.ne.2 .or. itype.ne.j .or.
     *          si1im.ne.xx .or. si2im.ne.yy) goto 117
            name2d = name
            call addc2d(i,error)
            if (error.eq.1) goto 103
         elseif(ww.eq.3) then
            if (dim.ne.3 .or. it3d.ne.j .or.
     *          si3d1.ne.xx .or. si3d2.ne.yy .or.
     *          si3d3.ne.zz  ) goto 117
            name3d = name
            call addc3d(i,error)
            if (error.eq.1) goto 103
         endif
         call ch_close(i,error)
         max0 = 0
         refresh = 1

c******************************************************
      elseif (command.eq.'GETHEADER') then
         if (id_cache.eq.0) goto 134
         call message ('Parameter name ?')
         call getstring2(st,error)
         if (error.ne.0) goto 93
         call trailing(st,i)
         call ch_gtparam(id_cache,stp2, j,  st, i, error)
         if (error.ne.0) goto 98
         c_header = stp2(1:j)
         if (verbose .ne.0) call gifaout(c_header)

c******************************************************
      elseif (command.eq.'PUTHEADER') then
         if (id_cache.eq.0) goto 134
         call message ('Parameter name ?')
         call getstring2(st,error)
         if (error.ne.0) goto 93
         call trailing(st,i)
         call ch_gtparam(id_cache,stp2, j,  st, i, error)
         if (error.ne.0) then       ! means : param not found, not error condition
            error = 0
            c_header = ' '
         else
            c_header = stp2(1:j)
         endif
         call message ('enter new value')
         call getstring2(c_header,error)
         if (error.ne.0) goto 93
         call trailing(c_header,j)
         call ch_putparam(id_cache, c_header, j,  st, i, error)
         if (error.ne.0) goto 102

c******************************************************
      elseif (command.eq.'GETC') then
        if (id_cache.eq.0) goto 134
C in dim 1
        if (dim.eq.1) then
C   dim1 to dim1
        if (c_dimmanip.eq.1) then
          call get1d_1d(win1,win2,error)
          if (error.ne.0) goto 93
          zz = win2-win1+1
          call c_to_1d(1,zz,error)
          if (error.ne.0) goto 93
          call ch_ac1darea(column, id_cache, win1, win2,
     *                     CB_READ, error)
          if (error.ne.0) goto 93
C   dim 2 to dim1
        elseif (c_dimmanip.eq.2) then
          call get2d_1d(axis1d,n1d1,win1,win2,error)
          if (error.ne.0) goto 93
          zz = win2-win1+1
          call c_to_1d(axis1d,zz,error)
          if (error.ne.0) goto 93
          if (axis1d.eq.1) then
             call ch_ac2darea(column, id_cache,
     *          win1, n1d1,  win2, n1d1, CB_READ, error)
          elseif (axis1d.eq.2) then
             call ch_ac2darea(column, id_cache,
     *          n1d1, win1, n1d1,  win2, CB_READ, error)
          endif
          if (error.ne.0) goto 93
C  dim 3 to dim1
        elseif (c_dimmanip.eq.3) then
          call get3d_1d(axis1d,n1d1,n1d2,win1,win2,error)
          if (error.ne.0) goto 93
          zz = win2-win1+1
          call c_to_1d(axis1d,zz,error)
          if (error.ne.0) goto 93
          if (axis1d.eq.1) then
             call ch_ac3darea(column, id_cache,
     *          win1, n1d1, n1d2, win2, n1d1, n1d2, CB_READ, error)
          elseif (axis1d.eq.2) then
             call ch_ac3darea(column, id_cache,
     *          n1d1, win1, n1d2, n1d1, win2, n1d2, CB_READ, error)
          elseif (axis1d.eq.3) then
             call ch_ac3darea(column, id_cache,
     *          n1d1, n1d2, win1, n1d1, n1d2, win2, CB_READ, error)
          endif
          if (error.ne.0) goto 93
        else
          goto 110
        endif
C in dim 2
        elseif (dim.eq.2) then
C   dim 1 to dim2
        if (c_dimmanip.eq.1) then
             goto 124
C   dim 2 to dim 2
        elseif (c_dimmanip.eq.2) then
          call get2d_2d(win1,win2,win3,win4,error)
          if (error.ne.0) goto 93
          xx = win3-win1+1
          zz = win4-win2+1
          call c_to_2d(1,xx,zz,error)
          if (error.ne.0) goto 93
          call ch_ac2darea(plane2d, id_cache,
     *        win1, win2, win3, win4, CB_READ, error)
          if (error.ne.0) goto 93
C   dim 3 to dim 2
        elseif (c_dimmanip.eq.3) then
          call get3d_2d(axis2d,n2d,win1,win2,win3,win4,error)
          if (error.ne.0) goto 93
          xx = win3-win1+1
          zz = win4-win2+1
          call c_to_2d(axis2d,xx,zz,error)
          if (error.ne.0) goto 93
          if (axis2d.eq.1) then
             call ch_ac3darea(plane2d, id_cache,
     *          n2d, win1, win2, n2d, win3, win4, CB_READ, error)
          elseif (axis2d.eq.2) then
             call ch_ac3darea(plane2d, id_cache,
     *          win1, n2d, win2, win3, n2d, win4, CB_READ, error)
          elseif (axis2d.eq.3) then
             call ch_ac3darea(plane2d, id_cache,
     *          win1, win2, n2d, win3, win4, n2d, CB_READ, error)
          endif
          if (error.ne.0) goto 93
        endif                      
C in dim 3
        elseif (dim.eq.3) then
        if (c_dimmanip.ne.3)   goto 120
C     dim 3 to dim 3
          call get3d_3d(win1,win2,win3,win4,win5,win6,error)
          if (error.ne.0) goto 93
          xx = win4-win1+1
          yy = win5-win2+1
          zz = win6-win3+1
          call c_to_3d(xx,yy,zz,error)
          if (error.ne.0) goto 93
          call ch_ac3darea(image, id_cache,
     *        win1,win2,win3,win4,win5,win6, CB_READ, error)
          if (error.ne.0) goto 93
        endif
        max0 = 0
        refresh = 1
    
c******************************************************
      elseif (command.eq.'NEWFILEC') then
           call message('enter file-name')
           call getstring2(c_name,error)
           if (error.ne.0) goto 93
           call trailing(c_name,i)
           call message( 'enter dimension of file (1,2,3)?')
           if (dim.eq.1) then
                st = '1D'
           elseif (dim.eq.2) then
                st = '2D'
           elseif (dim.eq.3) then
                st = '3D'
           endif
           call getstring2(st,error)
           if (error.ne.0) goto 93
           call uppercase(st,256)
           if (.not.(st.eq.'1D' .OR. st.eq.'2D' 
     *        .OR. st.eq.'3D')) goto 98
           call message('nb of points in F1 ?')
           call getint2(c_sizef1,error)
           call message('enter offset in F1')
           call getreal2(c_offsf1,error)
           call message('enter spectral widths in F1')
           call getreal2(c_specwf1,error)
           call message('enter frequency in F1')
           call getreal2(c_freq1,error)
           if (error.ne.0) goto 98
           c_type = it1d
           c_dimmanip = 1
           if (st.eq.'2D'.OR. st.eq.'3D') then
             call message('nb of points in F2 ?')
             call getint2(c_sizef2,error)
             call message('enter offset in F2')
             call getreal2(c_offsf2,error)
             call message('enter spectral widths in F2')
             call getreal2(c_specwf2,error)
             call message('enter frequency in F2')
             call getreal2(c_freq2,error)
             if (error.ne.0) goto 98
             c_type = itype
             c_dimmanip = 2
           else
             c_sizef2 = 1
             c_offsf2 = 0
             c_specwf2 = 2000.0*pi
             c_freq2 = c_freq1
           endif
           if (st.eq.'3D') then 
             call message('nb of points in F3 ?')
             call getint2(c_sizef3,error)
             call message('enter offset in F3')
             call getreal2(c_offsf3,error)
             call message('enter spectral widths in F3')
             call getreal2(c_specwf3,error)
             call message('enter frequency in F3')
             call getreal2(c_freq3,error)
             if (error.ne.0) goto 98
             c_type = it3d
             c_dimmanip = 3
           else
             c_sizef3 = 1
             c_offsf3 = 0
             c_specwf3 = 2000.0*pi
             c_freq3 = c_freq1
           endif
           call message('enter reference (1H) frequency')
           call getreal2(c_freq,error)
           if ((c_sizef1*c_sizef2*c_sizef3).LE.0) goto 103       
           call message('itype ?')
           call getint2(c_type,error)
           if (error.ne.0) goto 98           
           call ch_create(id_cache,c_name,i,CF_WRITE,error)
           if (error.ne.0) goto 101
           call ch_setup(id_cache, c_dimmanip, c_sizef1, c_sizef2,
     *                   c_sizef3, error)
           if (error.ne.0) goto 101
           call ch_putparam_type(id_cache,c_freq, 'Frequency', 9, 
     *            H_FLOAT, error)
           call ch_putparam_type(id_cache,c_specwf1, 'Specw1', 6, 
     *            H_FLOAT, error)
           call ch_putparam_type(id_cache,c_offsf1, 'Offset1', 7, 
     *            H_FLOAT, error)
           call ch_putparam_type(id_cache,c_freq1, 'Freq1', 5, 
     *            H_FLOAT, error)
           call ch_putparam_type(id_cache,c_type, 'Type', 4,
     *            H_INT, error)
           if (error.ne.0) goto 101

           if (st.eq.'2D' .or. st.eq.'3D') then
             call ch_putparam_type(id_cache,c_specwf2, 'Specw2', 6, 
     *            H_FLOAT, error)
             call ch_putparam_type(id_cache,c_offsf2, 'Offset2', 7, 
     *            H_FLOAT, error)
             call ch_putparam_type(id_cache,c_freq2, 'Freq2', 5, 
     *            H_FLOAT, error)
             if (error.ne.0) goto 101
           endif
           if (st.eq.'3D') then
             call ch_putparam_type(id_cache,c_specwf3, 'Specw3', 6, 
     *            H_FLOAT, error)
             call ch_putparam_type(id_cache,c_offsf3, 'Offset3', 7, 
     *            H_FLOAT, error)
             call ch_putparam_type(id_cache,c_freq3, 'Freq3', 5, 
     *            H_FLOAT, error)
             if (error.ne.0) goto 101
           endif
           call ch_putparam_type(id_cache,dmin,'Dmin',4,H_FLOAT,error)
           call ch_putparam_type(id_cache,dmax,'Dmax',4,H_FLOAT,error)
           call ch_putparam_type(id_cache,dfactor,'Dfactor',
     *          7,H_FLOAT,error)
           if (error.ne.0) goto 101
           c_dmin = dmin
           c_dmax=dmax
           c_dfactor = dfactor
         write(st,*) id_cache
         call leading(st)
         call trailing(st,i)
         stp = '$CACHE[' // st(1:i) // ']' 
         call assign(stp,20,c_name,error)
         if (error.ne.0) goto 128


c******************************************************
      elseif (command.eq.'WRITEC' .or. command.eq.'WRITE') then
           call message('enter file-name')
	   if (dim.eq.1) then
            st = name1d
           elseif (dim.eq.2) then
            st = name2d
           elseif (dim.eq.3) then
            st = name3d
           endif
	   call getstring2(st,error)
           if (error.ne.0) goto 93
           name = st
c           call getstring2(name,error)
c           if (error.ne.0) goto 93
           call trailing(name,j)
           call ch_create(i,name,j,CF_WRITE,error)
           if (i.eq.0 .or. error.ne.0) goto 101
           if (dim.eq.1) then
	     name1d = name
             call ch_setup(i, dim, sizeimage1d, 1,1, error)
             if (error.ne.0) goto 102
             call ch_putparam_type(i,it1d, 'Type', 4,H_INT, error)
             call ch_putparam_type(i,frequency, 'Frequency', 9,
     *            H_FLOAT, error)
             call ch_putparam_type(i,max0, 'Absmax', 6,H_FLOAT, error)
             call ch_putparam_type(i,sp1d, 'Specw1', 6,H_FLOAT, error)
             call ch_putparam_type(i,of1d, 'Offset1', 7,H_FLOAT, error)
             call ch_putparam_type(i,freq1d, 'Freq1', 5,H_FLOAT, error)
             if (error.ne.0) goto 102
             call ch_ac1darea(column, i, 1, sizeimage1d,
     *                CB_WRITE, error)
             if (error.ne.0) goto 102
           elseif (dim.eq.2) then
	     name2d = name
             call ch_setup(i, dim, si1im, si2im,1, error)
             if (error.ne.0) goto 102
             call ch_putparam_type(i,itype, 'Type', 4,H_INT, error)
             call ch_putparam_type(i,frequency, 'Frequency', 9, 
     *                H_FLOAT, error)
             call ch_putparam_type(i,max0, 'Absmax', 6,H_FLOAT, error)
             call ch_putparam_type(i,specw1, 'Specw1', 6, 
     *                H_FLOAT, error)
             call ch_putparam_type(i,offset1, 'Offset1', 7, 
     *                H_FLOAT, error)
             call ch_putparam_type(i,freq1, 'Freq1', 5, 
     *                H_FLOAT, error)
             if (error.ne.0) goto 102
             call ch_putparam_type(i,specw2, 'Specw2', 6, 
     *                H_FLOAT, error)
             call ch_putparam_type(i,offset2, 'Offset2', 7, 
     *                H_FLOAT, error)
             call ch_putparam_type(i,freq2, 'Freq2', 5, 
     *                H_FLOAT, error)
             if (error.ne.0) goto 102
             call ch_ac2darea(plane2d, i, 1, 1, si1im, si2im,
     *            CB_WRITE, error)
             if (error.ne.0) goto 102
           elseif (dim.eq.3) then
	     name3d = name
             call ch_setup(i, dim, si3d1, si3d2,si3d3, error)
             if (error.ne.0) goto 102
             call ch_putparam_type(i,it3d, 'Type', 4,H_INT, error)
             call ch_putparam_type(i,frequency, 'Frequency', 9, 
     *                H_FLOAT, error)
             call ch_putparam_type(i,max0, 'Absmax', 6,H_FLOAT, error)
             call ch_putparam_type(i,sp3d1, 'Specw1', 6, 
     *                H_FLOAT, error)
             call ch_putparam_type(i,of3d1, 'Offset1', 7, 
     *                H_FLOAT, error)
             call ch_putparam_type(i,freq3d1, 'Freq1', 5, 
     *                H_FLOAT, error)
             if (error.ne.0) goto 102
             call ch_putparam_type(i,sp3d2, 'Specw2', 6, 
     *                H_FLOAT, error)
             call ch_putparam_type(i,of3d2, 'Offset2', 7, 
     *                H_FLOAT, error)
             call ch_putparam_type(i,freq3d2, 'Freq2', 5, 
     *                H_FLOAT, error)
             if (error.ne.0) goto 102
             call ch_putparam_type(i,sp3d3, 'Specw3', 6, 
     *                H_FLOAT, error)
             call ch_putparam_type(i,of3d3, 'Offset3', 7, 
     *                H_FLOAT, error)
             call ch_putparam_type(i,freq3d3, 'Freq3', 5, 
     *                H_FLOAT, error)
             if (error.ne.0) goto 102
             call ch_ac3darea(image, i, 1, 1, 1,
     *                si3d1, si3d2, si3d3, CB_WRITE, error)
             if (error.ne.0) goto 102
           endif
           call ch_putparam_type(i,dmin,'Dmin',4,H_FLOAT,error)
           call ch_putparam_type(i,dmax,'Dmax',4,H_FLOAT,error)
           call ch_putparam_type(i,dfactor,'Dfactor',7,H_FLOAT,error)
           if (error.ne.0) goto 102
           call ch_close(i,error)
           if (error.ne.0) goto 102

c******************************************************
      elseif (command.eq.'PUTC') then
         if (id_cache.eq.0) goto 134
         if (dim.eq.1) then
C   dim1 to dim1
           if (c_dimmanip.eq.1) then
             call get1d_1d(win1,win2,error)
             if (error.ne.0) goto 93
             call ch_ac1darea(column, id_cache, win1, win2,  
     *                CB_WRITE, error)
             if (error.ne.0) goto 102
C   dim 1 to dim 2
           elseif (c_dimmanip.eq.2) then
             call get2d_1d(i,j,win1,win2,error)
             if (error.ne.0) goto 93
             if (i.eq.1) then
               call ch_ac2darea(column, id_cache, win1, j, win2, j,
     *              CB_WRITE, error)
             else
               call ch_ac2darea(column, id_cache, j, win1, j, win2,
     *              CB_WRITE, error)
             endif
             if (error.ne.0) goto 102      
C  dim 1 to dim 3
           elseif (c_dimmanip.eq.3) then
             call get3d_1d(i,j,k,win1,win2,error)
             if (error.ne.0) goto 93
             if (i.eq.1) then
               call ch_ac3darea(column, id_cache, win1, j, k,
     *	            win2, j, k, CB_WRITE, error)
             elseif (i.eq.2) then
               call ch_ac3darea(column, id_cache, j, win1, k,
     *	            j, win2, k, CB_WRITE, error)
             elseif (i.eq.3) then
               call ch_ac3darea(column, id_cache, j, k, win1,
     *	            j, k, win2, CB_WRITE, error)
             endif
             if (error.ne.0) goto 102             
           endif
C in dim 2
         elseif (dim.eq.2) then
C   dim 2 to dim 1
            if (c_dimmanip.eq.1) then
               goto 124
C   dim 2 to dim 2
            elseif (c_dimmanip.eq.2) then
          	call get2d_2d(win1,win2,win3,win4,error)
          	if (error.ne.0) goto 93
	        call ch_ac2darea(plane2d, id_cache,
     *             win1, win2, win3, win4, CB_WRITE, error)
                if (error.ne.0) goto 93
C   dim 2 to dim 3
            elseif (c_dimmanip.eq.3) then
          	call get3d_2d(i,j,win1,win2,win3,win4,error)
          	if (error.ne.0) goto 93
		if (i.eq.1) then
	          call ch_ac3darea(plane2d, id_cache,
     *             j, win1, win2, j, win3, win4, CB_WRITE, error)
		elseif (i.eq.2) then
	          call ch_ac3darea(plane2d, id_cache,
     *             win1, j, win2, win3, j, win4, CB_WRITE, error)
		elseif (i.eq.3) then
	          call ch_ac3darea(plane2d, id_cache,
     *             win1, win2, j, win3, win4, j, CB_WRITE, error)
		endif
                if (error.ne.0) goto 93
            endif
C in dim 3
         elseif (dim.eq.3) then
C    dim 3 to dim 1 or dim 2 
            if (c_dimmanip.ne.3) then
              goto 124
            else
C     dim 3 to dim 3
          	call get3d_3d(win1,win2,win3,win4,win5,win6,error)
          	if (error.ne.0) goto 93
	        call ch_ac3darea(image, id_cache,
     *                 win1, win2, win3, win4, win5, win6,
     *                 CB_WRITE, error)
                if (error.ne.0) goto 93
            endif
         endif

c******************************************************
      elseif (command.eq.'SHOWC') then
        if (id_cache.eq.0) goto 134
        if (on_x_win.eq.0) goto 91
        if (dim.eq.1) then
          if (disp1d.ne.1) goto 127
          if (c_dimmanip.ne.1) goto 99
          z = scale
          call message('Enter Scale :')
          call getreal2(z,error)
          call gcolor(vd_id(1),scolor)
          if (zoom.eq.1) then
	          zz1 = itopr(float(zo1dl),sizeimage1d,sp1d,of1d,freq1d)
	          zz2 = itopr(float(zo1du),sizeimage1d,sp1d,of1d,freq1d)
	      else
	          zz1 = itopr(float(1),sizeimage1d,sp1d,of1d,freq1d)
	          zz2 = itopr(float(sizeimage1d),sizeimage1d,sp1d,
     *                        of1d,freq1d)
	      endif
C	      win1 = (c_sizef1-1)*(c_offsf1 - zz1*c_freq1)/c_specwf1
C     *         + c_sizef1 + 0.5
C	      win2 = (c_sizef1-1)*(c_offsf1 - zz2*c_freq1)/c_specwf1
C     *           + c_sizef1 + 0.5
	      win1 = c_sizef1*(c_offsf1 - zz1*c_freq1)/c_specwf1
     *           + c_sizef1 + 1.5
	      win2 = c_sizef1*(c_offsf1 - zz2*c_freq1)/c_specwf1
     *           + c_sizef1 + 1.5
	      call chkborder2d(win1,win2,2,c_type)
	      if (win2-win1+1 .gt. sizemax) goto 103
          call CH_AC1DAREA(work1d, id_cache, win1, win2, CB_READ, error)
          if (error.eq.C_OUTSIDE) goto 108
          if (error.ne.0) goto 93
          call display1d(vd_id(1),(win2-win1+1),work1d,
     *                 max0,z,c_type,vheight,clear)
          call gcolor(vd_id(1),color)
        elseif (dim.eq.2) then
          if (cdisp2d.ne.1) goto 127
          if (c_dimmanip.ne.2) goto 97
          z = scale
          call message('Enter Scale :')
          call getreal2(z,error)
          call gcolor(vd_id(3),scolor)
          if (zoom.eq.1) then
            call plot2dj(vd_id(3),id_cache,z,
     *        level,sign,loga,ccolor,
     *        itopr(float(zolf1),si1im,specw1,offset1,freq1),
     *        itopr(float(zolf2),si2im,specw2,offset2,freq2),
     *        itopr(float(zour1),si1im,specw1,offset1,freq1),
     *        itopr(float(zour2),si2im,specw2,offset2,freq2), clear)
          else
             call plot2dj(vd_id(3),id_cache,z,
     *        level,sign,loga,ccolor,
     *        itopr(float(1),si1im,specw1,offset1,freq1),
     *        itopr(float(1),si2im,specw2,offset2,freq2),
     *        itopr(float(si1im),si1im,specw1,offset1,freq1),
     *        itopr(float(si2im),si2im,specw2,offset2,freq2), clear)
          endif
          call gcolor(vd_id(3),color)
        else
           goto 124
        endif


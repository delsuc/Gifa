c angles are in degrees
      elseif (command.eq.'ALPHA') then
      	call getreal2(alpha,error)

      elseif (command.eq.'BETA') then
      	call getreal2(beta,error)

      elseif (command.eq.'GAMA') then
      	call getreal2(gama,error)

      elseif (command.eq.'SCALE3D') then
      	call getreal2(scale3d,error)

      elseif (command.eq.'CZ') then
      	call getreal2(cz,error)

      elseif (command.eq.'ZNOT') then
      	call getreal2(znot,error)

      elseif (command.eq.'OFFSET3D') then
        call message(' offset on X axis : ')
      	call getreal2(off3dx,error)
        call message(' offset on Y axis : ')
      	call getreal2(off3dy,error)
        call message(' offset on Z axis : ')
      	call getreal2(off3dz,error)

      elseif (command.eq.'AXIS3D') then
        call message('Contours in which direction ?')
        call getdim(axis3d,error)
        if (error.ne.0) then
             axis3d = 1
             goto 93
        endif

      elseif (command.eq.'DISP3D') then
        if (on_x_win.eq.0) goto 91
        call message('Open or close 3D display window (1/0)')
        zz = disp3d
        call getint2(disp3d,error)
        if (error.ne.0) goto 93
        if (disp3d.eq.1) then
      	  if (vd_id(10).eq.0) then	! opening
      	     st = '3D Display'
             call trailing(st,title_length)
      	     call win3d_open(vd_id(10),15.0,15.0,st,title_length)
             if (vd_id(10).eq.0) then
                   disp3d = 0
                   goto 128
             endif
C  call g3d_init()
      	  endif
        elseif (disp3d.eq.0) then
      	  if (vd_id(10).ne.0) then	! closing
             call win3d_close(vd_id(10))
             vd_id(10) = 0
          endif
        else
          disp3d = zz
          goto 98
        endif

      elseif (command.eq.'REF3D') then
        if (on_x_win.eq.0) goto 91
      	if (dim.ne.3)  goto 120
        if (disp3d.ne.1) goto 119
      	call g3d_setalpha(alpha*pi/180)
      	call g3d_setbeta(beta*pi/180)
      	call g3d_setgama(gama*pi/180)
      	call g3d_setscale(scale3d)
      	call g3d_setznot(znot)
      	call g3d_setoff(off3dx,off3dy,off3dz)
      	call plot3d(0,vd_id(10),si3d1,si3d2,si3d3,image,
     *        max0,scale,it3d,axis3d,cx,cy,cz,level,sign,loga,ccolor,
     *        zoom,zo3df1l,zo3df2l,zo3df3l,zo3df1u,zo3df2u,zo3df3u,
     *        clear)

      elseif (command.eq.'CHECK3D') then
        if (on_x_win.eq.0) goto 91
        if (dim.ne.3)  goto 120
        if (disp3d.ne.1) goto 119
	call message('Open or close 3D Box (1/0)')
	call getint2(zz,error)
	if (error.ne.0) goto 93
	if (zz.eq.1) then
        	call create_3dbox(vd_id(10))
	elseif(zz.eq.0) then
		call close_3dbox()
	endif

      elseif (command.eq.'PLOT3D') then
      	if (dim.ne.3)  goto 120
        st = '*PLOTTER'
        call getout2(st,error)
        if (error.ne.0) goto 93
        if (st.eq.'*SCREEN') then
            goto 106
        elseif (st(1:3).eq.'*PR') then
           goto 106
        else          ! else on file
           pl_id = 99
           call plopen(pl_id,st,error)
           if (error.ne.0) goto 101
        endif
      	call g3d_setalpha(alpha*pi/180)
      	call g3d_setbeta(beta*pi/180)
      	call g3d_setgama(gama*pi/180)
      	call g3d_setscale(scale3d)
      	call g3d_setznot(znot)
      	call g3d_setoff(off3dx,off3dy,off3dz)
      	call plot3d(pl_id,vd_id(10),si3d1,si3d2,si3d3,image,
     *        max0,scale,it3d,axis3d,cx,cy,cz,level,sign,loga,0,
     *        zoom,zo3df1l,zo3df2l,zo3df3l,zo3df1u,zo3df2u,zo3df3u,
     *        clear)
        if (pl_id.eq.99) call plclose(pl_id)

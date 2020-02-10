C disptypep.inc
C display commands
C
      elseif (command.eq.'REFPEAKS') then
         call message('Peak display  (0/1)')
         i = refpeaks
         call getint2(refpeaks,error)
         if (refpeaks.ne.i)  refresh=1

      elseif (command.eq.'REFSPECTRA') then
         call message('Spectral display (0/1)')
         i = refpeaks
         call getint2(refspectra,error)
         if (refspectra.ne.i) refresh=1
 
      elseif (command.eq.'REFMACRO') then
         call message('Display during macro (0/1)')
         call getint2(refmacro,error)

      elseif (command.eq.'VHEIGHT') then
         z = vheight
         call getreal2(z,error)
         if (error.ne.0) goto 93
         vheight = z
         if (dim.eq.1) refresh = 1

      elseif (command.eq.'SIGN') then
         call message(
     *   'Plot Positive(1), Negative(-1) or Both (0) levels? ')
         zz = sign
         call getint2(sign,error)
         if (sign.lt.-1 .or. sign.gt.1) then
                sign = 0
                goto 98
         endif
         if (zz.ne.sign) refresh = 1

      elseif (command.eq.'LEVEL') then
         call message("Number of contour levels :")
         zz = level
         call getint2(level,error)
         if (level.lt.1 .or. level.gt.64) then
              level = 8
              goto 98
         endif
         if (zz.ne.level .and. cdisp2d.ne.0) refresh = 1

      elseif (command.eq.'SCALE') then
         call message ("Scale Value")
         z=scale
         call getreal2(scale,error)
         if (z.ne.scale) refresh=1

      elseif (command.eq.'LOGA') then
         call message("Spacing between contour levels (1=equidistant)")
         z=loga
         call getreal2(loga,error)
         if (loga.lt.1.0) then
            loga = 1.0
            goto 98
         endif
         if (z.ne.loga .and. cdisp2d.eq.1 ) refresh=1

      elseif (command.eq.'STSKIP') then
        call getint2(stskip,error)

      elseif (command.eq.'STSHIFT') then
        call getreal2(stshift,error)

      elseif (command.eq.'STSKEW') then
        call getreal2(stskew,error)

      elseif (command.eq.'STDY') then
        z = stdy
        call getreal2(z,error)
        if (z.gt.cy) goto 98
        stdy = z

      elseif (command.eq.'STPL') then    ! do 2D stacked plot
        if (dim.ne.2) goto 97
        if (zoom.eq.1) goto 91
        call getout2(st,error)
        if (st(1:2).eq.'*S') then           ! plot on graphic screen
           pl_id = 0
C       Printer now supported...
        elseif (st(1:3).eq.'*PR')  then
           goto 106
        else                                   ! else on file
           pl_id = 99
           call plopen(pl_id,st,error)
           if (error.ne.0) goto 101
        endif
        call stplot(pl_id,plane2d,si1im,si2im,itype,
     *              scale,stskip,stshift,stskew,cx,cy,stdy)
        if (st(1:2).ne.'*S') then
           call plclose(pl_id)
        endif

      elseif (command.eq.'STPL?') then    ! check parameters for stpl
        call gifaout( 'Length of a single line (cx):  ')
        call getreal2(cx,error)
        if (error.ne.0) goto 93
        call gifaout( 'Height of the larger line (cy):  ')
        call getreal2(cy,error)
        if (error.ne.0) goto 93
        call gifaout( 'Scaling factor (scale):  ')
        call getreal2(scale,error)
        if (error.ne.0) goto 93
        call gifaout( 'Increment between each line (stdy):  ')
        call getreal2(stdy,error)
        if (error.ne.0) goto 93
        call gifaout( 'Skip every n-th line (stskip):  ')
        call getint2(stskip,error)
        if (error.ne.0) goto 93
        call gifaout( 'Global shifting of the plot (stshift):  ')
        call getreal2(stshift,error)
        if (error.ne.0) goto 93
        call gifaout( 'Skewing of the horizontal line (stskew):  ')
        call getreal2(stskew,error)
        if (error.ne.0) goto 93

      elseif (command.eq.'FREEZE') then
         if (on_x_win.eq.0) goto 91
         call message('Enter window (1:disd1d 2:disp2d 3:contour 4:3D)')
         zz = 1
         call getint2(zz,error)
         if (error.ne.0 .or. zz.lt.1 .or. zz.gt.4) goto 93
         call createfreeze(zz)
         if (zz.eq.0) goto 93
         refresh = 0

      elseif (command.eq.'ZM') then
        if (on_x_win.eq.0) goto 91
	call message('Open or close zoom box (1/0) ')
        call getint2(zz,error)
	if (error.ne.0) goto 93
        if (zz.eq.0) then
		call zoombox_close()
	elseif (zz.eq.1) then
		call create_zoombox()
		call zoom_catchcb(xx,yy,zz)
	endif

      elseif (command.eq.'ZOOM') then
        call message('Zoom mode on (1) or off (0)? ')
        zz = zoom
        call getint2(zz,error)
        if (zz.eq.0) then
           if (zz.ne.zoom) refresh=1
           zoom = zz
           if (on_x_win.ne.0) call trace_cadre_icon(0)
        elseif (zz.eq.1) then
             call message('Enter zoom window coordinates:  ')
             if (dim.eq.1)  then
               call getrect(zo1dl,xx,zo1du,yy,error)
             elseif (dim.eq.2) then
               call getrect(zolf2,zolf1,zour2,zour1,error)
             elseif (dim.eq.3) then
               call getcube(zo3df1l,zo3df2l,zo3df3l,zo3df1u,
     & zo3df2u,zo3df3u,error)

c old syntax removed with 4.10
c               write(st,'(i1)') axis2d
c               call message (' along axis F'//st)
c               dim = 1    ! play around with dim to fake getrect
c               if (axis2d.eq.1) then
c                 call getrect(zo3df1l,xx,zo3df1u,yy,error)
c                 call message (' on F23 plane')
c                 dim = 2
c                 call getrect(zo3df3l,zo3df2l,zo3df3u,zo3df2u,error)
c               elseif (axis2d.eq.2) then
c                 call getrect(zo3df2l,xx,zo3df2u,yy,error)
c                 call message (' on F13 plane')
c                 dim = 2
c                 call getrect(zo3df3l,zo3df1l,zo3df3u,zo3df1u,error)
c               elseif (axis2d.eq.3) then
c                 call getrect(zo3df3l,xx,zo3df3u,yy,error)
c                 call message (' on F12 plane')
c                 dim = 2
c                 call getrect(zo3df2l,zo3df1l,zo3df2u,zo3df1u,error)
c               else
c                 dim = 3
c                 goto 118
c               endif
c               dim = 3
             endif
             refresh=1
             zoom = zz
             nclick = 0
	     if (on_x_win.ne.0) call trace_cadre_icon(1)
        else
           goto 98
        endif

      elseif (command.eq.'DISP1D') then
         if (on_x_win.eq.0) goto 91
         call message('(0) turn 1D display off.')
         call message('(1) turn 1D display on.')
         zz = disp1d
         call getint2(disp1d,error)
         if (disp1d.lt.0 .or. disp1d.gt.1) then
            disp1d = zz
            goto 98
         endif
C opening a window
         if (zz.eq.0 .and. disp1d.ne.0) then
               zz1 = 16.8
               zz2=zz1/2
            if (vd_id(1).eq.0) then
               win_title = '1D Display'
               call trailing(win_title,title_length)
               call win_open_gene(vd_id(1),zz1,zz2,
     *                        win_title,title_length,0.0,0.0,10)
               if (vd_id(1).eq.0) then
                   disp1d = 0
                   goto 128
               endif
            endif
            refresh = 1
C closing a window
          elseif (zz.ne.0. .and. disp1d.eq.0) then
                call win_close(vd_id(1))
                vd_id(1) = 0
           endif

      elseif (command.eq.'DISP1DSZ') then
         if (on_x_win.eq.0) goto 91
         call message('(0) turn 1D display off.')
         call message('(1) turn 1D display on.')
         zz = disp1d
         call getint2(disp1d,error)
         if (disp1d.lt.0 .or. disp1d.gt.1) then
            disp1d = zz
            goto 98
         endif
         call message('Enter view size along X (in cm) :')
         zz1 = cx
         call getreal2(zz1,error)
         if (zz1 .lt. 0.0) goto 98
         if (error.ne.0)    goto 93
         call message('Enter view size along Y (in cm) :')
         zz2 = cy
         if (zz2 .lt. 0.0) goto 98
         call getreal2(zz2,error)
         if (error.ne.0)    goto 93
         call message('Enter view position along X (in cm) :')
         zz3 = 1.0
         call getreal2(zz3,error)
         if (error.ne.0)    goto 93
         call message('Enter view position along Y (in cm) :')
         zz4 = 1.0
         call getreal2(zz4,error)
         if (error.ne.0)    goto 93  ! ui finished
C opening a window
         if (zz.eq.0 .and. disp1d.ne.0) then
            if (vd_id(1).eq.0) then
               win_title = '1D Display'
               call trailing(win_title,title_length)
               call win_open_gene(vd_id(1),zz1,zz2,
     *                      win_title,title_length,zz3,zz4,10)
               if (vd_id(1).eq.0) then
                   disp1d = 0
                   goto 128
               endif
            endif
            refresh = 1
C closing a window
          elseif (zz.ne.0. .and. disp1d.eq.0) then
                call win_close(vd_id(1))
                vd_id(1) = 0
           endif

      elseif (command.eq.'DISP2D') then
           if (on_x_win.eq.0) goto 91
           call message('(0) turn 2D display off.')
           call message('(1) turn 2D display on.')
           zz = disp2d
           call getint2(disp2d,error)
           if (disp2d.ne.0 .and. disp2d.ne.1) then
               disp2d=zz
               goto 98
           endif
           if (zz.eq.1 .and. disp2d.eq.0) then
              call win_close(vd_id(2))
              vd_id(2) = 0
           elseif (zz.eq.0 .and. disp2d.eq.1) then
              win4 = 1
              win_title = '2D Display'
              call trailing(win_title,title_length)
              call win_open_gene(vd_id(2),
     *                cx,cy,win_title,title_length,0.0,0.0,win4)
              if (vd_id(2).eq.0) then
                   disp2d = 0
                   goto 128
              endif
             refresh = 1
           endif

      elseif (command.eq.'CDISP2D') then
           if (on_x_win.eq.0) goto 91
           call message('(0) turn contour plot off.')
           call message('(1) turn contour plot on.')
           zz = cdisp2d
           call getint2(cdisp2d,error)
           if (cdisp2d.ne.0 .and. cdisp2d.ne.1) then
               cdisp2d=zz
               goto 98
           endif
           if (zz.eq.1 .and. cdisp2d.eq.0) then
              call win_close(vd_id(3))
              vd_id(3) = 0
           elseif (zz.eq.0 .and. cdisp2d.eq.1) then
              win4 = 2
              win_title = 'Contour Plot'
              call trailing(win_title,title_length)
              call win_open_gene(vd_id(3),
     *              15.0,15.0,win_title,title_length,0.0,0.0,win4)
              if (vd_id(3).eq.0) then
                   cdisp2d = 0
                   goto 128
              endif
              refresh=1
           endif

      elseif (command.eq.'CDISP2DSZ') then
           if (on_x_win.eq.0) goto 91
           call message('(0) turn contour plot off.')
           call message('(1) turn contour plot on.')
           zz = cdisp2d
           call getint2(cdisp2d,error)
           if (cdisp2d.ne.0 .and. cdisp2d.ne.1) then
               cdisp2d=zz
               goto 98
           endif
           call message('Enter view size along X (in cm) :')
           zz1 = cx
           call getreal2(zz1,error)
           if (zz1 .lt. 0.0) goto 98
           if (error.ne.0)    goto 93
           call message('Enter view size along Y (in cm) :')
           zz2 = cy
           if (zz2 .lt. 0.0) goto 98
           call getreal2(zz2,error)
           if (error.ne.0)    goto 93
           call message('Enter view position along X (in cm) :')
           zz3 = 1.0
           call getreal2(zz3,error)
           if (error.ne.0)    goto 93
           call message('Enter view position along Y (in cm) :')
           zz4 = 1.0
           call getreal2(zz4,error)
           if (error.ne.0)    goto 93  ! ui finished
           if (zz.eq.1 .and. cdisp2d.eq.0) then
              call win_close(vd_id(3))
              vd_id(3) = 0
           elseif (zz.eq.0 .and. cdisp2d.eq.1) then
              win4 = 2
              win_title = 'Contour Plot'
              call trailing(win_title,title_length)
              call win_open_gene(vd_id(3),
     *              zz1,zz2,win_title,title_length,zz3,zz4,win4)
              if (vd_id(3).eq.0) then
                   cdisp2d = 0
                   goto 128
              endif
              refresh=1
           endif

      else if (command.eq.'MONOPOINT') then
         if (on_x_win.eq.0) goto 91
         call monopoint()

C POINT stack management
      elseif (command.eq.'POINT_PUSH') then
         if (nclick.eq.clickmax) goto 146
         if (nclick.eq.0) goto 145
         do i=nclick,1,-1
           clickt(i+1,1) = clickt(i,1)
           clickt(i+1,2) = clickt(i,2)
         enddo
         nclick = nclick + 1

      elseif (command.eq.'POINT_POP') then
         if (nclick.gt.0) then
           do i=1,nclick-1
             clickt(i,1) = clickt(i+1,1)
             clickt(i,2) = clickt(i+1,2)
           enddo
           nclick = nclick - 1
         endif

      elseif (command.eq.'POINT_INPUT') then
         if (dim.eq.1) then
           call message('Enter point coordinates')
           call getreal2(zz1,error)
           if (error.ne.0) goto 93
           clickt(1,1)=zz1
           clickt(1,2)=1
         else if (dim.eq.2) then
           call message('Enter point coordinates in F1')
           call getreal2(zz2,error)
           if (error.ne.0) goto 93
           call message('Enter point coordinates in F2')
           call getreal2(zz1,error)
           if (error.ne.0) goto 93
           clickt(1,2)=zz2
           clickt(1,1)=zz1
         else
           goto 124
         endif
         if (nclick.eq.0) nclick = 1

      elseif (command.eq.'POINT_CLEAR') then
         nclick = 0

c      elseif (command.eq.'ALERT') then
c         call message('Enter string')
c         st = 'Alert'
c         call getstring2(st,error)
c         if (error.ne.0) goto 91
c         if (on_x_win .eq. 1) then
c            call trailing(st,i)
c            call f_msg_ok('Alert',5,st,i,error)
c         else
c           call gifaout('Alert :'//st)
c         endif

      elseif (command.eq.'PH') then
         if (on_x_win.eq.0) goto 91
         if (dim.ne.1) goto 99
         if (disp1d.eq.0) goto 127
         if (it1d.ne.1) goto 104
         if (sizeimage1d.gt.sizemax) goto 103
	 call create_phasebox(column,vd_id(1),ph0,ph1,sizeimage1d)
         refresh = 1
         max0 = 0.0

      elseif (command.eq.'SHOWPATTERN') then
C SHOWPATTERN x sizex y sizey pat_type
C
C draws a pattern on screen, at location 'x y' (in spectral coordinate)
C     and of sizex , sizey  (in spectral coordinate)
C The pattern can be chosen with pat_type :
C             x  cross
C             +  cross
C             *  star
C             s  square
C             d  diamond
C             c  circle
         if (on_x_win.eq.0) goto 91
         if (dim.eq.1 .and. disp1d.eq.1) then
            zz = vd_id(1)
         elseif (dim.eq.2) then
            if (cdisp2d.ne.0) then
               zz = vd_id(3)
            elseif (disp2d.ne.0) then
               zz = vd_id(2)
            else
               goto 127
            endif
         else
             goto 124
         endif
C x - y
         call message
     *     ('Enter central X position (in spectral coord)')
         call getcoordf(zz1,2,error)
         if (error.ne.0) goto 93
         call message('Enter size along X')
         call getreal2(zz3,error)
         if (error.ne.0) goto 93
         if (dim.eq.1) then
            call message('Enter Y position (0-100%)')
            zz2=10.0
            call getreal2(zz2,error)
            if (error.ne.0) goto 93
            call message('Enter size along Y')
            call getreal2(zz4,error)
            if (error.ne.0) goto 93
            if (zoom.eq.0) then
              if (it1d.eq.0) then
                zz1 = (zz1-1) / (sizeimage1d-1)
              else
                zz1 = ((zz1-1)/2) / (sizeimage1d/2-1)
              endif
              zz3 = zz3/sizeimage1d
            else
              if (it1d.eq.0) then
                 zz1 = (zz1-zo1dl) / (zo1du-zo1dl)
              else
                 zz1 = ((zz1-zo1dl)/2) /
     *                 float(int((zo1du-zo1dl+1)/2)-1)
              endif
              zz3 = zz3/(zo1du-zo1dl+1)
            endif
            zz2 = zz2 / 100.0
            zz4 = zz4 / 100.0
         else
           call message
     *       ('Enter central Y position (in spectral coord)')
           call getcoordf(zz2,1,error)
           if (error.ne.0) goto 93
           call message('Enter size along Y')
           call getreal2(zz4,error)
           if (error.ne.0) goto 93
           if (zoom.eq.0) then
             zz1 = (zz1-0.5) / si2im
             zz2 = (zz2-0.5) / si1im
             zz3 = zz3/si2im
             zz4 = zz4/si1im
           else
             zz1 = (zz1-zolf2+0.5) / (zour2-zolf2+1)
             zz2 = (zz2-zolf1+0.5) / (zour1-zolf1+1)
             zz3 = zz3/(zour2-zolf2+1)
             zz4 = zz4/(zour1-zolf1+1)
           endif
         endif
         call message('Enter pattern: (+ x * s d o b) ')
         st = '+'
         call getstring2(st,error)
         if (error.ne.0) goto 93
         if (st.ne.'x' .and. st.ne.'+' .and. st.ne.'*'
     * .and. st.ne.'s' .and. st.ne.'d' .and. st.ne.'o'
     * .and. st.ne.'b') then
            goto 98
         endif
         call gcolor(zz,scolor)
         call showpattern(zz,zz1,zz2,zz3,zz4,st(1:1))
         call gcolor(zz,color)
         if (refmacro .eq.1) call win_update(zz)

      elseif (command.eq.'SHOWLINE') then
         if (on_x_win.eq.0) goto 91
         if (dim.eq.1 .and. disp1d.eq.1) then
            zz = vd_id(1)
         elseif (dim.eq.2) then
            if (cdisp2d.ne.0) then
               zz = vd_id(3)
            elseif (disp2d.ne.0) then
               zz = vd_id(2)
            else
               goto 127
            endif
         else
             goto 124
         endif
C From
         call message
     *     ('Enter initial X position (in spectral coord)')
         call getcoordf(zz1,2,error)
         if (error.ne.0) goto 93
         if (dim.eq.1) then
            call message('Enter Y position (0-100%)')
            zz2=10.0
            call getreal2(zz2,error)
            if (error.ne.0) goto 93
            if (zoom.eq.0) then
              if (it1d.eq.0) then
                zz1 = (zz1-1) / (sizeimage1d-1)
              else
                zz1 = ((zz1-1)/2) / (sizeimage1d/2-1)
              endif
            else
              if (it1d.eq.0) then
                 zz1 = (zz1-zo1dl) / (zo1du-zo1dl)
              else
                 zz1 = ((zz1-zo1dl)/2) /
     *                 float(int((zo1du-zo1dl+1)/2)-1)
              endif
            endif
            zz2 = zz2 / 100.0
         else
           call message
     *       ('Enter initial Y position (in spectral coord)')
           call getcoordf(zz2,1,error)
           if (zoom.eq.0) then
            zz1 = (zz1-0.5) / si2im
            zz2 = (zz2-0.5) / si1im
           else
            zz1 = (zz1-zolf2+0.5) / (zour2-zolf2+1)
            zz2 = (zz2-zolf1+0.5) / (zour1-zolf1+1)
           endif
         endif
C To
         zz3 = zz1+2
         zz4 = zz2+2
         call message
     *      ('Enter final X position (in spectral coord)')
         call getcoordf(zz3,2,error)
         if (error.ne.0) goto 93
         if (dim.eq.1) then
            if (it1d.eq.1) zz3 = 2*int((zz3-1)/2)+1
            call message('Enter Y position (0-100%)')
            zz4=10.0
            call getreal2(zz4,error)
            if (error.ne.0) goto 93
            if (zoom.eq.0) then
              if (it1d.eq.0) then
                zz3 = (zz3-1) / (sizeimage1d-1)
              else
                zz3 = ((zz3-1)/2) / (sizeimage1d/2-1)
              endif
              zz4 = zz4 / 100.0
            else
              if (it1d.eq.0) then
                 zz3 = (zz3-zo1dl) / (zo1du-zo1dl)
              else
                 zz3 = ((zz3-zo1dl)/2) /
     *                 float(int((zo1du-zo1dl+1)/2)-1)
              endif
              zz4 = zz4 / 100.0
            endif
         else
           call message
     *       ('Enter final Y position (in spectral coord)')
           call getcoordf(zz4,1,error)
           if (zoom.eq.0) then
            zz3 = (zz3-0.5) / si2im
            zz4 = (zz4-0.5) / si1im
           else
            zz3 = (zz3-zolf2+0.5) / (zour2-zolf2+1)
            zz4 = (zz4-zolf1+0.5) / (zour1-zolf1+1)
           endif
         endif
         call gcolor(zz,scolor)
         call win_plot_1d(zz,zz1,zz2,zz3,zz4)
         call gcolor(zz,color)
         if (refmacro .eq.1) call win_update(zz)

      elseif (command.eq.'SHOWLINETAB') then
C SHOWLINETAB unit number offsetx offsety
         if (on_x_win.eq.0) goto 91
         if (dim.eq.1) then
            if (disp1d.ne.1) goto 127
         else 
            goto 124
         endif
C unit
         call message('Enter unit for TAB buffer')
         st = unitx
         call getunit2(st,error)
         if (error.ne.0) goto 93
C number of points
         call message('Enter number of points to draw')
         call getint2(xx,error)
         if (error.ne.0) goto 93
         if (xx.lt.2) goto 93
         if (xx.gt.sizeimage1d) goto 117
C offsetx, offsety
         call message('Enter offset along X (in' // st //')')
         call getreal2(zz1,error)
         if (error.ne.0) goto 93
         call message('Enter offset along Y (in' // st //')')
         call getreal2(zz2,error)
         if (error.ne.0) goto 93
C call
         call gcolor(vd_id(1),scolor)
         call showlinetab(vd_id(1),sampling,column,xx,st,zz1,zz2)
         call gcolor(vd_id(1),color)
         if (refmacro .eq.1) call win_update(zz)

      elseif (command.eq.'SHOWTEXT') then
         if (on_x_win.eq.0) goto 91
         if (dim.eq.1 .and. disp1d.eq.1) then
            zz = vd_id(1)
         elseif (dim.eq.2) then
            if (cdisp2d.ne.0) then
               zz = vd_id(3)
            elseif (disp2d.ne.0) then
               zz = vd_id(2)
            else
               goto 127
            endif
         else
             goto 124
         endif
         call message('Enter string')
         st = name
         call getstring2(st,error)
         call message('Enter X position (in spectral coord)')
         call getcoordf(zz1,2,error)
         if (error.ne.0) goto 93
         if (dim.eq.1) then
            call message('Enter Y position (0-100%)')
            zz2 = 10.0
            call getreal2(zz2,error)
            if (error.ne.0) goto 93
            if (zoom.eq.0) then
              if (it1d.eq.0) then
                zz1 = (zz1-1) / (sizeimage1d-1)
              else
                zz1 = ((zz1-1)/2) / (sizeimage1d/2-1)
              endif
            else
              if (it1d.eq.0) then
                 zz1 = (zz1-zo1dl) / (zo1du-zo1dl)
              else
                 zz1 = ((zz1-zo1dl)/2) /
     *                 float(int((zo1du-zo1dl+1)/2)-1)
              endif
            endif
            zz2 = zz2 / 100.0
         else
           call message('Enter Y position (in spectral coord)')
           call getcoordf(zz2,1,error)
           if (zoom.eq.0) then
            zz1 = (zz1-0.5) / si2im
            zz2 = (zz2-0.5) / si1im
           else
            zz1 = (zz1-zolf2+0.5) / (zour2-zolf2+1)
            zz2 = (zz2-zolf1+0.5) / (zour1-zolf1+1)
           endif
         endif
         call trailing(st,xx)
         call gcolor(zz,scolor)
         call win_write(zz,zz1,zz2,st,xx)
         call gcolor(zz,color)
         if (refmacro .eq.1)  call win_update(zz)

      elseif (command.eq.'CLEAR') then
         if (on_x_win.eq.0) goto 91
         call message('Clear mode 1/0')
         call getint2(clear,error)
         if (error.ne.0 .or. clear.lt.0 .or. clear.gt.1) then
             clear = 1
             goto 93
         endif

      elseif (command.eq.'COLOR') then
         if (on_x_win.eq.0) goto 91
         call message('Enter color index')
         call message('1:white 2:red  3:yellow 4:green')
         call message('5:cian  6:blue 7:purple 8:black')
         call getint2(color,error)
         if (error.ne.0) goto 93
         call message('Enter window (1:disd1d 2:disp2d 3:contour 4:3D')
         zz = 1
         call getint2(zz,error)
         if (error.ne.0 .or. zz.lt.1 .or. zz.gt.4) goto 93
         if (zz.eq.4) zz=10
         if (vd_id(zz).ne.0) then
            call gcolor(vd_id(zz),color)
         else
            goto 127
         endif

      elseif (command.eq.'SCOLOR') then
         if (on_x_win.eq.0) goto 91
         call message('Enter color index')
         call message('1:white 2:red  3:yellow 4:green')
         call message('5:cian  6:blue 7:purple 8:black')
         call getint2(scolor,error)
         if (error.ne.0) goto 93

      elseif (command.eq.'CCOLOR') then
         if (on_x_win.eq.0) goto 91
         call message('Contour plots in color (0/1)')
         call getint2(ccolor,error)
         if (error.ne.0) goto 93

      elseif (command.eq.'BCOLOR') then
         if (on_x_win.eq.0) goto 91
         call message('Enter color index')
         call message('1:white 2:red  3:yellow 4:green')
         call message('5:cian  6:blue 7:purple 8:black')
         call getint2(bcol,error)
         if (error.ne.0) then
             bcol = 8
             goto 93
         endif
         call message('Enter window (1:disp1d 2:disp2d 3:contour 4:3D')
         call getint2(zz,error)
         if (error.ne.0 .or. zz.lt.1 .or. zz.gt.4) goto 93
         if (zz.eq.4) zz=10
         if (vd_id(zz).ne.0) then
            call bcolor(vd_id(zz),bcol)
         else
            goto 127
         endif

      elseif (command.eq.'AXIS') then
         if (on_x_win.eq.0) goto 91
         call message('Axis: NONE, F1, F2, F12, X, Y, XY')
         zz = axes
         call getdim(zz,error)
         if (error.ne.0)    goto 93
         if (zz.gt.3)	    goto 98
         if (zz.ne.axes) refresh=1
         axes = zz

      elseif (command.eq.'OPEN_VIEW') then
c OPEN_VIEW view_name x_size y_size x_pos y_pos
c
c opens a VIEW window, to be used with the DISP_VIEW command
c to display JOINed files
c x_size y_size x_pos y_pos are in cm
c
c view_name is the name of the view, any string will do,
c the list of the opened views is kept in the (regular) array
c $VIEW[view_name] so structure as
c foreach view_name in view
c    do someting with view_name
c endfor
c can be used.
c
c $view[view_name] does not hold anything usable.
c
c related contexts : $VIEW_NAME $VIEW[..]
c
c see also : CLOSE_VIEW DISP_VIEW SHOWC
c
         if (on_x_win.eq.0) goto 91
         call message('Enter view name :')
         call getstring2(view_name,error)
         if (error.ne.0)    goto 93
         call message('Enter view size along X (in cm) :')
         zz1 = cx
         call getreal2(zz1,error)
         if (error.ne.0)    goto 93
         call message('Enter view size along Y (in cm) :')
         zz2 = cy
         call getreal2(zz2,error)
         if (error.ne.0)    goto 93
         call message('Enter view position along X (in cm) :')
         zz3 = 1.0
         call getreal2(zz3,error)
         if (error.ne.0)    goto 93
         call message('Enter view position along Y (in cm) :')
         zz4 = 1.0
         call getreal2(zz4,error)
         if (error.ne.0)    goto 93  ! ui finished
         call trailing(view_name,i)
         st = '$VIEW[' // view_name(1:i) // ']'             ! store in view[]
         call checkvar(st,20,xx)
         if (xx.eq.0) then                 ! view already exists
            call gifaout('view :' // view_name // 'already exists')
         else                              ! else open and store
            call trailing(view_name,i)
            call win_open_gene(yy,zz1,zz2,view_name,i,zz3,zz4,4)
            if (yy.eq.0) goto 128
            ist = yy
            stp3 = st4
            call assign(st,20,stp3,error)
            if (error.ne.0) goto 128
         endif


      elseif (command.eq.'CLOSE_VIEW') then
c CLOSE_VIEW view_name
c
c closes a VIEW window, opened with OPEN_VIEW
c 
c related contexts : $VIEW_NAME $VIEW[..]
c
c see also : OPEN_VIEW
c
         if (on_x_win.eq.0) goto 91
         call message('Enter view name :')
         call getstring2(view_name,error)
         if (error.ne.0)    goto 93
         call trailing(view_name,i)
         st = '$VIEW[' // view_name(1:i) // ']'             ! store in view[]
         call checkvar(st,20,xx)
         if (xx.ne.0) goto 170                 ! view does not exist
         call getvar(st,20,stp3,error)
         st4 = stp3
         call win_close(ist)
         call dealloc(st,20,error)
         if (error.ne.0) goto 90

      elseif (command.eq.'DISP_VIEW') then
c DISP_VIEW view_name type(1D/2D) { axis index } f_low f_up / f1_low f2_low f1_up f2_up
c
c uses a VIEW window, opened with OPEN_VIEW to display the content
c of the currently joined data-set.
c type (1D or 2D) is the type of display (1D or 2D)
c fx_low fx_up are the zoom coordinates, ** in ppm ** not index !
c   2 parameters syntax is for 1D, 4 param. is for 2D
c
c all other display param. are taken from the current values.
c
c related contexts : $VIEW_NAME $VIEW[..] all graphic contexts
c
c see also : OPEN_VIEW CLOSE_VIEW SHOWC JOIN CDISP2D
         if (on_x_win.eq.0) goto 91
         if (id_cache.eq.0) goto 134
         call message('Enter view name :')
         call getstring2(view_name,error)
         if (error.ne.0)    goto 93
         call trailing(view_name,i)
         st = '$VIEW[' // view_name(1:i) // ']'             ! store in view[]
         call checkvar(st,20,xx)
         if (xx.ne.0) goto 170                 ! view does not exist
         call getvar(st,20,stp3,error)
         st4 = stp3
c ist is now the view Id (due to ist // st4 equivalence)
         call message('Enter dim of display  : 1D / 2D')
         stp2 = '2D'
         call getstring2(stp2,error)
         if (error.ne.0)    goto 93
         call uppercase(stp2,256)
         if (stp2.eq.'1D') then
            zz = 1
         elseif (stp2.eq.'2D') then
            zz = 2
         else
           goto 142
         endif
C now parse according to DIM (zz)
C   dim = 1
         if (zz.eq.1) then
            if (c_dimmanip .eq. 1) then  ! prepare zoom coord
              zz1 = itopr(float(zo1dl),sizeimage1d,sp1d,of1d,freq1d)
              zz2 = itopr(float(zo1du),sizeimage1d,sp1d,of1d,freq1d)
	    else if (c_dimmanip .eq. 2) then   ! prompt also for axis
              call message('view along which axis (F1 or F2)')
              call getdim(axis1d,error)
              if (error.ne.0) goto 93
              if (axis1d.eq.1) then
                zz1 = itopr(float(zolf1),si1im,specw1,offset1,freq1)
                zz2 = itopr(float(zour1),si1im,specw1,offset1,freq1)
              else if (axis1d.eq.2) then
                zz1 = itopr(float(zolf2),si2im,specw2,offset2,freq2)
                zz2 = itopr(float(zour2),si2im,specw2,offset2,freq2)
              else
                goto 98
              endif
              call message('which index :')
              call getint2(n1d1,error)
              if (error.ne.0) goto 93
            else if (c_dimmanip .eq.3) then
              goto 121
            endif
c now get coord
            call message('Enter lower zoom coordinate in ppm')
            call getreal2(zz1,error)
            if (error.ne.0)    goto 93
            call message('Enter upper zoom coordinate in ppm')
            call getreal2(zz2,error)
            if (error.ne.0)    goto 93
c transform coord
            if (c_dimmanip.eq.1) then
              win1 = c_sizef1*(c_offsf1 - zz1*c_freq1)/c_specwf1
     *           + c_sizef1 + 1.5
              win2 = c_sizef1*(c_offsf1 - zz2*c_freq1)/c_specwf1
     *           + c_sizef1 + 1.5     
              call chkborder2d(win1,win2,2,c_type)
            else if (c_dimmanip.eq.2) then
              if (axis1d.eq.1) then
                win1 = c_sizef1*(c_offsf1 - zz1*c_freq1)/c_specwf1
     *           + c_sizef1 + 1.5
                win2 = c_sizef1*(c_offsf1 - zz2*c_freq1)/c_specwf1
     *           + c_sizef1 + 1.5     
                call chkborder2d(win1,win2,2,c_type)
              elseif (axis1d.eq.2) then
                win1 = c_sizef2*(c_offsf2 - zz1*c_freq2)/c_specwf2
     *           + c_sizef2 + 1.5
                win2 = c_sizef2*(c_offsf2 - zz2*c_freq2)/c_specwf2
     *           + c_sizef2 + 1.5
                call chkborder2d(win1,win2,1,c_type)
              endif
            endif
c get data
            ww=win2-win1+1
            if (ww .gt. sizemax) goto 103
            if (c_dimmanip.eq.1) then
               call CH_AC1DAREA(work1d, id_cache,
     *	        win1, win2, CB_READ, error)
            else if (c_dimmanip.eq.2) then
              if (axis1d.eq.1) then
                call ch_ac2darea(work1d, id_cache,
     *            win1, n1d1,  win2, n1d1, CB_READ, error)
              elseif (axis1d.eq.2) then
               call ch_ac2darea(work1d, id_cache,
     *            n1d1, win1, n1d1,  win2, CB_READ, error)
              endif
            endif
	    if (error.eq.C_OUTSIDE) goto 108
	    if (error.ne.0) goto 93
	    call display1d(ist, ww, work1d,
     *                 c_max0,scale,c_type,vheight,clear)
            call setup_view(ist,1,zz1,zz1,zz2,zz2)

C   dim == 2         
         else if (zz.eq.2) then
            if (c_dimmanip .eq. 1) then
              goto 91
	    else if (c_dimmanip .eq. 2) then
              call message('Enter lower F1 zoom coordinate')
              zz1 = itopr(float(zolf1),si1im,specw1,offset1,freq1)
              call getreal2(zz1,error)
              if (error.ne.0)    goto 93
              call message('Enter lower F2 zoom coordinate')
              zz2 = itopr(float(zolf2),si2im,specw2,offset2,freq2)
              call getreal2(zz2,error)
              if (error.ne.0)    goto 93
              call message('Enter upper F1 zoom coordinate')
              zz3 = itopr(float(zour1),si1im,specw1,offset1,freq1)
              call getreal2(zz3,error)
              if (error.ne.0)    goto 93
              call message('Enter upper F2 zoom coordinate')
              zz4 = itopr(float(zour2),si2im,specw2,offset2,freq2)
              call getreal2(zz4,error)
              if (error.ne.0)    goto 93
            else if (c_dimmanip.eq.3) then
              goto 121
            endif
            call plot2dj(ist,id_cache,scale,
     *        level,sign,loga,ccolor,zz1,zz2,zz3,zz4,clear)
            call setup_view(ist,2,zz1,zz2,zz3,zz4)
         else
            goto 121
         endif


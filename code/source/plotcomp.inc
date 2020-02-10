C plotcomp.inc
C
C this file contains the plot commands
C
      elseif (command.eq.'PLOTTER') then
         call message('Enter plotter type : p(ostscript) / H(P-GL)')
         if (pl_mode.eq.hpgl) then
            st = 'HP-GL'
         else
            st = 'postscript'
         endif
         call getstring2(st,error)
         if (error.ne.0) goto 93
         if (st(1:1).eq.'p') then
            pl_mode = post
         else if (st(1:1).eq.'H') then
            pl_mode = hpgl
         else
            goto 93
         endif

      elseif (command.eq.'CX') then
         z = cx
         call getreal2(cx,error)
         if (cx.le.0.0 .or. error.ne.0) then
            cx=20.0
            goto 98
         endif

      elseif (command.eq.'CY') then
         z=cy
         call getreal2(cy,error)
         if (cy.le.0.0 .or. error.ne.0) then
            cy=20.0
            goto 98
         endif


      elseif (command.eq.'PLOTTEXT') then
         call message('Enter string')
         st = name
         call getstring2(st,error)
         if (error.ne.0) goto 93
         zz1 = 0.0
         zz2 = 0.0
         call message('Enter X position (in cm)')
         call getreal2(zz1,error)
         if (error.ne.0) goto 93
         call message('Enter Y position (in cm)')
         call getreal2(zz2,error)
         if (error.ne.0) goto 93
         stp = '*PLOTTER'
         call getout2(stp,error)
         if (error.ne.0) goto 93
C       No screen support yet...
        if (st(1:3).eq.'*PR') goto 106
        if (stp(1:2).eq.'*S') goto 106
         pl_id = 99
         call plopen(pl_id,stp,error)
         if (error.ne.0) goto 101
         zz=256
         call trailing(st,zz)
         call pltext(pl_id,st,zz,zz1,zz2)
         call plclose(pl_id)
        
      elseif (command.eq.'PLOTLINE') then
         zz1 = 0.0
         zz2 = 0.0
         call message(' From (X Y)  (in cm)')
         call getreal2(zz1,error)
         if (error.ne.0) goto 93
         call getreal2(zz2,error)
         if (error.ne.0) goto 93
         call message(' To   (X Y)  (in cm)')
         call getreal2(zz3,error)
         if (error.ne.0) goto 93
         call getreal2(zz4,error)
         if (error.ne.0) goto 93
         stp = '*PLOTTER'
         call getout2(stp,error)
         if (error.ne.0) goto 93
C       No screen support yet...
        if (st(1:3).eq.'*PR') goto 106
        if (stp(1:2).eq.'*S') goto 106
         pl_id = 99
         call plopen(pl_id,stp,error)
         if (error.ne.0) goto 101
         call plmove(pl_id,zz1,zz2)
         call pldraw(pl_id,zz3,zz4)
         call plclose(pl_id)

      elseif (command.eq.'PLOTTEXTS') then
         call message('Enter string')
         st = name
         call getstring2(st,error)
         if (error.ne.0) goto 93
         zz1 = 1
         zz2 = 1
         call message('Enter X position (in spectral coord)')
         call getcoordf(zz1,2,error)
         if (error.ne.0) goto 93
         if (dim.eq.1) then
            call message('Enter Y position (0-100%)')
            zz2 = 10.0
            call getreal2(zz2,error)
            if (error.ne.0) goto 93
            if (zoom.eq.0) then
              zz1 = (zz1-0.5) / sizeimage1d
              zz2 = zz2 / 100.0
            else
              zz1 = (zz1-zo1dl+0.5) / (zo1du-zo1dl+1)
              zz2 = zz2 / 100.0
            endif
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
c clip coordinates to 0,0 cx,cy
		 zz1 = max( 0.0, min(zz1*cx,cx))
		 zz2 = max( 0.0, min(zz2*cy,cy))
         stp = '*PLOTTER'
         call getout2(stp,error)
         if (error.ne.0) goto 93
C       No screen support yet...
        if (stp(1:2).eq.'*S') goto 106
        if (st(1:3).eq.'*PR') goto 106
         pl_id = 99
         call plopen(pl_id,stp,error)
         if (error.ne.0) goto 101
         zz=256
         call trailing(st,zz)
         call pltext(pl_id,st,zz,zz1,zz2)
         call plclose(pl_id)

      elseif (command.eq.'PLOTLINES') then
C From
         zz1 = 1
         zz2 = 1
         call message('From X,Y position (in spectral coord)')
         call getcoordf(zz1,2,error)
         if (error.ne.0) goto 93
         if (dim.eq.1) then
            call message('Enter Y position (0-100%)')
            zz2=10.0
            call getreal2(zz2,error)
            if (error.ne.0) goto 93
            if (zoom.eq.0) then
              zz1 = (zz1-0.5) / sizeimage1d
              zz2 = zz2 / 100.0
            else
              zz1 = (zz1-zo1dl+0.5) / (zo1du-zo1dl+1)
              zz2 = zz2 / 100.0
            endif
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
C To
         zz3 = 1
         zz4 = 1
         call message('To X,Y position (in spectral coord)')
         call getcoordf(zz3,2,error)
         if (error.ne.0) goto 93
         if (dim.eq.1) then
            call message('Enter Y position (0-100%)')
            zz4=10.0
            call getreal2(zz4,error)
            if (error.ne.0) goto 93
            if (zoom.eq.0) then
              zz3 = (zz3-0.5) / sizeimage1d
              zz4 = zz4 / 100.0
            else
              zz3 = (zz3-zo1dl+0.5) / (zo1du-zo1dl+1)
              zz4 = zz4 / 100.0
            endif
         else
           call message('Enter Y position (in spectral coord)')
           call getcoordf(zz4,1,error)
           if (zoom.eq.0) then
            zz3 = (zz3-0.5) / si2im
            zz4 = (zz4-0.5) / si1im
           else
            zz3 = (zz3-zolf2+0.5) / (zour2-zolf2+1)
            zz4 = (zz4-zolf1+0.5) / (zour1-zolf1+1)
           endif
         endif
c clip coordinates to 0,0 cx,cy
		 zz1 = max( 0.0, min(zz1*cx,cx))
		 zz2 = max( 0.0, min(zz2*cy,cy))
		 zz3 = max( 0.0, min(zz3*cx,cx))
		 zz4 = max( 0.0, min(zz4*cy,cy))
         stp = '*PLOTTER'
         call getout2(stp,error)
         if (error.ne.0) goto 93
C       No screen support yet...
        if (stp(1:2).eq.'*S') goto 106
        if (st(1:3).eq.'*PR') goto 106
         pl_id = 99
         call plopen(pl_id,stp,error)
         if (error.ne.0) goto 101
         call plmove(pl_id,zz1,zz2)
         call pldraw(pl_id,zz3,zz4)
         call plclose(pl_id)

c      elseif (command.eq.'TITLE') then
c         call message('Enter string')
c         st = name
c         call getstring2(st,error)
c         if (error.ne.0) goto 93
c         stp = '*PLOTTER'
c         call getout2(stp,error)
c         if (error.ne.0) goto 93
C       No screen support yet...
c        if (stp(1:2).eq.'*S') goto 106
c         pl_id = 99
c         call plopen(pl_id,stp,error)
c         if (error.ne.0) goto 101
c         zz=256
c         call trailing(st,zz)
c         call pltext(pl_id,st,zz,0.0,cy+1.0)
c         call plclose(pl_id)

      elseif (command.eq.'ROTATE') then
         call getint2(rotate,error)
          if (rotate.gt.1 .or. rotate.lt.0 .or. error.ne.0) then
            rotate = 0
            goto 98
         endif

      elseif (command.eq.'FORGET') then
         st = '*PLOTTER'
         call getout2(st,error)
         if (error.ne.0) goto 93
         call plforget(st,error)
         if (error.ne.0) goto 93

      elseif (command.eq.'PAGE') then
         pl_id = 99
         st = '*PLOTTER'
         call getout2(st,error)
         if (error.ne.0) goto 93
C       No screen support yet...
        if (st(1:2).eq.'*S') goto 106
        if (st(1:3).eq.'*PR') goto 106
         call plopen(pl_id,st,error)
         if (error.ne.0) goto 101
         call pleject(pl_id)

      elseif (command.eq.'PEN') then
         call message('Enter pen number (1-8):  ')
         zz = 1
         call getint2(zz,error)
         if (error.ne.0) goto 93
         if (zz.lt.1 .or. zz.gt.8) goto 98
         pl_id = 99
         st = '*PLOTTER'
         call getout2(st,error)
         if (error.ne.0) goto 93
C       No screen support yet...
        if (st(1:2).eq.'*S') goto 106
        if (st(1:3).eq.'*PR') goto 106
         call plopen(pl_id,st,error)
         if (error.ne.0) goto 101
         call plchpen(pl_id,zz)
         call plclose(pl_id)

      elseif (command.eq.'PCOLOR') then
         call message('Enter color index')
         call message('1:black 2:red  3:yellow 4:green')
         call message('5:cian  6:blue 7:purple 8:white')
         zz = 1
         call getint2(zz,error)
         if (error.ne.0) goto 93
         if (zz.lt.1 .or. zz.gt.8) goto 98
         pl_id = 99
         st = '*PLOTTER'
         call getout2(st,error)
         if (error.ne.0) goto 93
C       No screen support yet...
        if (st(1:2).eq.'*S') goto 106
        if (st(1:3).eq.'*PR') goto 106
         call plopen(pl_id,st,error)
         if (error.ne.0) goto 101
         call plcolor(pl_id,zz)
         call plclose(pl_id)

      elseif (command.eq.'PLOTOFFSET') then
         call message('Enter offset along x axis:  ')
         call getreal2(offx,error)
         if (error.ne.0) goto 93
         call message('Enter offset along y axis:  ')
         call getreal2(offy,error)
         if (error.ne.0) goto 93

      else if (command.eq.'PLOTAXIS?') then
        call getunit2(plotunit,error)
        if (error.ne.0) goto 93
        call message('Enter mark separation along x axis.')
        call getreal2(sepax,error)
        if (error.ne.0) goto 93
        if (sepax.eq.0.0) then
          sepax = 1.0
          goto 98
        endif
        call message('Enter mark separation along y axis.')
        call getreal2(sepay,error)
        if (error.ne.0) goto 93
        if (sepay.eq.0.0) then
          sepay = 1.0
          goto 98
        endif

      elseif (command.eq.'PLOTAXIS') then
        call message('Enter axis direction.')
        call message('(F1)  Plot Y Axis.')
        call message('(F2)  Plot X Axis.')
        call message('(F12)  Plot Both Axes.')
        zz = 2
        call getdim(zz,error)
        if (error.ne.0) goto 93
        if (zz.lt.1 .or. zz.gt.3) goto 98
        st = '*PLOTTER'
        call getout2(st,error)
        if (error.ne.0) goto 93
C       No screen support yet...
        if (st(1:2).eq.'*S') goto 106
        if (st(1:3).eq.'*PR') goto 106
        pl_id = 99
        call plopen(pl_id,st,error)
        if (error.ne.0) goto 101
        call plotaxis(pl_id,cx,cy,0.0,0.0,
     *               zz,plotunit,sepax,sepay,0)
        call plclose(pl_id)

      elseif (command.eq.'GRID') then
        call message('Enter axis direction.')
        call message('(F1)  Plot Y Axis.')
        call message('(F2)  Plot X Axis.')
        call message('(F12)  Plot Both Axes.')
        zz = 2
        call getdim(zz,error)
        if (error.ne.0) goto 93
        if (zz.lt.1 .or. zz.gt.3) goto 98
        st = '*PLOTTER'
        call getout2(st,error)
        if (error.ne.0) goto 93
C       No screen support yet...
        if (st(1:2).eq.'*S') goto 106
        if (st(1:3).eq.'*PR') goto 106
        pl_id = 99
        call plopen(pl_id,st,error)
        if (error.ne.0) goto 101
        call plotaxis(pl_id,cx,cy,0.0,0.0,
     *               zz,plotunit,sepax,sepay,1)
        call plclose(pl_id)

      else if (command.eq.'PLOT') then
C do contour plot of the data stored in "plane2d",
C param can be either *pl or name_of_file
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
        if (dim.eq.1) then
          if (zoom.eq.0) then
             call plot1d(pl_id,sizeimage1d,
     *                   column,max0,scale,it1d,cx,cy,vheight)
          else
             call plot1d(pl_id,zo1du-zo1dl+1,
     *                 column(zo1dl),max0,scale,it1d,cx,cy,vheight)
          endif
        else if (dim.eq.2) then
           call plot2d(pl_id,0,si1im,si2im,plane2d,
     *            max0,scale,itype,cx,cy,level,sign,loga,0,
     *                 zoom,zolf1,zolf2,zour1,zour2,clear)
        else if (dim.eq.3) then
            if (max0.eq.0.0) then
               write(*,*) 'Looking for the Maximum intensity'
               call mnxvect(mini,maxi,zz,i,plane2d,sizeimage3d)
               if (maxi.eq.0.0) maxi = 1.0
               max0 = maxi
            endif
            if (si1im*si2im.gt.256*256)
     *             write(*,*) 'Computing plot...'
            call plot2d(pl_id,0,si1im,si2im,
     *             plane2d,
     *               max0,scale,itype,cx,cy,level,sign,loga,0,
     *                    zoom,zolf1,zolf2,zour1,zour2,clear)
        endif
        if (pl_id.eq.99) call plclose(pl_id)


      else if (command.eq.'FPLOT') then
        if (dim.eq.1) goto 124
        if (dim.eq.2) then
            if (itype.ne.0) goto 104
        else if (dim.eq.3) then
            if (it3d.ne.0) goto 104
        endif
C do contour plot of the data stored in "plane2d",
C param can be either *pl or name_of_file
        st = '*PLOTTER'
        call getout2(st,error)
        if (error.ne.0) goto 93
        if (st.eq.'*SCREEN') then
            goto 106
        elseif (st(1:3).eq.'*PR') then
           goto 106
        else          ! else on file
           pl_id = 98
           open(unit=pl_id,file=st,iostat=error,form='unformatted',
     *          status='new')
           if (error.ne.0) goto 101
        endif
        if (dim.eq.2) then
            call fplot2d(pl_id,0,si1im,si2im,plane2d,
     *            max0,scale,itype,cx,cy,level,sign,loga,
     *                 zoom,zolf1,zolf2,zour1,zour2)
        else if (dim.eq.3) then
            if (max0.eq.0.0) then
               write(*,*) 'Looking for the Maximum intensity'
               call mnxvect(mini,maxi,zz,i,plane2d,sizeimage3d)
               if (maxi.eq.0.0) maxi = 1.0
               max0 = maxi
            endif
            call fplot2d(pl_id,0,si1im,si2im,
     *             plane2d,
     *               max0,scale,itype,cx,cy,level,sign,loga,
     *                    zoom,zolf1,zolf2,zour1,zour2)
        endif
        close(pl_id)







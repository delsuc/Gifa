      elseif (command.eq.'CLOSEBUTTON') then
         if (on_x_win.eq.0) goto 91
         call quit_all(error)
         if (error.ne.0) goto 140

      elseif (command.eq.'PULLDOWNMENU') then
         if (on_x_win.eq.0) goto 91
         call message('Pulldown Menu ON:1 or OFF:0')
         zz = pdmenu
         call getint2(pdmenu,error)
	 if (error.ne.0) goto 93
         if(zz.ne.pdmenu) then 
	  call set_pdmenu(pdmenu)
	  if (pdmenu.eq.2) then 
           call gifaout
     *  ('Cannot change Pulldown mode when the Menu Bar is open !')
	   pdmenu = zz
          endif
         endif
         if (error.ne.0) goto 140

      elseif (command.eq.'NUMTITLEROWS') then
         if (on_x_win.eq.0) goto 91
         call message('Title rows number:')
         zz = nbrowtitle
         call getint2(nbrowtitle,error)
         if (error.ne.0) goto 93
         if(zz.ne.nbrowtitle) then
          call set_nb_rows(nbrowtitle)
          if (nbrowtitle.eq.-1) then
           call gifaout
     *  ('Cannot change title rows number when the Menu Bar is open !')
           nbrowtitle = zz
          endif
         endif
         if (error.ne.0) goto 140


     
      elseif (command.eq.'BUTTONBOX') then
         if (on_x_win.eq.0) goto 91
         call message('Enter title for Button Box')
         st = 'Title'
         call getstring2(st,error)
         if (error.ne.0) goto 93
         call trailing(st,i)
         call creat_titre(st,i,win1)
         if (win1.eq.0) goto 136
         do while(.true.)
           call message('Enter list of label / command. Finish with *')
           call message('Enter command label')
           st = '*'
           call getstring2(st,error)
           if (st.eq.'*') goto 80
           if (error.ne.0) goto 93
           stp = st
           call uppercase(stp,256)
	   if (stp.eq.'SEPARATOR') then 
		call make_separateur(win1,error)
	   else
           	call message('Enter command')
           	stp = 'cmd'
           	call getstring2(stp,error)
           	if (error.ne.0) goto 93
           	call trailing(st,i)
           	call trailing(stp,j)
           	call butt_comm(win1,st,i,stp,j,error)
	   endif 
           if (error.ne.0) goto 136
         enddo
80       if (error.ne.0) goto 136
         call aff_all_titre(error)
         if (error.ne.0) goto 136

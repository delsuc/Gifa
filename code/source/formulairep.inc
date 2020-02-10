C formulairep.inc
C
C Construction des formulaires
C
C Les infos necessaires a envoyer pour les differents champs:
C	Widget_mere
C	Titre_formulaire
C	Commande_formulaire		
C	Label/Nom_de_fichier
C	Champs: string     message     text     action     enum      multienum	  cursor     file
C		var        *           *        commande   list      list	  min        var
C		def_val                         *          var       var_list	  max	    def_val
C               *                                          def_val   def_val_list dec_point  *
C                                                          *         *		  var
C                                                                   		  def_val
C								    		  *
C	
      elseif (command.eq.'FORMBOX'.or.command.eq.'DIALOGBOX') then
         if (on_x_win.eq.0) goto 91
	 if (command.eq.'FORMBOX') then
         	call message('Enter title for Form')
         	st = 'FormBox'
         	call getstring2(st,error)
         	if (error.ne.0) goto 93
         	call trailing(st,i)
	 	call message('Enter General Command of Form')
	 	stp2 = 'Command'
	 	call getstring2(stp2,error)
	 	if (error.ne.0) goto 93
	 	call trailing(stp2,j)
	 	if (error.ne.0) goto 93
		call creat_formulaire(st,i,stp2,j,win1,7)
                if (win1.eq.0) goto 136
	 elseif (command.eq.'DIALOGBOX') then
		call message('Enter title for Dialog box')
	        st = 'DialogBox'
         	call getstring2(st,error)
         	if (error.ne.0) goto 93
         	call trailing(st,i)
         	stp2 = ''
         	j = 0
         	call creat_formulaire(st,i,stp2,j,win1,22)
         	if (win1.eq.0) goto 136
         endif
         do while(.true.)
	   call message(
     *   'Enter Field Label, Filename, separator or * to end')
	   st = '*'
	   call getstring2(st,error)
           if (st.eq.'*') goto 87
           if (error.ne.0) goto 93
           stp = st
           call uppercase(stp,256)
           if (stp.eq.'SEPARATOR') then
                call make_separateur_form(win1,error)
	   elseif (stp.eq.'NORETURN') then
		call align_field_form(win1,error)
	   else
            call message('Enter Field Type')
            ii = 'string'
            call getstring2(ii,error)
            if (ii.eq.'*') goto 87
            if (error.ne.0) goto 93
            call uppercase(ii,256)
            stp = ''
            stp2 = 'null'
            stp3 = 'null'
	    stp4 = 'null'
	    stp5 = 'null'
            if(ii.eq.'MESSAGE'.or.ii.eq.'TEXT') then
	  	goto 89
	    elseif(ii.eq.'STRING'.or.ii.eq.'INT'.or.ii.eq.'REAL') then
		stp3 = 'var'
               	call message('Enter variable')
               	stp = 'var'
               	call getstring2(stp3,error)
               	if (error.ne.0) goto 93
               	stp = "$" // stp3
               	call message('Enter default value of variable')
               	stp3 = ''
               	call getstring2(stp3,error)
               	if (error.ne.0) goto 93
                if (ii.ne.'STRING') call leading(stp3)  ! do not remove leading blanks for strings
            elseif(ii.eq.'FILE') then
		stp3 = 'var'
                call message('Enter variable')
                stp = 'var'
                call getstring2(stp3,error)
                if (error.ne.0) goto 93
                stp = "$" // stp3
                call message('Enter default filename')
                stp3 = ''
                call getstring2(stp3,error)
                if (error.ne.0) goto 93
                call leading(stp3)
	     elseif(ii.eq.'ACTION') then
		if(command.ne.'DIALOGBOX') then
               		stp3 = ' '
               		call message('Enter action command ')
               		stp3 = 'command'
               		call getstring2(stp3,error)
                        if (error.ne.0) goto 93
		        call leading(stp3)
                else
			goto 91
		endif
	     elseif(ii.eq.'ENUM') then
		call message('Enter states of variables ",," ')
		stp2 = 'yes,no'
		call getstring2(stp2,error)
         	if (error.ne.0) goto 93
		stp3 = 'var'
                call message('Enter variable')
                stp = 'var'
                call getstring2(stp3,error)
                if (error.ne.0) goto 93
                stp = "$" // stp3
                call message('Enter default value of variable')
                stp3 = ' '
                call getstring2(stp3,error)
                if (error.ne.0) goto 93
             elseif(ii.eq.'MULTIENUM') then
                call message('Enter states of variables ",," ')
                stp2 = '1,2,3,4'
                call getstring2(stp2,error)
                if (error.ne.0) goto 93
                stp3 = 'var'
                call message('Enter variable')
                stp = 'var'
                call getstring2(stp3,error)
                if (error.ne.0) goto 93
                stp = "$" // stp3
                call message('Enter pre selected values ",," ')
                stp3 = ' '
                call getstring2(stp3,error)
                if (error.ne.0) goto 93
	     elseif(ii.eq.'CURSOR'.or.ii.eq.'CURSOR_REAL') then
		 call message('Enter min value')
		 stp2 = '0'
                 call getstring2(stp2,error)
                 if (error.ne.0) goto 93
                 call message('Enter max value')
                 stp4 = '100'
                 call getstring2(stp4,error)
                 if (error.ne.0) goto 93
                 call message('Enter number of decimal points')
                 stp5 = '0'
                 call getstring2(stp5,error)
                 if (error.ne.0) goto 93
                 stp3 = 'var'
                 call message('Enter Variable')
                 stp = 'var'
                 call getstring2(stp3,error)
                 if (error.ne.0) goto 93
                 stp = "$" // stp3
                 call message('Enter default value of variable')
                 stp3 = ' '
                 call getstring2(stp3,error)
                 if (error.ne.0) goto 93
                 call leading(stp3)
	      else
		 goto 91
	      endif
89            call trailing(stp3,n)
	      call trailing(stp2,it)
	      call trailing(stp4,it2)
	      call trailing(stp5,it3)
              call trailing(st,i)
              call trailing(stp,j)
              call trailing(ii,k)
              call create_champs(win1,st,i,stp,j,
     *         ii,k,stp2,it,stp3,n,stp4,it2,stp5,it3,error)
	      if (error.ne.0) goto 136
	   endif 
           if (error.ne.0) goto 136
         enddo
87       if (error.ne.0) goto 136
  	 call aff_formulaire(win1,error)
         if (error.ne.0) then
             if (command.eq.'DIALOGBOX') then
                goto 116
             else
                goto 136
             endif
         endif

      elseif (command.eq.'CLOSEFORM') then
	 if (on_x_win.eq.0) goto 91
	 call message('Enter the widget associated variable')
	 st = ' '
         call getstring2(st,error)
         if (error.ne.0) goto 93
         call trailing(st,i)
	 call close_box(st,i,error)
	 if (error.ne.0) goto 136


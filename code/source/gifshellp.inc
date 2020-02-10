C this file contains the commands for the gifa shell
C
      if (command(1:1).eq.';') then
         param = ' '
c         goto 3                    ! skip the line, comment

      elseif (command(1:1).eq.'@') then      ! execute a command file
c        code partially copied from the end of the elseif struct of execute.for
         if (input.eq.29) goto 113
         i = input-19
         buffer(i) = param            ! store command line

         call trailing(lcommand,j)  
         st = lcommand(2:j+1) // param     ! remove leading @
         param = st
         call getstring2( currinp(i),error)   ! parsing of lcommand permits @(expression)
         if (error.ne.0) goto 93

         if (input.eq.20) savecontext = vcontext
         input=input+1
         call opengif(currinp(i),input,error)   ! try to open as a file
         if (error.ne.0) then
            input = input - 1                 ! if not then error.
            goto 101
         endif
         if (debug.ne.0) then
           stp = '....................'
           call gifaout(stp(1:2*(input-20)) // lcommand)
         endif 
         vcontext = input
         param = ' '			! clear input line
         currline(i) = 0

      elseif (command(1:1).eq.'=') then      ! label, do nothing
         param = ' '
         goto 3

      elseif (command.eq.'GOTO') then      ! goto a label
         if (input.eq.20) goto 114
         call getstring2(stp,error)
         if (error.ne.0) goto 93
         st = '=' // stp
         call checkvar(st,vcontext,xx)
         if (xx.eq.0) then                 ! label already stored
              call getvar(st,vcontext,stp3,error)
              st4 = stp3
              call goto_line(ist+1,error)
              if (error.ne.0) goto 198
         else                              ! else go there and store
              call goto_label(st,error)
              if (error.ne.0) goto 198
              ist = currline(input-20)
              stp3 = st4
              call assign(st,vcontext,stp3,error)
              if (error.ne.0) goto 93
         endif
         goto 3

      elseif (command.eq.'SH') then      !do a shell
         if (param.eq.' ') then
#vms            write(*,*) 'Get back to GIFA by typing lo'
#_vms            write(*,*) 'Get back to GIFA by typing ^D'
            st = ' '
C if readline, then you should remove handler before spawning system
#readline         call rem_rl_handler
            call shell(st)          ! spawn to O.S.
            write(*,*) 'And Back to GIFA Again.........'
C and put it back (set_prompt will do it ok)
#readline        call trailing(prompt,i)
#readline        call set_prompt(prompt,i)
         else
            call getstring2(st,error)
            if (error.ne.0) goto 93
c            stp = 'csh -c "('
c            call trailing(st,zz)
c            call trailing(stp,xx)
c            yy = xx+zz
c            xx=xx+1
c            stp(xx:yy) = st(1:zz)
c            call trailing(stp,zz)
c            st = stp(1:zz) 
c            call trailing(st,zz)
c            yy = zz+2
c            zz = zz+1
c            st(zz:yy) = ')"'
            call shell(st)   ! do a OS command
         endif

      elseif (command.eq.'CD') then
         if (param.eq.' ') then
            st = home
         else
            call getstring2(st,error)
            if (error.ne.0) goto 93
         endif
         call changedir(st,error)
         if (error.ne.0) goto 90

      elseif (command.eq.'DEBUG') then
         call getint2(debug,error)
         if (debug.lt.0 .or. debug.gt.1) debug=0
         if (error.ne.0) goto 93

      elseif (command.eq.'VERBOSE') then
         call getint2(verbose,error)
         if (verbose.lt.0 .or. verbose.gt.1) verbose=0
         if (error.ne.0) goto 93

      elseif (command.eq.'TIMER') then
         zz  = timer
         if (zz.eq.1) then
               call show_timer   ! forces show if timer already set
         endif
         call getint2(timer,error)
         if (timer.eq.1) then
            if (zz.eq.0) call init_timer
         elseif (timer.ne.0) then
            timer = 0
            goto 98
         endif

      elseif(command.eq.'IF') then
63       call getint2(xx,error)
         call getstring2(st,error)
         if (error.ne.0) goto 93
         stp = st
         call uppercase(st,4)
         if (st.eq.'THEN') then		! the  -- if (test) then  -- case
           if (input.eq.20) goto 114
           if (xx.eq.0) then		! false
              call goto_endif(2,i,error)
              if (error.ne.0) goto 137
              if (i.eq.1) goto 63	! means - was elsif -
              goto 3
           endif
         else                       ! the  -- if (test) operation -- case
           if (xx.eq.0) then        !false
71            call trailing(param,zz)
#sgi          if (param(zz:zz).eq.'\\') then       ! continuation line
#f2c          if (param(zz:zz).eq.'\\') then       ! continuation line
#hp           if (param(zz:zz).eq.'\') then       ! continuation line
#aix          if (param(zz:zz).eq.'\\') then       ! continuation line
cdec          if (param(zz:zz).eq.'\\') then       ! continuation line
#sun          if (param(zz:zz).eq.'\\') then       ! continuation line
cgunix        if (param(zz:zz).eq.'\\') then     ! continuation line
call          if (param(zz:zz).eq.'\') then        ! continuation line
#vms          if (param(zz:zz).eq.'\') then        ! continuation line
                call getinput(1,error)
                goto 71
              endif
              param = ' '
              goto 3
           else
             call trailing(stp,i)
             st = stp(1:i)//' '//param      ! but back string on entry
             param = st
           endif
         endif

      elseif (command.eq.'ELSE') then
         if (input.eq.20) goto 114
         call goto_endif(1,i,error)
         if (error.ne.0) goto 137

      elseif (command.eq.'ELSIF') then
         if (input.eq.20) goto 114
         call goto_endif(3,i,error)
         if (error.ne.0) goto 137

      elseif (command.eq.'ENDIF') then
         if (input.eq.20) goto 114				! do nothing

      elseif (command.eq.'WHILE') then
c will build two pseudo-variables :
c 'n_line WHILE' where n_line is the address of current while
c                 contains the address of corresponding endwhile
c 'n_line2 ENDWHILE' where n_line2 is the address of corresponding endwhile
c                 contains the address of parent while
c address are code in integer thru the equivalence ist/st4
c
         if (input.eq.20) goto 114
         st4 = '    '
         ist = currline(input-20)
         stp3 = st4
         st = stp3(1:4)//' WHILE'		! name of 1st  pseudo-var
         call checkvar(st,vcontext,xx)
         if (xx.eq.1) then           ! if 1st time - search for ENDWHILE
             ! memorize where we are
             call assign(st,vcontext,'$reserved',xx)
             nest = 0
             location = currline(input-20)
             call goto_endif(4,i,error)       ! search ENDWHILE
             if (error.ne.0) goto 138
             ist = currline(input-20)
             stp3 = st4
             call assign(st,vcontext,stp3,error)  ! put addr of endwhile
             stp = stp3(1:4)//' ENDWHILE'      ! name of 2st  pseudo-var
             ist = location
             stp3 = st4
             call assign(stp,vcontext,stp3,error)  ! put addr of while
             call goto_line(location,error)        ! and rewind
             call getstring2(stp,error)
             if (error.ne.0) goto 200
             call uppercase(stp,5)
             if (stp.ne.'WHILE') goto 115
         endif
         call getint2(xx,error)
         if (error.ne.0) goto 93
         if (xx.eq.0) then		! false
            call getvar(st, vcontext, stp3, error)
            st4 = stp3
#assert     (ist.gt.1)
            call goto_line(ist+1,error)        ! jump over
            if (error.ne.0) goto 138
         endif         ! if true, nothing to do
      
      elseif (command.eq.'ENDWHILE') then
         if (input.eq.20) goto 114
         ist = currline(input-20)
         stp3 = st4
         st = stp3(1:4)//' ENDWHILE'			! name of  pseudo-var
         call getvar(st, vcontext, stp3, error)
         st4 = stp3
         if (error.ne.0) goto 138
#assert  (ist.ge.1)
         call goto_line(ist,error)           ! jump back
         if (error.ne.0) goto 138

      elseif (command.eq.'FOR') then
c FOR var = val1 TO val2 [ STEP val3 ]
c will build two pseudo-variables :
c 'n_line FOR' where n_line is the address of current for
c      contains the address of corresponding endfor
c      the current value, the target value and the increment of the variable
c      or the name of the array and the current key
c 'n_line2 ENDFOR' where n_line2 is the address of corresponding endfor
c                 contains the address of parent for
c
         if (input.eq.20) goto 114
         call getstring2(st,error)
         if (error.ne.0) goto 93		! get var name
         ii = '$' // st                         ! stored in ii
         st4 = '    '
         ist = currline(input-20)
         stp3 =st4
         st = stp3(1:4)//' FOR'			! name of 1st  pseudo-var
C         call checkvar(st,vcontext,xx)
C comefrom is set by by ENDFOR, so tells wheter we're comming from a jump
         if (comefrom.eq.0) then           ! if 1st time - search for ENDFOR
             ! memorize where we are
             call assign(st,vcontext,'$reserved',xx)
             location = currline(input-20)
             call goto_endif(5,i,error)       ! search ENDFOR
             if (error.ne.0) goto 139
             ist16(1) = currline(input-20)    ! memorize addr
             ist = currline(input-20)
             stp3 = st4
             stp = stp3(1:4)//' ENDFOR'      ! name of 2st  pseudo-var
             ist = location
             stp3 = st4
             call assign(stp,vcontext,stp3,error)  ! put addr of for
             call goto_line(location,error)        ! and rewind
             call getstring2(stp,error)        !FOR
             if (error.ne.0) goto 200
             call uppercase(stp,3)
             if (stp.ne.'FOR') goto 115
             call getstring2(stp,error)        ! var
             if (error.ne.0) goto 115
             call getstring2(stp,error)        ! =
             if (error.ne.0) goto 115
             if (stp .ne. '=') goto 115
             call getstring2(stp,error)      ! val1
             if (error.ne.0) goto 115
             call assign(ii,vcontext,stp,error) ! store initial value
             call readval(stp,dpz1,error)   ! interpret value
             ist16(2) = int(dpz1)
             call getstring2(stp,error)        ! TO
             if (error.ne.0) goto 115
             call uppercase(stp,2)
             if (stp .ne. 'TO') goto 115
             call getint2(ist16(3),error)      ! val2
             if (error.ne.0) goto 115
             if (param.eq.' ') then
               ist16(4) = 1
             else
               call getstring2(stp,error)        ! STEP or trailing comment
               if (error.ne.0) goto 115
               if (stp(1:1) .eq.';') then
                 ist16(4) = 1
                 param = ' '
               else
                 call uppercase(stp,4)
                 if (stp .ne. 'STEP') goto 115
                 call getint2(ist16(4),error)      ! val3
                 if (error.ne.0 .or. ist16(4).eq.0) goto 115
               endif
             endif
             ist16(2) = ist16(2)-ist16(4)       ! prepare 1st increm (a bit sloppy!)
             stp3 = st16
             call assign(st,vcontext,stp3,error)  ! put complete set in pseudo-var
         else     ! if first time
             comefrom = 0
             call getvar(st,vcontext,stp3,error)  ! get pseudovar
             st16 = stp3
             if (error.ne.0) goto 90
             call getvar(ii,vcontext,stp3,error)  ! get current value
             call readval(stp3,dpz1,error)   ! interpret value
             if (error.ne.0) goto 93
             ist16(2) = int(dpz1)
         endif    ! if first time
         ist16(2) = ist16(2) + ist16(4)	! realise incrementation
         if ((ist16(4).gt.0 .and. ist16(2).gt.ist16(3)) .or.
     *       (ist16(4).lt.0 .and. ist16(2).lt.ist16(3)) )then		! if Finished
#assert     (ist16(1).gt.1)
            call dealloc(st,vcontext,error)
            call goto_line(ist16(1)+1,error)        ! jump over
            if (error.ne.0) goto 139
         else
            stp3 = st16
            call assign(st,vcontext,stp3,error)    ! store new values
            write (stp,'(i9)') ist16(2)
            call leading(stp)
            call assign(ii,vcontext,stp,error)
            if (error.ne.0) goto 115
            param = ' '     ! works because the following = is taken for a =label !!!
         endif              ! ( Who said "very ugly ?" )
      
      elseif (command.eq.'FOREACH') then
c FOREACH var IN array  { WITHIN nD f1 f2 f3 f1 f2 f3 }
c will build two pseudo-variables :
c 'n_line FOREACH' where n_line is the address of current foreach
c      contains the address of corresponding endfor
c 'n_line2 ENDFOR' where n_line2 is the address of corresponding endfor
c                 contains the address of parent foreach
c
c if WITHIN is used, the coordinates of a zone are to be given
C with the same syntax as in zoom
         if (input.eq.20) goto 114
         call getstring2(st,error)
         if (error.ne.0) goto 93		! get var name
         ii = '$' // st
         st4 = '    '
         ist = currline(input-20)
         stp3 =st4
         st = stp3(1:4)//' FOREACH'		! name of 1st  pseudo-var
C         call checkvar(st,vcontext,xx)
C comefrom is set by by ENDFOR, so tells wheter we're comming from a jump
         if (comefrom.eq.0) then           ! if 1st time - search for ENDFOR
             ! memorize where we are
C             call assign(st,vcontext,'$reserved',xx)
             location = currline(input-20)
             call goto_endif(5,i,error)       ! search ENDFOR
             if (error.ne.0) goto 139
             win1 = currline(input-20)    ! memorize addr
             ist = currline(input-20)
             stp3 = st4
             stp = stp3(1:4)//' ENDFOR'      ! name of 2st  pseudo-var
             ist = location
             stp3 = st4
             call assign(stp,vcontext,stp3,error)  ! put addr of foreach
             call goto_line(location,error)        ! and rewind
             call getstring2(stp,error)        !FOREACH
             if (error.ne.0) goto 200
             call uppercase(stp,7)
             if (stp.ne.'FOREACH') goto 115
             call getstring2(stp,error)        ! var
             if (error.ne.0) goto 115
             call getstring2(stp,error)        ! in
             if (error.ne.0) goto 115
             call uppercase(stp,2)
             if (stp .ne. 'IN') goto 115
             call getstring2(stp5,error)      ! array
             if (error.ne.0) goto 115
                           ! now check syntax for WITHIN
             if (param.eq.' ') then
               fst28(1) = 0       ! 0 means no WITHIN code.
             else
               call getstring2(stp,error)        ! WITHIN or trailing comment
               if (error.ne.0) goto 115
               if (stp(1:1) .eq.';') then
                 param = ' '
                 fst28(1) = 0
               else
                 call uppercase(stp,6)
                 if (stp .ne. 'WITHIN') goto 115
                 call getint2(j,error)		! nD
                 if (j.lt.1 .or. j.gt.3 .or. error.ne.0) goto 139
                 fst28(1) = j		! keep dim
                 do i=1, 2*j
                    call getreal2(zz1,error)	! 2 coordinates per dim
                    if (error.ne.0) goto 98
                    fst28(1+i) = zz1	! store coord
                 enddo
               endif
             endif
             stp3 = ' '
             stp3(1:1) = char(127)
             call assign(ii,vcontext,stp3,error) ! store initial value
             ist = win1
             stp2 = st4
             stp3 = st28
             stp = stp2(1:4) // stp3(1:28) // stp5    ! addr // within_stuff
             call assign(st,vcontext,stp,error)  ! put complete set in pseudo-var
         endif    ! if first time
         call getvar(st,vcontext,stp3,error)
         if (error.ne.0) goto 98
         st4 = stp3(1:4)		! endfor address
         st28 = stp3(5:32)		! within code
         stp = stp3(33:)		! array name
         win1 = ist			! location of the endfor
         call getvar(ii, vcontext, stp3, error)    ! current key
         if (error.ne.0) goto 98
         if (fst28(1) .eq. 0.0) then       ! check for WITHIN code
           call nextelem(stp, vcontext, stp3, stp2, error)   ! get next
           if (error.eq.1 .and. comefrom.ne.0) then    ! may happend when foreach .. unset .. endfor
              error = 0
              call nextelem(stp, vcontext, ' ', stp2, error)   ! then try first
              if (error.ne.0) then
                 stp2 = ' '   ! in this case, we have gone thru
                 error = 0
              endif
           endif
           if (error.ne.0) goto 98
         else     ! WITHIN case
              xx = fst28(1)
333           call nextelem(stp, vcontext, stp3, stp2, error)   ! get next
              if (error.eq.1 .and. comefrom.ne.0) then    ! may happend when foreach .. unset .. endfor
                 error = 0
                 call nextelem(stp, vcontext, ' ', stp2, error)   ! then try first
                 if (error.ne.0) then
                    stp2 = ' '   ! in this case, we have gone thru
                    error = 0
                 endif
              endif
              if (error.ne.0) goto 98
              if (stp2 .eq. ' ' .or. stp2 .eq. char(127)) goto 334
              call trailing(stp,zz)
              call trailing(stp2,yy)
              stp3 = '$' // stp(1:zz) // '[' // stp2(1:yy) // ']'
              call getvar(stp3, vcontext, stp4, error)	! stp4 : var content
              stp3 = stp2   ! keep key (for looping)
              do j = 1,xx  ! check coord
                 call leading(stp4)		! remove leading blanks
                 k = index(stp4,' ')
                 read (stp4(1:k),*,IOSTAT=error) zz1
C                 call readval(stp4(1:k),zz1,error) ! read  entry
                 if (error.ne.0) then
                     error = 0
                     GOTO 333    ! skip invalid entries
                 endif
                 if (zz1 .lt. fst28(1+j) .or. zz1 .gt.fst28(1+j+xx))
     *                 goto 333 ! skip if outside             
                 stp4 = stp4(k+1:)
              enddo
334           continue
         endif
         param = ' '
         comefrom = 0
         if (stp2.eq.' ' .or. stp2(1:1).eq.char(127)) then     ! means finished
            call dealloc(st,vcontext,error)
c il faudrait aussi desallouer ENDFOR (idem dans le code FOR)
            call goto_line(win1+1,error)        ! jump over
            if (error.ne.0) goto 139
         else
            call assign(ii,vcontext,stp2,error)   ! store new value
            if (error.ne.0) goto 115
         endif


      elseif (command.eq.'ENDFOR') then
         if (input.eq.20) goto 114
         ist = currline(input-20)
         stp3 = st4
         st = stp3(1:4)//' ENDFOR'			! name of  pseudo-var
         call getvar(st, vcontext, stp3, error)
         if (error.ne.0) goto 139
         st4 = stp3
#assert  (ist.ge.1)
         call goto_line(ist,error)           ! jump back
         if (error.ne.0) goto 139
         comefrom = 1       ! will tell FOR/FOREACH not to start from scratch

      elseif (command.eq.'PRINT') then
         call message('String to output')
         call getstring2(st,error)
         if (error.ne.0) goto 93
         call cleanst(st,256)
         call gifaout(st)

      elseif (command.eq.'FPRINT') then
         call message('Enter output file')
         call getstring2(st,error)
         if (error.ne.0) goto 93
         inquire(file=st,number=i,opened=bool1)
         if (.not.bool1) goto 132
         call message('String to output')
         call getstring2(stp,error)
         if (error.ne.0) goto 93
         call trailing(stp,zz)
c_hp         write (i,*,err=102)stp(1:zz)
chp         write (i,'(A)',err=102)stp(1:zz)
        write (i,'(A)',err=102)stp(1:zz)

      elseif (command.eq.'MESSAGE') then
        call message('Enter string')
        call getstring2(st,error)
        if (error.ne.0) goto 93
        call message(st)
        cleanable = 1

      elseif (command.eq.'ERROR') then
         call getstring2(st,error)
         call cleanst(st,256)
         call gifaerr(st)
         goto 90

      elseif (command.eq.'DUMP') then
         call dumpvar

      elseif (command.eq.'CLOSE') then
         call getstring2(st,error)
         if (error.ne.0) goto 93
         inquire(file=st,number=i,opened=bool1)
         if (.not.bool1) goto 132
         close(i,status='keep')

      elseif (command.eq.'OPEN') then
         call getstring2(st,error)
         if (error.ne.0) goto 93
         inquire(file=st,number=i,opened=bool1)
         if (bool1) then		! already open means rewind
            if(i.ge.30 .and. i.le.39) then
                   rewind(i)
                   inpeof(i-29) = 0
            else
                   goto 132		! if here -> not OPENed file
            endif
         else
            bool1 = .true.			! search free unit
            do i=30,39
               if (bool1) then
                  inquire (unit=i,opened=bool1)
                  zz = i
               endif
            enddo
            if (bool1) goto 131		! means still not found
            open(zz,file=st,iostat=error)
            if (error.ne.0) goto 101
            inpeof(zz-29) = 0
         endif

      elseif (command.eq.'CONNECT') then
         call getstring2(st,error)
         if (error.ne.0) goto 93
         call jconnect(st,error)
         if (error.ne.0) goto 101
         
      elseif (command.eq.'DISCONNECT') then
         call disjconnect(error)
         if (error.ne.0) goto 90
         
      elseif (command.eq.'UNSET') then
         call message('Which variable ?')
         st = ' '
         call getstring2(st,error)
         if (error.ne.0) goto 115
         call trailing(st,xx)
         do i=1,xx
           if (.not. alphanum(st(i:i))) then
              if (st(i:i).ne.'[') goto 115              ! if array
              j = index(st,']')
              if (i+1.gt.j-1) goto 115
              stp = st(i+1:j-1)
              call eval(stp,ii,error)
              if (error.ne.0) goto 115
              stp = st(1:i) // ii
              call trailing(stp,xx)
              st = stp(1:xx) // ']'             !make name
              goto 158                                  ! break
           endif
         enddo
158      stp = '$' // st
         call dealloc(stp,vcontext,error)
         if (error.ne.0) goto 90

      elseif (command.eq.'MUNSET') then
         call message('Enter list of variables, finish with a *')
         st = '*'
35       call getstring2(st,error)
         if (error.ne.0) goto 90
         if (st.ne.'*') then
            stp = '$'//st
            call dealloc(stp,vcontext,error)
            if (error.ne.0) goto 90
            goto 35
         endif

      elseif (command.eq.'SET') then
         call getstring2(st,error)
         if (error.ne.0) goto 115
         call trailing(st,xx)
         do i=1,xx
           if (.not. alphanum(st(i:i))) then
              if (st(i:i).ne.'[') goto 115		! if array
              j = index(st,']')
              if (i+1.gt.j-1) goto 115
              stp = st(i+1:j-1)
              call eval(stp,ii,error)
              if (error.ne.0) goto 115
              stp = st(1:i) // ii
              call trailing(stp,xx)
              st = stp(1:xx) // ']'		!make name
              goto 159					! break
           endif
         enddo
159      ii = '$' // st
         st = ii
         call trailing(st,xx)
         if (xx.gt.varnmlen) goto 149
         stp = '='
         call getstring2(stp,error)
         if (stp.eq.'=') then
            zz = vcontext
         elseif (stp.eq.':=') then
           zz = 20 
         else
            goto 115
         endif
         call  checkvar(st,zz,xx)
         if (xx.eq.0) then
           call getvar(st,zz,ii,error)
         else
           ii = ' '
         endif
         call getstring2(ii,error)
	 if (error.ne.0) goto 93
         call assign(st,zz,ii,error)
         if (error.ne.0) goto 115

      elseif (command.eq.'INITINPROGRESS') then
        call message('how many iteration ?')
        call getint2(it,error)
        if (error.ne.0) goto 93
        call initinprog(it)

      elseif (command.eq.'INPROGRESS') then
        call message('which iteration ?')
        call getint2(j,error)
        if (error.ne.0) goto 93
        call inprog(j)

      elseif (command.eq.'SETPATH') then
        call message('Enter new path')
        stp = path
        call getstring2(stp,error)
        if (error.ne.0) goto 93
        path = stp

      elseif (command.eq.'SETPROMPT') then
        call message('Enter new prompt')
        call getstring2(prompt,error)
        if (error.ne.0) then
           prompt = 'Gifa>'
           goto 93
        endif
        call trailing(prompt,i)
        call set_prompt(prompt,i)

      elseif (command.eq.'DBOPEN') then
           call message('enter database name')
           call getstring2(stp,error)
           if (error.ne.0) goto 93
           call message('enter associative array name')
           call getstring2(st,error)
           if (error.ne.0) goto 93
           call dbmassign(stp,st,error)
           if (error.ne.0) goto 90

      elseif (command.eq.'DBCLOSE') then
           call message('enter associative array name')
           call getstring2(st,error)
           if (error.ne.0) goto 93
           call dbmdealloc(st,error)
           if (error.ne.0) goto 90


C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in CBS/Montpellier
C
C     Authors :       M.A.Delsuc
C                     C.B.S. Faculte de Pharmacie
C                     34060 Montpellier   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.

       subroutine goto_line(line,err)
c IN	: line
c
c jump to line "line" in the current input file
c used by the GIFA control langage
c
      implicit none
#include "gifshellv.inc"
      integer line,i,err

#assert (line.gt.0)

      if (line.lt.currline(input-20)) then
        rewind(input)
        currline(input-20) = 0
      endif
      do i = currline(input-20),line-1
         read(unit=input,fmt='(A256)',err=30,end=40) param
         currline(input-20) = currline(input-20)+1
      enddo
C Ok
      call cleanst(param,len(param))
      return

C error
30    err = 1
      return

C end of file
 40   param = ' '
      currline(input-20) = currline(input-20)+1
      return
      end
      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine goto_endif(job,retest,err)
c IN	: job
c OUT	: retest, err
c
c if job = 1 search for next ENDIF    ( jump after else - no else/elsif permitted)
c if job = 2 search for next ELSE / ELSIF / ENDIF ( jump after false if )
c       retest = 0 means was ELSE / ENDIF
c       retest = 1 means was ELSIF
c if job = 3 search for next ENDIF    ( jump after 'else' elsif )
c if job = 4 search for next ENDWHILE
c if job = 5 search for next ENDFOR
c
c updates currline, param

      implicit none
#include "gifshellv.inc"
      integer job,err,nest,retest,i
      character*(256) st,stt

#assert ((job.ge.1) .and. (job.le.5))
      nest = 0
      do while (0.eq.0)
              read(unit=input,fmt='(A256)',err=62,end=62) param
              currline(input-20) = currline(input-20)+1
              if (param.eq.' ') goto 61
              call cleanst(param,256)
              call leading(param)
c              call getstring2(st,err)
              i = 1
              do while (param(i:i) .ne. ' ' .and. i.lt.256)
                i = i+1
              enddo
              st = param(1:i)
              param = param(i:256)
              call uppercase(st,8)
              if (st.eq.'IF') then
c                 call getstring2(stt,err)	! read test
                 call parsest(param,stt,i,1,err)	! skip test
                 if (err.ne.0) goto 62
                 param = param(i:256)
c                 call parsest(param,stt,i,1,err) ! read then
c                 if (err.ne.0) goto 62
c                 stt = param(1:i)
c                 param = param(i:256)
                 call getstring2(stt,err)	! read then
                 call uppercase(stt,4)
                 if (stt.eq.'THEN') then	! increm only if if .. then
                    nest = nest+1
                 endif
              elseif (st.eq.'WHILE') then
                 nest = nest+1
              elseif (st.eq.'FOR') then
                 nest = nest+1
              elseif (st.eq.'FOREACH') then
                 nest = nest+1
              elseif (st.eq.'ENDIF' ) then
                 if (nest.eq.0) then          ! break
                    goto 63
                 else
                    nest = nest-1
                 endif
              elseif (st.eq.'ELSIF' .or. st.eq.'ELSE') then
                 if (nest.eq.0) goto 63       ! break
              elseif (st.eq.'ENDWHILE') then
                 if (nest.eq.0) then          ! break
                    goto 63
                 else
                    nest = nest-1
                 endif
              elseif (st.eq.'ENDFOR') then
                 if (nest.eq.0) then          ! break
                    goto 63
                 else
                    nest = nest-1
                 endif
              endif
              if (nest.lt.0) goto 62
 61   enddo

c found something :
63    if (err.ne.0) goto 62
      if (job.eq.1) then			! only ENDIF permitted
         if (st.eq.'ENDIF') then
            return
         else
            goto 62
         endif
      elseif (job.eq.2) then			! ELSE / ELSIF / ENDIF
         if (st.eq.'ENDIF' .or. st.eq.'ELSE') then
            retest = 0
            return
         else if (st.eq.'ELSIF') then
            retest = 1
            return
         else
            goto 62
         endif
      elseif (job.eq.3) then			! ENDIF is exit
         if (st.eq.'ENDIF') then
            return
         else if ( st.eq.'ELSE' .or. st.eq.'ELSIF') then   ! is continue
            goto 61
         endif
      elseif (job.eq.4) then
         if (st.eq.'ENDWHILE') then
            return
         else
            goto 62
         endif
      elseif (job.eq.5) then
         if (st.eq.'ENDFOR') then
            return
         else
            goto 62
         endif
      else
         goto 62
      endif
      
62    err = 1
      return

      end
              
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine goto_label(label,err)
c
c jumps to label - used by GOTO
c
c updates currline, param, etc..

      implicit none
#include "gifshellv.inc"
      integer err,llb,i
      character*32 label,st

         call trailing(label,llb)
         rewind(unit=input)
         currline(input-20) = 0

         do while (0.eq.0)
           read(unit=input,fmt='(A256)',err=70,end=70) param
           currline(input-20) = currline(input-20)+1
           if (param.eq.' ') goto 60
           i = 1
           do while(param(i:i) .eq. ' ')
              i = i+1
           enddo
c           call leading(param)
           if (param(i:i).ne.'=') goto 60
           st = param(i:llb+i)
           call uppercase(st,llb)
           if (st.eq.label) goto 65
 60      enddo

 65      return

70       err = 1
         return

         end

C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Authors :       M.A.Delsuc
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully 
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C
       subroutine braket(ax,bx,cx,fa,fb,fc,func,verb)
c IN	: func,verb
c OUT	: ax,bx,cx,fa,fb,fc
C  brakets the minimum value of function func with the three values ax,bx
C  cx. fa,fb,fc are the values of func at ax,bx,cx
c verb.eq.1 is verbose mode

       implicit none
       real    gold,glitmit,tiny
       parameter (gold=1.618034, glitmit=100.0, tiny=1.E-20)

       real   fa,fb,fc,func,ax,bx,cx       
       external func
       integer verb
       real   dum,ulim,u,r,q,fu

       fa=func(ax)
       fb=func(bx)
       if (fb.gt.fa) then
         dum=ax   
         ax=bx
         bx=dum
         dum=fb
         fb=fa
         fa=dum
       endif
       cx=bx+gold*(bx-ax)
       fc=func(cx)
1      if (fb.ge.fc) then
         if (verb.eq.1) write(*,*) 'Braket: a, b, c: ',ax,bx,cx
         if (verb.eq.1) write(*,*) '       fa,fb,fc: ',fa,fb,fc
         if (fb.eq.fc .and. fa.eq.fc) then
            if (verb.eq.1) write(*,*) 'Flat function, exiting'
            goto 2
         endif
         r=(bx-ax)*(fb-fc)
         q=(bx-cx)*(fb-fa)
         u=bx-((bx-cx)*q-(bx-ax)*r)/(2.*sign(max(abs(q-r),tiny),q-r))
         ulim=bx+glitmit*(cx-bx)
         if ((bx-u)*(u-cx).gt.0.) then
           fu=func(u)
           if (fu.lt.fc) then
             ax=bx
             fa=fb
             bx=u
             fb=fu
             go to 1
           else if (fu.gt.fb) then
             cx=u
             fc=fu
             go to 1
           endif
           u=cx+gold*(cx-bx)
           fu=func(u)
         else if ((cx-u)*(u-ulim).gt.0.) then
           fu=func(u)
           if (fu.lt.fc) then
             bx=cx
             cx=u
             u=cx+gold*(cx-bx)
             fb=fc
             fc=fu
             fu=func(u)
           endif
         else if ((u-ulim)*(ulim-cx).ge.0.) then
           u=ulim
           fu=func(u)
         else
           u=cx+gold*(cx-bx)
           fu=func(u)
         endif
         ax=bx
         bx=cx
         cx=u
         fa=fb
         fb=fc
         fc=fu
         go to 1
       endif     
 2     if (verb.eq.1) then
          write(*,*) '----- Done Braketing -----'
          write(*,*) 'Braket: a, b, c: ',ax,bx,cx
          write(*,*) '       fa,fb,fc: ',fa,fb,fc
       endif
       return
       end

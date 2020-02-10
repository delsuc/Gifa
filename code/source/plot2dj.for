c This file is a part of the GIFA program
c     This software has been developped by the NMR Group in
c      Gif/Yvette and in Montpellier
c
c     Authors :       M.A.Delsuc

c     This software cannot be used unless the user have been fully
c licensed to do so form the above author.
c     This file cannot be copied, duplicated or used in any other program,
c without written permission from the authors.
c

c    This subroutine is based on a program kindly provided by H.DELACROIX and
c J.L.RISLER,    C.G.M.   - C.N.R.S.
c   91190 Gif-Sur-Yvette
c---------------------------------------------------------------------

      subroutine plot2dj(vd_id,id,
     *        scaling,levels,sign2,loga,col,
     *        zolf1,zolf2,zour1,zour2,clear)
c IN	: all parameters
c
c  Plots a contour-plot of array a joined file, with `id'
c  levels =  number of levels
c  scaling = scaling factor
c  loga = 1.0 means equidistant levels,
c      .gt.1.0 means each level is *loga above the previous
c
c  sign =  1        -> plot only positive levels
c  sign = -1        -> plot only negative levels
c  sign = 0         -> plot both levels
c
c  col  = 1         -> levels are drawn in color
c
c  vd_id = visual display id
c                      The window must have been
c                      opened with 0-1 world coords.
c zolf1,zolf2,zour1,zour2 describe the zoom state (in ppm ! not indexes)
c
c clear = 1    -> clear screen before displaying
c
c---------------------------------------------------------------------
c
        implicit none


        integer vd_id,id
        integer levels
        integer sign2,col
        integer clear
        real zolf1,zolf2,zour1,zour2
        real scaling,loga

#include "controlc.inc"
#include "gcolor.inc"
#include "cachev.inc"

        integer mode
        integer thecolor

        integer NX,NY
        integer KMAX
        integer i,j,k
        integer nf,nl
        integer si1im,si2im
        integer llf1,llf2,lur1,lur2
        integer writingmode

        real nivmin
        real interv
        real iniv(64),niv
        real sign,max0

        integer jmin,jmax,jstep
        integer imin,imax,istep
        integer ii,jj,ith,jth
        integer jmi,imi,jma,ima,jst,jfin,ist,ifin
        integer iimax,jjmax
        integer fbox(64,64)
        integer ipt,last,ib,jb
        integer ntrav,first
        integer flag(4)

        real z11,z12,z21,z22
        integer buff_siz
        parameter (buff_siz=4096)
        real x(buff_siz+12),y(buff_siz+12)   ! see plot2d.for
        real z(64,64),zz(128*128)
#f2c        save x,y, z, zz, fbox   ! this is needed because of a limited size for local C variables
        integer error
	 

        logical remain

C-------------------------------------------------------------

        if (c_dimmanip.ne.2) then
        	call gifaerr('Only on 2D!')
        	return
        endif
        if (scaling.eq.0.0) then
            call gifaerr('Scaling is zero!')
            return
        endif

        mode = c_type
        if (mod(mode,2).eq.0) then
           istep = 1
        else
           istep = 2
        endif
        if (mode.lt.2) then
           jstep = 1
        else
           jstep = 2
        endif
C

c convert zoom to index.        
c           lur1 = (c_sizef1-1)*(c_offsf1 - zour1*c_freq1)/c_specwf1
c     *          + c_sizef1 + 0.5
c           llf1 = (c_sizef1-1)*(c_offsf1 - zolf1*c_freq1)/c_specwf1
c     *          + c_sizef1 + 0.5
c           lur2 = (c_sizef2-1)*(c_offsf2 - zour2*c_freq2)/c_specwf2
c     *          + c_sizef2 + 0.5
c           llf2 = (c_sizef2-1)*(c_offsf2 - zolf2*c_freq2)/c_specwf2
c     *          + c_sizef2 + 0.5
           lur1 = c_sizef1*(c_offsf1 - zour1*c_freq1)/c_specwf1
     *          + c_sizef1 + 1.5
           llf1 = c_sizef1*(c_offsf1 - zolf1*c_freq1)/c_specwf1
     *          + c_sizef1 + 1.5
           lur2 = c_sizef2*(c_offsf2 - zour2*c_freq2)/c_specwf2
     *          + c_sizef2 + 1.5
           llf2 = c_sizef2*(c_offsf2 - zolf2*c_freq2)/c_specwf2
     *          + c_sizef2 + 1.5
           call chkborder2d(lur1,llf1,1,c_type)
           call chkborder2d(lur2,llf2,2,c_type)
           if (lur1.le.llf1 .or. lur2.le.llf2) then
               call gifaerr('Wrong zoom window')
               return
           endif
           si1im = lur1-llf1+1
           si2im = lur2-llf2+1

        if (si1im.le.1 .or. si2im.le.1) then
           call gifaerr('Size too small for display')
           return
        endif

C nx, ny number of x and y partitions...
        nx = si2im/istep
        ny = si1im/jstep

           writingmode = 1
           call win_set_writing_mode(vd_id,writingmode)
           call win_enable_display_list(vd_id)
           if (clear.eq.1) call win_erase(vd_id)
C          Draw the frame on the screen
           x(1) = 0.0
           y(1) = 0.0
           x(2) = 1.0
           y(2) = 0.0
           x(3) = 1.0
           y(3) = 1.0
           x(4) = 0.0
           y(4) = 1.0
           x(5) = 0.0
           y(5) = 0.0

           call win_plot_array(vd_id,5,x,y)
           call win_update(vd_id)

        max0 = c_max0
        if (max0.eq.0.0) max0 = 1000000.0


C
	DO 1002 JMIN=0,NY-1,63     ! move the box
	JMAX=JMIN+63
C
	DO 1002 IMIN=0,NX-1,63
	IMAX=IMIN+63
C                 copy the matrix into z(ii,jj)
      error = 0
      jmi = (JMIN)*jstep+llf1
      imi = (IMIN)*istep+llf2
      jma = min( (JMAX)*jstep+llf1, lur1)
      ima = min( (IMAX)*istep+llf2, lur2)
      if (jma-jmi.eq.63 .and. ima-imi.eq.63  .and.
     *    jmi.ge.1  .and.  imi.ge.1          .and.
     *    jma.le.c_sizef1 .and. ima.le.c_sizef2 ) then
c   completely in => do it direct
c          write(*,*) 'direct'
          call CH_AC2DAREA(z, id, jmi, imi, jma, ima, CB_READ, error)
          if (error .ne.0) then
            call gifaerr('Error while accessing the file')
            return
          endif
          iimax = 63
          jjmax = 63
      elseif ((jmi.gt.c_sizef1 .or. imi.gt.c_sizef2) .or.
     *        (jma.lt.1 .or. ima.lt.1)) then
c   completely out => skip it
c          write(*,*) 'out!'
          goto 1002
      else
c    work it out !
c          write(*,*) 'do it : ',jmi,imi,jma,ima
          jst = max(jmi,1)			! coord of window to load
          ist = max(imi,1)
          jfin = min(jma,c_sizef1)
          ifin = min(ima,c_sizef2)
          call CH_AC2DAREA(zz,id,jst,ist,jfin,ifin,CB_READ,error)
          if (error .ne.0) then
            call gifaerr('Error while accessing the file')
            return
          endif
          iimax = 0
          jjmax = 0
          do ii = 1, 64
          do jj = 1, 64
             ith = imi+(ii-1)*istep		! location in data-set
             jth = jmi+(jj-1)*jstep
             if ((jth.ge.jst .and. jth.le.jfin) .and.
     *           (ith.ge.ist .and. ith.le.ifin)) then		! within zz window
              z(ii,jj) = zz((jth-jst)*(ifin-ist+1) + ith-ist+1 )
              iimax=max(iimax,ii-1)
              jjmax=max(jjmax,jj-1)
          else
              z(ii,jj) = 0.0
          endif
          enddo
          enddo

c          iimax = (ima-imi)/istep
c          jjmax = (jma-jmi)/jstep
c          iimax = 63
c          jjmax = 63
      endif
      if (error .ne.0) then
        call gifaerr('Error while accessing the file')
        return
      endif

       sign = sign2

C compute levels, according to sign
10000   nivmin = max0/(scaling*32)         ! lowest level
        if (sign.eq.-1) nivmin=-nivmin     ! plot negative levels
        interv = nivmin                    ! inter level spacing

C
C	VALEUR DES COURBES DE NIVEAU DANS INIV(64)
	IF (levels.EQ.1) THEN
	  KMAX=1
	  INIV(1)=NIVmin
        ELSE
           KMAX = levels
           KMAX = MIN(KMAX,64)
           iniv(1) = nivmin
           if (loga.gt.1.0) then
              do i=2,kmax
                 iniv(i) = iniv(i-1)*loga
              enddo
           else
	      DO 255 I=2,KMAX
        	 INIV(I)=INIV(I-1)+INTERV
255	      CONTINUE
           endif
      ENDIF

      remain=.true.        ! remain.eq..false. ==> no plotting left,
      DO 1001 K=1,KMAX    ! sur les niveaux
C
        if (.not.remain) then    !no plotting left
           goto 1010
        endif
      NIV=INIV(K)
CCCCCCCCCCCCCCCCCCCCCCCc
          if (col.eq.1) then
            if (sign.eq.-1) then
             thecolor = black-max(int(black*float(k)/float(kmax)),1)
            else
             thecolor = black+max(int((black+1)*float(k)/float(kmax)),1)
            endif

            if (thecolor.gt.63) then
c              write(*,*) thecolor
              thecolor = 63
            endif

            if (thecolor.lt.0) then
c              write(*,*) thecolor
              thecolor = 0
            endif
            call win_fgcolor(vd_id,thecolor)
          endif
CCCCCCCCCCCCCCCCCCCCCCCc
C
	DO 320 II=1,63
	DO 320 JJ=1,63   !met fbox a 1
	  FBOX(II,JJ)=1
320	CONTINUE
C

       if ((niv.gt.0.0 .and. z(1,1).lt.niv) .or. 
     *     (niv.lt.0.0 .and. z(1,1).gt.niv))    then
           remain = .false.		! assume no plotting left to do
       else
           remain = .true.		! unless niv is below this point
       endif
	IPT=0
	LAST=1
C
	DO 1000 IB=1,iimax
	DO 1000 JB=1,jjmax      ! dans la boite
C
C	IL FAUT DEMARRER POUR CELA ON CHERCHE UN POINT
C	QUELCONQUE EN SUIVANT LES LIGNES ET LES COLONNES
C
	IF(FBOX(IB,JB).EQ.0) GOTO 1000	! PLUS RIEN A VOIR
C
	Z11=Z(IB,JB)-NIV
	Z21=Z(IB+1,JB)-NIV
	Z12=Z(IB,JB+1)-NIV
	Z22=Z(IB+1,JB+1)-NIV
C

	IF(FBOX(IB,JB).GE.2)GOTO 35
C
	DO 15 NF=1,4
	FLAG(NF)=0
15	CONTINUE
	NTRAV=0
C
	IF((Z11.GT.0.AND.Z12.LT.0).OR.
     +	(Z11.LT.0.AND.Z12.GT.0))THEN
	 FLAG(1)=1
	 NTRAV=NTRAV+1
	ENDIF
C
	IF((Z11.GT.0.AND.Z21.LT.0).OR.
     +	(Z11.LT.0.AND.Z21.GT.0))THEN
	 FLAG(2)=1
	 NTRAV=NTRAV+1
	ENDIF
C
	IF((Z21.GT.0.AND.Z22.LT.0).OR.
     +	(Z21.LT.0.AND.Z22.GT.0))THEN
	 FLAG(3)=1
	 NTRAV=NTRAV+1
	ENDIF
C
	IF((Z22.GT.0.AND.Z12.LT.0).OR.
     +	(Z22.LT.0.AND.Z12.GT.0))THEN
	 FLAG(4)=1
	 NTRAV=NTRAV+1
	ENDIF
C
	GOTO(25,1000,30,1000,35)(NTRAV+1)
C
25	FBOX(IB,JB)=0		! AUCUN COTE TRAVERSE
	GO TO 1000		! PLUS RIEN A VOIR
C
C	CAS SIMPLE TRAVERSEE
C
30	FIRST=0
	LAST=0
C
	DO 20 NF=1,4
	IF(FLAG(NF).EQ.0)GOTO 20
	IF(FIRST.EQ.0)THEN
	  FIRST=NF
	ELSE
  	  LAST=NF
	  FBOX(IB,JB)=0
	  GO TO 40
	ENDIF
20	CONTINUE
C
C	CAS DES DOUBLES TRAVERSEES
C
35	GO TO(1000,31,32,33)(FBOX(IB,JB)+1)
C
C	CAS DES DEUX TRAVERSEES POUR LA PREMIERE FOIS
C
31	FIRST=1
	LAST=2
	FBOX(IB,JB)=2
	GO TO 40
C
C	CAS D'UNE DEUXIEME TRAVERSEE
C
32	FIRST=3
	LAST=4
	FBOX(IB,JB)=0
	GO TO 40
C
33	FIRST=1
	LAST=2
	FBOX(IB,JB)=0
C
C	DETERMINATION DES POINTS DE DEPART
C
40	IPT=IPT+1
	GOTO(41,42,43,44)FIRST
C
41	X(IPT)=FLOAT(IB+IMIN-1)
	Y(IPT)=FLOAT(JB+JMIN-1)+(Z11/(Z11-Z12))
	GO TO 45
C
42	X(IPT)=FLOAT(IB+IMIN-1)+(Z11/(Z11-Z21))
	Y(IPT)=FLOAT(JB+JMIN-1)
	GO TO 45
C
43	X(IPT)=FLOAT(IB+IMIN)
	Y(IPT)=FLOAT(JB+JMIN-1)+(Z21/(Z21-Z22))
	GO TO 45
C
44	X(IPT)=FLOAT(IB+IMIN-1)+(Z12/(Z12-Z22))
	Y(IPT)=FLOAT(JB+JMIN)
C
45	IPT=IPT+1
	GOTO(46,47,48,49)LAST
C
46	X(IPT)=FLOAT(IB+IMIN-1)
	Y(IPT)=FLOAT(JB+JMIN-1)+(Z11/(Z11-Z12))
	GO TO 50
C
47	X(IPT)=FLOAT(IB+IMIN-1)+(Z11/(Z11-Z21))
	Y(IPT)=FLOAT(JB+JMIN-1)
	GO TO 50
C
48	X(IPT)=FLOAT(IB+IMIN)
	Y(IPT)=FLOAT(JB+JMIN-1)+(Z21/(Z21-Z22))
	GO TO 50
C
49	X(IPT)=FLOAT(IB+IMIN-1)+(Z12/(Z12-Z22))
	Y(IPT)=FLOAT(JB+JMIN)
C
C	POURSUITE DE LA COURBE
C
50	I=IB
	J=JB
C
80	CONTINUE
C
	IF(IPT.EQ.512)GOTO 90
C
	GOTO(81,82,83,84)LAST
C
81	I=I-1
	IF(I.LT.1)GOTO 90
	FIRST=3
	GO TO 85
C
82	J=J-1
	IF(J.LT.1)GOTO 90
	FIRST=4
	GO TO 85
C
83	I=I+1
	IF(I.GT.iimax)GOTO 90
	FIRST=1
	GO TO 85
C
84	J=J+1
	IF(J.GT.jjmax)GOTO 90
	FIRST=2
C
85	IF(FBOX(I,J).EQ.0)GO TO 90
C
	Z11=Z(I,J)-NIV
	Z21=Z(I+1,J)-NIV
	Z12=Z(I,J+1)-NIV
	Z22=Z(I+1,J+1)-NIV
C
	IF(FBOX(I,J).GE.2)GOTO 135
C
	DO 86 NF=1,4
	FLAG(NF)=0
86	CONTINUE
	NTRAV=0
C
	IF((Z11.GT.0.AND.Z12.LT.0).OR.
     +	(Z11.LT.0.AND.Z12.GT.0))THEN
	 FLAG(1)=1
	 NTRAV=NTRAV+1
	ENDIF
C
	IF((Z11.GT.0.AND.Z21.LT.0).OR.
     +	(Z11.LT.0.AND.Z21.GT.0))THEN
	 FLAG(2)=1
	 NTRAV=NTRAV+1
	ENDIF
C
	IF((Z21.GT.0.AND.Z22.LT.0).OR.
     +	(Z21.LT.0.AND.Z22.GT.0))THEN
	 FLAG(3)=1
	 NTRAV=NTRAV+1
	ENDIF
C
	IF((Z22.GT.0.AND.Z12.LT.0).OR.
     +	(Z22.LT.0.AND.Z12.GT.0))THEN
	 FLAG(4)=1
	 NTRAV=NTRAV+1
	ENDIF
C
	GO TO(1000,130,1000,135)NTRAV
C
C	CAS SIMPLE TRAVERSEE
C
130	DO 105 NL=1,4
	IF(FLAG(NL).EQ.0)GOTO 105
	IF(NL.EQ.FIRST)GOTO 105
	LAST=NL
	FBOX(I,J)=0
	GOTO 170
105	CONTINUE
C
C	CAS DES DOUBLES TRAVERSEES
C
135	GO TO(1000,150,160,160)(FBOX(I,J)+1)
C
C	CAS DES DEUX TRAVERSEES POUR LA PREMIERE FOIS
C
150	GO TO(151,152,153,154)LAST
151	FBOX(I,J)=2
	LAST=2
	GOTO 170
152	FBOX(I,J)=2
	LAST=1
	GOTO 170
153	FBOX(I,J)=3
	LAST=4
	GOTO 170
154	FBOX(I,J)=3
	LAST=3
	GOTO 170
C
C	CAS D'UNE DEUXIEME TRAVERSEE
C
160	FBOX(I,J)=0
	GO TO(161,162,163,164)LAST
161	LAST=2
	GOTO 170
162	LAST=1
	GOTO 170
163	LAST=4
	GOTO 170
164	LAST=3
	GOTO 170
C
170	IPT=IPT+1
	GOTO(111,112,113,114)LAST
C
111	 X(IPT)=FLOAT(I+IMIN-1)
	 Y(IPT)=FLOAT(J+JMIN-1)+(Z11/(Z11-Z12))
	 GO TO 80
112	 X(IPT)=FLOAT(I+IMIN-1)+(Z11/(Z11-Z21))
	 Y(IPT)=FLOAT(J+JMIN-1)
	 GO TO 80
113	 X(IPT)=FLOAT(I+IMIN)
	 Y(IPT)=FLOAT(J+JMIN-1)+(Z21/(Z21-Z22))
	 GO TO 80
114	 X(IPT)=FLOAT(I+IMIN-1)+(Z12/(Z12-Z22))
	 Y(IPT)=FLOAT(J+JMIN)
	 GO TO 80
C
90	CONTINUE
C
C	ECRITURE DES POINTS DANS LE FICHIER DE SORTIE
C
c	WRITE(2)INIV(K),IPT,((X(M),Y(M)),M=1,IPT)
C
C Send IPT points to the plotter
        if (ipt.gt.1) then
           remain = .true.                ! still plotting to do.
             do i=1,ipt
               x(i) = (x(i)+0.5)/float(nx)
               y(i) = (y(i)+0.5)/float(ny)
c               y(i) = (y(i)+1.0)/float(ny-1)	!!!!!!!!!!!!!!
             enddo
             call win_plot_array(vd_id,ipt,x,y)
            IF(IPT.EQ.512)THEN
              X(1)=X(512)
              Y(1)=Y(512)
              IPT=1
              GO TO 80
            ENDIF
        endif
        ipt = 0
C
1000	CONTINUE  !dans la boite
        if (control.eq.1) return		! test for abort
1001	CONTINUE  !sur les niveaux
1010    if (sign.eq.0) then     !if plot both, then do now negatives
           sign=-1
           goto 10000
        endif
        call win_update(vd_id)	! update image
1002	CONTINUE  !deplace la boite

C Final
        call win_update(vd_id)	! update image
        return
C
C
	END




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
C    This subroutine is based on a program kindly provided by H.DELACROIX and
C J.L.RISLER,    C.G.M.   - C.N.R.S.
C   91190 Gif-Sur-Yvette
C---------------------------------------------------------------------
      subroutine plot3d(pl_id,vd_id,si1,si2,si3,cube,
     *  maxi,scaling,mode2,direcd,dx3d,dy3d,dz3d,levels,sign2,loga,col,
     *  zoom,zl1,zl2,zl3,zu1,zu2,zu3,clear)
c IN	: all parameters, but
c INOUT	: maxi
C
C  Plots a 3D contour-plot of cube matrix(si1,si2,si3)
C  levels =  number of levels
C  scaling = scaling factor (0 means just the cube)
C  loga = 1.0 means equidistant levels,
C      .gt.1.0 means each level is *loga above the previous
C  drawing size =  dx x dy x dz (in cm.)
C  mode = 0         -> plots everything
C  mode = 1         -> one over two in dim 3
C  mode = 2         -> one over two in dim 2
C  mode = 3         -> one over two in dim 3 and 2
C  mode = 4         -> one over two in dim 1
C  mode = 5         -> one over two in dim 1 and 3
C  mode = 6         -> one over two in dim 1 and 2
C  mode = 7         -> one over two in dim 1 and 2 and 3
C
C  if direc = 1     -> contour on planes 1
C  if direc = 2     -> contour on planes 2
C  if direc = 4     -> contour on planes 3
C  etc...
C
C  sign =  1        -> plot only positive levels
C  sign = -1        -> plot only negative levels
C  sign = 0         -> plot both levels
C
C  col  = 1         -> levels are drawn in color
c
c clear = 1    -> clear screen before displaying
C
C  if pl_id <> 99  -> plot on graphics screen
C                      vd_id = visual display id
C                      the window must have been opened with win3d_open
C
C  In any case the 3D space must have been set with :
C 		g3d_init			initialize
C 		g3d_setalpha g3d_setbeta g3d_setgama	set viewing angles
C 		g3d_setscale		set global scale
C		g3d_setznot			set 1/distance from object
C		g3d_setoff			set offset from center
C
C  Some bugs have been reported
C   -drawing extra lines
C   -division by 0 if 2 connected points are exactly at the same level,
C      and that level being the one currently plot.
C---------------------------------------------------------------------
C
        implicit none
		
c#include "sizebase.inc"
c        real plane2d(smxmax)
#include "sizeparam.inc"
#include "unitextern.inc"
        integer thecolor

        integer pl_id,vd_id
        integer si1,si2,si3
        integer mode2,direcd
        integer levels
        integer sign2,col
        integer zoom
        integer zl1,zl2,zl3,zu1,zu2,zu3,clear
        real cube(si3,si2,si1)
        real scaling,loga,dx3d,dy3d,dz3d

#include "controlc.inc"
#include "gcolor.inc"

	integer NX,NY
        integer i,j,k,plane,direc
        integer nf,nl
        integer si1im,si2im
        integer llf1,llf2,lur1,lur2
		integer zolf1,zolf2,zour1,zour2,pldeb,plfin

        real mini,maxi
        real nivmin
        real interv
        real iniv(64),niv
        integer sign,mode,plstep,currdir

        integer jmin,jmax,jstep
        integer imin,imax,istep
        integer kmax
        integer ii,jj
        integer irec,jrec
        integer iimax,jjmax
	 integer fbox(63,63)
        integer ipt,last,ib,jb
        integer ntrav,first
        integer flag(4)

	real z11,z12,z21,z22,zero,fsi1,fsi2,fsi3
        real x(512),y(512),xyz(512)
	real*4 z(64,64)
#f2c        save z,x,y,xyz     ! needed for C array
        real scx,scy,scz,box1,box2,box3

        logical remain

C-------------------------------------------------------------
C
C                 4
C               .....
C             1 .   . 3
C               .....
C                 2
C
C
C	FBOX:	0 RIEN A TRACER
C		1 PAS ENCORE PASSE
C		2 PASSE UNE FOIS HORIZONTALEMENT
C		3 PASSE UNE FOIS VERTICALEMENT
C
C-------------------------------------------------------------
		
        zero = 0.0

        if (zoom.eq.1) then
         if (zl1.lt.1 .or. zl2.lt.1 .or. zl3.lt.1 .or.
     *       zu1.gt.si1 .or. zu2.gt.si2 .or. zu3.gt.si3) then
            write(*,*) 'Wrong zoom window'
            return
         endif
          fsi1 = float(zu1-zl1)
          fsi2 = float(zu2-zl2)
          fsi3 = float(zu3-zl3)
        else
          fsi1 = float(si1-1)
          fsi2 = float(si2-1)
          fsi3 = float(si3-1)
        endif

C setup x y z scales such that natural unit is number of points,
c check with mode though !
        scx = dx3d/fsi3
        scy = dy3d/fsi2
        scz = dz3d/fsi1
c and prepare for 3D box
        box1 = fsi1
        box2 = fsi2
        box3 = fsi3
        if (checkdim(4,mode2,3)) then
           scx = scx*2
           box3 = box3/2
        endif
        if (checkdim(2,mode2,3)) then
           scy = scy*2
           box2 = box2/2
        endif
        if (checkdim(1,mode2,3)) then
           scz = scz*2
           box1 = box1/2
        endif
C Check for which output device...
C       On plotter...
        if (pl_id.eq.99) then
          call g3d_setscxyz(scx,scy,scz)
C   Draw a cube of dx x dy x dz cm. around the plot...
          call pl3d_move(pl_id,zero,zero,zero)
          call pl3d_draw(pl_id,box3,zero,zero)
          call pl3d_draw(pl_id,box3,box2,zero)
          call pl3d_draw(pl_id,zero,box2,zero)
          call pl3d_draw(pl_id,zero,zero,zero)
          call pl3d_draw(pl_id,zero,zero,box1)
          call pl3d_draw(pl_id,box3,zero,box1)
          call pl3d_draw(pl_id,box3,box2,box1)
          call pl3d_draw(pl_id,zero,box2,box1)
          call pl3d_draw(pl_id,zero,zero,box1)
          call pl3d_move(pl_id,box3,zero,box1)
          call pl3d_draw(pl_id,box3,zero,zero)
          call pl3d_move(pl_id,zero,box2,zero)
          call pl3d_draw(pl_id,zero,box2,box1)
          call pl3d_move(pl_id,box3,box2,box1)
          call pl3d_draw(pl_id,box3,box2,zero)
          call pl3d_move(pl_id,zero,zero,zero)
c then axes labels
          call pl3d_text(pl_id,zero,zero,1.05*box1,'F1',2)
          call pl3d_text(pl_id,zero,1.05*box2,zero,'F2',2)
          call pl3d_text(pl_id,1.05*box3,zero,zero,'F3',2)
          call pl3d_text(pl_id,-.05*box3,-.05*box2,-.05*box1,'0',1)

        else
C on screen
C Setup x y z scales such that natural unit is number of points
c relative to cx, cy,cz (1.0 on window is max(dx3d,dy3d,dz3d))
C 
           call g3d_setscxyz(scx/max(dx3d,dy3d,dz3d),
     *                       scy/max(dx3d,dy3d,dz3d),
     *                       scz/max(dx3d,dy3d,dz3d))
           if (clear.eq.1) call win3d_erase(vd_id)
           if (col.eq.1) call win_fgcolor(vd_id,white) ! white for frame
C          Draw the frame on the screen
           x(1) = zero
           y(1) = zero
           x(2) = box3
           y(2) = zero
           x(3) = box3
           y(3) = box2
           x(4) = zero
           y(4) = box2
           x(5) = zero
           y(5) = zero

           do i=1,5
             xyz(i) = zero
           enddo
           call win3d_plot_array(vd_id,5,x,y,xyz)

           do i=1,5
             xyz(i) = box1
           enddo
           call win3d_plot_array(vd_id,5,x,y,xyz)

           call win3d_plot_line(vd_id, 
     *		zero,zero,zero ,zero,zero,box1)
           call win3d_plot_line(vd_id,
     *		box3,zero,zero, box3,zero,box1)
           call win3d_plot_line(vd_id,
     *		zero,box2,zero, zero,box2,box1)
           call win3d_plot_line(vd_id,
     *		box3,box2,zero,box3,box2,box1)
c then axes labels
           call win3d_write(vd_id,zero,zero,1.05*box1,'F1',2)
           call win3d_write(vd_id,zero,1.05*box2,zero,'F2',2)
           call win3d_write(vd_id,1.05*box3,zero,zero,'F3',2)
           call win3d_write(vd_id,-.05*box3,-.05*box2,-.05*box1,'0',1)
           call win3d_update(vd_id)
        endif


        if (scaling.eq.0.0) then
c            write(*,*)' Scaling is zero!'
            return
        endif
		

        if (maxi.eq.zero) then
         call mxavect(mini,maxi,i,j,cube,si1*si2*si3)
        endif


control on direc
C  loop back on 10 for each directions
      direc = direcd

10    if (checkdim(4,direc,3)) then      ! in dim 1 / plane F2-F3
         currdir = 1
         direc = direc-1
         mode = mod(mode2,4)
         plstep = mode2/4 + 1
         si2im = si3
         si1im = si2
         zolf1 = zl2
         zolf2 = zl3
         zour1 = zu2
         zour2 = zu3
         if (zoom .eq.1) then
            pldeb = zl1
            plfin = zu1
         else
            pldeb = 1
            plfin = si1
         endif
      elseif (checkdim(2,direc,3)) then   ! in dim 2 / plane F1-F3
         currdir = 2
         direc = direc-2
         mode = mod(mode2,2) + 2*mod(mode2,4)
         plstep = mod(mode2/2,2) + 1
         si2im = si3
         si1im = si1
         zolf1 = zl1
         zolf2 = zl3
         zour1 = zu1
         zour2 = zu3
         if (zoom .eq.1) then
            pldeb = zl2
            plfin = zu2
         else
            pldeb = 1
            plfin = si2
         endif
      elseif (checkdim(1,direc,3)) then	   ! in dim 3 / plane F1-F2
         currdir = 3
         direc = direc-4
         mode = mode2/2
         plstep = mod(mode2,2) + 1
         si2im = si2
         si1im = si1
         zolf1 = zl1
         zolf2 = zl2
         zour1 = zu1
         zour2 = zu2
         if (zoom .eq.1) then
            pldeb = zl3
            plfin = zu3
         else
            pldeb = 1
            plfin = si3
         endif
      else
         goto 999                 ! on a finit
      endif

C we're now more or less independent on currdir

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
      if (zoom.eq.1) then      ! zoom mode is on
         lur1 = zour1
         lur2 = zour2
         llf1 = zolf1
         llf2 = zolf2
c         si1im = zour1-zolf1+1
c         si2im = zour2-zolf2+1
      else
         lur1 = si1im
         lur2 = si2im
         llf1 = 1
         llf2 = 1
      endif

C nx, ny number of x and y partitions...
      nx = (lur2-llf2+1)/istep
      ny = (lur1-llf1+1)/jstep


      do 1003, plane=pldeb,plfin,plstep		! on plane number
      if (currdir.eq.1) then
	      call getplane(plane2d,cube,
     *                     si1,si2,si3,plane)
      elseif (currdir.eq.2) then
	      call getplane2(plane2d,cube,
     *                     si1,si2,si3,plane)
      elseif (currdir.eq.3) then
	      call getplane3(plane2d,cube,
     *                     si1,si2,si3,plane)
      endif
        do i=1,512
             xyz(i) = (float(plane)-pldeb)/plstep     ! will be used by 3D plotting
        enddo

        sign = sign2


10000   nivmin = maxi/(scaling*32)         ! lowest level
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
C
	DO 1002 JMIN=0,NY-1,63     ! move the box
	JMAX=JMIN+63
C
	DO 1002 IMIN=0,NX-1,63
	IMAX=IMIN+63
C                 copy the matrix into z(ii,jj)
	JJ=0
        iimax = 0
        jjmax = 0
	DO 310 JREC=(JMIN)*Jstep+llf1,(JMAX)*jstep+llf1,jstep
	  JJ=JJ+1
	  II=0
	  DO 310 IREC=(IMIN)*istep+llf2,(IMAX)*istep+llf2,istep
	    II=II+1
            if (irec.le.lur2 .and. jrec.le.lur1) then
	       Z(II,JJ)=plane2d(irec + si2im*(jrec-1))
               iimax = max(iimax,ii)
               jjmax = max(jjmax,jj)
            else
               z(ii,jj)=zero
            endif
310	CONTINUE
        iimax = min(iimax,63)
        jjmax = min(jjmax,63)
C
C
        remain=.true.        ! remain.eq..false. ==> no plotting left,
	DO 1001 K=1,KMAX    ! sur les niveaux
C
        if (.not.remain) then    !no plotting left
           goto 1002
        endif
	NIV=INIV(K)
CCCCCCCCCCCCCCCCCCCCCCCc
        if (pl_id.ne.99) then
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
c          else
c            call win_fgcolor(vd_id,white)
          endif
        endif

CCCCCCCCCCCCCCCCCCCCCCCc
C
	DO 320 II=1,63
	DO 320 JJ=1,63   !met fbox a 1
	  FBOX(II,JJ)=1
320	CONTINUE
C

       if ((niv.gt.zero .and. z(1,1).lt.niv) .or. 
     *     (niv.lt.zero .and. z(1,1).gt.niv))    then
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
           if (currdir.eq.1) then
C the way to plot will depend on currdir : the current plotting direction
              if (pl_id.eq.99) then
                call pl3d_move(pl_id,x(1),y(1),xyz(1))
                do i=2,ipt
                   call pl3d_draw(pl_id,x(i),y(i),xyz(i))
                enddo
              else
                call win3d_plot_array(vd_id,ipt,x,y,xyz)
              endif
           elseif (currdir.eq.2) then
              if (pl_id.eq.99) then
                call pl3d_move(pl_id,x(1),xyz(1),y(1))
                do i=2,ipt
                   call pl3d_draw(pl_id,x(i),xyz(i),y(i))
                enddo
              else
                call win3d_plot_array(vd_id,ipt,x,xyz,y)
              endif
           elseif (currdir.eq.3) then
              if (pl_id.eq.99) then
                call pl3d_move(pl_id,xyz(1),x(1),y(1))
                do i=2,ipt
                   call pl3d_draw(pl_id,xyz(i),x(i),y(i))
                enddo
              else
                call win3d_plot_array(vd_id,ipt,xyz,x,y)
              endif
           endif
           remain = .true.                ! still plotting to do.
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
1001	CONTINUE  !sur les niveaux
        if (control.eq.1) return		! test for abort
1002	CONTINUE  !deplace la boite

        if (sign.eq.0) then     !if plot both, then do now negatives
           sign=-1
           goto 10000
        endif

        if (control.eq.1) return		! test for abort
        if (pl_id.ne.99) call win_update(vd_id)	! update image

1003	continue	! sur les plans

        goto 10		! loop on drawing directions

C Final
999     if (pl_id.eq.99) then
	   call plhome(pl_id)
        else
           call win_update(vd_id)	! update image
        endif

        return
C
C
	END

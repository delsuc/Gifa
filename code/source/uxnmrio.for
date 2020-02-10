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
C

	subroutine WUXNMR1D(name,expno,procno,error)
c IN :  name,expno,procno
c OUT : error
c
c Write a UXNMR compatible 1d file
c
      implicit none

      integer error,expno,procno,m,nsize
  10  character*256 name
      logical ex

#include "sizeparam.inc"
#include "paramfilev.inc"

      character*256 lname,lnames
      character*8 no1, no2
      integer l1,l2,l3,ln
  20  integer imagesize1d
      
      call leading(name)
      call trailing(name,l3)
      write(no1,*) expno
      call leading(no1)
      call trailing(no1,l1)
      write(no2,*) procno
      call leading(no2)
      call trailing(no2,l2)

      call shell('if [ ! -d ' // name(1:l3) // ' ] ; then mkdir ' //
     *name(1:l3) //' ; fi')

C              verify directory name

      lname = name(1:l3) // '/' // no1(1:l1)
      call shell('if [ ! -d ' // lname //
     * ' ] ; then mkdir ' // lname // ' ; fi')
C              verify directory name/no1
      lname = name(1:l3) // '/' // no1(1:l1) // '/acqu'
      inquire(file=lname,exist=ex,iostat=error)
      if (error.ne.0) return
C              verify file name/no1/acqu
      if (ex.eq. .FALSE.) then
       
      open(unit= 10,file= lname, iostat=error,
     * status='new',form='formatted')
      if (error.ne.0) return

C Writing Bruker Formated file acqu & acqus
      write(10,'(''##TITLE= Parameter file, UXNMR Version 920801'')')
      write(10,'(''##DATATYPE= UXNMR Parameter Values'')')
      write(10,'(''##ORIGIN= Gifa V4'')')
      write(10,'(''##OWNER= melanie'')')
   50 write(10,'(''##$AQ_mod= 1'')')
      write(10,'(''##$AUNM= <au_zg>'')')   !File acqus from UXNMR
      write(10,'(''##$BF1=''g12.6)') freq1d
      write(10,'(''##$BF2= 600.13'')')
      write(10,'(''##$BF3= 600.13'')')
      write(10,'(''##$BF4= 500.13'')')
      write(10,'(''##$BYTORDA= 0'')')
      write(10,'(''##$CNST= (0..31)'')')
      write(10,'(''1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
     * 1 1 1 1 1'')')
      write(10,'(''##$CPDPRG= <>'')')
      write(10,'(''##$CPDPRGB= <>'')')
      write(10,'(''##$CPDPRGT= <>'')')
      write(10,'(''##$D= (0..31)'')')
      write(10,'(''3e-06 1 0.0001 5e-06 0.0005 0 0 0 0 0 0 0.0 2e-05 3e-
     *06 0.07 0 0 0 0'')')
      write(10,'(''0 0 0 0 0 0 0 0 0 0 0 0 0'')')
      write(10,'(''##$DATE= 765637049'')')
  70  write(10,'(''##$DBL= (0..7)'')')
      write(10,'(''120 120 120 120 120 120 120 120'')')
      write(10,'(''##$DBP= (0..7)'')')
      write(10,'(''150 150 150 150 150 150 150 150'')')
      write(10,'(''##$DBP07= 0'')')
      write(10,'(''##$DBPNAM0= <>'')')
      write(10,'(''##$DBPNAM1= <>'')')
      write(10,'(''##$DBPNAM2= <>'')')
      write(10,'(''##$DBPNAM3= <>'')')
      write(10,'(''##$DBPNAM4= <>'')')
  80  write(10,'(''##$DBPNAM5= <>'')')
      write(10,'(''##$DBPNAM6= <>'')')
      write(10,'(''##$DBPNAM7= <>'')')
      write(10,'(''##$DBPOFFS= (0..7)'')')
      write(10,'(''0 0 0 0 0 0 0 0'')')
      write(10,'(''##$DE= 130'')')
      write(10,'(''##$DECBNUC= <off>'')')
      write(10,'(''##$DECNUC= <off>'')')
      write(10,'(''##$DECSTAT= 4'')')
      write(10,'(''##$DIGTYP= 1'')')
  90  write(10,'(''##$DL= (0..7)'')')
      write(10,'(''120 120 120 120 120 120 120 120'')')
      write(10,'(''##$DP= (0..7)'')')
      write(10,'(''150 150 150 150 150 150 150 150'')')
      write(10,'(''##$DP07= 0'')')
      write(10,'(''##$DPNAME0= <>'')')
      write(10,'(''##$DPNAME1= <>'')')
      write(10,'(''##$DPNAME2= <>'')')
      write(10,'(''##$DPNAME3= <>'')')
      write(10,'(''##$DPNAME4= <>'')')
 100  write(10,'(''##$DPNAME5= <>'')')
      write(10,'(''##$DPNAME6= <>'')')
      write(10,'(''##$DPNAME7= <>'')')
      write(10,'(''##$DPOFFS= (0..7)'')')
      write(10,'(''0 0 0 0 0 0 0 0'')')
      write(10,'(''##$DR= 16'')')
      write(10,'(''##$DS= 4'')')
      write(10,'(''##$DSLIST= <SSSSSSSSSSSSSSS>'')')
      write(10,'(''##$DTYPA= 0'')')
      write(10,'(''##$EXP= <>'')')
 110  write(10,'(''##$F1LIST= <111111111111111>'')')
      write(10,'(''##$F2LIST= <222222222222222>'')')
      write(10,'(''##$F3LIST= <333333333333333>'')')
      write(10,'(''##$FL1= 90'')')
      write(10,'(''##$FL2= 90'')')
      write(10,'(''##$FL3= 90'')')
      write(10,'(''##$FL4= 90'')')
      write(10,'(''##$FOV= 20'')')
      write(10,'(''##$FS= (0..7)'')')
      write(10,'(''83 83 83 83 83 83 83 83'')')
      write(10,'(''##$FW= 8200'')')
      write(10,'(''##$GRDPROG= <>'')')
      write(10,'(''##$HL1= 2'')')
      write(10,'(''##$HL2= 85'')')
      write(10,'(''##$HL3= 16'')')
      write(10,'(''##$HL4= 17'')')
      write(10,'(''##$IN= (0..31)'')')
      write(10,'(''5e-05 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001
     * 0.001 0.001 0.001'')')
      write(10,'(''0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001
     * 0.001 0.001 0.001'')')
      write(10,'(''0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001'')')
      write(10,'(''##$INP= (0..31)'')')
      write(10,'(''0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
     * 0 0 0 0 0'')')
      write(10,'(''##$INSTRUM= <gifa>'')')
      write(10,'(''##$L= (0..31)'')')
      write(10,'(''1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
     * 1 1 1 1 1'')')
      write(10,'(''##$LOCNUC= <2H>'')')
      write(10,'(''##$NBL= 1'')')
      write(10,'(''##$NC= 0'')')
      write(10,'(''##$NS= 4'')')
      write(10,'(''##$NUCLEUS= <1H>'')')
      write(10,'(''##$O1= 0'')')       ! a modifier
      write(10,'(''##$O2= 0'')')
 150  write(10,'(''##$O3= 0'')')
      write(10,'(''##$O4= 0'')')
      write(10,'(''##$P= (0..31)'')')
      write(10,'(''3 9.5 20 0 0 0 0 0 0 0 0 80000 0 0 0 0 0 0 1000000 15
     *0000 26 0 0 0 0 0'')')
      write(10,'(''0 0 0 0 0 0'')')
      write(10,'(''##$PAPS= 1'')')
      write(10,'(''##$PARMODE= 0'')')
      write(10,'(''##$PHCOR= (0..31)'')')
      write(10,'(''0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
     * 0 0 0 0 0'')')
      write(10,'(''##$PHP= 1'')')
      write(10,'(''##$PH_ref= 93.5'')')
      write(10,'(''##$POWMOD= 0'')')
      write(10,'(''##$PR= 1'')')
      write(10,'(''##$PRGAIN= 0'')')
      write(10,'(''##$PROBHD= <>'')')
      write(10,'(''##$PULPROG= <zgpr>'')')
      write(10,'(''##$PW= 0'')')
      write(10,'(''##$QNP= 1'')')
      write(10,'(''##$QS= (0..7)83 83 83 83 83 83 83 22'')')
      write(10,'(''##$QSB= (0..7)83 83 83 83 83 83 83 83'')')
      write(10,'(''##$RD= 0'')')
      write(10,'(''##$RG= 4096'')')
      write(10,'(''##$RO= 20'')')
      write(10,'(''##$ROUTWD1= (0..23)'')')
      write(10,'(''0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0'')')
      write(10,'(''##$ROUTWD2= (0..23)'')')
      write(10,'(''0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 0 0'')')
      write(10,'(''##$S= (0..7)'')')
      write(10,'(''83 4 83 83 83 83 83 83'')')
      write(10,'(''##$SEOUT= 0'')')
      write(10,'(''##$SFO1=''g12.6)')  freq1d
      write(10,'(''##$SFO2= 600.13'')')
      write(10,'(''##$SFO3= 600.13'')')
      write(10,'(''##$SFO4= 500.13'')')
      write(10,'(''##$SOLVENT= <CDCl3>'')')
      write(10,'(''##$SP= 1'')')
      write(10,'(''##$SW=''g12.6)')  (sp1d/freq1d)
      write(10,'(''##$SW_h=''g12.6)') sp1d
      write(10,'(''##$TD=''g12.6)') sizeimage1d
      write(10,'(''##$TE= 305'')')
      write(10,'(''##$TL= (0..7)'')')
      write(10,'(''10 120 120 120 120 120 120 120'')')
      write(10,'(''##$TP= (0..7)'')')
      write(10,'(''150 62 150 150 150 150 150 150'')')
      write(10,'(''##$TP07= 0'')')
      write(10,'(''##$TPNAME0= <>'')')
 200  write(10,'(''##$TPNAME1= <gauss512>'')')
      write(10,'(''##$TPNAME2= <>'')')
      write(10,'(''##$TPNAME3= <>'')')
      write(10,'(''##$TPNAME4= <>'')')
      write(10,'(''##$TPNAME5= <>'')')
      write(10,'(''##$TPNAME6= <>'')')
      write(10,'(''##$TPNAME7= <>'')')
      write(10,'(''##$TPOFFS= (0..7)'')')
      write(10,'(''0 0 0 0 0 0 0 0'')')
      write(10,'(''##$V9= 5'')')
      write(10,'(''##$VCLIST= <CCCCCCCCCCCCCCC>'')')
      write(10,'(''##$VD= 0'')')
      write(10,'(''##$VDLIST= <DDDDDDDDDDDDDDD>'')')
      write(10,'(''##$VPLIST= <PPPPPPPPPPPPPPP>'')')
      write(10,'(''##$VTLIST= <TTTTTTTTTTTTTTT>'')')
      write(10,'(''##$WBST= 256'')')
      write(10,'(''##$WBSW= 10'')')
      write(10,'(''##$WS= (0..7)83 83 83 83 83 83 83 83'')')
      write(10,'(''##$XL= 0'')')
      write(10,'(''##$YL= 0'')')
      write(10,'(''##$YMAX_a= 0'')')
      write(10,'(''##$YMIN_a= 0'')')
      write(10,'(''##END='')')
 
      close(10,iostat=error)
      if (error.ne.0) return
      lnames = name(1:l3) // '/' // no1(1:l1) // '/acqus'
      call shell('cp ' // lname // ' ' // lnames)
C       create acqus by copying acqu
      endif	! acqu exists
        
      lname = name(1:l3) // '/' // no1(1:l1) // '/pdata'
      call shell('if [ ! -d ' // lname // ' ] ; then mkdir ' //
     *lname //' ; fi')
C              verify directory name/no1/pdata
      lname = name(1:l3) // '/' // no1(1:l1) // '/pdata/' // no2(1:l2)
      call shell('if [ ! -d ' // lname //
     *' ] ; then mkdir ' // lname // ' ; fi')
c              verify directory name/no1/pdata/no2
      lname = name(1:l3) // '/' // no1(1:l1) // '/pdata/' //
     *        no2(1:l2) // '/proc'
      inquire(file=lname,exist=ex,iostat=error)
      if (error.ne.0) return

      if (ex .eq. .TRUE.) call shell('rm ' // lname)
c           The proc and procs files are always erased

      open (unit= 10 , file= lname, iostat= error ,
     * status= 'new', form= 'formatted')

      if (error.ne.0) return

C         writing Bruker Formated file proc & procs
      write(10,'(''##TITLE= Parameter file, UXNMR Version 920801'')')
      write(10,'(''##JCAMPDX= 5.0'')')
      write(10,'(''##DATATYPE= UXNMR Parameter Values'')')
 250  write(10,'(''##ORIGIN= Gifa V4'')')
      write(10,'(''##OWNER= melanie'')')
      write(10,'(''##$ABSF1= 10'')')  ! a voir
      write(10,'(''##$ABSF2= 0'')')
      write(10,'(''##$ABSG= 0'')')
      write(10,'(''##$ABSL= 0'')')
      write(10,'(''##$ALPHA= 0'')')
      write(10,'(''##$AQORDER= 0'')')
      write(10,'(''##$ASSFAC= 0'')')
      write(10,'(''##$ASSFACI= 0'')')
      write(10,'(''##$ASSFACX= 0'')')
      write(10,'(''##$ASSWID= 0'')')
      write(10,'(''##$AUNMP= <proc_1d>'')')
      write(10,'(''##$AZFE= 0.1'')')
      write(10,'(''##$AZFW= 0.1'')')
      write(10,'(''##$BCFW= 1'')')
      write(10,'(''##$BC_mod= 2'')')
      write(10,'(''##$BYTORDP= 0'')')
      write(10,'(''##$DATMOD= 1'')')
      write(10,'(''##$DC= 2'')')
      write(10,'(''##$DFILT= <>'')')
      write(10,'(''##$DTYPP= 0'')')
      write(10,'(''##$FCOR= 0.5'')')
      write(10,'(''##$FT_mod= 6'')')
      write(10,'(''##$GAMMA= 1'')')
      write(10,'(''##$GB= 0'')')
      write(10,'(''##$INTBC= 1'')')
      write(10,'(''##$INTSCL= 1'')')
      write(10,'(''##$ISEN= 128'')')
      write(10,'(''##$LB= 0'')')
      write(10,'(''##$LEV0= 0'')')
      write(10,'(''##$LPBIN= 0'')')
      write(10,'(''##$MAXI= 10000'')')
      write(10,'(''##$MC2= 0'')')
      write(10,'(''##$MEAN= 0'')')
      write(10,'(''##$ME_mod= 0'')')
      write(10,'(''##$MI= 0'')')
      write(10,'(''##$NCOEF= 0'')')
      write(10,'(''##$NC_proc= -8'')')
      write(10,'(''##$NLEV= 6'')')
      write(10,'(''##$NOISF1= 0'')')
      write(10,'(''##$NOISF2= 0'')')
      write(10,'(''##$NSP= 1'')')
      write(10,'(''##$NTH_PF= 0'')')
      write(10,'(''##$NTH_PI= 0'')')
      write(10,'(''##$NZP= 0'')')
      write(10,'(''##$OFFSET=''g12.6)') of1d
      write(10,'(''##$PC= 1'')')
      write(10,'(''##$PHC0= 0.0'')')
      write(10,'(''##$PHC1= 0.0'')')
 300  write(10,'(''##$PH_mod= 1'')')
      write(10,'(''##$PKNL= yes'')')
      write(10,'(''##$PPARMOD= 0'')')
      write(10,'(''##$PSCAL= 4'')')
      write(10,'(''##$PSIGN= 0'')')
      write(10,'(''##$REVERSE= no'')')
      write(10,'(''##$SF=''g12.6)') freq1d
      write(10,'(''##$SI= 819'')')
      write(10,'(''##$SIGF1= 0'')')
      write(10,'(''##$SIGF2= 0'')')
      write(10,'(''##$SINO= 400'')')
      write(10,'(''##$SREGLST= <1H.CDCl3>'')')
      write(10,'(''##$SSB= 0'')')
      write(10,'(''##$STSI= 0'')')
      write(10,'(''##$STSR= 0'')')
      write(10,'(''##$SW_p=''g12.6)') sp1d
      write(10,'(''##$SYMM= 0'')')
      write(10,'(''##$S_DEV= 0'')')
      write(10,'(''##$T1D= 5'')')
      write(10,'(''##$TDeff= 2048'')')
      write(10,'(''##$TDoff= 0'')')
      write(10,'(''##$TI= <>'')')
      write(10,'(''##$TILT= no'')')
      write(10,'(''##$TM1= 0'')')
      write(10,'(''##$TM2= 0'')')
      write(10,'(''##$TOPLEV= 0'')')
      write(10,'(''##$WDW= 0'')')
      write(10,'(''##$XDIM=''g12.6)') imagesize1d * 2
      write(10,'(''##$YMAX_p= 356540237'')')
      write(10,'(''##$YMIN_p= -42443999'')')
      write(10,'(''##END='')')
 
      close(10,iostat= error)
      if (error.ne.0) return
     
      lnames = name(1:l3) // '/' // no1(1:l1) // '/pdata/' //
     *        no2(1:l2) // '/procs'
      call shell ('cp ' // lname // ' ' // lnames)
C   create procs by copying proc

      if (it1d .eq. 0) then         ! real data-set
        lname = name(1:l3) // '/' // no1(1:l1) // '/pdata/' // 
     *          no2(1:l2) // '/1r'
        call trailing(lname,ln)
        call cux1d(lname,ln,column,sizeimage1d,error)
      else
        call powtwo(sizeimage1d,m)
        if (m.ne.0) then
           nsize = (sizeimage1d-m)*2
           call cp2vect(column,column,nsize,sizeimage1d)
 350    else
           nsize = sizeimage1d
        endif
        call swavect(column,nsize)
        lname = name(1:l3) // '/' // no1(1:l1) // '/pdata/' //
     *           no2(1:l2) // '/1r'
        call trailing(lname,ln)
        call cux1d(lname,ln,column,(sizeimage1d/2),error)
        lname = name(1:l3) // '/' // no1(1:l1) // '/pdata/' //
     *           no2(1:l2) // '/1i'
        call trailing(lname,ln)
        call cux1d(lname,ln,column(nsize/2+1),(sizeimage1d/2),error)
        call uswvect(column,nsize)
      endif
      return
      end


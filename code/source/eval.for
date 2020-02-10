C This is the lexical analyser for arithmetic expressions in GIFA
C
C Author M.A.Delsuc	first version spring '92
C last modification     spring 2001
C
C
C gram : (exp)
C
C exp   -> op1 term					unitary op(+ - !)
C          term op2 exp				binary op (+ - * / ... )
C          term
C
C term ->  func( exp )				func ( cos sin tan ... )
C          func(exp, exp,...)			func ( nextlm, itoh, ...)
C          ( exp )
C          $variable
C          $variable[ exp ]
C          numb
C          %
C
C numb ->  lit k
C          lit
C
C lit ->   literal number
C          literal string
C
C op1 ->   + | - | ! | noop
C prio     10  10  10   0
C
C op2 ->   // | ; | | | & | == | != | s= | s! | <= | >= | < | > | ....
C prio :    1   1   2   2   3     3   3    3    4    4    4   4
C 
C        .... | + | - | * | / | % | ^ | E
C prio :        6   6   7   7   7   8   8
C
C func  ->	see list below.
C
C
C the parser implements a stack machine, with 2 stacks :
C one for operators and their priority : oper(ioper),prio(ioper)
C and one for operands : stack(istack)
C
C it's an operator driven implementation, recursivity is implemented
C through the stacks
C
C the operands are type-free (-> stack elemts are in char), typing is imposed
C by the operators. eg    ( cos(23) // " another string" ) is legal.
C
C eval build the stacks from the input string,
C evalstack operates the stack and do the arithmetics
C
C arrays (i.e. $var[ exp ]) are implemented through a pseudo func
C
C logical false is coded as 0, false is everything else (1)
C
      block data initeval
      integer i
#include "evalexp.inc"
C external code for functions
      data (func(i),i=1,nfunc)
     *  / 'COS(    ','SIN(    ','ATAN(   ','LOG(    ','EXP(    ',
     *    'TOUPPER(','TOLOWER(','[       ','INT(    ','EOF(    ',
     *    'POWER2( ','ABS(    ','SP(     ','HEAD(   ','TAIL(   ',
     *    'SQRT(   ','NEXTLM( ','VAL1D(  ','VAL2D(  ','VAL3D(  ',
     *    'ITOH(   ','HTOI(   ','ITOP(   ','PTOI(   ','HTOP(   ',
     *    'PTOH(   ','MAX(    ','MIN(    ','LEN(    ','SUBST(  ',
     *    'INDEX(  ','HEADX(  ','TAILX(  ','EXIST(  ','DBM(    ',
     *    'ITOS(   ','STOI(   ','ISNUMB( ','ISALPHA(','ITOD(   ',
     *    'DTOI(   ','ITOT(   ','TTOI(   ','VALAMB( ','ROUND(  ',
     *    'FEXIST( ','SH(     ','HEXIST( ','HEADER( '/

C lfunc holds the length of the function word, speeds - up the parsing
      data (lfunc(i),i=1,nfunc)
     *  / 3, 3, 4, 3, 3,
     *    7, 7, 0, 3, 3,
     *    6, 3, 2, 4, 4,
     *    4, 6, 5, 5, 5,
     *    4, 4, 4, 4, 4,
     *    4, 3, 3, 3, 5,
     *    5, 5, 5, 5, 3,
     *    4, 4, 6, 7, 4,
     *    4, 4, 4, 6, 5,
     *    6, 2, 6, 6/

c
c internal codes
      data (codefunc(i),i=1,nfunc)
     *  /   51,       52,        53,        54,         55,
     *      56,       57,        58,        59,         60,
     *      61,       62,        63,        64,         65,
     *      66,       67,        68,        69,         70,
     *      71,       72,        73,        74,         75,
     *      76,       77,        78,        79,         80,
     *      81,       82,        83,        84,         85,
     *      86,       87,        88,        89,         90,
     *      91,       92,        93,        94,         96,
     *      97,       98,        99,       100/


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine eval(st,result,err)
c IN	: st
c OUT	: result,err
c performs the evaluation of the string st and put the result back into 
c result.
c evaluates operators, functions, variables
c operates through a virtual stack machine, implemented with
c pushval stackop evalstack
c
      implicit none
      character*(*) st,result
      integer err,lf
#include "evalexp.inc"
C
      character*256 stloc, var, ster
      integer lst, i

      err = 0
      istack = 0
      ioper = 0
      cprio = 0
      stack(1) = ' '
      he = 1
C remove leading blanks
100   if (istack.gt.stackmax .or. ioper.gt.stackmax) goto 904
101   if (st(he:he).eq.' ') then
         he = he+1
         goto 101
      endif
      if (err.ne.0) goto 999
C accept exp
      if (st(he:he) .eq. '+' ) then
         call stackop(uplus,puplus,1)
      elseif (st(he:he).eq. '-' ) then
         call stackop(uminus,puminus,1)
      elseif (st(he:he).eq. '!' ) then
         call stackop(not,pnot,1)
      endif
C accept term
200   continue
      if (st(he:he).eq.' ') then
         he = he+1
         goto 200
      endif
      if (err.ne.0) goto 999
      lst = len(st)
      if (st(he:he).eq. '(' ) then
         cprio = cprio + pparan
         he = he+1
         goto 100
      elseif (st(he:he) .eq. '$' ) then
         call parsevar(st,var,err)
         if (err.ne.0) goto 999
         call pushval(var,err)
         if (st(he:he) .eq. '[') goto 100   ! if array go through exp again
      elseif (st(he:he) .eq. '%' ) then
         call pushval(result,err)
         he = he+1
      else
C check for functions
         stloc = st(he:he+lfuncmax)
         call uppercase(stloc,lfuncmax)
         do i=1,nfunc				       ! for all func
C            call trailing(func(i),lf)
            lf = lfunc(i)+1
            if ( stloc(1:lf) .eq. func(i) ) then
                call stackop(codefunc(i),pfunc,lf)
                cprio = cprio + pparan
                goto 100
            endif
         enddo
C if still there, assume numeral
         call parselit(st,var,err)
         if (err.ne.0) goto 999
         call pushval(var,err)
      endif
C accept op2 or  )  or ] or ,
300   if (err.ne.0) goto 999
      if (st(he:lst) .eq. ' ') then             ! eof
         call evalstack(0,err)
         goto 900
      endif
310   continue
      if (st(he:he).eq.' ') then
         he = he+1
         goto 310
      endif
      if (st(he:he).eq. ',' ) then                ! ,
         call evalstack(0,err)
         he = he+1
         goto 100
      elseif (st(he:he).eq. ')' ) then                ! )
         if (cprio.eq.0) goto 999
         cprio = cprio - pparan
         call evalstack(pparan,err)
         he = he+1
         goto 300
      elseif (st(he:he).eq. ']' ) then                ! ]
         if (cprio.eq.0) goto 999
         cprio = cprio - pparan
         call evalstack(pparan,err)
         he = he+1
         goto 300
      elseif (st(he:he) .eq. '|' ) then           ! |
         call evalstack(por,err)
         call stackop(or,por,1)
         goto 100
      elseif (st(he:he) .eq. '&' ) then           ! &
         call evalstack(pand,err)
         call stackop(and,pand,1)
         goto 100
      elseif (st(he:he+1) .eq. '==' ) then           ! ==
         call evalstack(pequal,err)
         call stackop(equal,pequal,2)
         goto 100
      elseif (st(he:he+1) .eq. '!=' ) then           ! !=
         call evalstack(pnequal,err)
         call stackop(nequal,pnequal,2)
         goto 100
      elseif (st(he:he+1) .eq. 's=' ) then           ! s=
         call evalstack(psequal,err)
         call stackop(sequal,psequal,2)
         goto 100
      elseif (st(he:he+1) .eq. 's!' ) then           ! s!
         call evalstack(psnequal,err)
         call stackop(snequal,psnequal,2)
         goto 100
      elseif (st(he:he+1) .eq. '<=' ) then           ! <=
         call evalstack(psmaleq,err)
         call stackop(smaleq,psmaleq,2)
         goto 100
      elseif (st(he:he) .eq. '<' ) then           ! <
         call evalstack(psmal,err)
         call stackop(smal,psmal,1)
         goto 100
      elseif (st(he:he+1) .eq. '>=' ) then           ! >=
         call evalstack(pgreateq,err)
         call stackop(greateq,pgreateq,2)
         goto 100
      elseif (st(he:he) .eq. '>' ) then           ! >
         call evalstack(pgreat,err)
         call stackop(great,pgreat,1)
         goto 100
      elseif (st(he:he) .eq. '+' ) then           ! +
         call evalstack(pplus,err)
         call stackop(plus,pplus,1)
         goto 100
      elseif (st(he:he) .eq. '-' ) then           ! -
         call evalstack(pminus,err)
         call stackop(minus,pminus,1)
         goto 100
      elseif (st(he:he) .eq. ';' ) then        ! ;
         call evalstack(pconcat2,err)
         call stackop(concat2,pconcat2,1)
         goto 100
      elseif (st(he:he+1) .eq. '//' ) then        ! //
         call evalstack(pconcat,err)
         call stackop(concat,pconcat,2)
         goto 100
      elseif (st(he:he) .eq. '/' ) then           ! /
         call evalstack(pdiv,err)
         call stackop(div,pdiv,1)
         goto 100
      elseif (st(he:he) .eq. '*' ) then           ! *
         call evalstack(pmult,err)
         call stackop(mult,pmult,1)
         goto 100
      elseif (st(he:he) .eq. '%' ) then           ! %
         call evalstack(pmodulo,err)
         call stackop(modulo,pmodulo,1)
         goto 100
      elseif (st(he:he) .eq. '^' ) then           ! ^
         call evalstack(ppow,err)
         call stackop(pow,ppow,1)
        goto 100
      elseif (st(he:he) .eq. 'E' .or. st(he:he) .eq. 'e' ) then
         call evalstack(pexp,err)
         call stackop(exp,pexp,1)
         goto 100
      else
         goto 910
      endif
C error processing and normal return
900   if (cprio.gt.0) goto 901
      if (cprio.lt.0) goto 902
      if (istack.ne.1) goto 903
      if (ioper.ne.0) goto 903
      if (err.ne.0) goto 999
      result = stack(1)
      call cleanst(result,len(result))
      return

901   call gifaout( '*** missing ) ')
      goto 999      
902   call gifaout( '*** missing ( ')
      goto 999      
903   call gifaout( '*** syntax error in expression ')
      goto 999      
904   call gifaout( '*** expression stack full  ')
      goto 999      
910   call gifaout( '*** unknown expression ')
      goto 999      
999   err = 1

      call trailing(st,lf)
      call gifaout( st(1:lf))
      do i =1,he
        ster(i:i) = '-'
      enddo
      ster(he:he) = '^'
      call gifaout(ster(1:he))
      return
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine parsevar(st,var,err)
c IN	: st
c OUT	: var,err
c
C this one parse a variable name in the exp. string st, at location he
c (global with eval) and returns the value into var.
c if the var is an array, just the name is return, since [ is a pseudo 
c operator
c
      implicit none
#include "evalexp.inc"
#include "variable.inc"
#include "gifshellv.inc"
      external alphanum
      logical alphanum
      character*(*) st,var
      integer err,lst,i
      character*(varnmlen) name

c      if (st(he:he).ne.'$') goto 100
      lst = len(st)
      do i=he+1,lst
         if (.not. alphanum(st(i:i))) goto 10
      enddo
      i = lst+1
10    name = st(he:i-1)
      if (st(i:i) .eq. '[') then
        var = name		 ! if array just push the plain var name
      else
        call getvar(name,vcontext,var,err)  ! else evaluate
      endif
      he = i
      return
100   err = 1
      return
      end 

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine parselit(st,var,err)
c IN	: st
c OUT	: var,err
C
C this one parse a literal ('string' "string" or number) in the exp. string
C will set var, and move he accordingly
C
C I known, this is sloppy, but hey, I got it working, and I WON't change it now
C
C change 20-3-93 blanks in strings are changed to DEL to keep leading 
C and trailing blanks in literals
C
      implicit none
#include "evalexp.inc"
      character*(*) var,st
      integer err,i,j,icc,count_e,count_dot
      character*1 cc,char0,char9
      logical foundst
      data char0/'0'/,char9/'9'/
      i = he
      cc = st(he:he)
      foundst = .false.
      if (cc.eq.'''') then               ! first a literal string
         i = index(st(he+1:len(st)),'''')     ! either ' string '
         if (i.eq.0) goto 990
         var = st(he+1:he+i-1)
         foundst = .true.
      elseif (cc.eq.'"') then
         i = index(st(he+1:len(st)),'"')      ! or " another string "
         if (i.eq.0) goto 990
         var = st(he+1:he+i-1)
         foundst = .true.
      else
C now search for d..d{.d..d}     {E{-/+}d..d}{k}
         count_e = 0
         count_dot = 0
100      cc = st(i:i)
         icc = ichar(cc)
         if (icc .ge. ichar(char0) .and. icc .le. ichar(char9)) then   !   d
            i = i+1
            goto 100
         endif
         if (cc.eq.'.' .and. count_dot.eq.0 .and. count_e.eq.0) then     !  .
            count_dot = count_dot + 1
            i = i+1
            goto 100
         endif
c         if (cc.eq.'E' .or. cc.eq.'e'.and. count_e.eq.0) then         ! E
c            count_e = count_e + 1
c            i = i+1
c            if (st(i:i).eq.'-' .or. st(i:i).eq.'+') then           ! -/+
c                i = i+1
c            endif
c            goto 100
c         endif
         if ((cc.eq.'K'.or.cc.eq.'k').and. i.gt.he) then           !  k
            i = i+1
         endif
         if (i.eq.he) goto 991
         var = st(he:i-1)
      endif
      if (foundst) then
         do j=1,i-1
           if (var(j:j).eq.' ') var(j:j) = char(127)  	! blanks to DEL
         enddo
         he = he+i+1
      else
         he = i
      endif
      return

990   call gifaout( '*** error in processing quoted string')
      err = 1
      return
991   call gifaout( '*** error in getting literal value')
      err = 1
      return
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine stackop(code,codeprio,length)
c IN	: code,codeprio,length
c SIDE	: he, oper stack
c
c push on operator stack the operator "code" with priority "prio", and
c increment the he pointer by length
c
      implicit none
      integer code,codeprio,length
#include "evalexp.inc"
      ioper = ioper+1                    
      oper(ioper) = code                 
      prio(ioper) = cprio + codeprio              
      he = he+length
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine pushval(val,err)
c IN	: val
c OUT	: err
c SIDE	: val stack
c
c push on value stack the value "val"
c
      implicit none
      integer err
      character*(*) val
#include "evalexp.inc"
      if (istack .ge. stackmax) then
        call gifaout( '*** stack overflow')
        err = 1
      else
        istack = istack + 1
        stack(istack) = val
      endif
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine popval(val,err)
c OUT	: val,err
c SIDE	: val stack
c
c pop from value stack the value "val"
c
      implicit none
      integer err
      character*(*) val
#include "evalexp.inc"
      if (istack .le. 0) then
        call gifaout( '*** stack underflow')
        err = 1
      else
        val = stack(istack)
        istack = istack - 1
      endif
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine readval(st,zz,error)
c IN	: st
c OUT	: zz,error
c
c interprets string st as a number, returned in zz
c
      implicit none
      character*(*) st
      integer error,i,i1,i2,iz,j
      real*8 zz
      i1 = index(st,'K')-1
      i2 = index(st,'k')-1
      i = max(i1,i2)
      j = i
      if (i.eq.-1) i = len(st)
c old stuff - buggy because of the iz
c      if (index(st(1:i),'.').eq.0 .and.
c     *    index(st(1:i),'E').eq.0) then
c#vms         read (st(1:i),*,err=10,iostat=error) iz
c#_vms         read (st(1:i),'(i20)',err=10,iostat=error) iz
c         zz = iz
c      else
c#vms         read (st(1:i),*,err=10,iostat=error) zz
c#_vms         read (st(1:i),'(f30.15)',err=10,iostat=error) zz
c      endif
c
      read (st(1:i),*,err=10,iostat=error) zz
      if (j.ne.-1) zz = zz*1024.
      return
10    call gifaout( '*** error in arithmetic format ' )
      error = 1
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine printstack(current)
c IN	: current
c 
c prints out the content of the stacks, used for debugging
c
      implicit none
#include "evalexp.inc"
      integer i,j,current
      write(*,*) '------------- cprio, current :',cprio,current
      write(*,*) '------------- value stack ',istack
      do i=1,istack
         call trailing(stack(i),j)
         write(*,10) stack(i)(1:j)
10       format(a,' ',$)
      enddo
      write(*,*)
      write(*,*) '------------- oper stack ',ioper
      write(*,*) (oper(i),i=1,ioper)
      write(*,*) '------------- prio stack'
      write(*,*) (prio(i),i=1,ioper)
      write(*,*) '**********************************'
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine evalstack(current,err)
c IN	: current
c OUT	: err
c
c operates the stack machine, at the "current" level of priority
c used by eval
c
      implicit none
      
#include "evalexp.inc"
#include "gifshellv.inc"
#include "sizeparam.inc"
#include "paramfilev.inc"
#include "unitextern.inc"
#include "constant.inc"
#include "laplacev.inc"
#include "cachev.inc"

      integer current,err,ll1,ll2,i,j
      character*256 stl1, stl2, stl3, var
      real*8 zz1,zz2,zz3
      logical bool
      integer dclosest
      external dclosest
      
100   if (ioper.gt.0 .and. istack.gt.0) then           ! if something to do

C setting global debug to 2 or higher dumps the content of the stack
         if (debug.gt.1) call printstack(current)

         if (prio(ioper) .ge. cprio + current) then    ! do it
C first noop
            if (oper(ioper) .eq. noop) then
                goto 110
C second alpha operations
            elseif (oper(ioper) .eq. concat) then      ! //
               call popval(stl1,err)
               call popval(stl2,err)
               call trailing(stl1,ll1)
               call trailing(stl2,ll2)
               call pushval(stl2(1:ll2) // stl1(1:ll1),err )
            elseif (oper(ioper) .eq. concat2) then      ! ;
               call popval(stl1,err)
               call popval(stl2,err)
               call trailing(stl1,ll1)
               call trailing(stl2,ll2)
               call pushval(stl2(1:ll2) // ' ' // stl1(1:ll1),err )
            elseif (oper(ioper) .eq. codefunc(17)) then		! nextlm
               call popval(stl1,err)
               call popval(stl2,err)
               call nextelem(stl2,vcontext,stl1,var,err)
               call pushval(var,err )
            elseif (oper(ioper) .eq. sequal) then		! s=
               call popval(stl1,err)
               call popval(stl2,err)
               call cleanst(stl1,256)
               call cleanst(stl2,256)
               if (stl1.eq.stl2) then
                    var = '1'
               else
                    var = '0'
               endif
               call pushval(var,err )
            elseif (oper(ioper) .eq. snequal) then		! s!
               call popval(stl1,err)
               call popval(stl2,err)
               call cleanst(stl1,256)
               call cleanst(stl2,256)
               if (stl1.eq.stl2) then
                    var = '0'
               else
                    var = '1'
               endif
               call pushval(var,err )
            elseif (oper(ioper) .eq. codefunc(8)) then      ! $var[ i ]
               call popval(stl1,err)  ! index
               call popval(stl2,err)  ! base name
               call trailing(stl1,ll1)
               call trailing(stl2,ll2)
               var = stl2(1:ll2) // '[' // stl1(1:ll1) // ']'  ! make name
               call getvar(var,vcontext,stl1,err)     ! fetch var
               call pushval(stl1,err)
            elseif (oper(ioper) .eq. codefunc(6)) then      ! toupp
               call uppercase(stack(istack),256)
            elseif (oper(ioper) .eq. codefunc(7)) then     ! tolow
               call lowercase(stack(istack),256)
            elseif (oper(ioper) .eq. codefunc(10)) then     ! eof
               call popval(stl1,err)  	! file name
               inquire(file=stl1,number=ll1,opened=bool)
               if (.not.bool) goto 990
               if (.not. ( ll1.ge.30 .and. ll1.le.39)) goto 990
               zz1 = dfloat( inpeof(ll1-29) )
               goto 120			! push value
            elseif (oper(ioper) .eq. codefunc(13)) then      ! sp
              call popval(stl1,err)
              call readval(stl1,zz1,err)
              if (zz1.gt.0.5) then
                do i=1,min(int(zz1),256)
                  stl1(i:i) = char(127)	! spaces are simulated by DEL
                enddo
              else
                stl1 = ' '
              endif
              call pushval(stl1,err)
            elseif (oper(ioper) .eq. codefunc(14)) then      ! head
               call popval(stl1,err)
               call uncode(stl1,ll1)
               call parsest(stl1,stl2,i,2,err)
               call pushval(stl2,err)
            elseif (oper(ioper) .eq. codefunc(15)) then      ! tail
               call popval(stl1,err)
               call uncode(stl1,ll1)
               call parsest(stl1,stl2,i,1,err)
               call pushval(stl1(i+1:),err)
            elseif (oper(ioper) .eq. codefunc(29)) then      ! len
               call popval(stl1,err)
               call trailing(stl1,i)
               zz1 = i
               goto 120			! push zz1
            elseif (oper(ioper) .eq. codefunc(30)) then      ! subst
               call popval(stl2,err)
               call readval(stl2,zz2,err)     ! get final value
               call popval(stl2,err)
               call readval(stl2,zz1,err)     ! get init value
               call popval(stl1,err)
               i = int(zz1)
               j = int(zz2)
               if (i.gt.0 .and. i.lt.(len(stl1)-j) .and.
     *             j.ge.i .and. j.le.len(stl1) ) then
                   call pushval(stl1(i:j),err)
               else
                   goto 990
               endif
            elseif (oper(ioper) .eq. codefunc(31)) then      ! index
               call popval(stl2,err)
               call uncode(stl2,ll2)
               call popval(stl1,err)
               call uncode(stl1,ll1)
               i = index(stl1,stl2(1:ll2))
               zz1 = i
               goto 120
            elseif (oper(ioper) .eq. codefunc(32)) then      ! headx
               call popval(stl2,err)
               call uncode(stl2,ll2)
               call popval(stl1,err)
               call uncode(stl1,ll1)
               i = index(stl1,stl2(1:ll2))
               if (i.gt.0 .and. i.lt.len(stl1)) then
                  call recode(stl1,i-1)
                  call pushval(stl1(1:i-1),err)
               else
                  call recode(stl1,ll1)
                  call pushval(stl1,err)
               endif
            elseif (oper(ioper) .eq. codefunc(33)) then      ! tailx
               call popval(stl2,err)
               call uncode(stl2,ll2)
               call popval(stl1,err)
               call uncode(stl1,ll1)
               i = index(stl1,stl2(1:ll2))
               if (i.gt.0 .and. (i+ll2).lt.len(stl1)) then
                  call recode(stl1,ll1)
                  call pushval(stl1(i+ll2:),err)
               else
                  call pushval(' ',err)
               endif
            elseif (oper(ioper) .eq. codefunc(34)) then      ! exist
               call popval(stl1,err)
               stl2 = '$' // stl1
               i = 0
               call existvar(stl2,vcontext,i)
               if (i.eq.0) then
                  stl2 = '1'
               else
                  stl2 = '0'
               endif
               call pushval(stl2,err)
            elseif (oper(ioper) .eq. codefunc(46)) then      ! fexist
               call popval(stl1,err)
               i = 0
               call file_exist(stl1,i)
               if (i.eq.0) then
                  stl2 = '1'
               else
                  stl2 = '0'
               endif
               call pushval(stl2,err)
            elseif (oper(ioper) .eq. codefunc(48)) then      ! hexist
               call popval(stl1,err)
               call uncode(stl1,ll1)
               j = 0
               if (id_cache .eq. 0) goto 990
               call ch_gtparam(id_cache,stl2,i,stl1, ll1, j)
               if (j.eq.0) then
                  stl2 = '1'
               else
                  stl2 = '0'
               endif
               call pushval(stl2,err)
            elseif (oper(ioper) .eq. codefunc(49)) then      ! header
               call popval(stl1,err)
               call uncode(stl1,ll1)
               j = 0
               if (id_cache .eq. 0) goto 990
               call ch_gtparam(id_cache,stl2,ll2,stl1, ll1, j)
               if (j.ne.0) goto 990
               call recode(stl2,ll2)
               call pushval(stl2(1:ll2),err)
            elseif (oper(ioper) .eq. codefunc(35)) then      ! DBM exist 
               call popval(stl1,err)
               stl2 = '%' // stl1
               call checkvar(stl2,20,i)
               if (i.eq.0) then
                  stl2 = '1'
               else
                  stl2 = '0'
               endif
               call pushval(stl2,err)
            elseif (oper(ioper) .eq. codefunc(38)) then      ! isnumb()
               call popval(stl1,err)
               call trailing(stl1,i)
               if (isnumeral(stl1,i)) then
                  stl2 = '1'
               else
                  stl2 = '0'
               endif
               call pushval(stl2,err)
            elseif (oper(ioper) .eq. codefunc(39)) then      ! isalpha()
               call popval(stl1,err)
               call uncode(stl1,ll1)
               if (isalphanum(stl1,ll1)) then
                  stl2 = '1'
               else
                  stl2 = '0'
               endif
               call pushval(stl2,err)
            elseif (oper(ioper) .eq. codefunc(47)) then      ! sh()
               call popval(stl1,err)
               call uncode(stl1,ll1)
               call pipe_sys(stl2, ll2, stl1, ll1, err)
               call recode(stl2,ll2)
               call pushval(stl2,err)
            else
              goto 220   ! goto monoadic
            endif
            goto 110     ! finished for us
C then monoadic numeric op.
220         call popval(stl1,err)
            call readval(stl1,zz1,err)          ! get value
            if (oper(ioper) .eq. uminus ) then              ! unit -
               zz1 = -zz1
            elseif (oper(ioper) .eq.uplus) then
               continue      ! do nothing
            elseif (oper(ioper) .eq.not) then	            ! not : !
               if (zz1.ne.0.0) then
                   zz1 = 0.0
               else
                   zz1 = 1.0
               endif
            elseif (oper(ioper) .eq. codefunc(1)) then      ! cos
               zz1 = cos(zz1)
            elseif (oper(ioper) .eq. codefunc(2)) then      ! sin
               zz1 = sin(zz1)
            elseif (oper(ioper) .eq. codefunc(3)) then      ! atan
               zz1 = atan(zz1)
            elseif (oper(ioper) .eq. codefunc(4)) then      ! log
               if (zz1.le.0.0) goto 990
               zz1 = log(zz1)
            elseif (oper(ioper) .eq. codefunc(5)) then      ! exp
               zz1 = dexp(zz1)
            elseif (oper(ioper) .eq. codefunc(9)) then      ! int
               zz1 = dfloat(int(zz1))
            elseif (oper(ioper) .eq. codefunc(45)) then      ! round
               if (zz1.gt.0.0) then
                  zz1 = dfloat(int(zz1+0.5))
               else
                  zz1 = dfloat(int(zz1-0.5))
               endif
            elseif (oper(ioper) .eq. codefunc(11)) then      ! power2
               if (zz1.le.0.0) goto 990
               zz1 = dfloat(2** int( log(zz1)/log(2.0d0) +depsilon ) )
            elseif (oper(ioper) .eq. codefunc(12)) then      ! abs
               zz1 = dabs(zz1)
            elseif (oper(ioper) .eq. codefunc(16)) then      ! sqrt
               zz1 = dsqrt(zz1)
            elseif (oper(ioper) .eq. codefunc(18)) then      ! val1d
               zz1 = dclosest(zz1)
               if (zz1 .lt. 1 .or. zz1.gt.sizeimage1d) goto 990
               zz1 = column(zz1)
            elseif (oper(ioper) .eq. codefunc(42)) then		! itot
                   zz1 = dble(itotr(real(zz1), sampling, szsamp))
            elseif (oper(ioper) .eq. codefunc(43)) then		! ttoi
                   zz1 = dble(ttoir(real(zz1), sampling, szsamp))
            else
               goto 230    ! goto dyadic
            endif
            goto 120
C then dyadic numeric op.
230         call popval(stl2,err)
            call readval(stl2,zz2,err)          ! get value
            if (oper(ioper) .eq. equal) then          ! ==
               if (zz1 .eq. zz2) then
                 zz1 = 1.0
               else
                 zz1 = 0.0
               endif
            elseif (oper(ioper) .eq. nequal) then      ! !=
               if (zz1 .ne. zz2) then
                 zz1 = 1.0
               else
                 zz1 = 0.0
               endif
            elseif (oper(ioper) .eq. smal) then       ! <
               if (zz2 .lt. zz1) then
                 zz1 = 1.0
               else
                 zz1 = 0.0
               endif
            elseif (oper(ioper) .eq. smaleq) then       ! <=
               if (zz2 .le. zz1) then
                 zz1 = 1.0
               else
                 zz1 = 0.0
               endif
            elseif (oper(ioper) .eq. great) then       ! >
               if (zz2 .gt. zz1) then
                 zz1 = 1.0
               else
                 zz1 = 0.0
               endif
            elseif (oper(ioper) .eq. greateq) then       ! >=
               if (zz2 .ge. zz1) then
                 zz1 = 1.0
               else
                 zz1 = 0.0
               endif
            elseif (oper(ioper) .eq. or) then         ! |
               if (zz2.ne.0.0 .or.  zz1.ne.0.0) then
                 zz1 = 1.0
               else
                 zz1 = 0.0
               endif
            elseif (oper(ioper) .eq. and) then        ! &
               if (zz2.ne.0.0 .and.  zz1.ne.0.0) then
                 zz1 = 1.0
               else
                 zz1 = 0.0
               endif
            elseif (oper(ioper) .eq. plus) then       ! +
               zz1 = zz1 + zz2
            elseif (oper(ioper) .eq. minus) then      ! -
               zz1 = zz2 - zz1
            elseif (oper(ioper) .eq. mult) then       ! *
               zz1 = zz2 * zz1
            elseif (oper(ioper) .eq. modulo) then     ! %
               zz1 = dfloat( mod(int(zz2),int(zz1)))
            elseif (oper(ioper) .eq. div) then        ! /
               if (zz1.eq.0.0) goto 990
               zz1 = zz2 / zz1
            elseif (oper(ioper) .eq. pow) then        ! ^
               zz1 = zz2 ** zz1
            elseif (oper(ioper) .eq. exp) then        ! E
               if (zz1 .gt. 100) goto 990
               zz1 = zz2 * (10 ** zz1)
            elseif (oper(ioper) .eq. codefunc(19)) then      ! val2d
               zz1 = dclosest(zz1)
               if (zz1 .lt. 1 .or. zz1.gt.si2im) goto 990
               zz2 = dclosest(zz2)
               if (zz2 .lt. 1 .or. zz2.gt.si1im) goto 990
               zz1 = plane2d(zz1 + si2im*(zz2-1))
            elseif (oper(ioper) .eq. codefunc(44)) then      ! valamb
               zz1 = work2(dclosest(zz1) + si2im*dclosest(zz2-1))
            elseif (oper(ioper) .eq. codefunc(27)) then      ! max
               zz1 = max(zz2,zz1)
            elseif (oper(ioper) .eq. codefunc(28)) then      ! min
               zz1 = min(zz2,zz1)
            else
               goto 240		! goto triadic
            endif
            goto 120
            
C then triadic numeric op.
240         call popval(stl3,err)
            call readval(stl3,zz3,err)          ! get value
            if (oper(ioper) .eq. codefunc(20)) then		! val3d
               zz1 = dclosest(zz1)
               if (zz1 .lt. 1 .or. zz1.gt.si3d3) goto 990
               zz2 = dclosest(zz2)
               if (zz1 .lt. 1 .or. zz1.gt.si3d2) goto 990
               zz3 = dclosest(zz3)
               if (zz3 .lt. 1 .or. zz3.gt.si3d1) goto 990
               zz1 = image(zz1 +
     *            si3d3*( zz2 + si3d2*(zz3-1) - 1))
            elseif (oper(ioper) .eq. codefunc(21)) then		! itoh
               zz1 = dble(d_itohr(real(zz3),int(zz2),int(zz1),err))
               if (err.ne.0) goto 990

            elseif (oper(ioper) .eq. codefunc(22)) then		! htoi
               zz1 = dble(d_htoir(real(zz3),int(zz2),int(zz1),err))
               if (err.ne.0) goto 990

            elseif (oper(ioper) .eq. codefunc(23)) then		! itop
               zz1 = dble(d_itopr(real(zz3),int(zz2),int(zz1),err))
               if (err.ne.0) goto 990

            elseif (oper(ioper) .eq. codefunc(24)) then		! ptoi
               zz1 = dble(d_ptoir(real(zz3),int(zz2),int(zz1),err))
               if (err.ne.0) goto 990

            elseif (oper(ioper) .eq. codefunc(25)) then		! htop
               if (zz2.eq.0.d0) then       ! switch on dim (0 is joined file)
                 if (zz1.eq.1.d0) then		! switch on axis
                    zz1 = zz3/c_freq1
                 elseif (zz1.eq.2.d0 .and. c_dimmanip.gt.1) then
                    zz1 = zz3/c_freq2
                 elseif (zz1.eq.3.d0 .and. c_dimmanip.gt.2) then
                    zz1 = zz3/c_freq3
                 else
                   goto 990
                 endif
               elseif (zz2.eq.1.d0) then		! switch on dim
                   zz1 = zz3/freq1d
               elseif (zz2.eq.2.d0) then
                 if (zz1.eq.1.d0) then		! switch on axis
                   zz1 = zz3/freq1
                 elseif (zz1.eq.2.d0) then
                   zz1 = zz3/freq2
                 else
                   goto 990
                 endif
               elseif (zz2.eq.3.d0) then
                 if (zz1.eq.1.d0) then		! switch on axis
                   zz1 = zz3/freq3d1
                 elseif (zz1.eq.2.d0) then
                   zz1 = zz3/freq3d2
                 elseif (zz1.eq.3.d0) then
                   zz1 = zz3/freq3d3
                 else
                   goto 990
                 endif
               else                 
                 goto 990
               endif
            elseif (oper(ioper) .eq. codefunc(26)) then		! ptoh
               if (zz2.eq.0.d0) then       ! switch on dim (0 is joined file)
                 if (zz1.eq.1.d0) then		! switch on axis
                    zz1 = zz3*c_freq1
                 elseif (zz1.eq.2.d0 .and. c_dimmanip.gt.1) then
                    zz1 = zz3*c_freq2
                 elseif (zz1.eq.3.d0 .and. c_dimmanip.gt.2) then
                    zz1 = zz3*c_freq3
                 else
                   goto 990
                 endif
               elseif (zz2.eq.1.d0) then		! switch on dim
                   zz1 = zz3*freq1d
               elseif (zz2.eq.2.d0) then
                 if (zz1.eq.1.d0) then		! switch on axis
                   zz1 = zz3*freq1
                 elseif (zz1.eq.2.d0) then
                   zz1 = zz3*freq2
                 else
                   goto 990
                 endif
               elseif (zz2.eq.3.d0) then
                 if (zz1.eq.1.d0) then		! switch on axis
                   zz1 = zz3*freq3d1
                 elseif (zz1.eq.2.d0) then
                   zz1 = zz3*freq3d2
                 elseif (zz1.eq.3.d0) then
                   zz1 = zz3*freq3d3
                 else
                   goto 990
                 endif
               else                 
                 goto 990
               endif
            elseif (oper(ioper) .eq. codefunc(36)) then		! itos
               if (zz2.eq.0.d0) then       ! switch on dim (0 is joined file)
                 if (zz1.eq.1.d0) then		! switch on axis
                    zz1 = dble(itosr(real(zz3),c_specwf1))
                 elseif (zz1.eq.2.d0 .and. c_dimmanip.gt.1) then
                    zz1 = dble(itosr(real(zz3),c_specwf2))
                 elseif (zz1.eq.3.d0 .and. c_dimmanip.gt.2) then
                    zz1 = dble(itosr(real(zz3),c_specwf3))
                 else
                   goto 990
                 endif
               elseif (zz2.eq.1.d0) then		! switch on dim
                   zz1 = dble(itosr(real(zz3),sp1d))
               elseif (zz2.eq.2.d0) then
                 if (zz1.eq.1.d0) then		! switch on axis
                   zz1 = dble(itosr(real(zz3),specw1))
                 elseif (zz1.eq.2.d0) then
                   zz1 = dble(itosr(real(zz3),specw2))
                 else
                   goto 990
                 endif
               elseif (zz2.eq.3.d0) then
                 if (zz1.eq.1.d0) then		! switch on axis
                   zz1 = dble(itosr(real(zz3),sp3d1))
                 elseif (zz1.eq.2.d0) then
                   zz1 = dble(itosr(real(zz3),sp3d2))
                 elseif (zz1.eq.3.d0) then
                   zz1 = dble(itosr(real(zz3),sp3d3))
                 else
                   goto 990
                 endif
               else                 
                 goto 990
               endif
            elseif (oper(ioper) .eq. codefunc(37)) then		! stoi
               if (zz2.eq.0.d0) then       ! switch on dim (0 is joined file)
                 if (zz1.eq.1.d0) then		! switch on axis
                    zz1 = dble(stoir(real(zz3),c_specwf1))
                 elseif (zz1.eq.2.d0 .and. c_dimmanip.gt.1) then
                    zz1 = dble(stoir(real(zz3),c_specwf2))
                 elseif (zz1.eq.3.d0 .and. c_dimmanip.gt.2) then
                    zz1 = dble(stoir(real(zz3),c_specwf3))
                 else
                   goto 990
                 endif
               elseif (zz2.eq.1.d0) then		! switch on dim
                   zz1 = dble(stoir(real(zz3),sp1d))
               elseif (zz2.eq.2.d0) then
                 if (zz1.eq.1.d0) then		! switch on axis
                   zz1 = dble(stoir(real(zz3),specw1))
                 elseif (zz1.eq.2.d0) then
                   zz1 = dble(stoir(real(zz3),specw2))
                 else
                   goto 990
                 endif
               elseif (zz2.eq.3.d0) then
                 if (zz1.eq.1.d0) then		! switch on axis
                   zz1 = dble(stoir(real(zz3),sp3d1))
                 elseif (zz1.eq.2.d0) then
                   zz1 = dble(stoir(real(zz3),sp3d2))
                 elseif (zz1.eq.3.d0) then
                   zz1 = dble(stoir(real(zz3),sp3d3))
                 else
                   goto 990
                 endif
               else                 
                 goto 990
               endif

            elseif (oper(ioper) .eq. codefunc(40)) then		! itod
               if (zz2.eq.0.d0) then       ! switch on dim (0 is joined file)
                 if (zz1.eq.1.d0) then		! switch on axis
                    zz1 = dble(itokr(real(zz3),
     *                   c_sizef1, c_dmin, c_dmax, c_dfactor))
                 elseif (zz1.eq.2.d0 .and. c_dimmanip.gt.1) then
                    zz1 = dble(itokr(real(zz3),
     *                   c_sizef1, c_dmin, c_dmax, c_dfactor))
                 elseif (zz1.eq.3.d0 .and. c_dimmanip.gt.2) then
                    zz1 = dble(itokr(real(zz3),
     *                   c_sizef1, c_dmin, c_dmax, c_dfactor))
                 else
                   goto 990
                 endif
               elseif (zz2.eq.1.d0) then		! switch on dim
                   zz1 = dble(itokr(real(zz3),sizeimage1d,
     *                        dmin,dmax,dfactor))
               elseif (zz2.eq.2.d0) then
                 if (zz1.eq.1.d0) then		! switch on axis
                   zz1 = dble(itokr(real(zz3),si1im,
     *                        dmin,dmax,dfactor))
                 elseif (zz1.eq.2.d0) then
                   zz1 = dble(itokr(real(zz3),si2im,
     *                        dmin,dmax,dfactor))
                 else
                   goto 990
                 endif
               elseif (zz2.eq.3.d0) then
                 if (zz1.eq.1.d0) then		! switch on axis
                   zz1 = dble(itokr(real(zz3),si3d1,
     *                        dmin,dmax,dfactor))
                 elseif (zz1.eq.2.d0) then
                   zz1 = dble(itokr(real(zz3),si3d2,
     *                        dmin,dmax,dfactor))
                 elseif (zz1.eq.3.d0) then
                   zz1 = dble(itokr(real(zz3),si3d3,
     *                        dmin,dmax,dfactor))
                 else
                   goto 990
                 endif
               else                 
                 goto 990
               endif

            elseif (oper(ioper) .eq. codefunc(41)) then		! dtoi
               if (zz2.eq.0.d0) then       ! switch on dim (0 is joined file)
                 if (zz1.eq.1.d0) then		! switch on axis
                    zz1 = dble(ktoir(real(zz3),
     *                   c_sizef1, c_dmin, c_dmax, c_dfactor))
                 elseif (zz1.eq.2.d0 .and. c_dimmanip.gt.1) then
                    zz1 = dble(ktoir(real(zz3),
     *                   c_sizef1, c_dmin, c_dmax, c_dfactor))
                 elseif (zz1.eq.3.d0 .and. c_dimmanip.gt.2) then
                    zz1 = dble(ktoir(real(zz3),
     *                   c_sizef1, c_dmin, c_dmax, c_dfactor))
                 else
                   goto 990
                 endif
               elseif (zz2.eq.1.d0) then		! switch on dim
                   zz1 = dble(ktoir(real(zz3),sizeimage1d,
     *                        dmin,dmax,dfactor))
               elseif (zz2.eq.2.d0) then
                 if (zz1.eq.1.d0) then		! switch on axis
                   zz1 = dble(ktoir(real(zz3),si1im,
     *                        dmin,dmax,dfactor))
                 elseif (zz1.eq.2.d0) then
                   zz1 = dble(ktoir(real(zz3),si2im,
     *                        dmin,dmax,dfactor))
                 else
                   goto 990
                 endif
               elseif (zz2.eq.3.d0) then
                 if (zz1.eq.1.d0) then		! switch on axis
                   zz1 = dble(ktoir(real(zz3),si3d1,
     *                        dmin,dmax,dfactor))
                 elseif (zz1.eq.2.d0) then
                   zz1 = dble(ktoir(real(zz3),si3d2,
     *                        dmin,dmax,dfactor))
                 elseif (zz1.eq.3.d0) then
                   zz1 = dble(ktoir(real(zz3),si3d3,
     *                        dmin,dmax,dfactor))
                 else
                   goto 990
                 endif
               else                 
                 goto 990
               endif

            else
               write(*,*) 'eval_exp should never get there'
               goto 990
            endif
            
            
120         if (zz1.eq.0.0) then	! jump here if zz1 is to be pushed
               stl1='0'
            else
               write(stl1,20) zz1
            endif
            call trailzero(stl1)
            call pushval(stl1,err)
110         ioper = ioper - 1		! jump here if nothing to push
            goto 100	
         endif
      endif       ! if something to do
      
      return
20    format(G21.15)
990   call gifaout( '*** expression error or arithmetic error' )
      err=1
      return
      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine trailzero(st)
c INOUT	: st
c
c removes the trailing zeros of a numeric string.
c removes also the leading blanks
c eg : "  1.10000" is returned as "1.1      :
c    
      implicit none
      character*(*) st
      integer i, j, k, ii

      call leading(st)
      if (index(st,'.') .ne. 0 .and. index(st,'E').eq.0) then
C  if a decimal point somewhere but not an E expression
         i = index(st,' ')-1
         if (i .le. 0) i = len(st)
10       if (st(i:i).eq.'0' .and. i.gt.2) then	! case x.0000
               st(i:i) = ' '
               i = i-1
               goto 10
         elseif (st(i:i).eq.'.') then
               st(i:i) = ' '
         elseif  (st(1:2).eq.'.0') then	     ! case '.0' entry
               st(1:2) = '0 '
         endif
      else if  (index(st,'E').ne.0) then
C  if an E expression
         i = index(st,'E')
         k = index(st,' ')
         j = i-1
         do while (st(j:j).eq.'0')
            st(j:j) = ' '
            j = j-1
         enddo

         if (j .lt. i-1) then
C    x.yyy    Ezzzz
C    1   j    i    k

            do ii=i,k
               st(j+ii+1-i:j+ii+1-i) = st(ii:ii)
               st(ii:ii) = ' '
            enddo
         endif
      endif
      return
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine uncode(st,l)
C INOUT   st
C OUT     l
C
C reset char(127) to blanks in string, and return length
      implicit none
      integer l
      character*(*) st

      call trailing(st,l)
      call cleanst(st,l)      ! blanks are coded as char(127)
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine recode(st,l)
C INOUT   st
C IN      l
C
C reset   char(127) to blanks in string
      implicit none
      integer l,i
      character*(*) st

      do i=1,l          ! blanks should be coded as char(127)
             if (st(i:i) .eq. ' ') st(i:i)=char(127)
      enddo
      return
      end




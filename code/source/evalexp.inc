C ---> include file
      integer stackmax
      parameter (stackmax=20)
      character*256 stack(stackmax)
      integer oper(stackmax), prio(stackmax)
      integer ioper,istack,cprio,he
C
c internal code for operators
      integer noop,uplus,uminus,plus,minus,mult,div,
     *  pow,exp,concat,or,and,equal,nequal,sequal,snequal,
     *  great,smal,greateq,smaleq,not,modulo,concat2
      parameter ( noop=0,uplus=1,uminus=2,plus=3,minus=4,
     *  mult=5,div=6,pow=7,exp=8,concat=9,
     *  or=11,and=12,equal=13,nequal=14,sequal=15,snequal=16,
     *  great=17,smal=18,greateq=19,smaleq=20,not=21,modulo=22,
     *  concat2=23 )

c priorities for op.
      integer pnoop,puplus,puminus,pplus,pminus,pmult,pdiv,
     *  ppow,pexp,pconcat,pconcat2,pfunc,pparan,
     *  por,pand,pequal,pnequal,psequal,psnequal,
     *  pgreat,psmal,pgreateq,psmaleq,pnot,pmodulo
      parameter ( pnoop=0,puplus=10,puminus=10,pplus=6,pminus=6,
     *  pmult=7,pdiv=7,ppow=8,pexp=8,pconcat=1,pconcat2=1,pfunc=20,
     *  pparan=100,pmodulo=7,
     *  por=2,pand=2,pequal=3,pnequal=3,psequal=3,psnequal=3,
     *  pgreat=4,psmal=4,pgreateq=4,psmaleq=4,pnot=10)

      integer nfunc,lfuncmax
      parameter (nfunc = 45,lfuncmax = 8)
      integer codefunc(nfunc), lfunc(nfunc)
c      character*(lfuncmax) func(nfunc)
      character*8 func(nfunc)
C
      common /evalcom/oper,prio,ioper,istack,cprio,he,codefunc,
     *          lfunc,stack,func
c





C Include file to define the size of static buffers
C
C DO NOT change this file, change sizebase.inc rather
C
#include "sizebase.inc"

      integer smxbig,szindex,crossmax,datamax

      parameter (smxbig = 8*smxmax)       !       This is the size of the
                                          !        big buffer
      parameter (szindex = sizemax)
      parameter (crossmax = 256)
      parameter (datamax = 4*smxmax)

      real mem_dummyarray(2*smxmax),datab(smxmax),
     *     dent(smxmax),dchi(smxmax),work(smxmax),
     *     work2(smxmax),work3(smxmax),
     *     work1d(sizemax),plane2d(smxmax),column(sizemax)
      real image(smxbig),imago(smxbig+sizemax+smxmax)
      integer tindex(szindex), imdim(3)

C mem_dummyarray keeps the place for the buffer image(smxmax) when running
C mem (software controled). When running only for 2d/3d nmr, image can get
C as big as 8*smxmax but datab, dent, dchi are lost.
C
C 20-DEC-91
C New memory management : the set up is now :
C 1d buf / 2d buf / 3d buf     contiguous in memory, 
C 1d and 2d can overflow, at the expense of the n+1 buffer(2d or 3d)
C imdim(i) i = 1..3 holds the entry point in the big buffer imago
C i.e. imago(imdim(1)) = column
C      imago(imdim(2)) = plane2d
C      imago(imdim(3)) = image
C
C 1d buf../2d buf.../3d buf 
C imago......................................................................
C column/plane2/image.......................................................
C              /mem_dummyarray.../dent../dchi../datab../work../work2../work3
C        smxmax   2*smxmax         ...smxmax each...
C
C   The symbol mem_dummyarray is never used in the code.
C

      equivalence( image(1),  mem_dummyarray(1))
	  equivalence( imago(1), column(1))

      common /things/tindex,imdim
      complex*16 dalign
      common /buffer/dalign,column,plane2d,mem_dummyarray,dent,dchi,
     *     datab,work,work2,work3,work1d

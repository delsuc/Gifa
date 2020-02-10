C Include file to define the size of static buffers
C
C  When changing this file, don't forget to check lvarian.c
C

      integer sizemax,smxmax,peakmax,smxbig,szindex,crossmax
      parameter (sizemax = 32*1024 + 256)
      parameter (smxmax = 512*1024  )    ! 2048k
      parameter (peakmax = smxmax/4   )   ! 128k
      parameter (smxbig = 8*smxmax)       ! 16 Meg  This is the size of the
                                        !        big buffer
      parameter (szindex = 100)
      parameter (crossmax = 256)

      real mem_dummyarray(2*smxmax),data(smxmax),
     *     dent(smxmax),dchi(smxmax),work(smxmax),
     *     work2(smxmax),work3(smxmax),
     *     work1d(sizemax)
      real image(smxbig),peak(peakmax,4)
      integer index(szindex), nbpic

C mem_dummyarray keeps the place for the buffer image(smxmax) when running
C mem (software controled). When running only for 2d nmr, image can get
C as big as 8*smxmax but data, dent, dchi are lost.
C   The symbol mem_dummyarray is never used in the code.
C
C Peak holds the peak picker and integrator informations,
C for i=1 to nbpic,     holds peak info.
C        peak(i,1) is x coord of a peak  (col) 
C        peak(i,2) is y coord of a peak  (row) (or width if line-fitted)
C        peak(i,3) is volume  of a peak
C        peak(i,4) is 0 if peak is a local maximum,
C                  is a pointer to another peak if this one is clustered.


      equivalence( image(1), mem_dummyarray(1))
      equivalence( peak(1,1), work(1))

      common /things/index,nbpic

      common /buffer/mem_dummyarray,data,dent,dchi,
     *               work,work2,work3,work1d

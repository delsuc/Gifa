C peakinti.inc
C
C this file initialises the peak table variables
C (PARIS)
      maxi=0.0
      mini=0.0
      nbpic1d=0
      nbpic2d=0
      nbpic3d=0
      porig1d=0
      porig2d=0
      porig3d=0
      pkradius=0
      factor=10.0
      thresh=0.0
      slope=0.0
      volume=0.0
      incert=0.0
      zq=0
      signpic=1
      amdisp=0
      found_pk = ' '
      peakpat = '+'
      do i=1,peakmax
        peak1d_id(i) = ' '
        peak2d_id(i) = ' '
        peak3d_id(i) = ' '
      enddo          

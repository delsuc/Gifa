C Include file to define the size of static buffers
C The larger 2D data set will be 8 x smxmax,
C the larger MaxEnt or LP run will be on smxmax
C the larger 1D data processed by lp will be ldmax
C
C
C this one is for large size, up to 4k x 4k in 2D
C
      integer ldmax
      parameter (ldmax = 720) 
c
c	Be carefull ! ldmax should be greater than 32
c	Be carefull ! 4*ldmax*ldmax should be smaller than smxmax
c
      integer smxmax,sizemax
      parameter (smxmax = 2048*1024)      ! 2048k
      parameter (sizemax = smxmax/16)

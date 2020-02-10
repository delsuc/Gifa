C constant.inc
C
C    Include file holding some constant value

      real pi,epsilon,maxreal,maxexp
      real*8 dpi,depsilon

      parameter(  pi = 3.1415926535898E0,
     *           dpi = 3.1415926535898D0,
     *           epsilon = 1.0e-7,
     *           depsilon = 1.0e-14,
     *           maxexp = 1.0e+38, 
     *           maxreal = 1.0/epsilon)


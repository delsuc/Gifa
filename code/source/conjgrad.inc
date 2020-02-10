C Common for the Conjugate gradient algorithm.
C used by the 1D function, start is the starting point for 1D search,
C and axis is the gradient used.
      real     eps
      integer  taillemax,actualsize
      parameter (taillemax=100,eps=1e-10)
      real axis(taillemax),start(taillemax)
      common/func1d/axis,start,actualsize

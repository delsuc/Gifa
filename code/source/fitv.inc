#include "levenberg.inc"
C size of prmrxd is based on size of levenberg internal buffers      
      integer    tlmxpk1,tlmxpk2,sdtmx
      parameter (tlmxpk1=sizelev, sdtmx=sizemax/2)
      parameter (tlmxpk2=tlmxpk1*4/6+1)
      real       prmr1d(4*tlmxpk1),prmr2d(6*tlmxpk2)
      real       absx(sdtmx),ordy(sdtmx),
     *           bid1,bid2
      real       minimize, minimize1d
      real       fitgene, fitgene1d
      external   flogau,dflogau,flogau2d,dflogau2d,
     *           minimize, minimize1d,
     *           fitgene, fitgene1d, fitg2, dfitg2
      
c bid1 and bid2 are there to insure that prmr2d will not overflow the common
      common    /fitc/prmr1d,bid1,bid2

C absx and ordy could moved to work3, in order to get more room
c for 2D fit.
      equivalence (absx(1), work1d(1))
      equivalence (ordy(1), work1d(1+sdtmx))
      equivalence (prmr1d,prmr2d)




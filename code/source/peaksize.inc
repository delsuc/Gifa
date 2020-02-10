c file peaksize.inc
c
C Peak tables definition for GIFA
c author M.A.DELSUC        25 may 1993
C modified M.A.D.          18 sep 1997 to add for uncertainties
c
c #include "sizebase.inc"
      integer peakmax,pk1d,pk2d,pk3d
      parameter (peakmax = 4*ldmax)                 ! length of peak tables
      parameter (pk1d = 8, pk2d = 11, pk3d = 7)     ! with of peak tables
      real peak1d(peakmax,pk1d),peak2d(peakmax,pk2d),
     *     peak3d(peakmax,pk3d)
      integer nbpic1d, nbpic2d, nbpic3d,
     *        porig1d, porig2d, porig3d
c
C Peak holds the peak picker, integrator and line-fitting informations,
c
c porignd tells who loaded it last
c     0:empty 1:peakpicker, 2:integrator, 3:fitter, 4:linear-prediction 5:user
c
c rem : case 2 and 4, amplitude computed as volume/width
c       in all cases but 1, volume can be obtained as amplitude*width
c
C for i=1 to nbpic,     holds peak info.
c in 1D     peak1d(i,1) = position		for i=1 to nbpic1d
c           peak1d(i,2) = width
c           peak1d(i,3) = amplitude
c           peak1d(i,4) = phase         (with LP)
c           peak1d(i,5) = type : unknown / lorentz / gauss (/ 0 / 1 / 2)
c           peak1d(i,6) = uncertainties on pos
c           peak1d(i,7) = uncertainties on width
c           peak1d(i,8) = uncertainties on amp

c in 2D     peak2d(i,1) = position	F2	for i=1 to nbpic2d
c           peak2d(i,2) = width F2
c           peak2d(i,3) = amplitude
c           peak2d(i,4) = position	F1
c           peak2d(i,5) = width F1
c           peak2d(i,6) = uncertainties on amp
c           peak2d(i,7) = type : unknown / lorentz / gauss (/ 0 / 1 / 2)
c           peak2d(i,8) = uncertainties on pos F2
c           peak2d(i,9) = uncertainties on width F2
c           peak2d(i,10) = uncertainties on pos F1
c           peak2d(i,11) = uncertainties on width F1

c in 3D     peak3d(i,1) = position	F3	for i=1 to nbpic3d
c           peak3d(i,2) = width F3
c           peak3d(i,3) = amplitude
c           peak3d(i,4) = position	F1
c           peak3d(i,5) = width F1
c           peak3d(i,6) = position	F2
c           peak3d(i,7) = width F2
c
c positions and widthes are in points, phases are in degrees,
c amplitude are in arbitrary unit.
c
c negative width is a pointer to another peak in table
C
      common/peak/nbpic1d, nbpic2d, nbpic3d, porig1d, porig2d, porig3d,
     *       peak1d, peak2d, peak3d







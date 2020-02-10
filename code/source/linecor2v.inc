      integer degre,degremax,segm1,segm2,approx,
     *lisse,nbportion,maxportion,blocbase,bcmax,
     *iterma2,blciter,blcitermax,
     *nueealgo,nueeiter,nueedistance    

      parameter (degremax = 10)
      parameter (blcitermax = 10)
      parameter (maxportion = 10)
      parameter (bcmax=smxmax/22)

      real ldbu,ldbv,ldbw,sdb,sds,scs,scb,seuilsegm2,
     *seuilhyste,nueefactor,morphob,morphos,fenma,fenma2,
     *fenlin,fensegm2
      real corr(bcmax),segm(bcmax),source1(bcmax),
     *source2(bcmax),source(bcmax),source0(bcmax),
     *base(bcmax,0:degremax),weigth(bcmax),x(bcmax),
     *stockage(3*bcmax) 

      character*256 mes

      equivalence (work3(21*bcmax+1),corr(1))
      equivalence (work3(20*bcmax+1),segm(1))
      equivalence (work3(20*bcmax+1),x(1))
      equivalence (work3(19*bcmax+1),source2(1))
      equivalence (work3(18*bcmax+1),source1(1))
      equivalence (work3(17*bcmax+1),source0(1))
      equivalence (work3(16*bcmax+1),source(1))
      equivalence (work3(15*bcmax+1),weigth(1))          
      equivalence (work3(4*bcmax+1),base(1,0))
      equivalence (work3(bcmax+1),stockage(1))

      common /line2/lisse,segm1,segm2,approx,degre,
     *       nbportion,morphob,morphos,fenma,fenma2,iterma2,
     *       blciter,fensegm2,fenlin,nueealgo,nueeiter,
     *       nueedistance,ldbu,ldbv,ldbw,sdb,sds,scb,scs,
     *       nueefactor,seuilsegm2,seuilhyste,blocbase,mes


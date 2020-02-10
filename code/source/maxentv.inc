C file maxentv.inc
C
C holds all the value used by the maximum entropy iteration process
C
      real    lambda,lambda2,lambdasp,
     *        ent,sum,inc,noise,shift,step,sigma,
     *        qu,chi2,conv,ivalue,norm,stepmax,
     *        escale,tolerance
      integer iter,algo,overit,
     *        done, output, itermax, nshow, miniter,
     *        filset,lambcont,nndisp,memmode

C memmode is code for kind of transform :
C	1 : FT
C	2 : laplace

      parameter (nshow=1000)

      real    sent(nshow), schi(nshow), slamb(nshow),
     *        sconv(nshow),sstep(nshow),ssum(nshow)

      real expa, aa
      common/expcom/ expa, aa

      common /gullcom/ lambda,lambda2,lambdasp,
     *        ent,sum,inc,noise,shift,step,sigma,
     *        qu,chi2,conv,ivalue,norm,stepmax,
     *        escale,tolerance, iter,algo,overit,
     *        done, output, itermax, miniter,
     *        filset,lambcont,nndisp,memmode

      common /show/ sent,schi,slamb,sconv,sstep,ssum

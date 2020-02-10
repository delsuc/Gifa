C include file for 3D display
      real alpha,beta,gama,		! the 3 display angles
     *	ca,sa,cb,sb,cg,sg,		! the sine and cosine
     *	scale, scx, scy, scz,		! scale 
     *  xr, yr, zr,			! translation
     *	znot			! 1 / position of visual point (0 = infinity)
      real rrr(4,3)	! the homogenous rotation matrix

      common /lib3dc/alpha,beta,gama,ca,sa,cb,sb,cg,sg,
     *	scale, scx, scy, scz, xr, yr, zr,znot,rrr


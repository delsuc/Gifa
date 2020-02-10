C peakintv.inc
C
C this file contains the peak integration variables
C (PARIS)
C the other variables used for peaks manipulations are in sizeparam.inc

	real		maxi,mini,factor,thresh,
     *                  slope,volume,incert, found_dist
	integer		colg,cold,ligh,ligb,limg,limd,
     *			signpic,comp,amdisp,zq
        character*256   found_pk
      common /peakintv/ maxi,mini,factor,thresh,
     *                  slope,volume,incert,
     *                  colg,cold,ligh,ligb,limg,limd,
     *                  signpic,comp,amdisp,zq,found_dist,found_pk
c
c variable pour nouvelles commandes AMOEBA
c
	integer getmaskno
	external getmaskno

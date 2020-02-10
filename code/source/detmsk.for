c Therese Malliavin,
c
c last modif : nov 1995

	subroutine detmsk (masktab,tab,taille1,taille2,
     & verbose,indxx,indxy,factor,slope,
     & threshold,rad,signpic,zq)
c
c Determine a peak amoeba from the peak maximum, and the peak 
c maximum coordinatesstab
c 
c IN	: tab,masktab,taille1,taille2,amp,factor,slope,
c IN    : threshold,radius,signpic,verbose,zq
c OUT	: peak2d
c
	implicit none

#include "sizebase.inc"
#include "peaksize.inc"

	integer taille2,taille1,rad,verbose
	integer nbi,nblt,nbneigh
	integer i,v,u,signx,signy,ltpeak
	integer indxx(8*rad),indxy(8*rad),ind(2)
	integer x0,y0,crdx,crdy,signpic,zq,xpic,ypic
	real factor,slope,threshold,amp,pente
	real dsdx,dsdy,dst2,pentemin
	real tabxy,tabx1y,tabxy1
	real masktab(taille2,taille1)
	real tab(taille2,taille1)
	character*256 line

	do i = 1,nbpic2d
           x0 = int(peak2d(i,1)+0.5)
           y0 = int(peak2d(i,4)+0.5)
	   amp = signpic*tab(x0,y0)
           if (zq.eq.1) amp = abs(amp)
           call doslide (indxx,indxy,x0,y0,rad)
	   do u = 1,(8*rad)
	      crdx = indxx(u)
	      crdy = indxy(u)
c
c calculate the number nbi of neighbors of the pixel belonging to peak i amoeba
c if (nbi = 0) the pixel will not be assigned to the peak i
c
              nbi = nbneigh (masktab,taille2,taille1,
     & crdx,crdy,i)
              if ((crdx.ge.0).and.(crdy.ge.0).and.
     & (crdx.le.taille2).and.(crdy.le.taille1).and.(nbi.gt.0)) then
		signx = 1
		signy = 1
                if (crdx.ne.x0) signx = int((crdx-x0)/abs(crdx-x0))
                if (crdy.ne.y0) signy = int((crdy-y0)/abs(crdy-y0))
		tabxy = signpic*tab(crdx,crdy)
		if (zq.eq.1) tabxy = abs(tabxy)
		tabx1y = signpic*tab(crdx-signx,crdy)
		if (zq.eq.1) tabx1y = abs(tabx1y)
		tabxy1 = signpic*tab(crdx,crdy-signy)
		if (zq.eq.1) tabxy1 = abs(tabxy1)
c	        dsdx = real(signx)*(tab(crdx,crdy) - tab(crdx-signx,crdy))
c	        dsdy = real(signy)*(tab(crdx,crdy) - tab(crdx,crdy-signy))
	        dsdx = real(signx)*(tabxy - tabx1y)
	        dsdy = real(signy)*(tabxy - tabxy1)
	        pente = (dsdx*real(crdx-x0)) + (dsdy*real(crdy-y0))
		dst2 = ( (real(crdx-x0)*real(crdx-x0)) + 
     & (real(crdy-y0)*real(crdy-y0)) )
		pente = signpic*pente/sqrt(dst2)
c	        if ( (signpic*tab(crdx,crdy).gt.threshold) .and.
c     & (abs(amp/tab(crdx,crdy)).le.factor).and.(pente.lt.slope) ) then
	        if ( (tabxy.gt.threshold) .and.
     & (amp/tabxy.le.factor).and.(pente.lt.slope) ) then
		  if (masktab(crdx,crdy).eq.0.0) then
		    masktab(crdx,crdy) = real(abs(i))
                  else
c this pixel was already put in another amoeba -> warning
		    if (verbose.ne.0) then
    		      write(line,*)'Warning: the pixel ',
     & crdx,crdy,' was assigned to two peaks: ',
     & int(masktab(crdx,crdy)),' and ',i
   		      call gifaout(line) 
		    endif
c if the pixel still belongs to another peak: ltpeak = masktab(crdx,crdy)
c calculate the number nblt of pixel neighbors belonging to ltpeak amoeba
c calculate the number nbi of pixel neighbors belonging to i amoeba
c (see the function nbneigh)
c if nbi = nblt = 0 masktab(crdx,crdy) = 0
c if (nbi > nblt)   masktab(crdx,crdy) = i
c if (nblt > nbi)   masktab(crdx,crdy) = ltpeak
c if (nblt = nbi)   recalculate the slope obtained with the other peak, 
c and attribute the pixel to the smallest slope
c
		    ltpeak = int(masktab(crdx,crdy))
		    nbi = nbneigh (masktab,taille2,taille1,
     & crdx,crdy,i)
		    nblt = nbneigh (masktab,taille2,taille1,
     & crdx,crdy,ltpeak)
		    if ((nbi.eq.0).and.(nblt.eq.nbi)) then
		       masktab(crdx,crdy) = 0.0
		    elseif (nbi.gt.nblt) then
		       masktab(crdx,crdy) = real(i)
		    elseif (nblt.gt.nbi) then
		       masktab(crdx,crdy) = real(ltpeak)
		    elseif (nblt.eq.nbi) then
c recalculate the slope obtained with the other peak, 
c and attribute the pixel to the smallest slope	
                       ind(1) = i
                       ind(2) = int(masktab(crdx,crdy))
		       pentemin = slope
		       do v = 1,2
	                  signx = 1
	                  signy = 1
                          xpic = int(peak2d(ind(v),1)+0.5)
                          ypic = int(peak2d(ind(v),4)+0.5)
                          if (crdx.ne.xpic) 
     & signx = int((crdx-xpic)/abs(crdx-xpic))
                          if (crdy.ne.ypic) 
     & signy = int((crdy-ypic)/abs(crdy-ypic))
		          tabxy = signpic*tab(crdx,crdy)
		          if (zq.eq.1) tabxy = abs(tabxy)
		          tabx1y = signpic*tab(crdx-signx,crdy)
		          if (zq.eq.1) tabx1y = abs(tabx1y)
		          tabxy1 = signpic*tab(crdx,crdy-signy)
		          if (zq.eq.1) tabxy1 = abs(tabxy1)
c	                  dsdx = real(signx)*(tab(crdx,crdy) - 
c     & tab(crdx-signx,crdy))
c	                  dsdy = real(signy)*(tab(crdx,crdy) - 
c     & tab(crdx,crdy-signy))
	                  dsdx = real(signx)*(tabxy - tabx1y)
	                  dsdy = real(signy)*(tabxy - tabxy1)
	                  pente = (dsdx*real(crdx-xpic)) + 
     & (dsdy*real(crdy-ypic))
	                  dst2 = (real(crdx-xpic)*real(crdx-xpic)) + 
     & (real(crdy-ypic)*real(crdy-ypic)) 
	                  pente = signpic*pente/sqrt(dst2)
	                  if (pente.lt.pentemin) then
		             pentemin = pente
		             masktab(crdx,crdy) = real(ind(v))
	                  endif
		       enddo
		    endif
		  endif 
	        endif
	      endif
	   enddo
	enddo

	return
	end
	

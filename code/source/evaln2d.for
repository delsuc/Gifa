C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Authors :       M.A.Delsuc, V.Stoven, M.Robin
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully 
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C
      subroutine evaln2D(smx, si1,si2,noise,shift,err)
c IN	: smx, si1,si2
c OUT	: noise,shift,err
c
C evaluates the noise from the 2D smx, prompt the user for the
c area to evaluate
c the mean is returned in shift
c the standard deviation is returned in noise

      implicit none
      
      integer si1, si2, llcol, lllin, urcol, urlin,
     *        line,col,nbcol,nblin,err
      real smx( si2*si1 ),som,noise,shift

c asks for the area on which noise will be calculated                      
      call getrect(llcol,lllin,urcol,urlin,err)
      nbcol=urcol-llcol+1
      nblin=urlin-lllin+1

c calculates the vertical offset        
      shift=0.0
      do 10 line=lllin,urlin
         call sumvect(som,smx(llcol+si2*(line-1)),nbcol)
         shift=shift+som
10    continue
      shift=shift/float(nbcol*nblin)                    
C calculates noise level
      noise=0.0
      do 20 line=lllin,urlin
        do 30 col=llcol,urcol
          noise=noise + (smx(col+si2*(line-1))-shift)**2  
30      continue
20    continue    
      noise=sqrt(noise/float(nbcol*nblin))
      return
      end
                        

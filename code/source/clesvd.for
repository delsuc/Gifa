c******************************************************************************
      subroutine clesvd (svd,before,after,job)
c
c IN	: before,after,job
c INOUT	: svd
c
c Clean the singular values of matrix x : remove noise
c from the singular values
c complex*16 svd(before) singular values
c integer after : number of signals others than noise
c when job = 1, the mean power of the removed SVD is removed from
c               the remaining ones
c
	implicit none
c
	integer i,job
	integer before,after
c
	real*8 wnoise, t, ttt, tt
	complex*16 svd(before)
c
c	set to 0 the imaginary part of the SVD's (if not already done)
c
	do i = 1,before
	   svd(i) = dcmplx(real(svd(i)),0.0d0)
	enddo
c
c now remove extraneaous svd, considering them as noise
c
	wnoise=0.d0
	if (after.ge.before) return
c
	do i=after+1,before
   	   wnoise=wnoise+real(svd(i))**2
	   svd(i)=dcmplx(0.d0,0.d0)
	enddo
c
c Calculate the mean quadratic noise from the rejected SVD.
c At the beginning, there were "before" SVD; there are "after" kept SVD
c so, there are "before-after" rejected SVD
c
        if (job.eq.1) then
          wnoise=wnoise/dble(before-after)
        else
          wnoise = 0.0d0
        endif
c
c  correct the singular values
c
	do i = 1,after
   	   t = 0.d0
	   tt = real(svd(i))
   	   if(tt.ne.0.d0) then
      		ttt = dsqrt(tt*tt-wnoise)
      		if (ttt.ge.1.d-08) t = ttt
   	   end if
   	   svd(i) = dcmplx(t,0.0d0)
	enddo
c
	return
	end
c******************************************************************************
      subroutine countlevel (svd,before,after,job,level)
c
c IN	: before,after,job,level
c INOUT	: svd
c
c Keep the singular values greater than level
c svd(before): complex*16 singular values
c after: number of singular values greater than level 
c Clean the singular values of matrix x : remove noise
c from the singular values
c complex*16 svd(before) singular values
c integer after : number of signals others than noise
c when job = 1, the mean power of the removed SVD is removed from
c               the remaining ones
c
	implicit none
c
	integer i,job
	integer before,after
c
	real*8 wnoise, t, ttt, tt, tlevel
        real level
	complex*16 svd(before)
c
c	set to 0 the imaginary part of the SVD's (if not already done)
c
        tlevel = dble(level)
        after = 0
	do i = 1,before
	   svd(i) = dcmplx(real(svd(i)),0.0d0)
	   if (real(svd(i)).ge.tlevel) then
	      after = after + 1
	   endif
	enddo
c
c now remove extraneaous svd, considering them as noise
c
	wnoise=0.d0
	if (after.ge.before) return
c
	do i=after+1,before
   	   wnoise=wnoise+real(svd(i))**2
	   svd(i)=dcmplx(0.d0,0.d0)
	enddo
c
c Calculate the mean quadratic noise from the rejected SVD.
c At the beginning, there were "before" SVD; there are "after" kept SVD
c so, there are "before-after" rejected SVD
c
        if (job.eq.1) then
          wnoise=wnoise/dble(before-after)
        else
          wnoise = 0.0d0
        endif
c
c  correct the singular values
c
	do i = 1,after
   	   t = 0.d0
	   tt = real(svd(i))
   	   if(tt.ne.0.d0) then
      		ttt = dsqrt(tt*tt-wnoise)
      		if (ttt.ge.1.d-08) t = ttt
   	   end if
   	   svd(i) = dcmplx(t,0.0d0)
	enddo
c
	return
	end




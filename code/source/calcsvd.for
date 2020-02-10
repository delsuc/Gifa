      subroutine calcsvd (x,svd,u,v,err,wk,sizdat,order,nd1,error)
c IN	: x,sizdat,order,nd1
c OUT	: u,v,svd,error
c WORK	: err,wk
c
c Calculate the singular decomposition of matrix x and remove noise
c from the singular values
c complex*16 x(nd1,order)
c complex*16 svd(order) singular values
c complex*16 u(nd1,nd1),v(order,order) transformation matrices
c err,wk : working areas
c
	implicit none
c
	integer job,info,ldu,ldv,ldx,nd1
	integer sizdat,order,error
c
	complex*16 x(nd1,order), wk(nd1),
     &      svd(order), err(order), v(order,order),
     &	    u(nd1,nd1)
c
c  now get Singular Value Decomposition of X using dsvdc
c       results are returned in S:svd, U & V
c
	ldx = nd1
	ldu = nd1
	ldv = order
	job = 11
c
c order:number of column, nd:number of rows
c x(i,j), i:row,1..ndprim  j:column,1..order  with nd>=order
c
	call cdsvdc(x,ldx,nd1,order,svd,err,u,ldu,v,ldv,wk,job,info)
	if (info.ne.0) then
	   call gifaout('*** Singular values decomposition failed !')
	   error = 1
	endif
c
	return
	end

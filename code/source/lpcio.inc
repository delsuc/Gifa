C lpcio.inc    Include file for LPCio
	integer maxbuf
	parameter (maxbuf=32*1024)
	integer buffer(maxbuf)
	real   rbuffer(maxbuf)
	integer offset,size1,size2,sizear,total
	equivalence (buffer,rbuffer)
	common/lpc/offset,size1,size2,sizear,total,buffer



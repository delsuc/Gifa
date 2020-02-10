#Makefile for Gifa utilities

# macros for the C routines
CC = cc
# be VERY careful ! -O tends to be buggy (for instance on HP-UX 9.03 )
COPT = -O

# CFLAGS has to be defined, depending on your machine :
# for HP-UX
#CFLAGS = -Aa -D_HPUX_SOURCE

# for SUN, SGI, IBM, linux PPC
#CFLAGS =

#for MacOS
CFLAGS = -DDARWIN 

# for linux i386 / Pentium
#CFLAGS = -Dis_big_endian


# for linking
L = $(CC)
LDFLAGS =  -lm -g

# link
convert_varian : convert_varian.o cache_mad.o util_mad.o reversebyte.o 
	$L -o convert_varian convert_varian.o cache_mad.o util_mad.o reversebyte.o $(LDFLAGS)

#then code itself
convert_varian.o : convert_varian.c cache_mad.h 
	$(CC) -c  $(CFLAGS) $(COPT) convert_varian.c

# for cache system
cache_mad.o : cache_mad.c cache_mad.h 
	$(CC) -c $(CFLAGS) $(COPT) cache_mad.c

util_mad.o : util_mad.c
	$(CC) -c $(CFLAGS) $(COPT) util_mad.c

reversebyte.o : reversebyte.c
	$(CC) -c $(CFLAGS) $(COPT) reversebyte.c

clean :
	rm -f *.o vn2cach *~ #*#



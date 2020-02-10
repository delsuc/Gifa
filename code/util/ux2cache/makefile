#Makefile for Gifa utilities

# macros for the C routines
CC = cc
# be VERY careful ! -O tends to be buggy (for instance on HP-UX 9.03 )
COPT =

# CFLAGS has to be defined, depending on your machine :
# for HP-UX
#CFLAGS = -Aa -D_HPUX_SOURCE

# for SUN, SGI, IBM, linux PPC
 CFLAGS = 

# for linux i386 / Pentium
#CLAGS = -Dis_big_endian

# for Apple MacOS X/Darwin
CFLAGS = -DDARWIN

# for linking
L = $(CC)
LDFLAGS =  -lm

#default entry
all: ux2cach 	# 2rr2cache


# link
ux2cach : ux2cach.o cache_mad.o util_mad.o reversebyte.o
	$L -o ux2cach ux2cach.o cache_mad.o reversebyte.o util_mad.o $(LDFLAGS)

2rr2cache : 2rr2cache.o cache_mad.o util_mad.o reversebyte.o
	$L -o 2rr2cache 2rr2cache.o cache_mad.o reversebyte.o util_mad.o $(LDFLAGS)

#then code itself
ux2cach.o : ux2cach.c cache_mad.h
	$(CC) -c  $(CFLAGS) $(COPT) ux2cach.c

2rr2cache.o : 2rr2cache.c cache_mad.h
	$(CC) -c  $(CFLAGS) $(COPT) 2rr2cache.c

# for cache system
cache_mad.o : cache_mad.c cache_mad.h
	$(CC) -c $(CFLAGS) $(COPT) cache_mad.c

util_mad.o : util_mad.c
	$(CC) -c $(CFLAGS) $(COPT) util_mad.c

reversebyte.o : reversebyte.c
	$(CC) -c $(CFLAGS) $(COPT) reversebyte.c

clean :
	rm -f *.o ux2cach 2rr2cache *~ #*#



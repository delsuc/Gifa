#Makefile for Gifa utilities

# macros for the C routines
CC = cc
# be VERY careful ! -O tends to be buggy (for instance on HP-UX 9.03 )
COPT = -O 

# CFLAGS has to be defined, depending on your machine :
# for HP-UX
#CFLAGS = -Aa -D_HPUX_SOURCE

# for SUN, SGI, IBM
CFLAGS = -32



# for linking
L = cc
LDFLAGS =  -32 -lm

# link
cach2xeasy : cach2xeasy.o cache_mad.o util_mad.o
	$L -o cach2xeasy cach2xeasy.o cache_mad.o util_mad.o $(LDFLAGS)

#then code itself
cach2xeasy.o : cach2xeasy.c cache_mad.h
	$(CC) -c  $(CFLAGS) $(COPT) cach2xeasy.c

# for cache system
cache_mad.o : cache_mad.c cache_mad.h
	$(CC) -c $(CFLAGS) $(COPT) cache_mad.c

util_mad.o : util_mad.c
	$(CC) -c $(CFLAGS) $(COPT) util_mad.c

clean :
	rm -f *.o cach2xeasy *~ #*#



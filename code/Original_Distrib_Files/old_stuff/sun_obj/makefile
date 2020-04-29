#Makefile for GIFA
# this one is for SUN
#uses perl scripts fpp for fortran preprocessing 
SHELL = /bin/sh

# first remove all definitions, and add ours
.SUFFIXES: 

.SUFFIXES: .o .c .for

#sources are in the directory VPATH, typically set by callers
#VPATH = /d1/people/melanie/gifav4/source/test_make

###################################################################
#START of the user-modifiable part
#macro for compilation

#set DEBUG to Y if you want to compile with debugger support
# typically set by callers
#DEBUG = Y

#CC and FORTRAN are the respective compilers
CC = /usr/local/bin/gcc
FORTRAN = /usr/lang/f77

#CFLAGS and FFLAGS are the compilation options (optimisation not included)
CFLAGS = -DSUN -DUNDERSCORE -I/usr/X11/include
FFLAGS = 

#COPT and FOPT are optimisation switches used if not DEBUG
COPT = -O
FOPT = -O

#FPP and FPPFLAGS are for the fortran preprocessor (here home-written in perl)
FPP = $$HOME/gifav4/source/fpp 
FPPFLAGS = -sun

# for linking
L = f77
LDFLAG =  -O -Bstatic -L/usr/lang/SC2.0.1 -lXm -lXt -lX11 /usr/local/lib/gcc-lib/sparc-sun-sunos4.1/2.4.5/libgcc.a 
 


# END of the user-modifiable part
###################################################################


#then rules
.c.o:
	@echo $<
	@if [ "$(DEBUG)" = "Y" ]; then \
		$(CC) $(CFLAGS) -g -c $(VPATH)/$< ; \
	else \
		$(CC) $(CFLAGS) $(COPT) -c $(VPATH)/$< ; \
	fi
	

.for.o:
	@echo $<
	@if [ "$(DEBUG)" = "Y" -o "$*" = "execute" ]; then \
		(cd $(VPATH); $(FPP) $(FPPFLAGS) -D $*) ; \
		mv $(VPATH)/$*.f .; \
		$(FORTRAN) $(FFLAGS) -g -c $*.f; \
	else \
		(cd $(VPATH);$(FPP) $(FPPFLAGS) $*) ; \
		mv $(VPATH)/$*.f .; \
		$(FORTRAN) $(FFLAGS) $(FOPT) -c $*.f; \
		rm -f $*.f; \
	fi



# then dependence list

include	/progs2/delsuc/gifav4/source/depend_list

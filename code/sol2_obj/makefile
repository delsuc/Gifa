#Makefile for GIFA
# this one is for SUN Solaris 2.x
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
CC = cc
FORTRAN = f77

#CFLAGS and FFLAGS are the compilation options (optimisation not included)
#CFLAGS = -DSUN -DSOLARIS -DUNDERSCORE -I/usr/dt/include -I/usr/openwin/include
CFLAGS = -DSUN -DSOLARIS -DUNDERSCORE -I/usr/ucb/include -I/usr/local/R5/include -I/usr/dt/share/include
FFLAGS = 

#COPT and FOPT are optimisation switches used if not DEBUG
#COPT = -O2 -ffast-math
COPT = -fast -O4 
#FOPT = -fast -O4 -cg89 -nolibmil
FOPT = -fast -O4

#FPP and FPPFLAGS are for the fortran preprocessor (here home-written in perl)
FPP = $(VPATH)/fpp 
FPPFLAGS = -sun

# for linking
#L = f77 -fast -O4 -cg89 -nolibmil
#L = f77 
L = f77 -O4 
#LDFLAG = -lXm -lXt -lX11 -lgen /usr/local/lib/gcc-lib/sparc-sun-solaris2.4/2.6.3/libgcc.a 
LDFLAG = -L/opt/SUNWspro/SC3.0.1/lib -L/usr/openwin/lib -L/usr/dt/lib -L/usr/dt/share/include -R/usr/openwin/lib -R/usr/dt/lib -Bstatic -lF77 -Bdynamic -lXm -lXt -lX11 -lm -lc 
#-lsocket -lnsl -lvolmgt -lgen
 


# END of the user-modifiable part
###################################################################


#then rules
.c.o:
	@echo $<
	@if [ "$(DEBUG)" = "Y" ]; then \
		$(CC) $(CFLAGS) -g -c $< ; \
	else \
		$(CC) $(CFLAGS) $(COPT) -c $< ; \
	fi
	

.for.o:
	@echo $<
	@if [ "$(DEBUG)" = "Y" ]; then \
		(cd $(VPATH); $(FPP) $(FPPFLAGS) -D $*) ; \
		mv $(VPATH)/$*.f .; \
		$(FORTRAN) $(FFLAGS) -g -c $*.f; \
	else \
		(cd $(VPATH);$(FPP) $(FPPFLAGS) $*) ; \
		mv $(VPATH)/$*.f .; \
	   	if [ $* = "execute" ]; then \
		  $(FORTRAN) $(FFLAGS) -c $*.f; \
		else \
		  $(FORTRAN) $(FFLAGS) $(FOPT) -c $*.f; \
		fi; \
		rm -f $*.f; \
	fi



# then dependence list

include	$(VPATH)/depend_list

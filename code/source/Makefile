#Makefile for GIFA

SHELL = /bin/sh

#list of architecture
LIST_DIR = sgi64_obj sgiv5_obj sgi3000_obj hpux9_obj hpux10_obj machten_obj aix_obj sun_obj sol2_obj spec_obj linux_obj darwin_obj

# location of distribution files
# DIST_DIR = /system_cbs/ftp/pub/gifa_v4_new
DIST_DIR = /CMPT/people/melanie/cdrom

#list of licence files
LICE_LIST = licence_eng.ps licence_eng.asc licence_fra.ps licence_fra.asc

#off-hand place
COPY_DIR = $$HOME

# RES_LIST is the list for source back-up
RES_LIST = source Makefile README_ppm README.html README README_2 com com_devel *_obj/makefile *_obj/fortran *_obj/get_licence.o macro util test journal
# RES_DIR is the off directory to put stuff on, SHOULD NOT BE '.' nor gifav4
RES_DIR = $$HOME

# KIT_DIR is the list of files and directories to put into the kit
KIT_DIR = $(LICE_LIST) README README_2 TODO History Brief_description List_of_files com macro test util help doc

#ARCHI sets-up shell variables for each platforms
ARCHI = -@archi="`uname`"; \
	echo "Making for $$archi"; \
	if [ $$archi = "HP-UX" ] ; then \
                release="`uname -r|cut -d . -f 2`"; \
                if [ $$release = "10" ] ; then \
                        echo "`uname -s -r`"; \
                        objdir=../hpux10_obj ; \
			kit=gifav4_HPUX10.tar.Z; \
			ddt=xdb; \
                else \
                        echo "`uname -s -r`"; \
                        objdir=../hpux9_obj ; \
                        kit=gifav4_HPUX9.tar.Z; \
                        ddt=xdb; \
                fi; \
	elif [ $$archi = "IRIX" ] ; then \
		ddt=dbx; \
		if ( uname -r | grep '6\.' >/dev/null ) ; then \
			echo "IRIX 6.x"; \
			objdir=../sgi64_obj ; \
			kit=gifav4_SGI6.tar.Z; \
		elif ( uname -r | grep '5\.' >/dev/null ) ; then \
			echo "IRIX 5.x"; \
			objdir=../sgiv5_obj ; \
			kit=gifav4_SGI5.tar.Z; \
                elif ( uname -r | grep '4\.' >/dev/null ) ; then \
			echo "IRIX 4.x"; \
			objdir=sgi3000_obj ; \
			kit=gifav4_SGI3000.tar.Z; \
		fi; \
	elif [ $$archi = "IRIX64" ] ; then \
		objdir=../sgi64_obj ; \
		ddt=dbx; \
		kit=gifav4_SGI6.tar.Z; \
	elif [ $$archi = "AIX" ] ; then \
		objdir=../aix_obj ; \
		ddt=dbx; \
		kit=gifav4_RS6000.tar.Z; \
	elif [ $$archi = "SunOS" ] ; then \
                release="`uname -r|cut -d . -f 1`"; \
                if [ $$release = "5" ] ; then \
                        echo "Solaris 2.`uname -r|cut -d . -f 2`"; \
                        objdir=../sol2_obj ; \
                        kit=gifav4_SOL2.tar.Z; \
                        ddt=dbx; \
                else \
                        echo "`uname -s -r`"; \
                        objdir=../sun_obj ; \
                        kit=gifav4_SUN.tar.Z; \
                        ddt=gdb; \
                fi; \
	elif [ $$archi = "Linux" ] ; then \
                echo "`uname -s -r`"; \
		if [ `uname -m` = "ppc" ] ; then \
			objdir=../linuxppc_obj ; \
			kit=gifav4_LINUXppc.tar.Z; \
                        ddt=gdb; \
		else \
			objdir=../linux_obj ; \
			kit=gifav4_LINUX.tar.Z; \
                        ddt=gdb; \
		fi; \
	elif [ $$archi = "Darwin" ] ; then \
		echo "`uname -s -r`"; \
		objdir=../Darwin_obj; \
		dtt=gdb; \
		kit=gifav4_DARWIN.tar.Z; \
	else echo "Platform unknown" ; \
	fi;


######################################################################

default :
	@echo "  To make Gifa :"
	@echo " "
	@echo "	make gifa       builds Gifa"
	@echo "	make debug      builds a debugging version of Gifa"
	@echo "	make depend	builds the dependence list for the make"
	@echo "	make all_version	builds all sizes of Gifa"
	@echo "	make clean      removes all temporary and *.o files"
	@echo "	make install    installs gifa on this machine"
	@echo " "
	@echo "	make kit_bin    makes a distribution kit for the binaries of the current machine"
	@echo "	make kit_base   makes a distribution kit for generic files"
	@echo "	make kit_data   makes a distribution kit for demo data files"
	@echo "	make doc        makes the help, html and doc files"
	@echo "	make doc_server copies the current macro defs to the server area"
	@echo "	make distrib    copy all the kits and doc into distribution directory"
	@echo "	make backup     builds a gifa.tgz backup file in $(RES_DIR)"
	@echo " "
	@echo "	make run        runs gifa"
	@echo "	make show_doc   builds and shows the programmer documentation"
	@echo "	make test       runs tests on gifa"

depend :
	./makedepend.perl

#build and run gifa
run :
	rm -f $$HOME/gifa.log
	$(ARCHI) \
	$$objdir/gifa

#test Gifa by running the standard test
test :
	$(ARCHI) \
        echo $$objdir ; rm -f $$HOME/gifa.log ; \
	(cd ../test ; echo 'exit n' | ../source/$$objdir/gifa)

#build gifa in obj sub_directory
gifa :
	$(ARCHI) \
	if [ ! -d $$objdir ] ; then \
		mkdir $$objdir; \
	fi; \
	here=`pwd`; cd $$objdir; $(MAKE) VPATH=$$here gifa;


#same as gifa with debug code added
debug :
	$(ARCHI) \
	here=`pwd`; cd $$objdir; $(MAKE) DEBUG=Y VPATH=$$here gifa; \
	rm -f $$HOME/gifa.log; \
	$$ddt ./gifa

#for removing all dirt
clean :
	$(ARCHI) \
	cd $$objdir ; \
	mv get_licence.o get_licence.oo ; \
	rm *.o gifa temp* toto* titi* *~ *.f *.c *.a ; \
	mv get_licence.oo get_licence.o
	-rm -f *.f *.lis *.tar *.log *.out *.o toto* titi* 
	-rm -f *~  ../*~ ../macro/*~ ../test/*~

all_clean : clean
	-for i in $(LIST_DIR); do ( \
		cd ../$$i; rm -f *.lis *.tar *.log *.out; \
		rm -f gifa* temp* toto* titi* *~ *.f *.o *.a); \
	done

#for building libraries
library :
	$(ARCHI) \
	here=`pwd`; cd $$objdir; $(MAKE) VPATH=$$here library;

# to create all size versions in one command
all_version :
	$(ARCHI) \
	echo "Doing big version"; \
	if [ $$objdir != machten_obj ] ; then \
		./big; \
		$(MAKE) gifa; \
		mv $$objdir/gifa $$objdir/gifa_big;  \
	else \
		echo "Skipped"; \
	fi; \
	echo "Doing tiny version"; \
	./tiny; \
	$(MAKE) gifa; \
	mv $$objdir/gifa $$objdir/gifa_tiny; \
	echo "Doing small version"; \
	./small; \
	$(MAKE) gifa; \
	mv $$objdir/gifa $$objdir/gifa_small; \
	echo "Doing norm version"; \
	./norm; \
	$(MAKE) gifa; \
	cp $$objdir/gifa $$objdir/gifa_med

#for installing gifa on that machine
install :
	if [ ! -d /usr/local ] ; then \
		mkdir /usr/local; \
	fi
	if [ ! -d /usr/local/gifa ] ; then \
		ln -s `cd .. ; pwd` /usr/local/gifa; \
	fi
	if [ ! -d /usr/local/bin ] ; then \
		mkdir /usr/local/bin; \
	fi
	$(ARCHI) \
	cp $$objdir/gifa /usr/local/bin/gifa 


#to create a binary distribution kit for each platform

kit_util:
	cp cache_mad.* ../util/cache
	-if [ ! -d ../bin ] ; then mkdir ../bin; fi
	rm -f ../bin/*
	-if [ "`uname`" = "HP-UX" ] ; then \
		cflags="-Aa -D_HPUX_SOURCE"; \
		cc="cc"; \
	elif [ "`uname`" = "IRIX" ] ; then \
		cflags="-DUNDERSCORE"; \
		cc="cc"; \
	elif [ "`uname`" = "IRIX64" ] ; then \
		cflags="-DUNDERSCORE"; \
		cc="cc"; \
	elif [ "`uname`" = "AIX" ] ; then \
		cflags=" "; \
		cc="cc"; \
	elif [ "`uname`" = "SunOS" ] ; then \
		cflags="-DUNDERSCORE"; \
		cc="gcc"; \
	elif [ "`uname`" = "Linux" ] ; then \
		cflags="-DUNDERSCORE -Dis_big_endian"; \
		cc="gcc"; \
	elif [ "`uname`" = "Darwin" ] ; then \
		cflags="-DUNDERSCORE -DDARWIN"; \
		cc="cc -traditional-cpp"; \
	elif [ "`uname`" = "MachTen" ]; then \
		echo "no utilities for MachTen"; \
		exit ; \
	else cflag=""; fi; \
	(cd ../util/ux2cache; $(MAKE) clean; $(MAKE) "CC=$$cc" "LD=$$cc" "CFLAGS=$$cflags" ); \
	cp ../util/ux2cache/ux2cach ../bin

kit_bin: kit_util
	$(ARCHI) \
	cp $$objdir/gifa_* ../bin; \
	chmod +x  ../bin/*; \
	cd .. ; \
	strip bin/* ; \
	tar cvf - bin  | compress  > $(COPY_DIR)/$$kit;

kit_base:
	cp cache_mad.* ../util/cache
	-rm ../doc/prog*
	-rm ../macro/*~ ../macro/#*# ..macro/att ..macro/att#*# /*~ ../test/*~ ../test/#*#
	-rm ../help/*~ ../help/#*#
	(cd .. ; find $(KIT_DIR) bin data -print > List_of_files )
	(cd .. ; tar cvf - $(KIT_DIR) | compress > $(COPY_DIR)/gifav4_basic.tar.Z )
	chmod 644 $(COPY_DIR)/gifav4_basic.tar.Z

kit_data:
	(cd .. ; tar cvf - data | compress > $(COPY_DIR)/gifav4_data.tar.Z )
	chmod 644 $(COPY_DIR)/gifa*.tar.Z

# to make a set of documentation files
doc:
	../com/do_list_html
	chmod -R o+r ../doc/*
	chmod o+r ../help/*

doc_server:
	@if [ "`hostname`" != "tome" ] ; then \
		echo "on Tome Only";exit 1;fi
	mv /system_cbs/home_mosaic/public_html/usr/local/gifa/macro /system_cbs/home_mosaic/public_html/usr/local/gifa/macro_old
	cp -R /usr/local/gifa/macro /system_cbs/home_mosaic/public_html/usr/local/gifa/
	chmod -R o+r /system_cbs/home_mosaic/public_html/usr/local/gifa/

#put all kits into $(DIST_DIR) (needs to be root)
distrib:
	@echo "Needs to be root"
	-mv $(COPY_DIR)/gifav4*.tar.Z  $(DIST_DIR)
	cp ../Makefile $(DIST_DIR)/makefile
	cp ../KNOWN_BUGS ../README ../README.html ../TODO ../History ../Brief_description ../MACHINE_REQUIREMENTS $(DIST_DIR)
	chmod -R o+r $(DIST_DIR)

# automatic doc from the source files.
../doc/programer_doc.html : *.for *.c
	../com_devel/dodoc_html

show_doc : ../doc/programer_doc.html
	netscape ../doc/programer_doc.html &

#to create a compressed archive off-hand, just in case
# RES_DIR is the off directory to put stuff on, SHOULD NOT BE .
backup :
	if [ "$(RES_DIR)" = "." ] ; then echo RES_DIR SHOULD NOT BE . ; exit 1; fi
	if (grep Gifa get*c) then \
		mv get_licence.c $(RES_DIR); \
		cp get_licence.distrib get_licence.c; fi
	(cd ..; tar cvf - $(RES_LIST) | gzip -c > $(RES_DIR)/gifa.temp)
	-mv $(RES_DIR)/gifa.tgz $(RES_DIR)/gifa.tgz.back
	-mv $(RES_DIR)/gifa.temp $(RES_DIR)/gifa.tgz
	-mv $(RES_DIR)/get_licence.c .

size :
	-(cd .. ; du -s $(KIT_DIR) data bin | awk \
		'{ som = som+$$1} \
		END {print "Total", som/2 ,"Ko"}')

# DO NOT DELETE THIS LINE -- make depend depends on it.

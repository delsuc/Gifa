/*
This file is a part of the GIFA program
This software has been developped by the NMR Group in CBS/Montpellier

     Authors :       M.A.Delsuc
                     C.B.S. Fac de Pharmacie
                     34000 Montpellier

This software cannot be used unless the user have been fully
licensed to do so form the above laboratory.
This file cannot be copied, duplicated or used in any other program,
without written permission from the authors.

*/

#if defined(MACHTEN) || defined(SPEC1)
#  define BSD
#else
#  define SYS5
#endif

#include <math.h>
#include <stdio.h>
#include <sys/types.h>
#include <time.h>
#include <sys/times.h>
#include <limits.h>
#include <unistd.h>
#include <string.h>
#include "util.h"
#include "sizebasec.h"


#ifndef NOREADLINE
    extern void rlg_execute();
#   include <readline/readline.h>
#endif

/* global PROMPT variable*/
char cprompt[MAX_CHAR];
/* global GRAPHIC variable*/
extern int versionx;

/**************************************************************/
/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     cconvert_string(st2,st1,l)                                         C
C     Function to convert a C string into a fortran string.              C
C                                                                        C
C     st1 = string to convert. (C)                                       C
C     st2 = converted string.  (fortran)                                 C
C     l = length of st.                                                  C
C                                                                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/
void cconvert_string(st2,st1,l)

#ifdef VMS
/*  ATTENTION   A REECRIRE */
char st2[];
struct dsc$descriptor_s *st1;
int l;
{
   strncpy(st2,st1->dsc$a_pointer,l);
   st2[l] = '\0';
}
#else
char *st1,*st2;
int *l;
{
  *l = strlen(st1);
  strcpy(st2,st1);
}
#endif
/**************************************************************/

/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     convert_string(st2,st1,l)                                          C
C     Function to convert a fortran string into a C string.              C
C                                                                        C
C     st1 = string to convert. (Fortran)                                 C
C     st2 = converted string.  (C)                                       C
C     l = length of st.                                                  C
C                                                                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/

void convert_string(st2,st1,l)

#ifdef VMS
char st2[];
struct dsc$descriptor_s *st1;
int l;
{
   strncpy(st2,st1->dsc$a_pointer,l);
   st2[l] = '\0';
}
#else
char *st1,*st2;
int l;
{
  strncpy(st2,st1,l);
  st2[l] = '\0';
}
#endif

/*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     adjust_cstring(st,l)                                               C
C   Function to delete end blank char end put /0 at the end of C string. C
C                                                                        C
C     st = string to adjust. (C)                                         C
C     l = length of st.                                                  C
C                                                                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*/

/**************************************************************/
void adjust_cstring(st,l) 
char *st;
int l;
{
	int i;
	i = l-1;
	while(st[i] == ' ')i--;
	st[i+1] = '\0';	
}

/**************************************************************/
void PIPE_SYS(stout, lout, stin, lin, err)
/*
   calls the system command held in stin(1..lin) through a pipe
   and returns the result in the stout(1..lout)  
*/

char *stout,*stin;
int *lout, *lin, *err;

{
FILE *PIPE;
char slo[MAX_CHAR], sli[MAX_CHAR], *sd;
  
  convert_string(sli,stin,*lin);
  
  if ( (PIPE = popen(sli,"r")) == NULL ) {
    perror("system error in pipe_syst :");
    *err = 1;
    return;
  }
  if (fgets(slo, MAX_CHAR, PIPE) == NULL) {
    perror("system error in pipe_syst :");
    *err = 1;
    return;
  }
  pclose(PIPE);
  if ( (sd = strchr(slo,'\n')) != NULL ) {  /*pad with blanks */
     while ( sd < (slo+MAX_CHAR) ) {   *sd++ = ' ';  }
  }

  cconvert_string(stout,slo,lout);
}

/**************************************************************/

  static int  nprog,iprog;

void INPROG(i)
int *i;

/*
set of command to show the progress of a long run.
M.A.Delsuc


 will write :
in progress 0%....25%....50%....75%....100%
 
 one dot each 1/20th of the run

initialisation : initinprog(n)
where   int *n   is the total numer of count to go

run :            inprog(i)
where   int *i   is the current index.

inprog(i) can be called as you wish :
     very sparsely or very often, it does not matter.
*/
{

if (versionx == 1){
        x_progress(i);
}
else{
  while ( ((*i*20/nprog) >= iprog ) & (iprog<20) )
  {
    iprog++; 
    switch (iprog)
    { case (5) :
        printf("25%%");  break;
      case(10) :
        printf("50%%");  break;
      case(15) :
        printf("75%%");  break;
      case(20) :
        printf("100%%\n");  break;
      default :
        printf(".");
      }
    fflush(stdout);
  }
}
}

/**************************************************************/

void INITINPROG(n)

int *n;
/*
  initialises for INPROG
*/

{

   nprog = *n;
   iprog = 0;
   if (versionx == 1){
	x_initprogress(n);
   }
   else{
   	printf("In Progress : 0%%");
   	fflush(stdout);
   }
 }


#include <unistd.h>
#include <signal.h>
#ifndef SPEC1 
#include <sys/time.h>
#endif

/**************************************************************/
void do_nothing()
{ }

extern  void WIN_REFRESH();

/**************************************************************/
void CWAITASEC(sec)
float *sec;

/* cwaitasec(sec) will wait take the process to sleep for a time
sec (in second) Called from fortran -waitasec-
*/

#ifdef SYS5
{ int    micro;
  struct itimerval tnew,old;
  struct sigaction snew;

  sigaction(SIGALRM, NULL, &snew);
  
  snew.sa_handler = *do_nothing;

  sigaction(SIGALRM, &snew, NULL);

  micro = 1000000*(*sec);

  tnew.it_value.tv_sec = 0;
  tnew.it_value.tv_usec = micro;
  tnew.it_interval.tv_sec = 0;
  tnew.it_interval.tv_usec = micro;  /* sometimes pause is launched too late */

  setitimer (ITIMER_REAL, &tnew, &old);
  pause();
  
/* disable TIMER */
  tnew.it_value.tv_usec = 0;
  tnew.it_interval.tv_usec = 0; 
  setitimer (ITIMER_REAL, &tnew, &old);

}
#else
 { }
#endif



/**************************************************************/
void GIFAPRINT(st,l)

char *st;
int  *l;

{ char stl[MAX_CHAR];

   convert_string(stl,st,*l);
   printf("%s\n",stl);
   fflush(stdout);

}

/**************************************************************/

void SET_PROMPT(pstring,lg)

/*
   SET_PROMPT can be used to chane the prompt
   can be used also to reinstall the handler
*/

int *lg;
char pstring[];
{
        /* Changement string fortran en C*/
        convert_string(cprompt,pstring,*lg);
#ifndef NOREADLINE
	if (versionx == 1){
            rl_callback_handler_install(cprompt, rlg_execute);
	}
#endif
}

/**************************************************************/
void REM_RL_HANDLER()
/* this is needed because called from Fortran */
{
   if (versionx == 1) {
#ifndef NOREADLINE
	rl_callback_handler_remove();
#endif
   }
}
/**************************************************************/
void SHOWPROMPT(l)

int  *l;

{
  if (strcmp ("", cprompt) == 0) strcpy(cprompt,"gifa>"); 
  if (*l == 0) printf("%s",cprompt);
  if (*l == 1) printf("\\%s",cprompt);

  fflush(stdout);
}

/**************************************************************/
void GIFAFLUSH(unit)

FILE    **unit;

{
  fflush(*unit);
}


/**************************************************************/
/* f2c does not know of chdir */
#ifdef F2C
int chdir_(path,lp)
char *path;
int  *lp;

{ char stl[MAX_CHAR];

   convert_string(stl,path,*lp);
   return(chdir(stl));
}

double dtime_ (float *time) /* f2c does not know dtime */
{
#ifdef SPEC1
#define CLK_TCK 60
#endif
/* this was added on feb 2020 */
#ifndef CLK_TCK
#define CLK_TCK CLOCKS_PER_SEC
#endif

  static float utime=0, stime=0;
  struct tms buffer;

  if (times(&buffer) == -1) return -1;
  time[0] = (float)buffer.tms_utime/CLK_TCK-utime;
  time[1] = (float)buffer.tms_stime/CLK_TCK-stime;
  utime = (float)buffer.tms_utime/CLK_TCK;
  stime = (float)buffer.tms_stime/CLK_TCK;
  return time[0]+time[1];
}
#endif

/**************************************************************/
void GIFA_EXIT(code)

int *code;
{
#ifdef SUN
#ifndef F2C
#  include <sys/ieeefp.h>
          char mode[32], out[32], in[32];
          ieee_flags("clearall",mode, in, &out);
#endif
#endif

  exit(*code);
}
/**************************************************************/
#ifndef NOREADLINE
     /* the following is used for realine input */
#ifdef UNDERSCORE 
#  define  RDLINE rdline_
#else
#ifdef F2C
#  define  RDLINE rdline_
#else
#  define  RDLINE rdline
#endif
#endif

#include <readline/readline.h>
#include <readline/history.h>

void RDLINE(int *lp, char *inp)

{
char lprp[MAX_CHAR];
char *local;
int l,i;

  rl_bind_key('\t',rl_insert);   /* inhibit completion */
  if (*lp == 0) sprintf(lprp,"%s",cprompt);
  if (*lp == 1) sprintf(lprp,"\\%s",cprompt);
  local = readline(lprp);
  if (local && *local) {
    l = strlen(local);
    if (l>255) {
     local[255] = '\0';
     l = 255;
    }
    cconvert_string(inp,local,&l);
    add_history(local);
    free(local);
  } else {
    for (i=0;i<MAX_CHAR;i++) {    inp[i] = ' '; }
  }
}
#endif


#ifdef LINUX
void _Xsetlocale()
{
}
#endif


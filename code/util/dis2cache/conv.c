/***************************************
 * ASPECT to UNIX data convert library *
 ***************************************/
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <math.h>

#include "nmrpar.h"
#include "conv.h"

#define DEBUG 0

/************************************************************
 * Convert 24-bit ASPECT integer into 32-bit native integer *
 ************************************************************/
int conv_integer (void *pntr)
{
  unsigned	par;
  INTEGER	*par3000 = (INTEGER *)pntr;

  par = par3000->l | (par3000->m<<8) | (par3000->h<<16);
  if (par3000->h & 0x80) par |= 0xff000000;		/* negative ? */
  return (int)par;
}

/********************************************************
 * Convert 48-bit ASPECT real into 64-bit native double *
 ********************************************************/
double conv_real (void *pntr)
{
  REAL		*par3000 = (REAL *)pntr;
  double	x1, x2;
  int		m1, m2, s, e;

  m1 = (par3000->ll>>5) | ((par3000->lm<<3)&0xf8) |
       (par3000->hl<<8) |  (par3000->hm<<16)      | (par3000->hh<<24);
  m2 = (par3000->ll)&0x1f;				/* mantissa */
  e  = (par3000->lm>>5) | (par3000->lh<<3);		/* exponent */

  if (m1 < 0)						/* negative ? */
  {
    s  = 1;						/* sign */
    m1 = ~m1;						/* mantissa negation */
    m2 = (~m2&0x1f)+1;
    if (m2 & 0x20)
    {
      m2 &= 0x1f;
      m1 ++;
    }
  }
  else s = 0;

  x1 = ldexp ((double)m1, 5) + ldexp ((double)m2, 0);	/* convert to double */
  x2 = ldexp (x1, e-36);				/* add exponent	     */
  if (s) x2 = -x2;					/* evtl negate	     */

  return x2;
}

/************************************************************
 * Convert 24-bit ASPECT duration into 64-bit native double *
 ************************************************************/
double conv_delay (void *pntr)
{
  int		i, cntr, rate;
  double	delay;
  INTEGER	*par3000 = (INTEGER *)pntr;

  cntr = ((par3000->l>>3)&0x1f) | ((par3000->m<<5)&0xfc0);	/* counter   */
  rate =   par3000->l&0x7;					/* time base */

  delay = (double)cntr;
  cntr  = 1;
  for (i=7; i>rate; i--) cntr *= 10;
  delay /= cntr;

  return delay;
}

/************************************************
 * Convert 4 ASPECT sixbits into 3 native ASCII *
 ************************************************/
void conv_sixbit (char *nam, char *nam3000)
{
  int i;

  nam[0] =  (nam3000[0]>>2)&0x3f;			/* parsing */
  nam[1] = ((nam3000[0]<<4)&0x30)|((nam3000[1]>>4)&0xf);
  nam[2] = ((nam3000[1]<<2)&0x3c)|((nam3000[2]>>6)&0x3);
  nam[3] =  (nam3000[2])&0x3f;

  for (i=0; i<4; i++)					/* converting */
  {
    if (nam[i] > 0 && nam[i] < 040) nam[i] += 0100;
    else if (nam[i] == 040) nam[i] = 0100;
  }
  nam[4] = '\0';
}

/************************************************************
 * Convert ASPECT mark parity ASCII into space parity ASCII *
 ************************************************************/
void conv_ascii (char *nam, char *nam3000, int len)
{
  int i;

  for (i=0; i<len; i++) nam[i] = nam3000[i]&0x7f;
  nam[len] = '\0';
}

/********************************************************
 * Convert 3-word ASPECT filename into intuitive format *
 ********************************************************/
void conv_filename (char *nam, char *nam3000)
{
  char nam2[5], ext[5];

  conv_sixbit (nam,  nam3000);
  conv_sixbit (nam2, nam3000+3);
  conv_sixbit (ext,  nam3000+6);

  strcat (nam, nam2);
  if (ext[0] != '\0')
  {
    strcat (nam, ".");
    strcat (nam, ext);
  }
}

/*******************************************************************
 * Convert 24-bit BCD ASPECT date and time into UNIX native time_t *
 *******************************************************************/
time_t conv_time (void *pntr1, void *pntr2)
{
  INTEGER	*par1 = (INTEGER *)pntr1;		/* date */
  INTEGER	*par2 = (INTEGER *)pntr2;		/* time */
  struct tm	tmstr;

  tmstr.tm_hour  = (par2->h>>4)*10+(par2->h&0xf);
  tmstr.tm_min   = (par2->m>>4)*10+(par2->m&0xf);
  tmstr.tm_sec   = (par2->l>>4)*10+(par2->l&0xf);
  tmstr.tm_mday  = (par1->h>>4)*10+(par1->h&0xf);
  tmstr.tm_mon   = (par1->m>>4)*10+(par1->m&0xf)-1;
  tmstr.tm_year  = (par1->l>>4)*10+(par1->l&0xf);
  tmstr.tm_wday  = 0;
  tmstr.tm_yday  = 0;
  tmstr.tm_isdst = -1;

  return mktime (&tmstr);
}
/*************************
 * E N D   O F   F I L E *
 *************************/

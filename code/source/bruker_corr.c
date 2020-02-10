/*
brukerphase.c

This file contains bruker specific stuff, as digital filter correction
DSPFIRM is named form bruker document, however, it seems it is called dspfvs in the header bruker files (acqu)
It is thus called accordingly in the Gifa User Interface.

version 1.0
10 oct 2001 - M.A.D.
*/

#define N_DSPFIRM 6         /* nb of valid DSPFIRM values : 10..15 */
#define N_DECIM  21         /* nb of valid DECIM values */

/* this table holds the delay values. -1 indicates an invalid entry */
static int delay[N_DSPFIRM][N_DECIM] = 
{
{ 179,201,533,709,1097,1449,2225,2929,4481,5889,8993,11809,18017,23649,36065,47329,72161, 94689,144353,189409,288737 },
{ 184,219,384,602, 852,1668,2312,3368,4656,6768,9344,13568,18560,27392,36992,50040,73856,110336,147584,220928,295040 },
{ 184,219,384,602, 852,1668,2292,3369,4616,6768,9264,13568,18560,27392,36992,50040,73856,110336,147584,220928,295040 },
{  11, 17, 23, 35,  47,  71,  95, 143, 191, 287, 383,  575,   -1,   -1,   -1,   -1,   -1,    -1,    -1,    -1,    -1 },
{  60, 90,118,179, 244, 360, 492, 724, 980,1444,1958, 2886, 3912, 5768, 7820,11532,   -1,    -1,    -1,    -1,    -1 },
{  -1, -1, 58,152, 202, 318, 418, 642, 842,1290,1690, 2586, 3386,   -1,   -1,   -1,   -1,    -1,    -1,    -1,    -1 }
};

/* this one contains valid DECIM values */
static int decim_offset[N_DECIM] =
{ 2,3,4,6,8,12,16,24,32,48,64,96,128,192,256,384,512,768,1024,1536,2048 };



/**********************************************************************/
#ifdef UNDERSCORE 
#  define  BRUKERPHASE brukerphase_
#else
#ifdef F2C
#  define  BRUKERPHASE brukerphase_
#else
#  define  BRUKERPHASE brukerphase
#endif
#endif

float BRUKERPHASE(int *dspfirm, int *dspfvs,int *decim, float *sw)
/*
The brukerphase function computes the correction for the Bruker figital filter.
the phase correction to apply is computed given the 3 parameters :
DSPFIRM DSPFVS DECIM
as found in the acqus parameter file in XwinNMR

dspfvs is not used so far

syntax compatible with F77 calling conventions

version 1.0
10 oct 2001 - M.A.D.
*/

{
int i,j,d;
float z;

/*** uncomment following code to check the array **
for (i=0;i<N_DECIM;i++) {
    printf("%5d : ",decim_offset[i]);
    for (j=0;j<N_DSPFIRM;j++) {
        printf("%7d",delay[j][i]);
        }
    printf("\n");
}
***/

/* first special cases */
    if (*dspfirm == 15 && *decim == 3 && *sw >= 104000.) {
        z = (-110.0*180.0)/(*decim);
        return(z);
    }
    if (*dspfirm == 0 || *decim == 1) {
        return(0.0);
    }
/* otherwise, First determines DECIM offset */
    j = -1;
    for (i=0; i<N_DECIM; i++) {
        if (decim_offset[i] == *decim)
            { j=i; break; }
    }
    if (j == -1) { 
        printf("*** wrong decim %d\n",*decim);
        return(-1.0);
        }

/* then get delay value */
    if (*dspfirm < 10 || *dspfirm > (N_DSPFIRM+9)) {
        printf("*** wrong dspfirm %d\n",*dspfirm);
        return(-1.0);
        }
    d = -delay[(*dspfirm)-10][j];
    if (d == 1) {
        printf("*** wrong parameter combination\n");
        return(-1.0);
        }
    z = ((float) d*180.00)/((float) *decim);
/*    printf("#decim = %d   delay = %d   phase = %f\n",j,d,z);  */
    return(z);
}

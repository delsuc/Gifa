
/* nmri.c */
/*
 * nmri.c is a librairy that makes life easier (I hope) when dealing 
 * with the cache_mad interface. 
 * It handles file opening, closing, reading and writing for 1D, 2D, and 3D
 * data sets. It frees the user from errors checking.
 * It provides functions for 1D, 2D, and 3D buffers allocations where
 * indexes may start from 0 or 1.
 */

#include "cache_mad.h"
#include <stdio.h>
#include <stdlib.h>
#include "nmri.h"

static nmri_initialized = 0 ; /* true if the cache librairy is initialized */
int nmrstatus = 0 ;     /* the error indicator of the cache librairy */

static int len ;        /* current string length */
static int htype ;      /* data type */
static char *pn ;       /* parameter name */

static void nmri_initial() ;
static void params(NMRI *pNMRI) ;

static void nmri_initial()
{
        if(!nmri_initialized) {
                CH_INITIALISE() ;
                if(nmrstatus) {
                        fprintf(stderr, 
"nmri_initial: unable to initialize cache system - error %d.\n", nmrstatus) ;
                        exit(1) ;
                }
                nmri_initialized = 1 ;
        }
}

static void params(NMRI *pNMRI)
{
float z;
        CH_GTPARBAS(
                &pNMRI->l_handle, 
                &pNMRI->l_dim, 
                &pNMRI->l_sz1, 
                &pNMRI->l_sz2, 
                &pNMRI->l_sz3, 
                &pNMRI->l_type, 
                &pNMRI->l_sw1, 
                &pNMRI->l_sw2, 
                &pNMRI->l_sw3, 
                &pNMRI->l_freq, 
                &pNMRI->l_fr1, 
                &pNMRI->l_fr2, 
                &pNMRI->l_fr3, 
                &pNMRI->l_off1, 
                &pNMRI->l_off2, 
                &pNMRI->l_off3, 
		&z,&z,&z,
                &pNMRI->l_max0) ;
}


NMRI *open_nD(char *fn, int mode) 
{
        NMRI *pNMRI ;

        nmri_initial() ;
        pNMRI = (NMRI *)malloc(sizeof(NMRI)) ;
        if(pNMRI == NULL) {
                fprintf(stderr, 
                        "open_nD: unable to create NMRinfo structure\n") ;
                exit(1) ;
        }

        pNMRI->l_fname = fn ;
        pNMRI->l_fnamelen = strlen(fn) ;
        pNMRI->l_fmode = mode ;
        CH_OPEN(&pNMRI->l_handle, 
                pNMRI->l_fname, 
                &pNMRI->l_fnamelen, 
                &pNMRI->l_fmode, 
                &nmrstatus) ;
/* fprintf(stderr, "open_nD: returns status %d for %s.\n", 
        nmrstatus, pNMRI->l_fname) ; */
        if((nmrstatus == COULD_NOT_OPEN) && (mode == CF_WRITE)) {
/* fprintf(stderr, "open_nD: creating %s.\n", pNMRI->l_fname) ; */
                nmrstatus = 0 ;
                CH_CREATE(&pNMRI->l_handle, 
                        pNMRI->l_fname, 
                        &pNMRI->l_fnamelen, 
                        &pNMRI->l_fmode, 
                        &nmrstatus) ;
                if(nmrstatus) {
                        fprintf(stderr, "open_nD: unable to create %s. error %d\n", 
                                fn, nmrstatus) ;
                        exit(1) ;
                }       
        } else if(!nmrstatus) {
                params(pNMRI) ;
        } else {
                fprintf(stderr, "open_nD: unable to open %s. error %d\n", 
                        fn, nmrstatus) ;
                exit(1) ;
        }
        return pNMRI ;
}

void w_basepar(NMRI *pNMRI, int idim, int isz1, int isz2, int isz3)
{
        pNMRI->l_dim = idim ;
        pNMRI->l_sz1 = isz1 ;
        pNMRI->l_sz2 = isz2 ;
        pNMRI->l_sz3 = isz3 ;
        CH_SETUP(
                &pNMRI->l_handle, 
                &pNMRI->l_dim, 
                &pNMRI->l_sz1, 
                &pNMRI->l_sz2, 
                &pNMRI->l_sz3, 
                &nmrstatus) ; 
        if(nmrstatus) {
                fprintf(stderr, 
                        "w_basepar: unable to set-up file %s.\n", pNMRI->l_fname) ;
                exit(1) ;
        }
}

void r_allpar(NMRI *pNMRI, int *dim, int *sz1, int *sz2, int *sz3, int *type, 
        float *sw1, float *sw2, float *sw3,
        float *freq, float *fr1, float *fr2, float *fr3, 
        float *off1, float *off2, float *off3, float *max0)
{
        *dim = pNMRI->l_dim ;
        *sz1 = pNMRI->l_sz1 ;
        *sz2 = pNMRI->l_sz2 ;
        *sz3 = pNMRI->l_sz3 ;
        *type = pNMRI->l_type ;
        *sw1 = pNMRI->l_sw1 ;
        *sw2 = pNMRI->l_sw2 ;
        *sw3 = pNMRI->l_sw3 ;
        *freq = pNMRI->l_freq ;
        *fr1 = pNMRI->l_fr1 ;
        *fr2 = pNMRI->l_fr2 ;
        *fr3 = pNMRI->l_fr3 ;
        *off1 = pNMRI->l_off1 ;
        *off2 = pNMRI->l_off2 ;
        *off3 = pNMRI->l_off3 ;
        *max0 = pNMRI->l_max0 ;
}

int r_dim(NMRI *pNMRI)
{
        return  pNMRI->l_dim ;
}

int r_sz1(NMRI *pNMRI)
{
        return  pNMRI->l_sz1 ;
}

int r_sz2(NMRI *pNMRI)
{
        return  pNMRI->l_sz2 ;
}

int r_sz3(NMRI *pNMRI)
{
        return  pNMRI->l_sz3 ;
}

int r_type(NMRI *pNMRI)
{
        return  pNMRI->l_type ;
}

float r_sw1(NMRI *pNMRI)
{
        return  pNMRI->l_sw1 ;
}

float r_sw2(NMRI *pNMRI)
{
        return  pNMRI->l_sw2 ;
}

float r_sw3(NMRI *pNMRI)
{
        return  pNMRI->l_sw3 ;
}

float r_freq(NMRI *pNMRI)
{
        return  pNMRI->l_freq ;
}

float r_fr1(NMRI *pNMRI)
{
        return  pNMRI->l_fr1 ;
}

float r_fr2(NMRI *pNMRI)
{
        return  pNMRI->l_fr2 ;
}

float r_fr3(NMRI *pNMRI)
{
        return  pNMRI->l_fr3 ;
}

float r_off1(NMRI *pNMRI)
{
        return  pNMRI->l_off1 ;
}

float r_off2(NMRI *pNMRI)
{
        return  pNMRI->l_off2 ;
}

float r_off3(NMRI *pNMRI)
{
        return  pNMRI->l_off3 ;
}

float r_max0(NMRI *pNMRI)
{
        return  pNMRI->l_max0 ;
}


void w_type(NMRI *pNMRI, int type)
{
        pNMRI->l_type = type ;
        pn = "Type" ;
        len = strlen(pn) ;
        htype = H_INT ;
        CH_PUTPARAM_TYPE
        (&pNMRI->l_handle, (void *)&pNMRI->l_type, pn, &len, &htype, &nmrstatus) ;
        if(nmrstatus) {
                fprintf(stderr, "unable to put parameter: type\n") ;
                exit(1) ;
        }
}

void w_sw1(NMRI *pNMRI, float sw1)
{
        pNMRI->l_sw1 = sw1 ;
        pn = "Specw1" ;
        len = strlen(pn) ;
        htype = H_FLOAT ;
        CH_PUTPARAM_TYPE
        (&pNMRI->l_handle, (void *)&pNMRI->l_sw1, pn, &len, &htype, &nmrstatus) ;
        if(nmrstatus) {
                fprintf(stderr, "unable to put parameter: sw1\n") ;
                exit(1) ;
        }
}

void w_sw2(NMRI *pNMRI, float sw2)
{
        pNMRI->l_sw2 = sw2 ;
        pn = "Specw2" ;
        len = strlen(pn) ;
        htype = H_FLOAT ;
        CH_PUTPARAM_TYPE
        (&pNMRI->l_handle, (void *)&pNMRI->l_sw2, pn, &len, &htype, &nmrstatus) ;
        if(nmrstatus) {
                fprintf(stderr, "unable to put parameter: sw2\n") ;
                exit(1) ;
        }
}

void w_sw3(NMRI *pNMRI, float sw3)
{       
        pNMRI->l_sw3 = sw3 ;
        pn = "Specw3" ;
        len = strlen(pn) ;
        htype = H_FLOAT ;
        CH_PUTPARAM_TYPE
        (&pNMRI->l_handle, (void *)&pNMRI->l_sw3, pn, &len, &htype, &nmrstatus) ;
        if(nmrstatus) {
                fprintf(stderr, "unable to put parameter: sw3\n") ;
                exit(1) ;
        }
}

void w_freq(NMRI *pNMRI, float freq)
{
        pNMRI->l_freq = freq ;
        pn = "Frequency" ;
        len = strlen(pn) ;
        htype = H_FLOAT ;
        CH_PUTPARAM_TYPE
        (&pNMRI->l_handle, (void *)&pNMRI->l_freq, pn, &len, &htype, &nmrstatus) ;
        if(nmrstatus) {
                fprintf(stderr, "unable to put parameter: freq\n") ;
                exit(1) ;
        }
}

void w_fr1(NMRI *pNMRI, float fr1)
{
        pNMRI->l_fr1 = fr1 ;
        pn = "Freq1" ;
        len = strlen(pn) ;
        htype = H_FLOAT ;
        CH_PUTPARAM_TYPE
        (&pNMRI->l_handle, (void *)&pNMRI->l_fr1, pn, &len, &htype, &nmrstatus) ;
        if(nmrstatus) {
                fprintf(stderr, "unable to put parameter: fr1\n") ;
                exit(1) ;
        }
}

void w_fr2(NMRI *pNMRI, float fr2)
{
        pNMRI->l_fr2 = fr2 ;
        pn = "Freq2" ;
        len = strlen(pn) ;
        htype = H_FLOAT ;
        CH_PUTPARAM_TYPE
        (&pNMRI->l_handle, (void *)&pNMRI->l_fr2, pn, &len, &htype, &nmrstatus) ;
        if(nmrstatus) {
                fprintf(stderr, "unable to put parameter: fr2\n") ;
                exit(1) ;
        }
}

void w_fr3(NMRI *pNMRI, float fr3)
{
        pNMRI->l_fr3 = fr3 ;
        pn = "Freq3" ;
        len = strlen(pn) ;
        htype = H_FLOAT ;
        CH_PUTPARAM_TYPE
        (&pNMRI->l_handle, (void *)&pNMRI->l_fr3, pn, &len, &htype, &nmrstatus) ;
        if(nmrstatus) {
                fprintf(stderr, "unable to put parameter: fr3\n") ;
                exit(1) ;
        }
}

void w_off1(NMRI *pNMRI, float off1)
{
        pNMRI->l_off1 = off1 ;
        pn = "Offset1" ;
        len = strlen(pn) ;
        htype = H_FLOAT ;
        CH_PUTPARAM_TYPE
        (&pNMRI->l_handle, (void *)&pNMRI->l_off1, pn, &len, &htype, &nmrstatus) ;
        if(nmrstatus) {
                fprintf(stderr, "unable to put parameter: off1\n") ;
                exit(1) ;
        }
}

void w_off2(NMRI *pNMRI, float off2)
{
        pNMRI->l_off2 = off2 ;
        pn = "Offset2" ;
        len = strlen(pn) ;
        htype = H_FLOAT ;
        CH_PUTPARAM_TYPE
        (&pNMRI->l_handle, (void *)&pNMRI->l_off2, pn, &len, &htype, &nmrstatus) ;
        if(nmrstatus) {
                fprintf(stderr, "unable to put parameter: off2\n") ;
                exit(1) ;
        }
}

void w_off3(NMRI *pNMRI, float off3)
{
        pNMRI->l_off3 = off3 ;
        pn = "Offset3" ;
        len = strlen(pn) ;
        htype = H_FLOAT ;
        CH_PUTPARAM_TYPE
        (&pNMRI->l_handle, (void *)&pNMRI->l_off3, pn, &len, &htype, &nmrstatus) ;
        if(nmrstatus) {
                fprintf(stderr, "unable to put parameter: off3\n") ;
                exit(1) ;
        }
}

void w_max0(NMRI *pNMRI, float max0)
{
        pNMRI->l_max0 = max0 ;
        pn = "Absmax" ;
        len = strlen(pn) ;
        htype = H_FLOAT ;
        CH_PUTPARAM_TYPE
        (&pNMRI->l_handle, (void *)&pNMRI->l_max0, pn, &len, &htype, &nmrstatus) ;
        if(nmrstatus) {
                fprintf(stderr, "unable to put parameter: max0\n") ;
                exit(1) ;
        }
}

void w_allpar(NMRI *pNMRI, int type, float sw1, float sw2, float sw3,
        float freq, float fr1, float fr2, float fr3,
        float off1, float off2, float off3, float max0) 
{
        w_type(pNMRI, type) ;
        w_sw1(pNMRI, sw1) ;
        w_sw2(pNMRI, sw2) ;
        w_sw3(pNMRI, sw3) ;
        w_freq(pNMRI, freq) ;
        w_fr1(pNMRI, fr1) ;
        w_fr2(pNMRI, fr2) ;
        w_fr3(pNMRI, fr3) ;
        w_off1(pNMRI, off1) ;
        w_off2(pNMRI, off2) ;
        w_off3(pNMRI, off3) ;
        w_max0(pNMRI, max0) ;
}


void close_nD(NMRI *pNMRI) 
{
        CH_CLOSE(&pNMRI->l_handle, &nmrstatus) ;
        if(nmrstatus) {
                fprintf(stderr, "unable to close file: %s.\n", pNMRI->l_fname) ;
                exit(1) ;
        }
}

void r_1Ddata(float *buf, int i0, NMRI *pNMRI, int left, int right)
{
        int bmod = CB_READ ;
        int l, r ;

        l = left ;
        r = right ;
        CH_AC1DAREA(&buf[i0], &pNMRI->l_handle, &l, &r, &bmod, &nmrstatus) ;
        if(nmrstatus) {
                fprintf(stderr, "r_1Ddata: unable to read 1D data from: \
%s - error %d.\n", pNMRI->l_fname, nmrstatus) ;
                exit(1) ;
        }
}

void w_1Ddata(float *buf, int i0, NMRI *pNMRI, int left, int right)
{
        int bmod = CB_WRITE ;
        int l, r ;

        l = left ;
        r = right ;
        CH_AC1DAREA(&buf[i0], &pNMRI->l_handle, &l, &r, &bmod, &nmrstatus) ;
        if(nmrstatus) {
                fprintf(stderr, "w_1Ddata: unable to write 1D data to: \
%s - error %d.\n", pNMRI->l_fname, nmrstatus) ;
                exit(1) ;
        }
}

void r_2Ddata(float **buf, int i0, NMRI *pNMRI, int down, int left, int up, int right)
{
        int bmod = CB_READ ;
        int l, r ;
        int u, d ;

        l = left ;
        r = right ;
        d = down ;
        u = up ;
        CH_AC2DAREA(&buf[i0][i0], &pNMRI->l_handle, &d, &l, &u, &r, &bmod, &nmrstatus) ;
        if(nmrstatus) {
                fprintf(stderr, "r_2Ddata: unable to read 2D data from: \
%s - error %d.\n", pNMRI->l_fname, nmrstatus) ;
                exit(1) ;
        }
}

void w_2Ddata(float **buf, int i0, NMRI *pNMRI, int down, int left, int up, int right)
{
        int bmod = CB_WRITE ;
        int l, r ;
        int u, d ;

        l = left ;
        r = right ;
        d = down ;
        u = up ;
        CH_AC2DAREA(&buf[i0][i0], &pNMRI->l_handle, &d, &l, &u, &r, &bmod, &nmrstatus) ;
        if(nmrstatus) {
                fprintf(stderr, "w_2Ddata: unable to write 2D data to: \
%s - error %d.\n", pNMRI->l_fname, nmrstatus) ;
                exit(1) ;
        }
}

void r_3Ddata(float ***buf, int i0, NMRI *pNMRI, int back, int down, int left, 
                int front, int up, int right)
{
        int bmod = CB_READ ;
        int l, r ;
        int u, d ;
        int b, f ;

        l = left ;
        r = right ;
        d = down ;
        u = up ;
        b = back ;
        f = front ;
        CH_AC3DAREA(&buf[i0][i0][i0], &pNMRI->l_handle, 
                &b, &d, &l, &f, &u, &r, &bmod, &nmrstatus) ;
        if(nmrstatus) {
                fprintf(stderr, "r_3Ddata: unable to read 3D data from: \
%s - error %d.\n", pNMRI->l_fname, nmrstatus) ;
                exit(1) ;
        }
}

void w_3Ddata(float ***buf, int i0, NMRI *pNMRI, int back, int down, int left, 
                int front, int up, int right)
{
        int bmod = CB_READ ;
        int l, r ;
        int u, d ;
        int b, f ;

        l = left ;
        r = right ;
        d = down ;
        u = up ;
        b = back ;
        f = front ;
        CH_AC3DAREA(&buf[i0][i0][i0], &pNMRI->l_handle, 
                &b, &d, &l, &f, &u, &r, &bmod, &nmrstatus) ;
        if(nmrstatus) {
                fprintf(stderr, "w_3Ddata: unable to write 3D data to: \
%s - error %d.\n", pNMRI->l_fname, nmrstatus) ;
                exit(1) ;
        }
}

float *alloc_1D(int i0, int sz1)
{
        float *v ;

        v = (float *)malloc((size_t) ((sz1+1)*sizeof(float))) ;
        if(v == NULL) {
                fprintf(stderr, "alloc_1D: failed\n") ;
                exit(1) ;       
        }
        return v + 1 - i0 ;
}

float **alloc_2D(int i0, int sz1, int sz2) 
{
        int i ;
        float **m ;

        m = (float **)malloc((size_t) ((sz1+1)*sizeof(float *))) ;
        if(m == NULL) {
                fprintf(stderr, "alloc_2D: failed (1)\n") ;
                exit(1) ;       
        }
        m = m + 1 - i0 ;
        m[i0] = (float *)malloc((size_t) ((sz1*sz2+1)*sizeof(float))) ;
        if(m[i0] == NULL) {
                fprintf(stderr, "alloc_2D: failed (2)\n") ;
                exit(1) ;       
        }
        m[i0] = m[i0] + 1 - i0 ;
        for(i=i0+1 ; i<=(sz1+i0-1) ; i++) {
                m[i] = m[i-1] + sz2 ;
        }
        return m ;
}

float ***alloc_3D(int i0, int sz1, int sz2, int sz3) 
{
        int i, j ;
        float ***t ;

        t = (float ***)malloc((size_t) ((sz1+1)*sizeof(float **))) ;
        if(t == NULL) {
                fprintf(stderr, "alloc_3D: failed (1)\n") ;
                exit(1) ;       
        }
        t = t + 1 - i0 ;
        t[i0] = (float **)malloc((size_t) ((sz1*sz2+1)*sizeof(float *))) ;
        if(t[i0] == NULL) {
                fprintf(stderr, "alloc_3D: failed (2)\n") ;
                exit(1) ;       
        }
        t[i0] = t[i0] + 1 - i0 ;
        t[i0][i0] = (float *)malloc((size_t) ((sz1*sz2*sz3+1)*sizeof(float ))) ;
        if(t[i0][i0] == NULL) {
                fprintf(stderr, "alloc_3D: failed (3)\n") ;
                exit(1) ;       
        }
        t[i0][i0] = t[i0][i0] + 1 - i0 ;
        for(j=i0+1 ; j<=(sz2+i0-1) ; j++) {
                t[i0][j] = t[i0][j-1] + sz3 ;
        }
        for(i=i0+1 ; i<=(sz1+i0-1) ; i++) {
                t[i] = t[i-1] + sz2 ;
                t[i][i0] = t[i-1][i0] + sz2*sz3 ;
                for(j=i0+1 ; j<=(sz2+i0-1) ; j++) {
                        t[i][j] = t[i][j-1] + sz3 ;
                }       
        }
}



/* np2cmpx.c */
/* cc np2cmpx.c -lnmri -lcache -lm -o np2cmpx */
/* 
        np2cmpx converts a GIFA cache file recorded in N+P mode 
        to hypercomplex data so that "revf f1 ft f1" processes
        data correctly. This avoids the ft_n+p macro that is
        lengthy and at the end of which no phase correction is possible.

        np2cmpx : converts the data file named "data"
        np2cmpx fn1 : does the same on the file named fn1
        np2cmpx fn1 fn2 : converts fn1 to fn2. fn1 is left unchanged.
*/

#include "cache_mad.h"
#include "nmri.h"
#include <stdio.h>

main(int argc, char **argv)
{
        NMRI *spec ;
        int dim, sz1, sz2, sz3, itype ;
        float freq, fr1, fr2, fr3, sw1, sw2, sw3, off1, off2, off3, max0 ;
        float **buf2D, tmpr, tmpi ;
        int i, j ;
        char *fn1, *fn2 ;

/* looking for filenames */

        switch(argc) {
        case 1 :
                fn1 = fn2 = "data" ;
                break ;
        case 2 :
                fn1 = fn2 = argv[1] ;
                break ;
        case 3 :
                fn1 = argv[1] ;
                fn2 = argv[2] ;
                break ;
        default :
                fprintf(stderr, "np2cmpx: usage: np2cmpx [fn1 [fn2]]\n") ;
                exit(1) ;
                break ;
        }

/* opening input file */

        spec = open_nD(fn1, R) ;
        r_allpar(spec, &dim, &sz1, &sz2, &sz3, &itype,
                &sw1, &sw2, &sw3,
                &freq, &fr1, &fr2, &fr3,
                &off1, &off2, &off3, &max0) ;

/* checking parameters */

        if(dim != 2) {
                fprintf(stderr, "np2cmpx: data should be 2D.\n") ;
                exit(1) ;
        }
        if(itype != 1) {
                fprintf(stderr, "np2cmpx: itype should be 1\n") ;
                exit(1) ;
        }

/* reading data, closing file */

        buf2D = alloc_2D(1, sz1, sz2) ;
        r_2Ddata(buf2D, 1, spec, 1, 1, sz1, sz2) ;
        close_nD(spec) ;

/* computing amplitude modulations from phase modulations */
/* cos(a) = (exp(ia) + exp(-ia))/2 */
/* sin(a) = (exp(ia) - exp(-ia))/2i */

        for(i=1 ; i<=sz1 ; i+=2) {
                for(j=1 ; j<=sz2 ; j+=2) {
                        tmpi = 0.5 * (buf2D[i][j] - buf2D[i+1][j]) ;
                        tmpr = -0.5 * (buf2D[i][j+1] - buf2D[i+1][j+1]) ;
                        buf2D[i][j] = 0.5 * (buf2D[i][j] + buf2D[i+1][j]) ;
                        buf2D[i][j+1] = 0.5 * (buf2D[i][j+1] + buf2D[i+1][j+1]) ;
                        buf2D[i+1][j] = tmpr ;
                        buf2D[i+1][j+1] = tmpi ;
                }
        }

/* writing the result */

        spec = open_nD(fn2, W) ;
        w_basepar(spec, dim, sz1, sz2, sz3) ;
        w_allpar(spec, 3, sw1, sw2, sw3, freq, fr1, fr2, fr3, 
                off1, off2, off3, max0) ;
        w_2Ddata(buf2D, 1, spec, 1, 1, sz1, sz2) ;
        close_nD(spec) ;
        exit(0) ;
}


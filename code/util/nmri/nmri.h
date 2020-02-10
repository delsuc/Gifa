/* nmri.h */

typedef struct nmrinmri
{
        DATASET *l_handle ;
        char *l_fname ;
        int l_fnamelen ;
        int l_fmode ;
        int l_dim ;
        int l_sz1 ;
        int l_sz2 ;
        int l_sz3 ;
        int l_type ;
        float l_sw1 ;
        float l_sw2 ;
        float l_sw3 ;
        float l_freq ;
        float l_fr1 ;
        float l_fr2 ;
        float l_fr3 ;
        float l_off1 ;
        float l_off2 ;
        float l_off3 ;
        float l_max0 ;
} NMRI ;

#define R CF_READ
#define W CF_WRITE
#define RW CF_READ_WRITE

NMRI *open_nD(char *fn, int mode) ;
void r_allpar(NMRI *pNMRI, int *dim, int *sz1, int *sz2, int *sz3, int *type, 
        float *sw1, float *sw2, float *sw3,
        float *freq, float *fr1, float *fr2, float *fr3, 
        float *off1, float *off2, float *off3, float *max0) ;
int r_dim(NMRI *pNMRI) ;
int r_sz1(NMRI *pNMRI) ;
int r_sz2(NMRI *pNMRI) ;
int r_sz3(NMRI *pNMRI) ;
int r_type(NMRI *pNMRI) ;
float r_sw1(NMRI *pNMRI) ;
float r_sw2(NMRI *pNMRI) ;
float r_sw3(NMRI *pNMRI) ;
float r_freq(NMRI *pNMRI) ;
float r_fr1(NMRI *pNMRI) ;
float r_fr2(NMRI *pNMRI) ;
float r_fr3(NMRI *pNMRI) ;
float r_off1(NMRI *pNMRI) ;
float r_off2(NMRI *pNMRI) ;
float r_off3(NMRI *pNMRI) ;
float r_max0(NMRI *pNMRI) ;
void r_1Ddata(float *buf, int i0, NMRI *pNMRI, int left, int right) ;
void r_2Ddata(float **buf, int i0, NMRI *pNMRI, int down, int left, int up, int right) ;
void r_3Ddata(float ***buf, int i0, NMRI *pNMRI, int back, int down, int left, 
                int front, int up, int right) ;
void w_basepar(NMRI *pNMRI, int idim, int isz1, int isz2, int isz3) ;
void w_allpar(NMRI *pNMRI, int type, float sw1, float sw2, float sw3,
        float freq, float fr1, float fr2, float fr3,
        float off1, float off2, float off3, float max0) ;
void w_type(NMRI *pNMRI, int type) ;
void w_sw1(NMRI *pNMRI, float sw1) ;
void w_sw2(NMRI *pNMRI, float sw2) ;
void w_sw3(NMRI *pNMRI, float sw3) ;
void w_freq(NMRI *pNMRI, float freq) ;
void w_fr1(NMRI *pNMRI, float fr1) ;
void w_fr2(NMRI *pNMRI, float fr2) ;
void w_fr3(NMRI *pNMRI, float fr3) ;
void w_off1(NMRI *pNMRI, float off1) ;
void w_off2(NMRI *pNMRI, float off2) ;
void w_off3(NMRI *pNMRI, float off3) ;
void w_max0(NMRI *pNMRI, float max0) ;
void w_1Ddata(float *buf, int i0, NMRI *pNMRI, int left, int right) ;
void w_2Ddata(float **buf, int i0, NMRI *pNMRI, int down, int left, int up, int right) ;
void w_3Ddata(float ***buf, int i0, NMRI *pNMRI, int back, int down, int left, 
                int front, int up, int right) ;
void close_nD(NMRI *pNMRI) ;
float *alloc_1D(int i0, int sz1) ;
float **alloc_2D(int i0, int sz1, int sz2) ;
float ***alloc_3D(int i0, int sz1, int sz2, int sz3) ;



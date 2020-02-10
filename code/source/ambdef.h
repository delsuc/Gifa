typedef struct PIXEL
{
    struct PIXEL *nxt;    
    int dx;
    int dy;
} PIXEL;

typedef struct AMB
{
    struct AMB *nxt;    
    struct PIXEL *pix;    
    int indpk;
    int pkx;
    int pky;
} AMB;

/* This include file is used to defined name of C function
  depending on the UNDERSCORE state */

#ifdef UNDERSCORE

#define CREAT_AMB creat_amb_
#define CHECK_AMB check_amb_
#define INI_AMB ini_amb_
#define DEL_AMB del_amb_
#define SUM_AMB sum_amb_
#define GET_AMB get_amb_
#define PUT_AMB put_amb_
#define PUT_PIX_AMB put_pix_amb_
#define CHECK_PIX_AMB check_pix_amb_
#define REM_PIX_AMB rem_pix_amb_

#else
#ifdef F2C

#define CREAT_AMB creat_amb__
#define CHECK_AMB check_amb__
#define INI_AMB ini_amb__
#define DEL_AMB del_amb__
#define SUM_AMB sum_amb__
#define GET_AMB get_amb__
#define PUT_AMB put_amb__
#define PUT_PIX_AMB put_pix_amb__
#define CHECK_PIX_AMB check_pix_amb__
#define REM_PIX_AMB rem_pix_amb__

#else

#define CREAT_AMB creat_amb
#define CHECK_AMB check_amb
#define INI_AMB ini_amb
#define DEL_AMB del_amb
#define SUM_AMB sum_amb
#define GET_AMB get_amb
#define PUT_AMB put_amb
#define PUT_PIX_AMB put_pix_amb
#define CHECK_PIX_AMB check_pix_amb
#define REM_PIX_AMB rem_pix_amb

#endif
#endif

/*************************************************/
/* prototype */
void CREAT_AMB(int *error);
void CHECK_AMB(int *ipk_no, int *check);
void INI_AMB(int *ipk_no, int *ipkx, int *ipky, int *error);
void DEL_AMB(int *ipk_no);
void SUM_AMB(int *ipk_no, int *total, float *sum, float *ftab, 
int *size1, int *size2);
void GET_AMB(int *ipk_no, int *liste, int *nbpixel);
void PUT_AMB(int *ipk_no, int *liste, int *nbpixel,int *error);
void PUT_PIX_AMB(int *ipk_no,int *ipixx,int *ipixy,int *error);
void CHECK_PIX_AMB(int *ipk_no,int *ipixx,int *ipixy,int *check);
void REM_PIX_AMB(int *ipk_no, int *ipixx, int *ipixy, int *error);

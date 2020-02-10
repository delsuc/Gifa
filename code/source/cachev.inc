C This file holds the common for the cache parameters

      integer id_cache

      integer*4  c_dimmanip
      integer*4  c_type,c_sizef1,c_sizef2,c_sizef3
       real*4    c_offsf1,c_offsf2,c_offsf3,c_specwf1,c_specwf2,
     *           c_specwf3,c_freq,c_freq1,c_freq2,c_freq3,
     *           c_dmin,c_dmax,c_dfactor,c_max0


      character*256 c_name,c_header

c for cache access
      integer CF_READ, CF_WRITE, CF_READ_WRITE,
     *        CB_READ, CB_WRITE, CB_LOOK
      parameter (CF_READ=1, CF_WRITE=2, CF_READ_WRITE=3,
     *        CB_READ=11, CB_WRITE=12, CB_LOOK=13)
c for typed parameters
      integer H_INT, H_FLOAT, H_DOUBLE
      parameter (H_INT=1, H_FLOAT=2, H_DOUBLE=3)

c for returned error code
      external ch_errlist
      character*(*) ch_errlist
      integer C_COULD_NOT_MALLOC,
     * C_COULD_NOT_OPEN,
     * C_TOO_MANY_FILES,
     * C_DIM_MISSING,
     * C_SIZE_MISSING,
     * C_VERSION_UNK,
     * C_BLKDEF_MISSING,
     * C_WRONG_DIM,
     * C_ACCESS,
     * C_WRONG_MODE,
     * C_NOT_OPENED,
     * C_OUTSIDE,
     * C_WRONG_FILE,
     * C_COULD_NOT_WRITE,
     * C_COULD_NOT_READ,
     * C_READ_VS_LOOK,
     * C_WRONG_HEADER,
     * C_WRONG_PARAM,
     * C_NO_ROOM_HEAD,
     * C_PARAM_NOT_FOUND

      parameter (C_COULD_NOT_MALLOC=2,
     * C_COULD_NOT_OPEN=3,
     * C_TOO_MANY_FILES=4,
     * C_DIM_MISSING=5,
     * C_SIZE_MISSING=6,
     * C_VERSION_UNK=7,
     * C_BLKDEF_MISSING=8,
     * C_WRONG_DIM=9,
     * C_ACCESS=10,
     * C_WRONG_MODE=11,
     * C_NOT_OPENED=12,
     * C_OUTSIDE=13,
     * C_WRONG_FILE=14,
     * C_COULD_NOT_WRITE=15,
     * C_COULD_NOT_READ=16,
     * C_READ_VS_LOOK=17,
     * C_WRONG_HEADER=18,
     * C_WRONG_PARAM=19,
     * C_NO_ROOM_HEAD=20,
     * C_PARAM_NOT_FOUND=21)

      character listjoinfile(256)*40

      common/filecache/c_name,c_header

      common/paracache/
     *   id_cache,c_dimmanip,
     *   c_type,c_sizef1,c_sizef2,c_sizef3,
     *   c_offsf1,c_offsf2,c_offsf3,c_specwf1,c_specwf2,
     *   c_specwf3,c_freq,c_freq1,c_freq2,c_freq3,
     *   c_dmin,c_dmax,c_dfactor,c_max0,
     *   listjoinfile





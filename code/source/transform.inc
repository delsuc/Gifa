C transform.inc

      integer deconv
      real lb, gb, filter(sizemax),lb1,gb1,j1,j2,lb2,gb2,
     *     window(sizemax)
      integer sumcons,nchannel
      character*1 cc
      common /transfcom1/filter,window,lb,gb,lb1,gb1,j1,j2,
     *     lb2,gb2
      common /transfcom2/deconv,sumcons,nchannel,cc

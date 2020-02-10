C
C holds all basic multi purpose gifa_main variables
c

c 'version' has been moved to "version.inc"

      real ph0, ph1, leastsq
      integer sizeimo,tm1,tm2,
     *     row,col,rowv,colv,
     *     cdisp,pdisp,refresh,ilenc,iterdone
      integer vd_id_max
      parameter (vd_id_max =10)
      integer vd_id(vd_id_max)
      character*256 name, pkname
      character*256 name1d, name2d, name3d
      character*256 license

      common/namecom/name, name1d, name2d, name3d, pkname
      integer on_graph,on_x_win
      common/win_common/on_graph,on_x_win
      integer cleanable,imsg,msgsiz,imsgmax
      parameter (msgsiz=128, imsgmax=5)
      character*(msgsiz) msg(imsgmax)
      common/cmmessage/cleanable,imsg,msg

      common /gifabasic/  ph0, ph1, leastsq ,tm1,tm2,row,col,rowv,colv,
     *     cdisp,pdisp,sizeimo,vd_id,refresh,ilenc,iterdone,license

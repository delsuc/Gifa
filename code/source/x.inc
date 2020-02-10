        common /x/dpy,screen,pixmap,window,attr_mask,gc,font,
     *         windowx,windowy,windowh,windoww,depth,func,status,
     *         visual,xswda,xgcvl,xszhn,event,font_name,win,
     *         color_map,screen_color,colorno,numcolors,black,white,
     *         cmconv,cursor1,image
        integer*4 dpy(10)                         ! display id
        integer*4 screen                          ! screen id
        integer*4 pixmap(10)                      ! window pixmap
        integer*4 window(10)                      ! window id
        integer*4 attr_mask                       ! attributes mask
        integer*4 gc(10)                          ! gc id
        integer*4 font                            ! font id
        integer*4 windowx(10),windowy(10)         ! window origin
        integer*4 windoww(10),windowh(10)         ! window width
        integer*4 depth(10)                       ! number of planes
        integer*4 func,status                     ! synchronous behavior
        integer*4 win(10)                         ! array of free window ids.
        integer*4 color_map
        integer*4 colorno(0:64)
        integer*4 numcolors,black,white
        integer*4 cursor1                         ! Default cursor struct.
        real      cmconv                          ! conv cm. to pixels val

        record /x$visual/ visual                  ! visual type
        record /x$set_win_attributes/ xswda       ! window attributes
        record /x$gc_values/ xgcvl                ! gc values
        record /x$size_hints/ xszhn               ! hints
        record /x$event/ event                    ! input event
        record /x$color/ screen_color
        record /x$image/ image                    ! 2d image struct.

        character*60 font_name
        data font_name 
     *  /'-ADOBE-NEW CENTURY SCHOOLBOOK-MEDIUM-R-NORMAL--*-140-*-*-P-*'/


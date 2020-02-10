C gifshellv.inc file, used for parsing command line.

      character*256 buffer(9),currinp(9),param,path,prompt,home,returned
      integer input,currline(9),inpeof(10)
      integer profiler,timer,verbose,debug,vcontext,savecontext
      integer comefrom
      common/parameter/currline,input,inpeof,buffer,currinp,param,
     *       path,prompt,home,returned,profiler,timer,verbose,debug,
     *       vcontext,savecontext,comefrom


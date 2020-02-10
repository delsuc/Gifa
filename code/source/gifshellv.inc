C gifshellv.inc file, used for parsing command line.

      character*256 buffer(9),currinp(9),param,path,prompt,home
      integer input,currline(9),inpeof(10)
      integer timer,verbose,debug,refmacro,vcontext,savecontext
      integer comefrom
      common/parameter/currline,input,inpeof,buffer,currinp,param,
     *       path,prompt,home,timer,verbose,debug,refmacro,
     *       vcontext,savecontext,comefrom


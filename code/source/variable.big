C this is the static area for GIFA variable storage
c
c varval  holds the value of the ith variable
c varname holds its name
c conttab hods its context (usually input), 
c each context is used as a different variable holder,
c permiting to dealloc all variables associated with context when exiting a macro
c
      integer varmax,varnmlen,varcontlen
#_aix      parameter (varmax = 4000, varnmlen=32, varcontlen=256)
#aix      parameter (varmax = 500, varnmlen=32, varcontlen=256)

      character*(varcontlen) varval(varmax)
      character*(varnmlen)  varname(varmax)
      integer conttab(varmax) 
      integer collision
      common /var_common/ conttab,varname,varval,collision


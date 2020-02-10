/* definitions for util.c */

/*
void cconvert_string(char *st2, char *st1, int *l);
void convert_string(char *st2, char *st1, int l);
*/
#ifdef UNDERSCORE 

#  define  INPROG inprog_
#  define  INITINPROG initinprog_
#  define  WIN_REFRESH win_refresh_
#  define  CWAITASEC cwaitasec_
#  define  GIFAPRINT gifaprint_
#  define  SHOWPROMPT showprompt_
#  define  GIFAFLUSH gifaflush_
#  define  GIFAOUTC gifaoutc_
#  define  WUXNMR1D wuxnmr1d_
#  define  GET_LICENCE get_licence_
#  define  GIFA_EXIT gifa_exit_
#  define  SET_PROMPT set_prompt_
#  define  REM_RL_HANDLER rem_rl_handler_

#else
#ifdef F2C

#  define  INPROG inprog_
#  define  INITINPROG initinprog_
#  define  WIN_REFRESH win_refresh__
#  define  CWAITASEC cwaitasec_
#  define  GIFAPRINT gifaprint_
#  define  SHOWPROMPT showprompt_
#  define  GIFAFLUSH gifaflush_
#  define  GIFAOUTC gifaoutc_
#  define  WUXNMR1D wuxnmr1d_
#  define  GET_LICENCE get_licence__
#  define  GIFA_EXIT gifa_exit__
#  define  SET_PROMPT set_prompt__
#  define  REM_RL_HANDLER rem_rl_handler__

#else
#  define INPROG inprog
#  define  INITINPROG initinprog
#  define  WIN_REFRESH win_refresh
#  define  CWAITASEC cwaitasec
#  define  GIFAPRINT gifaprint
#  define  SHOWPROMPT showprompt
#  define  GIFAFLUSH gifaflush
#  define  GIFAOUTC gifaoutc
#  define  WUXNMR1D wuxnmr1d
#  define  GET_LICENCE get_licence
#  define  GIFA_EXIT gifa_exit
#  define  SET_PROMPT set_prompt
#  define  REM_RL_HANDLER rem_rl_handler

#endif
#endif

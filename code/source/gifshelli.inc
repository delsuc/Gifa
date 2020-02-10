C This file holds the variables for the gifa shell
	debug = 0
	verbose = 0
	timer = 0
	profiler = 0
        comefrom = 0
#_vms      call gethome(home)
#_vms      call trailing(home,i)
#_vms      path = home(1:i) // '/macro' // ' /usr/local/gifa/macro'
#vms       path = 'SYS$LOGIN:[macro] gifa$macro'
        prompt = 'Gifa>'
        call trailing(prompt,i)
        call set_prompt(prompt,i)
        sttimer = ' '
        returned = ' '



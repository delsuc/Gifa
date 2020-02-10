; registration box
;
if (!$arg) then
  formbox Registration \
        "gm/licence.g $name $site" \
	" " message \
	"Gifa is distributed by anonymous FTP at :" message \
	"ftp://tome.cbs.univ-montp1.fr/pub/gifa_v4/" action \
        "sh ($viewer;'ftp://tome.cbs.univ-montp1.fr/pub/gifa_v4/ &')" \
	" " message \
	"You need to register by contacting :" message \
	"Marc-Andre Delsuc <mad@cbs.univ-montp1.fr>" message \
	"Registration is free for academical laboratories !" message \
	"Solutions are available for Companies." message \
	separator \
	"Academical laboratories are required to sign and fax a licence agreement, as found in the distribution kit" message \
	"Clicking on Apply or Ok will send a registration mail" message \
	separator \
	"Enter your name :" string name " " \
	"Enter the name and localisation of your laboratory" message \
	"(as complete as possible) " string site " " *
else
  set nn = $_
  set ss = $_
  dialogbox "Registration" \
    "The following message will be sent by e-mail to :  mad@cbs.univ-montp1.fr" message \
    separator \
    "Please register me" message \
    $nn message $ss message *
  sh ("echo Please register me `whoami` \@ `hostname` '\n'";$nn;"'\n'";$ss;"| mail mad@cbs.univ-montp1.fr")
  alert "... message sent. Remember that a signed licence is still required"
endif

#!/usr/bin/nawk
# un mot seul apres une ligne vide
# il faut nawk == a cause du close
BEGIN { fich = 1; som = 0;}
fich==0 { print $0 > ( nom ".hlp" ) }
NF == 0 { fich = 1;}
NF == 1 && fich==1 { if(som>0) close(nom ".hlp");
                     nom = $1 ; gsub(/->/,"2",nom);\
                     print nom; som = som+1; fich = 0; }
NF > 1 && fich==1 { print $0 > ( nom ".hlp" ) ; fich = 0}
END { print "un total de " som " fichiers" }

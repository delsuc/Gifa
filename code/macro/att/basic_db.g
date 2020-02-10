; basic
;
; create entries for protein assignment

error "Not used any more !"

set residue := "AMX,LONG,DiMet,ALA,ARG,ASN,ASP,CYS,GLN,GLU,GLY,HIS,ILE,LEU,LYS,MET,PHE,PRO,SER,THR,TRP,TYR,VAL,Arom-Phe,Arom-Tyr,Arom-His,Arom-Trp,Met,Amide,NH3+,H2O"
; residue duplicate the topo entries, 
set proton := "HN,HA,HA1,HA2,HA3,HB,HB1,HB2,HG,HG1,HG2,HD1,HD2,HE1,HE2,HZ1,HZ2,1H,2H,3H,4H,5H,6H,7H,HMET,H2O,HN+,HN1,HN2"
set carbon := "CO,CA,CB,CG,CG1,CG2,CD,CD1,..."

; open static db
dbopen /usr/local/gifa/macro/att/topology topo      ; for spin-system topology
dbopen /usr/local/gifa/macro/att/3let_1let rescode  ; for residue codes

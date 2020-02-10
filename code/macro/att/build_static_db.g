; build the static data_bases in the local db directory
;
; This macro is not used in normal work,
; the topology 3let_1let and 1let_3let  db    being permanently there.
; If you modify it, you will have to rerun it.
;
; You is also possible that you cannot read the dbm files in the distribution
; because your dbm set-up is not compatible (this is the case with Linux)
; in this case, remove the files *.pag and *.dir from the directory
; and rerun this macro, which will recreate them
;
; now the systems are IUPAC compliant
;
  ;

;first make sure no old file are left
sh '/bin/rm -f db/topology.* db/3let_1let.* db/1let_3let.*'

dbopen db/topology topo
; This db defines the spin-systems that are known
; each spin-syst has a name and a list of known spins.

set topo["AMX"] = "HN,HA,HB2,HB3,HB?,QB,N,C,CA,CB"
set topo["AMPTX"] = "HN,HA,HB2,HB3,HB?,QB,HG2,HG3,HG?,QG,N,C,CA,CB,CG"
set topo["LONG"] = "HN,HA,HB2,HB3,HB?,QB,HG2,HG3,HG?,QG,HD2,HD3,HD?,QD,N,C,CA,CB,CG,CD,CE,CZ"
set topo["DiMet"] = "HN,HA,HB,HB2,HB3,HB?,QB,HG,HG2,HG3,HG?,QG,HD2,HD3,HD?,QD,N,C,CA"
set topo["Arom-PHE"] = "HD1,HD2,HD?,QD,HE1,HE2,HE?,QE,HZ,QR,CG,CD,CD1,CD2,CE,CE1,CE2,CZ"
set topo["Arom-TYR"] = "HD1,HD2,HD?,QD,HE1,HE2,HE?,QE,QR,CG,CD,CD1,CD2,CE,CE1,CE2,CZ"
set topo["Arom-HIS"] = "HD1,HD2,HD?,QD,HE1,HE2,HE?,QE,ND1,NE2,CG,CD2,CE1"
set topo["Arom-TRP"] = "HD1,HE1,HE3,HE?,QE,HZ2,HZ3,HZ?,QZ,HH2,QR,NE1,CG,CD1,CD2,CE2,CE3,CZ2,CZ3,CH2"
set topo["Met"] = "HM"
set topo["MMET"] = "HMET"
set topo["NH3+"] = "HN+"
set topo["Amide"] = "HN1,HN2"
set topo["H2O"] = "H2O"
set topo["GLY"] = "HN,HA2,HA3,HA?,QA,N,C,CA"
set topo["ALA"] = "HN,HA,HB,N,C,CA,CB"
set topo["VAL"] = "HN,HA,HB,HG1,HG2,HG?,QG,N,C,CA,CB,CG,CG1,CG2"
set topo["ILE"] = "HN,HA,HB,HG2,HG12,HG13,HG1?,QG,HG,HD,N,C,CA,CB,CG,CG1,CG2,CD"
set topo["LEU"] = "HN,HA,HB2,HB3,HB?,QB,HG,HD1,HD2,HD?,QD,N,C,CA,CB,CG,CD,CD1,CD2"
set topo["PRO"] = "HA,HB2,HB3,HB?,QB,HG2,HG3,HG?,QG,HD2,HD3,HD?,QD,C,CA,CB,CG,CD"
set topo["MET"] = "HN,HA,HB2,HB3,HB?,QB,HG2,HG3,HG?,QG,HE,N,C,CA,CB,CG,CE"
set topo["ASP"] = "HN,HA,HB2,HB3,HB?,QB,N,C,CA,CB,CG"
set topo["GLU"] = "HN,HA,HB2,HB3,HB?,QB,HG2,HG3,HG?,QG,N,C,CA,CB,CG,CD"
set topo["GLN"] = "HN,HA,HB2,HB3,HB?,QB,HG2,HG3,HG?,QG,HE21,HE22,HE2?,QE,N,NE2,C,CA,CB,CG,CD"
set topo["LYS"] = "HN,HA,HB2,HB3,HB?,QB,HG2,HG3,HG?,QG,HD2,HD3,HD?,QD,HE2,HE3,HE?,QE,HZ,QZ,N,NZ,C,CA,CB,CG,CD,CE"
set topo["ARG"] = "HN,HA,HB2,HB3,HB?,QB,HG2,HG3,HG?,QG,HD2,HD3,HD?,QD,HE,HH11,HH12,HH1?,QH1,HH21,HH22,HH2?,QH2,QH,N,NE,NH,NH1,NH2,C,CA,CB,CG,CD,CE,CZ"
set topo["SER"] = $topo["AMX"]
set topo["CYS"] = $topo["AMX"]
set topo["THR"] = "HN,HA,HB,HG,HG2,N,C,CA,CB,CG2"
set topo["PHE"] = ($topo["AMX"] // ',' // $topo["Arom-PHE"])
set topo["TYR"] = ($topo["AMX"] // ',' // $topo["Arom-TYR"])
set topo["HIS"] = ($topo["AMX"] // ',' // $topo["Arom-HIS"])
set topo["TRP"] = ($topo["AMX"] // ',' // $topo["Arom-TRP"])
; we'll have to choose between ND and ND2 !
set topo["ASN"] = "HN,HA,HB2,HB3,HB?,QB,HD21,HD22,HD2?,QD,N,ND,ND2,C,CA,CB,CG"

;the following entries defines some lists
; residue duplicate the topo entries, permits to define an order neatly
set topo["RESIDUE"] = "AMX,AMPTX,LONG,DiMet,ALA,ARG,ASN,ASP,CYS,GLN,GLU,GLY,HIS,ILE,LEU,LYS,MET,PHE,PRO,SER,THR,TRP,TYR,VAL,Arom-PHE,Arom-TYR,Arom-HIS,Arom-TRP,Met,HMET,Amide,NH3+,H2O"
set topo["PROTON"] = "HN,HA,HA1,HA2,HB,HB2,HB3,HG,HG1,HG2,HG3,HD1,HD2,HD3,HE,HE1,HE2,HE3,HZ,HMET,H2O,HN+,HN1,HN2,N,ND,ND1,ND2,NE,NE2,NZ,NH,NH1,NH2,C,CA"
set topo["CARBON"] = "C,CA,CB,CG,CG1,CG2,CD,CD1,CD2,CE,CE1,CE2,CE3,CZ,CZ2,CZ3,CH2"
set topo["AZOTE"] = "N,ND,ND1,ND2,NE,NE2,NZ,NH,NH1,NH2"

dbclose topo

dbopen db/3let_1let code
; These data bases defines the known residues.
; There are 2 db, one for 3 letters entries, one for 1 letters
; is coded : name list_of_spin_systems

set code["GLY"] = "G GLY"
set code["ALA"] = "A ALA"
set code["VAL"] = "V DiMet"
set code["PHE"] = "F AMX,Arom-PHE"
set code["ILE"] = "I DiMet"
set code["LEU"] = "L DiMet"
set code["PRO"] = "P LONG"
set code["MET"] = "M AMPTX,MMET"
set code["ASP"] = "D AMX"
set code["GLU"] = "E AMPTX"
set code["GLN"] = "Q AMPTX,Amide"
set code["LYS"] = "K LONG,HN3+"
set code["ARG"] = "R LONG,Amide"
set code["SER"] = "S AMX"
set code["THR"] = "T THR"
set code["TYR"] = "Y AMX,Arom-TYR"
set code["HIS"] = "H AMX,Arom-HIS"
set code["CYS"] = "C AMX"
set code["ASN"] = "N AMX,Amide"
set code["TRP"] = "W AMX,Arom-TRP"

; now inverse the db
dbopen db/1let_3let code2
foreach i in code
  set j = $code[$i]
  set code2[head($j)] = ($i; tail($j))
endfor
dbclose code
dbclose code2


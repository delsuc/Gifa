; build the static data_bases in the /usr/local/gifa/macro/att directory
;
; This macro is not used in normal work,
; the topology 3let_1let and 1let_3let  db    being permanently there.
; If you modify it, you will have to rerun it.
; (Note that normal user do not have write access to the static db,
;  You will have to rerun it as the user to which belongs the Gifa package)
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
sh 'cd /usr/local/gifa/macro/att; /bin/rm topology.* 3let_1let.* 1let_3let.*'

dbopen /usr/local/gifa/macro/att/topology topo
; This db defines the spin-systems that are known
; each spin-syst has a name and a list of known spins.

set topo["AMX"] = "HN,HA,HB2,HB3,QB,N"
set topo["AMPTX"] = "HN,HA,HB2,HB3,QB,HG2,HG3,QG,N"
set topo["LONG"] = "HN,HA,HB2,HB3,QB,HG2,HG3,QG,HD2,HD3,QD,N"
set topo["DiMet"] = HN,HA,HB,HB2,HB3,QB,HG,HG2,HG3,HG3,QG,HD2,HD3,QD,N"
set topo["Arom-PHE"] = "HD1,HD2,QD,HE1,HE2,QE,HZ,QR"
set topo["Arom-TYR"] = "HD1,HD2,QD,HE1,HE2,QE,QR"
set topo["Arom-HIS"] = "HD1,HD2,QD,HE1,HE2,QE"
set topo["Arom-TRP"] = "HD1,HE1,HE3,QE,HZ2,HZ3,QZ,HH2,QR"
set topo["Met"] = "HM"
set topo["MMET"] = "HMET"
set topo["NH3+"] = "HN+"
set topo["Amide"] = "HN1,HN2"
set topo["H2O"] = "H2O"
set topo["GLY"] = "HN,HA2,HA3,QA,N"
set topo["ALA"] = "HN,HA,HB,N"
set topo["VAL"] = "HN,HA,HB,HG1,HG2,QG,N"
set topo["PHE"] = "HN,HA,HB2,HB3,QB,QD,QE,QR,N"
set topo["ILE"] = "HN,HA,HB,HG12,HG13,QG,HG,HD,N"
set topo["LEU"] = "HN,HA,HB2,HB3,QB,HG,HD1,HD2,QD,N"
set topo["PRO"] = "HA,HB2,HB3,QB,HG2,HG3,QG,HD2,HD3,QD"
set topo["MET"] = "HN,HA,HB2,HB3,QB,HG2,HG3,QG,HE,N"
set topo["ASP"] = "HN,HA,HB2,HB3,QB,N"
set topo["GLU"] = "HN,HA,HB2,HB3,QB,HG2,HG3,QG,N"
set topo["GLN"] = "HN,HA,HB2,HB3,QB,HG2,HG3,QG,HE21,HE22,QE,N,NE2"
set topo["LYS"] = "HN,HA,HB2,HB3,QB,HG2,HG3,QG,HD2,HD3,QD,HE2,HE3,QE,HZ,QZ,N,NZ"
set topo["ARG"] = "HN,HA,HB2,HB3,QB,HG2,HG3,QG,HD2,HD3,QD,HE,HH11,HH12,QH1,HH21,HH22,QH2,QH,N,NE,NH,NH1,NH2"
set topo["SER"] = "HN,HA,HB2,HB3,QB,N"
set topo["THR"] = "HN,HA,HB,HG,N"
set topo["TYR"] = "HN,HA,HB2,HB3,QB,QD,QE,QR,N"
set topo["HIS"] = "HN,HA,HB2,HB3,QB,N,ND1,NE2"
set topo["CYS"] = "HN,HA,HB2,HB3,QB,N"
set topo["ASN"] = "HN,HA,HB2,HB3,QB,HD21,HD22,QD,N,ND,ND2"
set topo["TRP"] = "HN,HA,HB2,HB3,QB,N.NE1"

;the following entries defines some lists
; residue duplicate the topo entries, permits to define an order neatly
set topo["RESIDUE"] = "AMX,AMPTX,LONG,DiMet,ALA,ARG,ASN,ASP,CYS,GLN,GLU,GLY,HIS,ILE,LEU,LYS,MET,PHE,PRO,SER,THR,TRP,TYR,VAL,Arom-PHE,Arom-TYR,Arom-HIS,Arom-TRP,Met,HMET,Amide,NH3+,H2O"
set topo["PROTON"] = "HN,HA,HA1,HA2,HB,HB2,HB3,HG,HG1,HG2,HG3,HD1,HD2,HD3,HE,HE1,HE2,HE3,HZ,HMET,H2O,HN+,HN1,HN2,N,ND,ND1,ND2,NE,NE2,NZ,NH,NH1,NH2"
set topo["CARBON"] = "CO,CA,CB,CG,CG1,CG2,CD,CD1,..."
set topo["AZOTE"] = "N,ND,ND1,ND2,NE,NE2,NZ,NH,NH1,NH2"

dbclose topo

dbopen /usr/local/gifa/macro/att/3let_1let code
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
dbopen /usr/local/gifa/macro/att/1let_3let code2
foreach i in code
  set j = $code[$i]
  set code2[head($j)] = ($i; tail($j))
endfor
dbclose code
dbclose code2

sh "cd /usr/local/gifa/macro/att/; chmod o+r topology.* 3let_1let.* 1let_3let.*"

#!/usr/local/bin/perl
#
# this prgm is used to compute the distances for 2 atom names
# in a PDB file.
# called by calc_dist macro in the assignment module of Gifa
#
# syntax: calc_dist.pl pdbfile nom_at1 no1 nom_at2 no2
#
#   where pdbfile : name of the pdb file to be used
#         nom_at1/nom_at2 : name of the atome to search : eg HA QB
#         no1/no2 : index of the residue : eg 56 1


$i = 0;
$pdbfile = "PDB/$ARGV[0]";
#$pdbfile = $ARGV[0];
 
open(IN,$pdbfile) || die "cannot open PDB file"; 
while (<IN>) {
if (/^ATOM/) {
    @ff = split(' ',$_);
    $i++;
    $at_name[$i] = $ff[2];
    $res_name[$i] = $ff[3];
    $res_num[$i] = $ff[4];
    $x[$i] = $ff[5];
    $y[$i] = $ff[6];
    $z[$i] = $ff[7];
  }
}
close(IN);
# determine the atom subsets
$num_of_at = $i;
$num_of_at1 = 0;
$num_of_at2 = 0;
for ($i = 1; $i <= $num_of_at; $i++) {
    if (($at_name[$i] =~ /$ARGV[1]/i) && ($res_num[$i] == $ARGV[2]) && ($ARGV[2] != ".")) {
      $num_of_at1++;
      $at1[$num_of_at1] = $i;
    }
    if (($at_name[$i] =~ /$ARGV[1]/i) && ($ARGV[2] == ".")) {
      $num_of_at1++;
      $at1[$num_of_at1] = $i;
    }
    if (($at_name[$i] =~ /$ARGV[3]/i) && ($res_num[$i] == $ARGV[4]) && ($ARGV[4] != ".")) {
      $num_of_at2++;
      $at2[$num_of_at2] = $i;
    }
    if (($at_name[$i] =~ /$ARGV[3]/i) && ($ARGV[4] == ".")) {
      $num_of_at2++;
      $at2[$num_of_at2] = $i;
    }
  }
# calculate the distances
for ($s = 1; $s <= $num_of_at1; $s++) {
  $n1 = $at1[$s];
  for ($p = 1; $p <= $num_of_at2; $p++) {
      $n2 = $at2[$p];
#      print "n2 $n2 n1 $n1 \n";
#      print "n1 x $x[$n1] $y[$n1] $z[$n1] \n";
#      print "n2 x $x[$n2] $y[$n2] $z[$n2] \n";
      $dist = (($x[$n1] - $x[$n2])**2) + (($y[$n1] - $y[$n2])**2) + (($z[$n1] - $z[$n2])**2);
      $dist = int(100*sqrt($dist))/100;
#      printf "'%s  %s  %s     %s  %s  %s     %6.2f' message \\ \n", $at_name[$n1],$res_name[$n1],$res_num[$n1],$at_name[$n2],$res_name[$n2],$res_num[$n2],$dist; 
      $indx = $s.",".$p;
      $line{$dist} = "'$at_name[$n1] $res_name[$n1] $res_num[$n1]     $at_name[$n2] $res_name[$n2] $res_num[$n2]       $dist' message \\";
      $dst{$s.",".$p} = $dist;
#     printf "$line{$dist} $dist \n";
    }
  }
# sort the distances
printf "formbox primary DO_NOTHING \\ \n";
printf "'Scanning PDB file %s ...' message \\ \n", $ARGV[0];
foreach $k (sort numerically (keys %line)) {
    printf "$line{$k} \n";
}
printf "* \n";

# sort the distances
sub numerically { $a <=> $b; }



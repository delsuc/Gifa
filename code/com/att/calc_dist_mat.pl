#!/usr/local/bin/perl
#
# this prgm is used to compute the distances for 2 lists of atom names
# in a PDB file.
# called by calc_dist_mat macro in the assignment module of Gifa
#
# syntax: calc_dist_mat.pl pdbfile lis_nom_at1 lis_no1 lis_nom_at2 lis_no2
#
#   where pdbfile : name of the pdb file to be used
#         lis_nom_at1/lis_nom_at2 : liste of the name of the atoms to search : eg HA:HN:QB
#         lis_no1/lis_no2 : liste of the index of the residues : eg 56:1:33


system('pwd');

$i = 0;
$pdbfile = "PDB/$ARGV[0]";
#$pdbfile = $ARGV[0];
 
#read PDB in
open(IN,$pdbfile) || die "cannot open PDB file $pdbfile"; 
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
$num_of_at = $i;
close(IN);

@lis_at1 = split(':',$ARGV[1]);
@lis_no1 = split(':',$ARGV[2]);
@lis_at2 = split(':',$ARGV[3]);
@lis_no2 = split(':',$ARGV[4]);


print "Distances are in Ansgtroem\n";
print "        ";
@lis_tt = @lis_no2;
foreach $at2 (@lis_at2) {
	$no2 = shift @lis_tt;
	printf "%4s%3d -",$at2, $no2;
}
print "\n";

# scan the table
foreach $at1 (@lis_at1) {
	$at1 =~ s/(...)./$1/;      # if 4 chars => means Pb ! 
	$no1 = shift @lis_no1;
	printf "%4s %3d", $at1, $no1;
	@lis_tt = @lis_no2;
	foreach $at2 (@lis_at2) {
		$at2 =~ s/(...)./$1/;      # if 4 chars => means Pb ! 
	        $no2 = shift @lis_tt;


#then for each pair, determine the atom subsets
$num_of_at1 = 0;
$num_of_at2 = 0;
for ($i = 1; $i <= $num_of_at; $i++) {
    if (($at_name[$i] =~ /$at1/i) && ($res_num[$i] == $no1)) {
      $num_of_at1++;
      $at1[$num_of_at1] = $i;
    }
    if (($at_name[$i] =~ /$at2/i) && ($res_num[$i] == $no2)) {
      $num_of_at2++;
      $at2[$num_of_at2] = $i;
    }
  }
# and calculate the distances
for ($s = 1; $s <= $num_of_at1; $s++) {
  $n1 = $at1[$s];
  for ($p = 1; $p <= $num_of_at2; $p++) {
      $n2 = $at2[$p];
#      print "n2 x $x[$n2] $y[$n2] $z[$n2] \n";
      $dist = (($x[$n1] - $x[$n2])**2) + (($y[$n1] - $y[$n2])**2) + (($z[$n1] - $z[$n2])**2);
      $dist = int(100*sqrt($dist))/100;
      push @dd, $dist;
  }
}
$dist = min(@dd);
printf "  %-5.2f  ", $dist;
undef @dd;
}
print "\n";
}
exit;

# sort the distances
printf "formbox primary DO_NOTHING \\ \n";
printf "'Scanning PDB file %s ...' message \\ \n", $ARGV[0];
foreach $k (sort numerically (keys %line)) {
    printf "$line{$k} \n";
}
printf "* \n";

# sort the distances
sub numerically { $a <=> $b; }

#find the min in a list
sub min {
  $mm = shift @_;
  foreach $m (@_) { if ($m < $mm) { $mm = $m;} }
  return $mm;
}

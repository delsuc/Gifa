#!/usr/local/bin/perl
open(IN,"<depend_list.in");
open(OUT,">depend_list");

print OUT "# DO NOT change this file,\n";
print OUT "# created automagically by makedepend.perl from the file depend_list.in \n";
print OUT "# change depend_list.in if desired\n\n";

$in=0;

while (<IN>) {
    print OUT $_;
    if (/^OBJECTS =/) {	$in=1;  next; }
    if (/^#END_OF_LIST/)  { $in=0; }

    if ($in == 1) {
	/^\s*(\w*\.o)/;
	push(@nom,$1);
    }
}

print OUT "# computed dependence list\n";
    foreach $key (@nom) {
	$key =~ /(\w*)/;
	$ff=$1;
	if (-e $ff.".for") { 
	    $nm = $ff.".for";
	} elsif (-e $ff.".c") { 
	    $nm = $ff.".c";
	} else {
	    die "Don't know how to make $key\n";
	}
	print "$nm\n";
	print OUT "$key : $nm ";
	@ll = do incl($nm,"fh00");
	%seen = ();   $n=1;
	foreach $ill (@ll) {
	    if (!$seen{$ill}++) {               # uniq
		if ($n == 0) {	                # debut de ligne
		    print OUT "\\\n   ";
		}
		print OUT $ill," ";
		if ($n++ == 3) {
		    $n=0;
		}
	    }
	}
	print OUT "\n\n";
    }

# fait le boulot
sub incl {
    local($file,$input) = @_;
    local(@list);
    $input++;

    open($input,$file);

    while (<$input>) {
	if (/^\#\s*include\s+"(\w+\.\w*)"/) {          # j'en tiens un
	    push(@list,$1);
	    push(@list,do incl($1,$input));
	}
    }
    @list;
}

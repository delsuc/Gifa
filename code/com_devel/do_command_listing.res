#!/usr/local/bin/perl

# this perl script builds a HTML documentation for the macros and commands
# macros are read in /usr/local/gifa/macro.
# commands are found in /usr/local/gifa/help
#
# The list is linked to basic.html
#
# see also do_list
#
#process switches
while	($ARGV[0]  =~ /^-/) {
	shift;
}

#open out files - with html headers
$doc  = "commands.html";

open(OUTF,">/usr/local/gifa/doc/$doc") || die "cannot open $doc\n";

print OUTF "<html>\n<head><!-- This document created by dodoc_html v1.0></head><body>\n";
print OUTF "<TITLE> List of standard commands and macros for GIFA</TITLE>\n";
print OUTF "<A NAME=\"Top\">\n";
print OUTF "<h1><A HREF=\"basic.html\">Jump to manual </A></h1>\n";
print OUTF "<H1> List of standard commands and macros for GIFA</H1>\n";

print OUTF "This is the copy of the on-line help (HELP command)<br>\n";
print OUTF "Native commands are in UPPERCASE, macros are in lower-case,\n";
print OUTF "but Gifa is case-insensitive and commands can be issued in any case.<p>\n";

print OUTF "<HR SIZE=4>\n";
print OUTF "List is sorted by first letter<br>\n";
$alpha="ABCDEFGHIJKLMNOPQRSTUVWXYZ";
print OUTF "<FONT SIZE=4> <B>\n";
for ($i=0;$i<26;$i++)
{
    $c=substr($alpha,$i,1);
    print OUTF "<A HREF=\"#$c\">$c</A>\n";
    }
print OUTF "</B> </FONT>\n";
print OUTF "<DL>\n";




# first macros...
# 
# loop on file-names
open(LS,"ls /usr/local/gifa/macro/* |");
while(<LS>) {
    chop;
    $fls = $_;
    if (-f $fls && !($fls =~ m/~$/)) { # (remove emacs backup)
	@name = split('/',$fls);
	$entry = pop(@name);	# found one
	open(FILE,$fls);
	while(<FILE>) {		# copy the comments into an assoc. array
	    if (/^;/) {
		$text{$entry}= "$text{$entry}$'";
	    }
	    else {
		last;
	    }
	}
    }
}
close(LS);

# then commands
open(LS,"ls /usr/local/gifa/help/*.hlp |");
while(<LS>) {
    chop;
    $fls = $_;
    if (-f $fls ) {
	($entry = $fls) =~ s#.*/(.*)\.hlp#\1#;
	$entry =~ tr/a-z/A-Z/;	# make it uppercase
	if (!($entry =~ m/DISP2D/)) {
	    $entry =~s/(.+)2(.+)/\1-&gt\2/; # replace ar2dt en ar->dt
	}
	open(FILE,$fls);
	while(<FILE>) {		# copy the comments into an assoc. array
	    $text{$entry} = "$text{$entry}$_";
	}
    }
}
close(LS);

# then output the files (sorted, one file per first letter)
$prev_flet = "0";

foreach $key (sort by_upper_case (keys %text)) {
    $key =~ /^./;
    ($flet = $&) =~ tr/a-z/A-Z/ ; # get first letter into $flet
    if ($flet ne $prev_let) {
	if ($prev_let ne "0") {
	    print "$flet\n";
	    print OFL "</DL> </pre> \n </body> \n ";
	    close (OFL);
	    $prev_let = $flet;
	}
	$ldoc = "$flet$doc";
	open(OFL, ">/usr/local/gifa/doc/$ldoc") || die "cannot open $ldoc\n";
	print OLF "<html>\n<head><!-- This document created by dodoc_html v1.0></head><body>\n";
	print OLF "<TITLE>Commands in $flet </TITLE>\n";
	print OFL "<H2> <A HREF=\"$doc\">Back to Alphabetical List</A> </H2>\n";
	print OFL "<DL>\n";
	print OFL "<pre>\n";
	print OUTF "<H2>\n";
	print OUTF "<DT> <HR SIZE=4> <pre> <A NAME=\"$flet\" > <A HREF=\"$ldoc\">$flet</A>      Back to <A HREF=\"#Top\">Top of Page</A> </pre>\n";
	print OUTF "</H2>\n";
	print OUTF "<DD> \n";
    }
#    print "$key\n";
    print OUTF "<A HREF=\"$ldoc#$key\">$key<\/A> <br> \n";
    print OFL  "<A NAME=\"$key\">\n"; # first sign the entry
    print OFL  "<DT> <HR> <H3> $key \n";
#    if ($key != (($z = $key) =~ tr/a-z/A-Z/)) { # if not in upper case => macro.
#	print OFL "    <A HREF=\"/usr/local/gifa/macro/$key\">Show macro</A>\n";
#    }
    print OFL "</H3> \n <DD>\n";
# finally, print
    $toprint = $text{$key};
# remove unlegal chars
    $toprint =~ s/&/&amp/g;
    $toprint =~ s/</&lt/g;
    $toprint =~ s/>/&gt/g;
    while ($toprint) {
	if ($toprint =~ /related contexts/i) { # special treatment for (related contexts)'s
	    print OFL $`;
	    print OFL "related <A HREF=\"C$doc#CONTEXTS\">contexts<\/A>";
	    $toprint = $';
	    next;
	}
	if ($toprint =~ /see also/i) { # special treatment for (see also)'s
	    print OFL $`;
	    print OFL "</pre>\n see also : ";
	    $_ = $';
	    foreach $eee (sort by_upper_case (keys %text)) {
		if (/[\s,^]$eee[,\s]/) {
		    $eee =~ /^./;
		    ($fff = $&) =~ tr/a-z/A-Z/ ; # get first letter
		    print OFL "<A HREF=\"$fff$doc#$eee\">$eee<\/A> \n";
		}
	    }
	    print OFL "<pre>\n";
	    $toprint = (1 == 0);
	} else {
	    print OFL $toprint;
	    $toprint = (1==0);
	}
    }
}
print OFL "</DL> </pre> \n </body> \n ";
close (OFL);

print OUTF "</DL> </pre> \n </body> \n ";
close(OUTF);

sub by_upper_case {		# used to sort the entries
    ($ua = $a) =~ tr/a-z/A-Z/ ;
    ($ub = $b) =~ tr/a-z/A-Z/ ;
    $ua cmp $ub;
}






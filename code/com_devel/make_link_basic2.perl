#!/usr/local/bin/perl
#
# a partir d'un fichier html, construit avec un mode plan,
# construit 2 fichiers, un fichier d'index construit sur le plan, et
# un fichier de texte pointe par le premier.
#
# bugs connus : si 2 entrees du plan on le meme nom, il y a collision.
#               pourrait etre mieux ecrit
#               tout est cable en dur !
#
#
# fabrique des liens a partir du plan html <h1> <h2> etc..
# pour configurer, changer les 4 variables ci dessous
#

$file_in="basic.html";
$file_text="basic_text.html";
$file_index="basic_index.html";
$profondeur = 3;

open(IN,"<$file_in");
open(OUT,">$file_text");
open(OUT_I,">$file_index");

# changer aussi les titres ici...

print OUT "<TITLE> The Gifa program <\/TITLE>";
print OUT "<H1> The Gifa program Version 4.0 <\/H1> <hr> \n";
print OUT "Back to the <A HREF=\"$file_index\"> Index file <\/A> \n";
print OUT "<hr> \n";

print OUT_I "<TITLE> The Gifa program <\/TITLE>";
print OUT_I "<H1> The Gifa program Version 4.0 <\/H1> <hr> \n";

$_ = <IN>;

#do_list contruit le plan de maniere recursive.

&do_list(1,$profondeur);

close(IN);
open(IN,"<$file_in");
while (<IN>) {
	if (/<h.>/) {
		if ($block != "rien") {
			print OUT "</A>\n";
		}
		$_ = <IN>;
		/^(.*)<\/h(.)>/;
		$block = $1;
		print OUT "<A NAME=\"$block\">\n";
		print OUT "<h$2>\n";
		print OUT "$block</h$2>\n";
	}
	else {
	    print OUT $_;
	}
}
close(OUT);
close(OUT_I);

sub do_list
# pour imprimer la hyper_liste de maniere recursive...
{
    local ($level,$prof) = @_;
    if ($level <= $prof) {
	print OUT_I "<UL> \n";
    }
    while () {
	if ( /^(.*)<\/h(.)>/ ) {
	    local($curr) = $2;
	    if ($curr == $level) {
		if ($level <= $prof) {
		    print OUT_I "<LI> <A HREF=\"$file_text#$1\">$1<\/A> \n";
		}
	    }
	    elsif ($curr > $level) {
		&do_list(($level+1),$prof);
		redo;
	    }
	    elsif ($curr < $level) {
		if ($level <= $prof) {
		    print OUT_I "</UL> \n";
		}
		return;
	    }
	}
	unless ($_ = <IN>) { 
	    print OUT_I "</UL> \n";
	    return;
	}
    }
}



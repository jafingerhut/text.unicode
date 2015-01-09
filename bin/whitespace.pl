: # use perl -*-Perl-*-
eval 'exec perl -S "$0" ${1+"$@"}'
    if 0;
# -*cperl-*-

use strict;

for (my $cp = 0; $cp < 0x110000; $cp++) {
    my $s = chr($cp);
    if (($s =~ /^\s$/) || ($s =~ /^\pZ$/)) {
	printf "%04X", $cp;
	if ($s =~ /^\s$/) {
	    printf " \\s";
	} else {
	    printf "   ";
	}
	if ($s =~ /^\pZ$/) {
	    printf " \\pZ";
	} else {
	    printf "    ";
	}
	printf "\n";
    }
}

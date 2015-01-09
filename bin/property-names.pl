: # use perl -*-Perl-*-
eval 'exec perl -S "$0" ${1+"$@"}'
    if 0;
# -*cperl-*-

use strict;

my $cp_should_match = { };

while (my $line = <>) {
    chomp $line;
    $line =~ s/#.*$//;
    if ($line =~ /^\s*$/) {
	next;
    }
    if ($line =~ / ^ \s*
                   ([0-9a-fA-F]+)         # first code point
                   (?:\.+([0-9a-fA-F]+))? # last code point
                                          # of a range
                   \s* ; \s*              # field separator
                   (\S+)                  # script name
                 /x)
    {
	my ($first_cp, $last_cp, $script_name) = ($1, $2, $3);
	if ($last_cp eq '') {
	    $last_cp = $first_cp;
	}
	$first_cp = oct('0x' . $first_cp);
	$last_cp = oct('0x' . $last_cp);
#	printf "script_name='%s' first_cp=%06X last_cp=%06X\n",
#	    $script_name, $first_cp, $last_cp;
	for (my $i = $first_cp; $i <= $last_cp; $i++) {
	    $cp_should_match->{$script_name}{$i} = 1;
	}
    } else {
	printf "Unrecognized format on line %d: %s\n", $., $line;
    }
}

my @script_names = sort keys %{$cp_should_match};

my $all_codepoints = [ ];
for (my $i = 0; $i <= 0x10FFFF; $i++) {
    # Don't put surrogate codepoints in the list
    if (! ((0xd800 <= $i) && ($i <= 0xdfff))) {
	push @{$all_codepoints}, $i;
    }
}

foreach my $script_name (@script_names) {
    my $pat = "^\\p{Script=" . $script_name . "}\$";
    if (!is_valid_pattern($pat)) {
        printf "Regex %s is NOT LEGAL\n", $pat;
    }
    printf "Regex %s is legal\n", $pat;
    
    my $num_should_match_cps = 0;
    my $good_match_cps = [ ];
    my $dont_match_but_should_cps = [ ];
    my $do_match_but_shouldnt_cps = [ ];

    for my $cp (@{$all_codepoints}) {
	if ($cp_should_match->{$script_name}{$cp}) {
	    ++$num_should_match_cps;
	}
	my $s = chr($cp);
#	if ($s =~ /^\p{Script=$script_name}$/) {
	if ($s =~ /$pat/) {
	    if ($cp_should_match->{$script_name}{$cp}) {
		push @{$good_match_cps}, $cp;
	    } else {
		push @{$do_match_but_shouldnt_cps}, $cp;
	    }
	} else {
	    if ($cp_should_match->{$script_name}{$cp}) {
		push @{$dont_match_but_should_cps}, $cp;
	    }
	}
    }

    printf "    %d should match.  %d do, %d do not.  %d that should not match, but do\n",
        $num_should_match_cps,
        1 + $#{$good_match_cps},
        1 + $#{$dont_match_but_should_cps},
        1 + $#{$do_match_but_shouldnt_cps};
    if (0 != (1 + $#{$dont_match_but_should_cps})) {
	printf "    Should match, but do not: %s\n",
            join(' ', map { hex_cp($_) } sort(@{$dont_match_but_should_cps}));
    }
    if (0 != (1 + $#{$do_match_but_shouldnt_cps})) {
	printf "    Should not match, but do: %s\n",
            join(' ', map { hex_cp($_) } sort(@{$do_match_but_shouldnt_cps}));
    }
}


sub hex_cp {
    my $cp = shift;

    if ($cp <= 0xffff) {
	return sprintf "%04X", $cp;
    } else {
	return sprintf "%06X", $cp;
    }
}


sub is_valid_pattern {
    my $pat = shift;
    return eval { "" =~ /$pat/; 1 } || 0;
}

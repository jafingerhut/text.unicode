: # use perl -*-Perl-*-
eval 'exec perl -S "$0" ${1+"$@"}'
    if 0;
# -*cperl-*-

open my $fh, '>:encoding(UTF-8)', "test-surrogate-perl-out.txt";
printf $fh "test boundary values for arguments to chr():\n";
printf $fh "chr(0x0000)='%s'\n", chr(0x0000);
printf $fh "chr(0xD7FF)='%s'\n", chr(0xD7FF);
printf $fh "chr(0xE000)='%s'\n", chr(0xE000);
printf $fh "chr(0xFFFF)='%s'\n", chr(0xFFFF);
printf $fh "chr(0x010000)='%s'\n", chr(0x010000);
printf $fh "chr(0x10FFFD)='%s'\n", chr(0x10FFFD);
printf $fh "chr(0x10FFFE)='%s'\n", chr(0x10FFFE);
printf $fh "chr(0x10FFFF)='%s'\n", chr(0x10FFFF);

printf $fh "\n";
printf $fh "test what Perl chr() does with a surrogate code point.\n";
printf $fh "chr(0xD800)='%s'\n", chr(0xD800);
printf $fh "chr(0xDFFF)='%s'\n", chr(0xDFFF);
close $fh;

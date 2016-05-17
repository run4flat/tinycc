use strict;
use warnings;

my $number = shift || 'list';

if ($number eq 'list') {
	print "Available tests include:\n";
	print "  $_\n" for glob("*.c");
	print "\nTo run a test, for example test 20, type\n";
	print "  perl exsymtests.pl 20\n\n";
	print "You do not need to enter the full test name, just the number\n";
	exit;
}

$number = "0$number" if length($number) == 1;
$number = '' if $number eq 'all';

use Env qw( @PATH );
unshift @PATH, "..\\..\\win32";

# Get all of the exsymtab test files
my @tests = glob("$number*.c");
for my $file (@tests) {
	my $file_to_print = $file;
	$file_to_print =~ s/^.*\\(\d+.*\.c$)/$1/;
	print "---- Running $file_to_print ----\n";
	system "gcc $file -I ..\\..\\win32\\libtcc -I . -I ..\\.. -L..\\..\\win32 -ltcc -o tcc-test.exe";
	system "tcc-test.exe lib_path=..\\..\\win32";
#	unlink "tcc-test.exe";
}

=head1 NAME

exsymtests.pl - run extended symbol table tests

=head1 DESCRIPTION

Producing the correct incantation to run the extended symbol table tests is
tricky. This Perl script does just that.

=head1 USAGE

To get a list of available extended symbol tests, type

 perl exsymtests.pl list

or simply

 perl exsymtests.pl

To run a particular test, give the test number, such as

 perl exsymtests.pl 20

To run all of the tests, give the argument C<all>:

 perl exsymtests.pl all

=cut

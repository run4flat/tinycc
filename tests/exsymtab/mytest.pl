use strict;
use warnings;

# Simplifies the invocation of tests by number only
my $cmd = shift;
die "Command must be 'prove', 'test', or 'dbg'\n"
	unless $cmd =~ /prove|test|dbg|valgrind/;

# I can either specify the test number or the whole file
# name.
my $file = shift;
$file = (glob("$file*.c"))[0] unless -f $file;
die "Unable to find test file\n" unless -f $file;

$file =~ s/\.c/.$cmd/;
system (make => $file)

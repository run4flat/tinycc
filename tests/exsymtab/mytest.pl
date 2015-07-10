use strict;
use warnings;

# Simplifies the invocation of tests by number only
my $cmd = shift;
die "Command must be 'prove', 'test', or 'dbg'\n"
	unless $cmd =~ /prove|test|dbg/;
my $test = shift;
my $file = (glob("$test*.c"))[0];
die "Unable to find test file for test number $test\n"
	unless -f $file;

$file =~ s/\.c/.$cmd/;
system (make => $file)

#!/usr/bin/perl
#
use strict;

open DATA, $ARGV[0] or die "Can't open file $ARGV[0]: $!\n";

my ($ip, $dtm, $method, $url);

while(<DATA>) {
	my @ar;
	my $ua;
	my @tmp;

	@ar = split ' ', $_;
	$ip = $ar[0];
	($dtm = $ar[3]) =~ s/[\[\]]+//g;
	($method = $ar[5]) =~ s/"//g;
	$url = $ar[6];

	@tmp = @ar[11 .. $#ar];
	$ua = join('_', @tmp);
	$ua =~ s/["-]+//g;

	if ($method =~ /GET|POST/) {
		print "\"$ip\",\"$dtm\",\"$url\", \"$ua\"\n";
	}
	$ip = $dtm = $method = $url = $ua = '';
}

close DATA;

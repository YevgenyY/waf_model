#!/usr/bin/perl
#
use strict;

open DATA, $ARGV[0] or die "Can't open file $ARGV[0]: $!\n";

my ($ip, $dtm, $method, $url, $tmp);

while(<DATA>) {
	my @ar = split ' ', $_;
	$ip = $ar[0];
	($dtm = $ar[3]) =~ s/[\[\]]+//g;
	($method = $ar[5]) =~ s/"//g;
	$url = $ar[6];

	if ($method =~ /GET|POST/) {
		print "$ip, $dtm, $method, $url\n";
	}
}

close DATA;

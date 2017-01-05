#!/usr/bin/perl
#
use strict;

open DATA, $ARGV[0] or die "Can't open file $ARGV[0]: $!\n";

open BOT, '>data/robots_ip.txt' or die "Can't open file robot_ip.txt: $!\n";

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

	$ip  =~ s/\"//g;
	$dtm =~ s/\"//g;
	$url =~ s/\"//g;
	$ua  =~ s/\"//g;

	$ua  = lc($ua);
	$url = lc($url);

	if ($method =~ /GET|POST/) {
		print "\"$ip\",\"$dtm\",\"$url\", \"$ua\"\n";
	}

	# Check if ua is bot or url is robot.txt
	if ($ua =~ /bot/) {
		print BOT "$ip\n";
	} elsif ($url =~ /robot\.txt/) {
		print BOT "$ip\n";
	}
	
	$ip = $dtm = $method = $url = $ua = '';
}

close BOT;
close DATA;

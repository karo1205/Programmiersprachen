#!/usr/bin/perl -w
use strict;
use warnings;

my $confFile;
my @fileContent;

sub proccedArguments() {
  if(@ARGV > 0 && $ARGV[0] =~ /conf$/) {
    $confFile = $ARGV[0];
  } else {
    die "You must define a config file!\n";
  }
}

sub openFile() {
  if(defined $confFile && -e $confFile) {
    open FILE, "<$confFile", or die &!;
  } else {
    print "Cannot find configuration file\n";
    die $!;
  }

#  @fileContent = <FILE>;
  while (<FILE>) { print $_; }
  
  close(FILE);
}

sub parseFile() {
  while (@fileContent) { print $_; }
}

&proccedArguments();
&openFile();
#&parseFile();

foreach my $arg (@ARGV) {
    print $arg, "\n";
}

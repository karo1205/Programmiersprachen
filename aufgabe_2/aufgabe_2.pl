#!/usr/bin/perl -w
use strict;
use warnings;

my $confFile;

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

  while (<FILE>) { &evalInput($_) }
  
  close(FILE);
}

sub evalInput() {
  my @split = split(' ', $_[0]);
  
  if(defined $split[0]) {
    if($split[0] =~ /^var$/) {
      &executeParam(@split);
    } else {
      &executeCmd(@split);
    }
  }
  
  
}

sub executeParam() {
  
}

sub executeCmd() {
  foreach my $value (@_) {
    my $status = system($value);
    return $status;
  }
   
}

&proccedArguments();
&openFile();
#&parseFile();

foreach my $arg (@ARGV) {
    print $arg, "\n";
}

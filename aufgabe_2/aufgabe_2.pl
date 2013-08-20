#!/usr/bin/perl -w
use strict;
use warnings;

my $confFile;
my %parameters;

sub proccedArguments() {
  my $index = 0;
  foreach my $argument (@ARGV) {
    if($index == 0) {
      if($ARGV[0] =~ /conf$/) {
	$confFile = $ARGV[0];
	$index++;
      } else {
	die "You must define a config file!\n";
      }
    } else {
      my $key = "cmdarg" . $index;
      $parameters{$key} = $argument;
      $index++;
    }
  }
}

sub execCmds() {
  if(defined $confFile && -e $confFile) {
    open FILE, "<$confFile", or die &!;
  } else {
    print "Cannot find configuration file\n";
    die $!;
  }

  while (<FILE>) {
    &evalInput($_);
  }
  
  close(FILE);
}

sub evalInput(@) {
  my @split = split(" ", $_[0]);
  
  if(defined $split[0]) {
    if($split[0] =~ /^var$/) {
      &executeParam(@split);
    } elsif($split[0] =~ /^alternative:$/) {
      
    } elsif($split[0] =~ /^sequence:$/) {
      
    } else {
      &executeCmd(@split);
    }
  }
}

sub executeParam(@) {
  $parameters{$_[1]} = $_[3];
}

sub replaceWithParam($) {
  foreach my $key (keys(%parameters)) {
    if($key eq $_[0]) {
      return $parameters{$key};
    }
  }
  return $_[0];
}

sub executeCmd(@) {
  my $index = 0;
  my $command;
  my @param;

  print "Split: " . @_ . "\n";
  foreach my $value (@_) {
    my $replaced = &replaceWithParam($value);
    push(@param, $replaced);
  }
  
  my $exec = join(" ", @param);
  my $result = `$exec 2>&1`;
    if($? < 0) {
      print "Executing command failed\n";
    } else {
      print "result: " . $result;
      return $0;
    } 
}

&proccedArguments();
&execCmds();

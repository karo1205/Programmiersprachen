#!/usr/bin/perl -w
use strict;
use warnings;

my $confFile;
my @fileLines;
my @actionStack = ("EMPTY");
my %parameters;

sub clearActionStack() {
  @actionStack = ("EMPTY");
}

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

sub readFile() {
  if(defined $confFile && -e $confFile) {
    open FILE, "<$confFile", or die &!;
    @fileLines = <FILE>;
    close FILE;
  } else {
    print "Cannot find configuration file\n";
    die $!;
  }
}

sub validateConf() {
  my $value;
  foreach my $line (@fileLines) {
    if($line =~ /^alternative:$/ || $line =~ /^sequence:$/) {
      push(@actionStack, $line);
    } elsif($line =~ /^:alternative$/ || $line =~ /^:sequence$/) {
      $value = pop(@actionStack);
      if($line =~ /^:sequence$/ && $value !~ /^sequence:$/) {
	die "Config file is not valid. False usage of action blocks";
      } elsif($line =~ /^:alternative$/ && $value !~ /^alternative:$/) {
	die "Config file is not valid. False usage of action blocks";
      }
    }
  }
  $value = pop(@actionStack);
  if($value ne "EMPTY") {
    die "Config file is not valid. Action block is open";
  }
}

sub execCmds() {
  foreach my $line (@fileLines) {
    &evalInput($line);
  }
}

sub evalInput(@) {
  my @split = split(" ", $_[0]);
  
  if(defined $split[0]) {
    if($split[0] =~ /^var$/) {
      &executeParam(@split);
    } elsif($split[0] =~ /^alternative:$/) {
      
    } elsif($split[0] =~ /^sequence:$/) {
      
    } elsif($split[0] =~ /^:alternative$/) {
      
    } elsif($split[0] =~ /^:sequence$/) {
    
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
&readFile();
&validateConf();
&execCmds();

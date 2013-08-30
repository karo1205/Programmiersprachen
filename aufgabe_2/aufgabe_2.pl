#!/usr/bin/perl -w
use strict;
use warnings;

my $confFile;
my @fileLines;
my @actionStack = ("EMPTY");
my %parameters;
my %quantityParameters;
my $returnValue;

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
      if($argument =~ /^\[/) {
	my $key = "cmdarg" . $index . "[]";
	my @par = &removeBraces($argument);
	$quantityParameters{$key} = \@par;
	$index++;
      } else {
	my $key = "cmdarg" . $index;
	$parameters{$key} = $argument;
	$index++;
      }
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
  @actionStack = ("EMPTY");
  foreach my $line (@fileLines) {
    &evalInput($line);
  }
  my $value = pop(@actionStack);
  if($value ne "EMPTY") {
    die "Something goes wrong";
  }
}

sub evalInput(@) {
  my @split = split(" ", $_[0]);
  
  if(defined $split[0]) {
    if($split[0] =~ /^var$/) {
      if($split[1] =~ /\[\]$/) {
	&executeMParam($_[0]);
      } else {
	&executeParam(@split);
      }
    } elsif($split[0] =~ /^alternative:$/) {
      my $lastAction = $actionStack[$#actionStack];
      if($lastAction eq "sequence:" && $returnValue == 0) {
	$returnValue = 1;
      } elsif($lastAction eq "sequence:" && $returnValue != 0) {
	$returnValue = 0;
      } elsif($lastAction eq "EMPTY") {
	$returnValue = 1;
      }
      push(@actionStack, $split[0]);
    } elsif($split[0] =~ /^sequence:$/) {
      my $lastAction = $actionStack[$#actionStack];
      if($lastAction eq "alternative:" && $returnValue != 0) {
	$returnValue = 0;
      } elsif($lastAction eq "alternative:" && $returnValue == 0) {
	$returnValue = 1;
      } elsif($lastAction eq "EMPTY") {
	$returnValue = 0;
      }
      push(@actionStack, $split[0]);
    } elsif($split[0] =~ /^:alternative$/) {
      pop(@actionStack);
    } elsif($split[0] =~ /^:sequence$/) {
      pop(@actionStack);
    } else {
      if($_[0] =~ /\[\]/) {
	&executeMCmd($_[0]);
      } else {
	&executeCmd(@split);
      }
    }
  }
}

sub executeParam(@) {
  my @split = split("/", $_[3]);
  my @tmpParam;
  foreach my $value (@split) {
    my $replaced = replaceWithParam($value);
    push(@tmpParam, $replaced);
  }
  my $tmp = join("/", @tmpParam);
  $parameters{$_[1]} = $tmp;

}

sub executeMParam(@) {
  my @splitEqual = split("=", $_[0]);
  my @split1 = split(" ", $splitEqual[0]);
  my @split2 = &removeBraces($splitEqual[1]);
  my $name = $split1[1];
  $quantityParameters{$name} = \@split2;
}

sub removeBraces(@) {
  my @split = split(/\[/, $_[0]);
  @split = split(/\]/, $split[1]);
  @split = split(" ", $split[0]);
  return @split;
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
  my @param;
  my $lastAction = $actionStack[$#actionStack];

  if($lastAction eq "EMPTY" || ($lastAction eq "sequence:" && $returnValue == 0) || ($lastAction eq "alternative:" && $returnValue != 0)) {
    foreach my $value (@_) {
      my @slashSplit = split("/", $value);
      my @tmpParam;
      foreach my $val (@slashSplit) {
	my $replaced = &replaceWithParam($val);
	push(@tmpParam, $replaced);
      }
      my $tmp = join("/", @tmpParam);
      push(@param, $tmp);
    }
  
    my $exec = join(" ", @param);
    $exec = "/bin/bash -c '" . $exec . "'";
    print "Executing: " . $exec . "\n";
    my $result = `$exec 2>&1`;
    $returnValue = $?;
    if($returnValue < 0 || $returnValue > 0) {
      print "Executing command failed: " . $result . "\n";
      return 1;
    } else {
      print "Success command execution: \n" . $result . "\n";
      return 0;
    } 
  }
}

sub executeMCmd(@) {
  my @split = split(" ", $_[0]);
  foreach my $key (keys(%quantityParameters)) {
    if($key eq $split[1]) {
      my $val = 0;
      my $ref = $quantityParameters{$key};
      foreach my $value (@$ref) {
	if($val != 0) {return;}
	my @tmp = ($split[0], $value);
	$val = &executeCmd(@tmp);
      }
    }
  }
}    

&proccedArguments();
&readFile();
&validateConf();
&execCmds();

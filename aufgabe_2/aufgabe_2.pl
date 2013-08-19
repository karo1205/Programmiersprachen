#!/usr/bin/perl -w
use strict;
use warnings;

my $confFile;
my %parameters = ();

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

  while (<FILE>) {
    &evalInput($_);
  }
  
  close(FILE);
}

sub evalInput(@) {
  my @split = split(' ', $_[0]);
  
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

sub replaceWithParam(@) {
  foreach my $key (keys(%parameters)) {
    if($key eq $_[0]) {
      return $parameters{$key};
    } else {
      return $_;
    }
  }
}

sub executeCmd(@) {
  my $index = 0;
  my $command;
  my @param;
  my $value;
  foreach $value (@_) {
    if($index eq 0) {
      $command = $value;
      $index++;
    } else {
      my $p = &replaceWithParam($value);
      print $p;
      push(@param, $p);
      $index++;
    }
  }
  
  foreach $value (@param) {
    $command = $command . " " . $value;
  }
  
  my $result = `$command 2>&1`;
    if($? < 0) {
      print "Executing command failed\n";
    } else {
      print $result;
      return $0;
    }
   
}

&proccedArguments();
&openFile();

# foreach my $arg (@ARGV) {
#     print $arg, "\n";
# }

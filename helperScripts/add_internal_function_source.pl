#! /usr/bin/perl -w

=head1 LICENCE

  Copyright (C) 2019-08-27 Stefan Lang

  This program is free software; you can redistribute it 
  and/or modify it under the terms of the GNU General Public License 
  as published by the Free Software Foundation; 
  either version 3 of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful, 
  but WITHOUT ANY WARRANTY; without even the implied warranty of 
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License 
  along with this program; if not, see <http://www.gnu.org/licenses/>.

=head1 CREATED BY
   
   binCreate.pl from  commit 
   

=head1  SYNOPSIS

    add_internal_function_source.pl


       -help           :print this help
       -debug          :verbose output
   
=head1 DESCRIPTION

  Add the internal function source file to every internal function call.

  To get further help use 'add_internal_function_source.pl -help' at the comman line.

=cut

use Getopt::Long;
use Pod::Usage;
use File::Copy;

use strict;
use warnings;

use FindBin;
my $plugin_path = "$FindBin::Bin";

my $VERSION = 'v1.0';


my ( $help, $debug, $database);

Getopt::Long::GetOptions(

	 "-help"             => \$help,
	 "-debug"            => \$debug
);

my $warn = '';
my $error = '';



if ( $help ){
	print helpString( ) ;
	exit;
}

if ( $error =~ m/\w/ ){
	helpString($error ) ;
	exit;
}

sub helpString {
	my $errorMessage = shift;
	$errorMessage = ' ' unless ( defined $errorMessage); 
	print "$errorMessage.\n";
	pod2usage(q(-verbose) => 1);
}



my ( $task_description, $path, $dir, $source, $line, @files );

$task_description .= 'perl '.$plugin_path .'/add_internal_function_source.pl';

$path = "../R/";
opendir( $dir, $path) or die "I can not open the R source fir ../R\n$!\n";
@files = grep ( /\.R$/, readdir($dir) );
closedir($dir);

foreach my $fname ( @files ) {
  print "source in file $fname\n";
  open ( IN, "<$path$fname") or die "I can not open the R source file $path$fname\n$!\n";
  while( <IN> ) {
    if ( $_ =~ m/setGeneric\(.(.*).,/) {
      print "found a generic: $1\n";
      $source -> {$1} = $fname;
    }
  }
  close ( IN );
}

## now we can dive through the source again
my $add;
#my $bad = { map { $_ => 1 } qw(rm for if rownames match stop print sapply apply exists numeric c nrow ncol message names rbind cbind )}
foreach my $fname ( @files ) {
  open ( IN, "<$path$fname") || die "I can not open the R source file $path$fname\n$!\n";
  open ( OUT, ">$path$fname.out.R") || die "I can not create the updated R source file $path$fname.out\n$!\n";
  while( <IN> ) {
    chomp();
    $add = 0;
    ## get all function calls like get.genes.cor.to( )
    foreach $line ( $_ =~ m/\s*([\w\.\_:]+)\s*\(/g ) {
      #print "we got an function name here: $line\n";
      if ( defined $source->{$line} ) {
        print "Function '$line' definined in file '$source->{$line}'\n";
        $add = $source->{$line};
        last;
      }
    }
    unless ( $add eq 0 ) {
      print OUT "$_ #function definition in file '$add'\n";
      if ( $debug ) {
        print "$_ #function definition in file '$add'\n";
      }
    }else {
      print OUT "$_\n";
    }
  }
  close ( IN );
}

#if ( ! $debug ) {
  ## replace the originals with the changed!
  foreach my $fname ( @files ) {
    move( "../R/$fname.out.R", "../R/$fname" )
  }
  #}else {
  #  print "Debug mode - check the .out files in the R source folder.\n";
  #}
## Do whatever you want!


#! /usr/bin/perl -w

=head1 LICENCE

  Copyright (C) 2017-04-03 Stefan Lang

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
   
   binCreate.pl from git@github.com:stela2502/Stefans_Lib_Esentials.git commit 8976a18c339e2885f28ff97a1210d805eeef87d7
   

=head1  SYNOPSIS

    convertR_S3_To_S4.pl


       -help           :print this help
       -debug          :verbose output
   
=head1 DESCRIPTION

  take all .R files in this folder and convert them to ../R/function_name.R s$ function definitions.

  To get further help use 'convertR_S3_To_S4.pl -help' at the comman line.

=cut

use Getopt::Long;
use Pod::Usage;

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



my ( $task_description);

$task_description .= 'perl '.$plugin_path .'/convertR_S3_To_S4.pl';


## Do whatever you want!

opendir ( my $folder, "$plugin_path" ) or die $!;

my @files = grep !/^\./,  grep '*\.R$/', readdir($folder);

my $className = "cellexalvrR";
my $outpath = $plugin_path."/outpath";
system( "mkdir -p $outpath") unless ( -d $outpath);
system( "rm -f $outpath/*" );
my $cmd;
for my $file ( @files ) {
	next unless ( $file =~ m/R$/ );
	## run convert_R_S3_R_S4.pl
	$cmd = "convert_R_S3_R_S4.pl "
		."-R_source $plugin_path/$file "
		."-outfile $outpath/$file "
		."-className $className";
	if ( $debug ) {
		print $cmd."\n";
	}else {
		warn "runnning \n$cmd\n";
		system( $cmd );
	}
	## run convert_R_S4_FileMess_into_function.pl
	$cmd = "convert_R_S4_FileMess_into_function.pl "
	."-infiles $outpath/$file "
	."-outpath $plugin_path/../R";
	if ( $debug ) {
		print $cmd."\n";
	}else {
		warn "runnning \n$cmd\n";
		system( $cmd );
	}
}

system( "rm -f $plugin_path/../R/*.log");

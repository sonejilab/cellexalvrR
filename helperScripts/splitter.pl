#! /usr/bin/perl -w

use warnings; use strict;
use File::Spec::Functions 'catfile';

#x_forceAbsoluteUniqueSample.R:setMethod('forceAbsoluteUniqueSample',

my $path = $ARGV[0];
$path = './'unless ( -d $path );

opendir (DIR,  $path );
my ($in, $data, @tmp);

foreach my $file ( readdir( DIR ) )  {
	next unless ($file =~m/.R$/);
	open ( $in, "<". catfile( $path, $file) ) or die "I could not open the file $file\n$!\n";
	while ( <$in> ) {
       	 #ontologyLogPage.R:knitr::kable(
      	 next if ( $_ =~m/^\s*#/);
       	 chomp();
       	 if ( $_ =~m/([\w_]+)::([\w_]+)\(/ ){
		$data->{$1} ||= {};
		$data->{$1}->{ $2 } ++;
	}
	}
}

closedir(DIR);

foreach my $key (sort keys %$data ) {
	print "#' \@importFrom $key ". join( " ", sort keys %{$data->{$key}} )."\n";
}


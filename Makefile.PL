use strict;
use warnings;

use inc::Module::Install;

my $dist = 'RDF-Everywhere';
my $fn   = "lib/$dist.pm"; $fn =~ s#-#/#g;

name                $dist;
perl_version_from   $fn;
version_from        $fn;
abstract_from       $fn;
readme_from         $fn;
author              'Toby Inkster <tobyink@cpan.org>';
license             'perl';

requires            'Data::UUID'         => 0;
test_requires       'Module::Signature'  => '0.66';
requires            'RDF::Trine'         => '0.130';
requires            'Scalar::Util'       => '1.00';
test_requires       'Test::More'         => '0.61';
requires            'URI'                => '1.30';

resources(
	'homepage'   => "http://search.cpan.org/dist/$dist/",
	'repository' => "http://goddamn.co.uk/viewvc/perlmods/$dist/",
	'bugtracker' => "http://rt.cpan.org/Dist/Display.html?Queue=$dist",
	);

keywords("Perl", "RDF", "SemWeb", "Semantic Web", "hashref", "class", "arrayref", "scalar", "UNIVERSAL");
	
write_doap_changes;
write_doap_changes_xml;

include 'Test::Signature';
auto_install;
WriteAll(
	'meta' => 1,
	'sign' => 1,
	);

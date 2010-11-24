use Test::More tests => 5;
BEGIN { use_ok('RDF::Everywhere') };

my $test = bless {}, QuuxFooBarBaz::Example::RE;

ok($test->can('rdf_node'), "Objects can rdf_node.");
ok($test->can('rdf_type'), "Objects can rdf_type.");
ok($test->can('to_rdf'), "Objects can to_rdf.");
ok($test->rdf_node->is_blank, "Our example object is a blank node.");

package QuuxFooBarBaz::Example::RE;
1;

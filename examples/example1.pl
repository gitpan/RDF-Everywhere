#!/usr/bin/perl

use lib "lib";
use RDF::Everywhere qw[rdf_node to_rdf];
use DateTime;
use Set::Array;
use URI;

my $resp = LWP::UserAgent->new->get("http://www.example.com/");

my @test;
push @test, {};
push @test, bless { '@about' => 'ex:Foo' , bar=>'baz', quux=>[1,2] }, 'Test::Bumbo';
push @test, bless { xyzzy => 'x' }, 'Test::Bumbo';
push @test, DateTime->now;
push @test, "Hello world";
push @test, $resp->headers;

$test[1]->{who} = $test[2];
$test[1]->{when} = $test[3];
$test[2]->{why} = $test[1];

print to_rdf({
	'@about' => 'http://tobyinkster.co.uk/#i',
	name => 'Toby Inkster',
	'foaf:homepage' => URI->new('http://tobyinkster.co.uk/'),
	'foaf:made' => \@test,
	array => Set::Array->new(1,2,3),
	empty => Set::Array->new(),
	single => Set::Array->new('garb'),
}, as=>'turtle', recurse=>1);

package Test::Bumbo;

1;

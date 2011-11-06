#!/usr/bin/perl

use lib "lib";
use RDF::Everywhere qw[to_rdf -universal];
use LWP::UserAgent;

print LWP::UserAgent->new->get("http://www.example.com/")->headers->to_rdf(as=>'RDFXML');

print to_rdf { foo=>1, bar=>2, baz=>[3,4,5], quux=>{xyzzy=>99} }, (as => 'Turtle');
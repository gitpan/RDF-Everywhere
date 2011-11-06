package RDF::Everywhere;

use 5.010;
use strict;
use utf8;

use Data::UUID 0 qw();
use Object::ID 0 qw(object_id);
use RDF::Trine 0 qw();
use Scalar::Util 0 qw(blessed reftype);
use Moan qw(assert);

our $AUTHORITY;
our $context;
our @EXPORT_OK;
our %mappings;
our @object_converters;
our %PRAGMATA;
our %terms;
our @node_converters;
our $VERSION;

BEGIN
{
	$AUTHORITY = 'cpan:TOBYINK';
	$VERSION   = '0.002';

	@EXPORT_OK = qw(rdf_node to_rdf rdf_type);
	
	%PRAGMATA = (
		universal => sub {
			*UNIVERSAL::rdf_node  = \&rdf_node;
			*UNIVERSAL::rdf_type  = \&rdf_type;
			*UNIVERSAL::to_rdf    = \&to_rdf;
			},
		);
		
	%mappings = (
		owl	=> "http://www.w3.org/2002/07/owl#",
		rdf	=> "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
		rdfs	=> "http://www.w3.org/2000/01/rdf-schema#",
		rdfa	=> "http://www.w3.org/ns/rdfa#",
		xhv	=> "http://www.w3.org/1999/xhtml/vocab#",
		xml	=> "http://www.w3.org/XML/1998/namespace",
		xsd	=> "http://www.w3.org/2001/XMLSchema#",
		grddl	=> "http://www.w3.org/2003/g/data-view#",
		powder	=> "http://www.w3.org/2007/05/powder#",
		powders	=> "http://www.w3.org/2007/05/powder-s#",
		rif	=> "http://www.w3.org/2007/rif#",
		atom	=> "http://www.w3.org/2005/Atom",
		xhtml	=> "http://www.w3.org/1999/xhtml/vocab#",
		formats	=> "http://www.w3.org/ns/formats/",
		xforms	=> "http://www.w3.org/2002/xforms/",
		xhtmlvocab	=> "http://www.w3.org/1999/xhtml/vocab#",
		xpathfn	=> "http://www.w3.org/2005/xpath-functions#",
#		http	=> "http://www.w3.org/2006/http#",
		link	=> "http://www.w3.org/2006/link#",
		time	=> "http://www.w3.org/2006/time#",
		acl	=> "http://www.w3.org/ns/auth/acl#",
		cert	=> "http://www.w3.org/ns/auth/cert#",
		rsa	=> "http://www.w3.org/ns/auth/rsa#",
		crypto	=> "http://www.w3.org/2000/10/swap/crypto#",
		list	=> "http://www.w3.org/2000/10/swap/list#",
		log	=> "http://www.w3.org/2000/10/swap/log#",
		math	=> "http://www.w3.org/2000/10/swap/math#",
		os	=> "http://www.w3.org/2000/10/swap/os#",
		string	=> "http://www.w3.org/2000/10/swap/string#",
		doc	=> "http://www.w3.org/2000/10/swap/pim/doc#",
		contact	=> "http://www.w3.org/2000/10/swap/pim/contact#",
		p3p	=> "http://www.w3.org/2002/01/p3prdfv1#",
		swrl	=> "http://www.w3.org/2003/11/swrl#",
		swrlb	=> "http://www.w3.org/2003/11/swrlb#",
		exif	=> "http://www.w3.org/2003/12/exif/ns#",
		earl	=> "http://www.w3.org/ns/earl#",
		ma	=> "http://www.w3.org/ns/ma-ont#",
		sawsdl	=> "http://www.w3.org/ns/sawsdl#",
		sd	=> "http://www.w3.org/ns/sparql-service-description#",
		skos	=> "http://www.w3.org/2004/02/skos/core#",
		fresnel	=> "http://www.w3.org/2004/09/fresnel#",
		gen	=> "http://www.w3.org/2006/gen/ont#",
		timezone	=> "http://www.w3.org/2006/timezone#",
		skosxl	=> "http://www.w3.org/2008/05/skos-xl#",
		org	=> "http://www.w3.org/ns/org#",
		ical	=> "http://www.w3.org/2002/12/cal/ical#",
		wgs84	=> "http://www.w3.org/2003/01/geo/wgs84_pos#",
		vcard	=> "http://www.w3.org/2006/vcard/ns#",
		turtle	=> "http://www.w3.org/2008/turtle#",
		pointers	=> "http://www.w3.org/2009/pointers#",
		dcat	=> "http://www.w3.org/ns/dcat#",
		imreg	=> "http://www.w3.org/2004/02/image-regions#",
		rdfg	=> "http://www.w3.org/2004/03/trix/rdfg-1/",
		swp	=> "http://www.w3.org/2004/03/trix/swp-2/",
		rei	=> "http://www.w3.org/2004/06/rei#",
		wairole	=> "http://www.w3.org/2005/01/wai-rdf/GUIRoleTaxonomy#",
		states	=> "http://www.w3.org/2005/07/aaa#",
		wn20schema	=> "http://www.w3.org/2006/03/wn/wn20/schema/",
		httph	=> "http://www.w3.org/2007/ont/httph#",
		act	=> "http://www.w3.org/2007/rif-builtin-action#",
		common	=> "http://www.w3.org/2007/uwa/context/common.owl#",
		dcn	=> "http://www.w3.org/2007/uwa/context/deliverycontext.owl#",
		hard	=> "http://www.w3.org/2007/uwa/context/hardware.owl#",
		java	=> "http://www.w3.org/2007/uwa/context/java.owl#",
		loc	=> "http://www.w3.org/2007/uwa/context/location.owl#",
		net	=> "http://www.w3.org/2007/uwa/context/network.owl#",
		push	=> "http://www.w3.org/2007/uwa/context/push.owl#",
		soft	=> "http://www.w3.org/2007/uwa/context/software.owl#",
		web	=> "http://www.w3.org/2007/uwa/context/web.owl#",
		content	=> "http://www.w3.org/2008/content#",
		vs	=> "http://www.w3.org/2003/06/sw-vocab-status/ns#",
		air	=> "http://dig.csail.mit.edu/TAMI/2007/amord/air#",
		ex	=> "http://example.org/",
		dc	=> "http://purl.org/dc/terms/",
		dc11	=> "http://purl.org/dc/elements/1.1/",
		dctype	=> "http://purl.org/dc/dcmitype/",
		foaf	=> "http://xmlns.com/foaf/0.1/",
		cc	=> "http://creativecommons.org/ns#",
		opensearch	=> "http://a9.com/-/spec/opensearch/1.1/",
		'void'	=> "http://rdfs.org/ns/void#",
		sioc	=> "http://rdfs.org/sioc/ns#",
		sioca	=> "http://rdfs.org/sioc/actions#",
		sioct	=> "http://rdfs.org/sioc/types#",
		lgd	=> "http://linkedgeodata.org/vocabulary#",
		moat	=> "http://moat-project.org/ns#",
		days	=> "http://ontologi.es/days#",
		giving	=> "http://ontologi.es/giving#",
		lang	=> "http://ontologi.es/lang/core#",
		like	=> "http://ontologi.es/like#",
		status	=> "http://ontologi.es/status#",
		og	=> "http://opengraphprotocol.org/schema/",
		protege	=> "http://protege.stanford.edu/system#",
		dady	=> "http://purl.org/NET/dady#",
		uri	=> "http://purl.org/NET/uri#",
		audio	=> "http://purl.org/media/audio#",
		video	=> "http://purl.org/media/video#",
		gridworks	=> "http://purl.org/net/opmv/types/gridworks#",
		hcterms	=> "http://purl.org/uF/hCard/terms/",
		bio	=> "http://purl.org/vocab/bio/0.1/",
		cs	=> "http://purl.org/vocab/changeset/schema#",
		geographis	=> "http://telegraphis.net/ontology/geography/geography#",
		doap	=> "http://usefulinc.com/ns/doap#",
		daml	=> "http://www.daml.org/2001/03/daml+oil#",
		geonames	=> "http://www.geonames.org/ontology#",
		sesame	=> "http://www.openrdf.org/schema/sesame#",
		cv	=> "http://rdfs.org/resume-rdf/",
		wot	=> "http://xmlns.com/wot/0.1/",
		media	=> "http://purl.org/microformat/hmedia/",
		ctag	=> "http://commontag.org/ns#",
		perl	=> "http://wiki.ontologi.es/perl-object#",
		blessed	=> "http://purl.org/NET/cpan-uri/class/",
		);
	
	@node_converters = (
		[ 'URI'        => \&__convert_URI ],
		[ 'DateTime'   => \&__convert_DateTime ],
		[ 'Set::Array' => \&__convert_Set_Array ],
		[ 'RDF::Trine::Node'   => \&__convert_RDF_Trine_Node ],
		);
	
	@object_converters = (
		[ 'Set::Array' => \&__to_rdf_Set_Array ],
		);
	
	%terms = (
		label  => 'rdfs:label',
		name   => 'foaf:name',
		);
}

BEGIN { ($context = Data::UUID->new->create_str) =~ s/\-//g; }

use Pragmatic qw();
use base qw(Exporter);
sub import { goto &Pragmatic::import; }

sub define_mapping ($$)
{
	shift if $_[0] eq __PACKAGE__;
	my ($pfx, $uri) = @_;
	$mappings{$pfx} = $uri;
}

sub define_term ($$)
{
	shift if $_[0] eq __PACKAGE__;
	my ($term, $uri) = @_;
	$terms{$term} = $uri;
}

sub install_object_converter (&@)
{
	shift if $_[0] eq __PACKAGE__;
	my ($callback, @class) = @_;
	assert(ref $callback eq 'CODE', 'callback is not a coderef', ':callback');
	push @object_converters, map { [$_ => $callback] } @class;
}

sub install_node_converter (&@)
{
	shift if $_[0] eq __PACKAGE__;
	my ($callback, @class) = @_;
	assert(ref $callback eq 'CODE', 'callback is not a coderef', ':callback');
	push @node_converters, map { [$_ => $callback] } @class;
}

sub rdf_node (*;%)
{
	my ($self, %options) = @_;
	
	if (blessed($self)
	and $self->can('rdf_node')
	and !$options{loop})
	{
		return $self->rdf_node(%options, loop => 1);
	}
	
	if (blessed($self))
	{
		foreach my $cnv (@node_converters)
		{
			my ($type, $code) = @$cnv;
			if ($self->isa($type))
			{
				my $r = $code->($self);
				return $r if defined $r;
			}
		}
	}
	
	if (reftype($self) eq 'HASH'
	and defined $self->{'@about'})
	{
		my $r = _resource( $self->{'@about'} );
		return $r if defined $r;
	}
	
	if (ref($self))
	{
		return _blank(object_id($self));
	}
	
	return RDF::Trine::Node::Literal->new("$self");
}

sub rdf_type (*;%)
{
	my ($self, %options) = @_;
	
	if (blessed($self)
	and $self->can('rdf_type')
	and !$options{loop})
	{
		return $self->rdf_type(%options, loop => 1);
	}
	
	if (reftype($self) eq 'HASH'
	and defined $self->{'@type'})
	{
		my $r = _resource( $self->{'@type'} );
		return $r if defined $r;
	}
	
	if (blessed($self))
	{
		return _resource('http://purl.org/NET/cpan-uri/class/' . ref($self));
	}
	
	return;
}

sub to_rdf (*;%)
{
	my ($self, %options) = @_;
	
	if (blessed($self)
	and $self->can('to_rdf')
	and !$options{loop})
	{
		return $self->to_rdf(%options, loop => 1);
	}
	
	my $model = _to_rdf($self, %options);
	if ($options{as})
	{
		my $ser = RDF::Trine::Serializer->new($options{as}, namespaces=>\%mappings);
		return $ser->serialize_model_to_string($model);
	}
	
	return $model;
}

sub _to_rdf
{
	my ($self, %options) = @_;
	my $model = $options{model} ||= RDF::Trine::Model->new;
	$options{visited} ||= {};
	
	$options{recurse} = 1
		unless defined $options{recurse};
		
	my $node = rdf_node($self);
	if ($node->is_literal)
	{
		return $model; # literals are self-describing.
	}
	
	if ($options{visited}->{$node->as_ntriples})
	{
		return $model;
	}
	
	$options{visited}->{$node->as_ntriples}++;
	
	if (blessed($self))
	{
		foreach my $cnv (@object_converters)
		{
			my ($type, $code) = @$cnv;
			if ($self->isa($type))
			{
				my $r = $code->($self, %options);
				return $r if defined $r;
			}
		}
	}
	
	my $type = rdf_type($self);
	$model->add_statement(
		RDF::Trine::Statement->new($node, _resource('rdf:type'), $type)
		) if defined $type;
	
	if (reftype($self) eq 'HASH')
	{
		while (my ($k, $v) = each %$self)
		{
			next if $k =~ /^[\@\_\-]/;
			my $p = _resource($k, vocab => 'http://wiki.ontologi.es/perl-object#');
			
			$v = [$v] unless ref $v eq 'ARRAY';
			foreach my $v1 (@$v)
			{
				my $o = rdf_node($v1);
				$model->add_statement(
					RDF::Trine::Statement->new($node, $p, $o)
					);
				
				if ($options{recurse}
				and not $o->is_literal)
				{
					_to_rdf($v1, %options);
				}
			}
		}
	}
	
	return $model;
}

sub __convert_DateTime
{
	my ($self) = @_;
	return RDF::Trine::Node::Literal->new("$self", undef, 'http://www.w3.org/2001/XMLSchema#dateTime');
}

sub __convert_URI
{
	my ($self) = @_;
	return RDF::Trine::Node::Resource->new("$self");
}

sub __convert_Set_Array
{
	my ($self) = @_;
	return _resource('rdf:nil') unless $self->length;
	return;
}

sub __convert_RDF_Trine_Node
{
	return $_[0];
}

sub __to_rdf_Set_Array
{
	my ($self, %options) = @_;
	
	my $model = $options{model} ||= RDF::Trine::Model->new;
	$options{visited} ||= {};
		
	my $node = rdf_node($self);
	if ($node->is_literal)
	{
		return $model; # should never happen.
	}
	
	if ($self->length)
	{
		my $rest; # closure
		$rest	= sub {
			my $i = shift;
			
			return _resource('rdf:nil') unless $i < $self->length;
			
			my $ident = _blank(object_id($self), $i);
			$model->add_statement(
				RDF::Trine::Statement->new($ident, _resource('rdf:first'), rdf_node($self->at($i)))
				);
			$model->add_statement(
				RDF::Trine::Statement->new($ident, _resource('rdf:rest'), $rest->($i + 1))
				);
			return $ident;
		};
		
		$model->add_statement(
			RDF::Trine::Statement->new($node, _resource('rdf:first'), rdf_node($self->first))
			);
		$model->add_statement(
			RDF::Trine::Statement->new($node, _resource('rdf:rest'), $rest->(1))
			);
	}
	
	return $model;
}

sub _resource
{
	my ($uri, %options) = @_;
	
	if ($uri =~ /^([A-Za-z0-9-+\._]+):(.*)$/)
	{
		my ($pfx, $sfx) = ($1, $2);
		
		if (defined $mappings{$pfx})
		{
			return RDF::Trine::Node::Resource->new($mappings{$pfx} . $sfx);
		}
		else
		{
			return RDF::Trine::Node::Resource->new("$uri");
		}
	}
	
	if (defined $terms{lc $uri})
	{
		return _resource($terms{lc $uri});
	}
	
	return RDF::Trine::Node::Resource->new($options{vocab} . "$uri");
}

sub _blank
{
	my @parts = @_;
	unshift @parts, $context;
	my $ident = join 'x', @parts;
	return RDF::Trine::Node::Blank->new('bn' . $ident);
}

1;

__END__

=head1 NAME

RDF::Everywhere - makes everything in your Perl code an RDF resource.

=head1 SYNOPSIS

  use RDF::Everywhere qw(-universal);
  
  # Methods are installed in UNIVERSAL, so can be called on any
  # blessed object.
  $headers = LWP::UserAgent->new->get('http://example.com/')->headers;
  printf("## %s\n", $headers->rdf_node->as_ntriples);
  print $headers->to_rdf(as => 'Turtle');
  
  use RDF::Everywhere qw[rdf_node to_rdf];
  
  # They can also be used as functions, which is useful when calling
  # on unblessed variables.
  print to_rdf {
    name      => 'Toby Inkster',
    cpanid    => 'TOBYINK',
    page      => URI->new('http://search.cpan.org/~tobyink/'),
    dist      => [
        { name => 'RDF-Everywhere',     version => '0.001' },
        { name => 'RDF-TrineShortcuts', version => '0.102' },
      ],
    }, as => 'RDFXML';

=head1 DESCRIPTION

I'd been thinking of doing something like this for a while, and then Nathan
Rixham went and wrote a Javascript version, which spurred me to do it for
Perl.

The idea is that RDF's graph data model is not dissimilar to Perl's data
model, so all Perl objects get a C<to_rdf> method which returns an RDF
equivalent of the object's data.

=head2 Exportable Functions

No functions exported by default - request them explicitly...

 use RDF::Everywhere qw[to_rdf];

=over

=item C<< rdf_node($object) >>

Returns an L<RDF::Trine::Node> object identifying the object. This will usually
be an L<RDF::Trine::Node::Blank> but occasionally a L<RDF::Trine::Node::Resource>
or even a L<RDF::Trine::Node::Literal>.

=item C<< rdf_type($object) >>

Returns an L<RDF::Trine::Node::Resource> object identifying the type of object.

=item C<< to_rdf($object, %options) >>

Returns an L<RDF::Trine::Model> object containing data about the object. The
three options worth bothering about are:

=over

=item B<as>: instead of returning a Model, return a string formatted using
this serialisation. e.g. C<< as=>'Turtle' >>. Accepts the same serialisations
as C<< RDF::Trine::Serializer->new >> (e.g. 'Turtle', 'RDFXML', 'NTriples', 'RDFJSON').

=item B<recurse>: also include data about "child" objects. This is smart
enough to avoid infinite loops (or should be). Defaults to true, but
C<< recurse=>0 >> can be passed to force the method to avoid recursion.

=item B<model>: an existing model to add data to. This is expected to be a
blessed object which has an C<add_statement> method accepting an
RDF::Trine::Statement. An RDF::Trine::Model would be what you'd normally
provide.

=back

=back

=head2 Universal Methods

You can make the functions available as UNIVERSAL methods:

 use RDF::Everywhere qw[-universal];

=head2 Config Functions

These are not exportable:

=over

=item C<< RDF::Everywhere::define_term $term => $uri >>

Instructs RDF::Everywhere to expand:

 $hash->{$term} = "value";

to:

 [] <$uri> "value" .

=item C<< RDF::Everywhere::define_mapping $prefix => $uri >>

Instructs RDF::Everywhere to expand:

 $hash->{$prefix . "suffix"} = "value";

to:

 [] <${prefix}suffix> "value" .

=item C<< RDF::Everywhere::install_object_converter { CODEBLOCK } $class1, $class2 ... >>

Provide a callback to handle RDF conversion for particular classes.
This is a coderef that takes an object and returns an RDF::Trine::Model
describing it.

The callback can return undef to indicate lack of success.

=item C<< RDF::Everywhere::install_node_converter { CODEBLOCK } $class1, $class2 ... >>

Provide a callback to handle RDF conversion for particular classes.
This is a coderef that takes an object and returns an RDF::Trine::Node
for it. This is mostly useful for things that can be converted to a
single literal - e.g. a DateTime object into an xsd:dateTime.

The callback can return undef to indicate lack of success.

=back

=head1 HOW IT WORKS

I<Advanced usage information.>

=head2 How are Perl variables converted to RDF::Trine::Nodes?

Blessed objects can provide their own rdf_node method, which will be
used if available.

Firstly, the list of node converters is checked for a converter function
that can handle it. If an entry in the list applies to the variable being
converted, it is use to perform the conversion.

Node converters are pre-defined for C<DateTime>, C<URI>, C<Set::Array> and
C<RDF::Trine::Node>.

Failing that, if the variable being converted is a hashref (including
blessed hashrefs) with a key called C<< '@about' >> then that is used as
a CURIE or URI to generate a L<RDF::Trine::Node::Resource>.

Otherwise, all references are taken to be anonymous blank nodes, and scalars
taken to be plain literals.

=head2 How the RDF type of a Perl variable is decided?

Blessed objects can provide their own rdf_type method, which will be
used if available.

If the variable being converted is a hashref (including blessed hashrefs)
with a key called C<< '@type' >> then that is used as a CURIE or URI to
generate a L<RDF::Trine::Node::Resource>.

Otherwise, if the variable is a blessed object, the type is the URI formed
by concatenating C<http://purl.org/NET/cpan-uri/class/> with the package
name the variable is blessed into.

Otherwise, it has no type.

=head2 How is RDF data for a variable generated?

Blessed objects can provide their own to_rdf method, which will be
used if available.

Firstly, the list of object converters is checked for a converter function
that can handle it. If an entry in the list applies to the variable being
converted, it is use to perform the conversion, and the result is returned.

A converter is pre-defined for <Set::Array> (treating it as an rdf:List).

If nothing has been returned so far, a new model is created. A triple is
added to it stating the rdf:type of the object (if any).

If the variable being converted is a hashref (including blessed hashrefs)
it is iterated through as a set of predicate (CURIE) => object pairs.
If the object is an arrayref this is treated as multiple triples, and not
as an rdf:List (use L<Set::Array> for rdf:Lists).

=head2 How does CURIE mapping work?

Tokens without a colon are looked up in the list of defined tokens.
If not found, for predicates a default prefix
'http://wiki.ontologi.es/perl-object#' is used; for non-predicates,
a relative URI.

Tokens with a colon are treated as a CURIE according to the defined
mappings, or a URI if no mapping is defined.

=head1 SEE ALSO

L<RDF::Trine>, L<UNIVERSAL>.

L<http://www.perlrdf.org/>.

=head1 BUGS

Please report any bugs to L<http://rt.cpan.org/>.

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT

Copyright 2010-2011 Toby Inkster

This library is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

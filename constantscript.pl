#!/usr/bin/perl
#
use XML::DOM;
use Getopt::Long;

my $parser = new XML::DOM::Parser;
$mode = "overwrite";
GetOptions("help" => sub { print <<EOF
A script to correct malformed mathml constant elements in cellml files.
Usage: constantscript.pl [flags] [files]
Where flags can be
  -h Prints this message
  -p Print the output of processing the files to the console
  -o Overwrite the files with their processed content. This is the default.
EOF
  },
           "print" => sub { $mode = "print"; },
           "overwrite" => sub { $mode = "overwrite" });

foreach $argnum (0 .. $#ARGV)
{
  my $doc = $parser->parsefile($ARGV[$argnum]);

  my $nodes = $doc->getElementsByTagName("cn");
  my $n = $nodes->getLength;

  for (my $i = 0; $i < $n; $i++)
  {
    my $node = $nodes->item($i);

    my $children = $node->getChildNodes();
    my $m = $children->getLength;
    unless ($m == 1)
    {
      next;
    }
    my $child = $children->item(0);

    my $text = $child->toString();
    unless ($text =~ /[\+\-]?[0-9]+(\.[0-9]+)?[Ee][\+\-]?[0-9]+(\.[0-9]+)?/) { next; }
    ($l, $r) = split(/[eE]/, $text);
    $node->removeChild($child);
    $node->addText($l);
    $node->setAttribute("type", "e-notation");
    $sep = $doc->createElement("sep");
    $node->appendChild($sep);
    $node->addText($r);
  }
  if ($mode eq "print")
  { print $doc->toString(); };
  if ($mode eq "overwrite")
  { $doc->printToFile($ARGV[$argnum]); }
  $doc->dispose;

}



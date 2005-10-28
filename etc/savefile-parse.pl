#!/usr/bin/perl -w
######
#
# Parse a mudmode (save_value) string
#
# A single variable value is parsed and represented as perl data structure.
# So a string like '({"a",3,(["b":2,]),})' gets ['a',3,{'b'=>2}]
#
# Fiona 23-Mai-03
#

use Data::Dumper;

$parse_err = undef; # Global error messages

%unquote = (
  'r' => "\r",
  'f' => "\f",
  'v' => "\v",
  'n' => "\n",
  't' => "\t",
  'b' => "\b",
  'a' => "\a",
);

# parse a single value
#
# Knows: int, string, array, float, mapping
# (Todo: ref_to_array/mapping, closures)
# 
# arguments: source string
#            reference to start index of value
# returns value in perl
# set start index to next char (if any) or -1
#
# If an error occured -1 will be returned and $parse_err set

sub get_value {
  my ($offsp, $str, $ret, $ret2, $data);
  $str = $_[0];
  $offsp = $_[1];

  # check for float
  #
  # [German:Koresh@OuterSpace] OS would encode it as  #-1.00000001e-01#  but
  #         not sure everybody does it this way.
  # [German:Fiona] means ({-0.1}) becomes ({#-1.00000001e-01#,})  ?
  # [German] Koresh@OuterSpace nods wildly
  # [German] Koresh@OuterSpace also notes that mudos does not seem  to include
  #         the # in floats: it just writes it as ({-1.00000e-08,})
  #
  # LDmud float
  # -9.999999997672e-02=fffd:9999999a

  pos($str) = $$offsp;
  $str =~ m/\G#?(-?\d.\d*[eE][-+]\d\d)(#|=[0-9a-f:]+)?/sg;
  if (defined $1) {
    $$offsp = pos($str);
    return $1;
  }

  # check for int (check *after* floats)
  pos($str) = $$offsp;
  $str =~ m/\G(-?\d+)/sg;
  if (defined $1) {
    $$offsp = pos($str);
    return $1;
  }

  # check for string
  pos($str) = $$offsp;
  if ($str =~ m/\G"((?:[^"\\]+|\\.)*)"/sg ) {
    $$offsp = pos($str);
    # unquote string
    $ret = $1;
    $ret =~ s/\\(.)/$unquote{$1}||$1/esg;
    return $ret;
  }

  # check for array
  pos($str) = $$offsp;
  if ($str =~ m/\G\({/sg ) {
    $$offsp = pos($str);
    $data = [];

    if ($str =~ m/\G}\)/sg ) {
      $$offsp += 2;
      return $data;
    }

    # recurse array
    while (1) {
      $ret = get_value($str, $offsp);
      return -1 if ($parse_err || $offsp == -1);

      push @{$data}, $ret;

      pos($str) = $$offsp;
      if (not ($str =~ m/\G,/sg )) {
        pos($str) = $$offsp;
        $str =~ m/\G(.{0,8})/sg;
        $parse_err = "Comma missing in array near '$1'";
	return -1;
      }
      $$offsp += 1;

      if ($str =~ m/\G}\)/sg ) {
        $$offsp += 2;
        return $data;
      }
    }
    # notreached
  }

  # check for mapping
  #
  # MudOS has no 2dimensional Mappings, so we dont support them
  # (perl is unable to represent them anyhow)
  # Zero-width mappings are impossible with mudmode/perl, so emulate them.

  pos($str) = $$offsp;
  if ($str =~ m/\G\(\[/sg ) {
    $$offsp = pos($str);
    $data = {};

    if ($str =~ m/\G]\)/sg ) {
      $$offsp += 2;
      return $data;
    }

    # recurse array
    while (1) {
      $ret = get_value($str, $offsp);
      return -1 if ($parse_err || $offsp == -1);


      pos($str) = $$offsp;
      if (not ($str =~ m/\G[:,]/sg )) {
        pos($str) = $$offsp;
        if ($str =~ m/\G;/sg ) {
          $parse_err = 'Multidimensional mapping not supported near ';
        } else {
          $parse_err = 'Colon missing in mapping near ';
        }
        pos($str) = $$offsp;
        $str =~ m/\G(.{0,8})/sg;
        $parse_err = $parse_err . "'$1'";
	return -1;
      }
      $$offsp += 1;

      pos($str) = $$offsp-1;
      if ($str =~ m/\G,/sg ) { # zero width, gets simulated...
        $ret2 = 0;
      } else {
        $ret2 = get_value($str, $offsp);
        return -1 if ($parse_err || $offsp == -1);

        pos($str) = $$offsp;

        if (not ($str =~ m/\G,/sg )) {
          pos($str) = $$offsp;
          $str =~ m/\G(.{0,8})/sg;
          $parse_err = "Comma missing in mapping near '$1'";
	  return -1;
        }
        $$offsp += 1;
      }

      $$data{$ret} = $ret2;

      if ($str =~ m/\G]\)/sg ) {
        $$offsp += 2;
        return $data;
      }
    }
    # notreached
  }

  # check for parse error
  pos($str) = $$offsp;
  $str =~ m/\G(.{1,8})/sg;
  if (defined $1) {
    $parse_err = "Unknown variable type near '$1'";
    return -1;
  }

  # end of string
  $$offsp = -1;
  return -1;
}

######
#
# Sample usage
#

$str = '({1234,45,({12,-1,({0,}),}),"fla\nb\"ber",-1.110999999642e+02=7:90e66667,({}),(["b":"a","c":({1,-2,}),]),([1,2,3,]),})';

#$str='({"error",5,"*gjs",0,"Test-PerlMud-i3",0,"unk-src","router does not know you",({"startup-req-3",5,"Test-PerlMud-i3",0,"",0,1016,4156424,330,0,0,0,"PerlLib v1","PerlLib v1","Perlv5.6.1","Perl","restricted access","castaway@desert-island.m.isar.de",(["channel":1,"nntp":0,"file":0,"amcp":0,"rcp":0,"emoteto":1,"auth":0,"who":1,"smtp":0,"mail":0,"finger":1,"ucache":1,"locate":1,"news":0,"http":0,"ftp":0,"tell":1,]),0,}),})';

$posi = 0;
$ret = get_value($str, \$posi);

if ($parse_err) {
  print("ERROR $parse_err\n");
} else {
  print(Dumper($ret));
}

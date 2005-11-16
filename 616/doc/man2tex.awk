#! /usr/local/bin/gawk -f
# a simple example script to convert a manpage into a LaTeX section
# ToDo:
# - preserve line breaks and indentation in Examples
# - generate index entries for the section names
# - generate cross references for the See Also entries.

## The behaviour of GNU awk was changed somewhere between versions 2.15.0
## and 2.15.5, regarding the backslash and & in the gsub() invocations
## below. If you get ampersands instead of backslash-underscores in
## the output, you need to experiment with the second arg of the gsub()s,
## maybe add two more backslashes (making 6 in total)
## Pepel sighs deeply
##

# firts, substitute electrical characters
		{ gsub(/\\/,"$\\backslash$");
		  gsub(/[_{}%&\#\^]/, "\\\\&");
		  gsub(/[<>]/,"$&$");
##		  gsub("\"","\\quote+\"+");
#		  gsub("\{", "\\{");
#		  gsub("\}", "\\}");
#		  print $0;
		}

/^NAME|^CONCEPT/	{ state = "name";
			  # getline line;
			  # split(line, name, "[ \t]+");
			  # gsub(/[_{}%&]/, "\\\\&", name[1]);
			  # printf("\\section{%s\\label{%s-section}}\n",
			  #	   name[1], name[1]);
			  # print line "\\\\";
			  next;
			}
/^SYNOPSIS/	{ state = "syn";
		  # getline line; name = line;
#		  ## gsub(/[ \t]+/," ",name);
#		  ## print l;
		  # sub(/^[ \t]*[a-z]+[ \t]+\**/,"", name);
		  # sub(/\([^)]*\)/,"()", name);
		  # sub(/\).*$/,")", name);
		  # gsub(/[_{}%&]/, "\\\\&", name);
		  # gsub(/[_{}%&]/, "\\\\&", line);
#		  # print name;
		  # printf("\\section{%s\\label{%s-section}}\n", name, name);
		  # print line "\\\\"
		  next;
		}
/^LAST UPDATE/	{ state = "X"; print "\\subsection*{Last Update}\n"; next; }
/^SEE ALSO/	{ state = "see"; print "\\subsection*{See Also}\n"; next; }
/^EXAMPLE/	{ state = "exm"; # EXAMPLE or EXAMPLES
		  printf("\\subsection*{%s%s}\n",
			 toupper(substr($1, 1, 1)),
			 tolower(substr($1, 2)));
		  next;
		}		
/^[A-Z]+/	{ state = "X";
		  printf("\\subsection*{%s%s}\n",
			 toupper(substr($1, 1, 1)),
			 tolower(substr($1, 2)));
		  next;
		}

# a match-all , again
		{ if ("X" == state) { print $0; next; }
		  if ("name" == state) {
		    split($0, namename, "[ \t]+");
		    # gsub(/[_{}%&]/, "\\\\&", namename[1]);
		    label = namename[1];
		    if ("" == label) label = $0;
		    gsub(/[_(){}%&^~\#\\\\]/, "", label);
		    printf("\\section{%s\\label{%s-section}}\n%s\\\\\n",
			   $0, label, line);
		    next;
		  }
		  if ("see" == state) {
		    # print $0;
		    gsub(/[ 	]/, "", $0);
		    nrefs = split($0, refs, ",");
		    for (i = 1; i < nrefs; i++) {
		      if (!length(refs[i])) continue;
		      seename = substr(refs[i], 1, index(refs[i], "(")-1);
		      printf("%s", refs[i]);
		      gsub(/[_(){}%&^~\\\\]/, "", seename);
		      if (length(seename))
			 printf(" (\\ref{%s-section})", seename);
		      printf(", ");
		    }
		    print "";			
		    next;
		  }
		  if ("syn" == state) {
		    syname = $0;
		    sub(/^[ \t]*[a-z]+[ \t]+\**/,"", syname);
		    sub(/\([^)]*\)/,"()", syname);
		    sub(/\).*$/,")", syname);
#		    # print syname;
		    label = syname;
		    gsub(/[_(){}%&^~\\\\]/, "", label);
		    printf("\\section{{\\tt %s}\\label{%s-section}}\n",
			   syname, label);
		    printf("~~{\\tt %s}\\\\\n", $0);
		    state = "syn2";
		    next;
		  }
		  if ("syn2" == state) {
		    printf("~~{\\tt %s}\\\\\n", $0);
		    next;
		  }
# print the entire examples section in \tt and lines separated.
# we need some heuristic to distinguish betwwen code and explanatory lines
		  if ("exm" == state) {
		    printf("{\\tt %s}\\\\\n", $0);
		    next;
		  }
		  print "unknown state " state > /dev/stderr;
		}

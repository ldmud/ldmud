# startmud.pl
#
#startmud for ldmud driver
#
#this is a reallly ugly throw together of a keep alive program for ldmud
#I know very little perl scripting so there probably is a better way to do this.
#if you are a more advanced perl scripter, please feel free to fix this and
#email me a copy via    punkette@hotmail.com
#
#modify this to where perl is located on your system

#!/usr/bin/perl -w
&check;
sub check {
$test = system("ps | grep parse > /dev/null");
if ($test eq "256") { &startmud; }
&check;

}
sub startmud {
#modify the next two lines sot that the "home/wetdirt/mud/ldmud/bin/" points to the bin dir
# of your lpmud installation. remember that there are 3 areas to correct for your system in the next
# 2 lines.
system("rm -f /home/wetdirt/mud/ldmud/bin/nohup.out");
system("nohup /home/wetdirt/mud/ldmud/bin/parse & > /home/wetdirt/mud/ldmud/bin/nohup.out");
$test = system("ps | grep parse > /dev/null");
print "test = $test\n";
return;
}

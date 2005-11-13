Amylaar-LPMud 3.2.1 for Be   (10th July 1998)
--------------------------

This is a rehost of Amylaar's LPMud gamedriver to the
Be Operating System (PR3) using the Metrowerks Compiler.
Rehosting the program involved mostly workarounds for the
quirks in Be's networking code and limited stack space,
else it was a simple recompilation.

The rehost is complete except for one thing: ERQs must be started
manually and then connect to the gamedriver, the driver can't
start them on his own.

Everything is still very unixish, and not tested very much.
As I'm more interested in other things at the moment, my work
on the gamedriver is on a at-need basis only.
So if anybody wishes to take over from here: be my guest.

Recompilation
-------------
  To recompile the gamedriver, copy the files machine.h, config.h
  and Makefile into the toplevel directory, change them to your
  needs and remake the gamedriver. See the comments in the Makefile
  for further details.

  The default settings are to compile for cross-compat mode
  (see config.h); all directories are expected under /boot/home/mud:
  /boot/home/mud/bin for binaries, /boot/home/mud/lib for the mudlib
  (both defined in Makefile), and /boot/home/mud/erq for any ERQ
  programs (defined in config.h).

Known Bugs
----------
  - When sprintf()/printf()ing deeply nested data structures (i.e.
    endless mapping or array loops), the driver might crash due to
    exhausted stack space before the internal safeguard of sprintf()
    can stop him.
  - When started in the background, the driver somehow can't set the
    first opened socket to 'nonblocking' (fcntl(FNDELAY)). I hacked
    around this by explicitely ignoring the error for the first
    socket. Ugh.
    
History
-------
 13.08.1998: Made sure that the erq is not started automatically,
             even if the option in config.h is set.
 10.07.1998: Updated the Be port with changes from the OSB driver.
             Added hack for fcntl(FNDELAY).
 05.07.1998: Added dual-compilation facilities for PPC and x86.
             Got rid of the 'duplicate' link warnings.
 17.05.1998: Updated the Be port with changes from the OSB driver.
  8.03.1998: First Be port of the gamedriver.


Share and enjoy!

  Lars Duening (lars@cableinet.co.uk)

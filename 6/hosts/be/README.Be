Amylaar-LPMud 3.2.1 for Be   (17th May 1998)
--------------------------

This is a quick rehost of Amylaar's LPMud gamedriver to the
Be Operating System (PR2) using the Metrowerks Compiler.
Rehosting the program involved mostly workarounds for the
quirks in Be's networking code and limited stack space,
else it was a simple recompilation.

The rehost is complete except for one thing: ERQs must be started
manually and then connect to the gamedriver, the driver can't
start them on his own.

The rehost is still very unixish, and wasn't tested very much,
because at the moment I am not very interested in this gamedriver.
So if anybody wishes to take over from here: be my guest.

Recompilation
-------------
  To recompile the gamedriver, copy the files machine.h, config.h
  and Makefile into the toplevel directory, change them to your
  needs and remake the gamedriver.

  The default settings are to compile for cross-compat mode
  (see config.h); all directories are expected under /boot/home/mud:
  /boot/home/mud/bin for binaries, /boot/home/mud/lib for the mudlib
  (both defined in Makefile), and /boot/home/mud/erq for any ERQ
  programs (defined in config.h).

History
-------
  8.03.1998: First Be port of the gamedriver.
 17.05.1998: Updated the Be port with changes from the OSB driver.


Share and enjoy!

  Lars Duening (lars@cableinet.co.uk)

LDMud 3.2 for Be
----------------

This is the Be version of the 'LDMud' gamedriver, ported for
the BeOS R4.

Rehosting the program involved mostly workarounds for the
quirks in Be's networking code and limited stack space,
else it was a simple recompilation.

The rehost is complete except for one thing: ERQs must be started
manually and then connect to the gamedriver, the driver can't
start them on his own.

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
    

Share and enjoy!

  Lars Duening (lars@cableinet.co.uk)

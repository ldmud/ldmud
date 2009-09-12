Platforms which require aligned memory access seem to be not used by the users
of LDMud (e.g. ARM, PowerPC, Sparc). For the sake of code simplicity and/or
speed some code pieces (bytecode, murmur hash functions) don't care about
alignment.

On these platforms, the driver will probably not work at all right
now.

That can be fixed, but only if needed (there are users) and if the
potential users support us (e.g. testing).

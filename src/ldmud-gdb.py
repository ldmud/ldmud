import os
import sys

if gdb.current_objfile():
    sys.path.insert(0, os.path.join(os.path.dirname(gdb.current_objfile().filename), "..", "gdb"))
else:
    sys.path.insert(0, "../gdb")

import ldmud
ldmud.register_printers()

print "LDMud 3.5 pretty printers loaded."

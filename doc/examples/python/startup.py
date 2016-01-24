# This module can be used as a startup script.
#
# It will load all the other modules and offers a python_reload() efun
# to reload all the other modules.

import os, sys
import ldmud

sys.path.insert(0, os.path.dirname(os.path.realpath(__file__)))

# If you wan't to add modules during runtime
# add an efun to supplement this list.
modules = ["console", "echo"]

def reload_modules():
    # For Python >= 3.4 use importlib
    import imp

    for mod in modules:
        if mod in sys.modules:
            modob = sys.modules[mod]
            try:
                modob.on_reload()
            except:
                pass
            imp.reload(modob)
        else:
            fp, path, desc = imp.find_module(mod)

            try:
                imp.load_module(mod, fp, path, desc)
            finally:
                if fp:
                    fp.close()

ldmud.register_efun("python_reload", reload_modules)

import console, echo

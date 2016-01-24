# This is a BIG BIG BIG security hole.
#
# It allows to execute arbitrary python code from inside the MUD.
# Guard the efun 'python_console' very tightly or better do
# not use this in production MUDs.
#
# This module can be used together with the python.c LPC program,
# that will offer a python console in the MUD.

import ldmud, code, io, sys

consoles = {}

class LDMudConsole(code.InteractiveConsole):
    def __init__(self, showbanner = True):
        code.InteractiveConsole.__init__(self)

        try:
            sys.ps1
        except AttributeError:
            sys.ps1 = ">>> "
        try:
            sys.ps2
        except AttributeError:
            sys.ps2 = "... "

        self.output = io.StringIO()
        self.write = self.output.write
        if showbanner:
            self.write("Python %s on %s\nType \"help\", \"copyright\", \"credits\" or \"license\" for more information.\n" %
                       (sys.version, sys.platform,))

    def input(self, line):
        oldout = sys.stdout
        olderr = sys.stderr
        sys.stdout = self.output
        sys.stderr = self.output

        try:
            more = self.push(line)
            if more:
                prompt = sys.ps2
            else:
                prompt = sys.ps1
        except SystemExit:
            prompt = None
        except:
            self.showtraceback()
            prompt = sys.ps1
        finally:
            sys.stdout = oldout
            sys.stderr = olderr

        text = self.output.getvalue()
        self.output.seek(0)
        self.output.truncate(0)
        return (text, prompt,)

def console(line = ""):
    ob = ldmud.efuns.this_object()
    obname = ldmud.efuns.object_name(ob)

    if obname in consoles:
        console = consoles[obname]
    else:
        console = LDMudConsole(len(line) == 0)
        consoles[obname] = console

    (output, prompt) = console.input(line)
    if prompt is None:
        del consoles[obname]
    return ldmud.Array((output, prompt,))

ldmud.register_efun("python_console", console)

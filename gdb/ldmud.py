import gdb

def unwind_ptr(val):
    type = val.type

    while True:
        if type.code == gdb.TYPE_CODE_REF:
            type = type.target()
        elif type.code == gdb.TYPE_CODE_PTR:
            val = val.dereference()
            type = val.type
        elif type.code == gdb.TYPE_CODE_ARRAY:
            val = val[0]
            type = val.type
        else:
            break

    return (type, val)

def print_ptr(val):
    return "0x%x" % (val.cast(gdb.lookup_type('int')))

class MStringPrinter:
    "Print an mstring"

    def __init__(self, val):
        self.val = val

    def to_string(self):
        # Handle references
        (type,val) = unwind_ptr(self.val)

        if val.address == 0:
            return "0x0"

        return '"' + val["txt"].string(length = val["size"]) + '"'

class PtrNamePrinter:
    "Print an pointer to a struct with an name entry"

    def __init__(self, val, indices):
        self.val = val
        self.indices = indices

    def to_string(self):
        (type,val) = unwind_ptr(self.val)
        if val.address == 0:
            return "0x0"

        nameval = val;
        for idx in self.indices:
            (type, nameval) = unwind_ptr(nameval[idx])
            if nameval.address == 0:
                return print_ptr(val.address)

        return print_ptr(val.address) + ' "' + nameval["txt"].string(length = nameval["size"]) + '"'

class ArrayPrinter:
    "Print an LPC array"

    class Iterator:
        def __init__(self, items, size):
            self.items = items
            self.size = int(size)
            self.pos = 0

        def __iter__(self):
            return self

        def next(self):
            if self.pos >= self.size:
                raise StopIteration

            result = ('[%d]' % (self.pos), self.items[self.pos].address)

            self.pos += 1

            return result

    def __init__(self, val):
        self.val = val

    def to_string(self):
        (type,val) = unwind_ptr(self.val)
        if val.address == 0:
            return "0x0"

        return print_ptr(val.address) + " Array with %d element%s" % (val["size"], val["size"]!=1 and "s" or "")

    def children(self):
        (type,val) = unwind_ptr(self.val)
        if val.address == 0:
            return []

        return self.Iterator(self.val["item"], self.val["size"])

    def display_hint(self):
        return "array"

class MappingPrinter:
    "Print an LPC mapping"

    class Iterator:
        def __init__(self, mapping):
            self.mapping = mapping
            self.width = mapping["num_values"]+1
            self.size = mapping["num_entries"]
            if mapping["cond"] == 0:
                self.condsize = 0
            else:
                self.condpart = mapping["cond"].dereference()
                self.condsize = self.condpart["size"]

            if mapping["hash"] == 0:
                self.hashsize = 0
            else:
                self.hashpart = mapping["hash"].dereference()
                self.hashsize = self.hashpart["used"]
                self.hashchains = self.hashpart["mask"] + 1

            self.pos = 0
            self.hashchain = 0
            self.hashpos = 0

        def __iter__(self):
            return self

        def next(self):
            item = None

            key = self.pos / self.width
            val = self.pos % self.width

            if key < self.condsize:
                # We have to look in the condensed part
                if val == 0:
                    item = self.condpart["data"][key].address
                else:
                    item = self.condpart["data"][self.condsize + key * (self.width-1) + val-1].address
            elif key < self.condsize + self.hashsize:
                while self.hashpos < self.hashsize and not self.hashchain:
                    self.hashchain = self.hashpart["chains"][self.hashpos]
                    self.hashpos += 1
                if self.hashchain:
                    item = self.hashchain.dereference()["data"][val].address

            if item == None:
                raise StopIteration

            if val == 0:
                result = ('[%d]' % (key), item)
            else:
                result = ('[%d,%d]' % (key, val-1), item)

            self.pos += 1
            if self.hashchain and (self.pos % self.width) == 0:
                self.hashchain = self.hashchain.dereference()["next"]

            return result

    def __init__(self, val):
        self.val = val

    def to_string(self):
        (type,val) = unwind_ptr(self.val)
        if val.address == 0:
            return "0x0"

        return print_ptr(val.address) + " Mapping with %d %s and %d value%s" % (val["num_entries"], val["num_entries"]!=1 and "entries" or "entry", val["num_values"], val["num_values"]!=1 and "s" or "")

    def children(self):
        (type,val) = unwind_ptr(self.val)
        if val.address == 0:
            return []

        return self.Iterator(self.val)

#    def display_hint(self):
#        return "map"

class SValuePrinter:
    "Print an svalue"

    T_INVALID                        = 0x0
    T_LVALUE                         = 0x1
    T_NUMBER                         = 0x2
    T_STRING                         = 0x3
    T_POINTER                        = 0x4
    T_OBJECT                         = 0x5
    T_MAPPING                        = 0x6
    T_FLOAT                          = 0x7
    T_CLOSURE                        = 0x8
    T_SYMBOL                         = 0x9
    T_QUOTED_ARRAY                   = 0xa
    T_STRUCT                         = 0xb
    T_CHAR_LVALUE                    = 0xc
    T_STRING_RANGE_LVALUE            = 0x0d
    T_POINTER_RANGE_LVALUE           = 0x0e
    T_PROTECTED_CHAR_LVALUE          = 0x0f
    T_PROTECTED_STRING_RANGE_LVALUE  = 0x10
    T_PROTECTED_POINTER_RANGE_LVALUE = 0x11
    T_PROTECTED_LVALUE               = 0x12
    T_PROTECTOR_MAPPING              = 0x13
    T_CALLBACK                       = 0x14
    T_ERROR_HANDLER                  = 0x15
    T_NULL                           = 0x16

    names = {
        T_INVALID:                        "T_INVALID",
        T_LVALUE:                         "T_LVALUE",
        T_NUMBER:                         "T_NUMBER",
        T_STRING:                         "T_STRING",
        T_POINTER:                        "T_POINTER",
        T_OBJECT:                         "T_OBJECT",
        T_MAPPING:                        "T_MAPPING",
        T_FLOAT:                          "T_FLOAT",
        T_CLOSURE:                        "T_CLOSURE",
        T_SYMBOL:                         "T_SYMBOL",
        T_QUOTED_ARRAY:                   "T_QUOTED_ARRAY",
        T_STRUCT:                         "T_STRUCT",
        T_CHAR_LVALUE:                    "T_CHAR_LVALUE",
        T_STRING_RANGE_LVALUE:            "T_STRING_RANGE_LVALUE",
        T_POINTER_RANGE_LVALUE:           "T_POINTER_RANGE_LVALUE",
        T_PROTECTED_CHAR_LVALUE:          "T_PROTECTED_CHAR_LVALUE",
        T_PROTECTED_STRING_RANGE_LVALUE:  "T_PROTECTED_STRING_RANGE_LVALUE",
        T_PROTECTED_POINTER_RANGE_LVALUE: "T_PROTECTED_POINTER_RANGE_LVALUE",
        T_PROTECTED_LVALUE:               "T_PROTECTED_LVALUE",
        T_PROTECTOR_MAPPING:              "T_PROTECTOR_MAPPING",
        T_CALLBACK:                       "T_CALLBACK",
        T_ERROR_HANDLER:                  "T_ERROR_HANDLER",
        T_NULL:                           "T_NULL",
    }

    def __init__(self, val):
         self.val = val

    def to_string(self):
        (type,val) = unwind_ptr(self.val)
        if val.address == 0:
            return "0x0"

        stype = int(val["type"])
        if stype in self.names:
            return print_ptr(val.address) + " " + self.names[stype]

        return print_ptr(val.address) + " svalue_t with unknown type %d" % (stype)

    def children(self):
        (type,val) = unwind_ptr(self.val)
        if val.address == 0:
            return []

        stype = val["type"]
        if stype == self.T_LVALUE:
            return [(".u.lvalue", val["u"]["lvalue"])]
        elif stype == self.T_NUMBER:
            return [(".u.number", val["u"]["number"])]
        elif stype == self.T_STRING:
            return [(".u.str", val["u"]["str"])]
        elif stype == self.T_POINTER:
            return [(".u.vec", val["u"]["vec"])]
        elif stype == self.T_OBJECT:
            return [(".u.ob", val["u"]["ob"])]
        elif stype == self.T_MAPPING:
            return [(".u.map", val["u"]["map"])]
        elif stype == self.T_FLOAT:
            return [(".u.mantissa", val["u"]["mantissa"]),
                    (".x.exponent", val["x"]["exponent"])]
        elif stype == self.T_CLOSURE:
            return [(".u.lambda", val["u"]["lambda"]),
                    (".x.closure_type", val["x"]["closure_type"])]
        elif stype == self.T_SYMBOL:
            return [(".u.str", val["u"]["str"]),
                    (".x.quotes", val["x"]["quotes"])]
        elif stype == self.T_QUOTED_ARRAY:
            return [(".u.vec", val["u"]["vec"]),
                    (".x.quotes", val["x"]["quotes"])]
        elif stype == self.T_STRUCT:
            return [(".u.strct", val["u"]["strct"])]
        elif stype == self.T_CHAR_LVALUE:
            return [(".u.charp", val["u"]["charp"])]
        elif stype == self.T_STRING_RANGE_LVALUE:
            return [(".u.str", val["u"]["str"])]
        elif stype == self.T_POINTER_RANGE_LVALUE:
            return [(".u.vec", val["u"]["vec"])]
        elif stype == self.T_PROTECTED_CHAR_LVALUE:
            return [(".u.protected_char_lvalue", val["u"]["protected_char_lvalue"])]
        elif stype == self.T_PROTECTED_STRING_RANGE_LVALUE:
            return [(".u.protected_range_lvalue", val["u"]["protected_range_lvalue"])]
        elif stype == self.T_PROTECTED_POINTER_RANGE_LVALUE:
            return [(".u.protected_range_lvalue", val["u"]["protected_range_lvalue"])]
        elif stype == self.T_PROTECTED_LVALUE:
            return [(".u.protected_lvalue", val["u"]["protected_lvalue"])]
        elif stype == self.T_PROTECTOR_MAPPING:
            return [(".u.map", val["u"]["map"])]
        elif stype == self.T_CALLBACK:
            return [(".u.cb", val["u"]["cb"]),
                    (".x.extern_args", val["x"]["extern_args"])]
        elif stype == self.T_ERROR_HANDLER:
            return [(".u.error_handler", val["u"]["error_handler"])]
        else:
            return []

def lookup_printer(val):
    "Returns a pretty printer for val (gdb.Value)."

    # Handle references and pointers
    type = val.type
    haveptr = False
    while type.code == gdb.TYPE_CODE_REF or ((type.code == gdb.TYPE_CODE_PTR or type.code == gdb.TYPE_CODE_ARRAY) and not haveptr):
        if type.code == gdb.TYPE_CODE_PTR or type.code == gdb.TYPE_CODE_ARRAY:
            haveptr = True
        type = type.target()

    type = type.strip_typedefs()

    if type.tag == None:
        return None

    if haveptr:
        printerlist = ptr_printers
    else:
        printerlist = direct_printers

    if type.tag in printerlist:
        return printerlist[type.tag](val)

    return None

def register_printers(obj = None):
    "Register pretty printers for LDMud with obj."

    if obj == None:
        obj = gdb

    obj.pretty_printers.append(lookup_printer)

ptr_printers = {
    'string_s':      MStringPrinter,
    'vector_s':      ArrayPrinter,
    'mapping_s':     MappingPrinter,
    'svalue_s':      SValuePrinter,
    'object_s':      lambda val: PtrNamePrinter(val, ["name"]),
    'program_s':     lambda val: PtrNamePrinter(val, ["name"]),
    'interactive_s': lambda val: PtrNamePrinter(val, ["ob", "name"]),
    'wiz_list_s':    lambda val: PtrNamePrinter(val, ["name"]),
    }

direct_printers = {}

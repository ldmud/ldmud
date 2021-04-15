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

    STRING_ASCII = 0
    STRING_UTF8  = 1
    STRING_BYTES = 2

    def __init__(self, val):
        self.val = val

    def to_string(self):
        # Handle references
        (type,val) = unwind_ptr(self.val)

        if val.address == 0:
            return "0x0"

        if val["info"]["unicode"] == self.STRING_BYTES:
            result = '"'
            for idx in range(val["size"]):
                ch = val["txt"][idx]
                if ch < 0:
                    ch += 256
                result += '\\x%02x' % (ch,)
            result += '"'
            return result
        else:
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

        def __next__(self):
            if self.pos >= self.size:
                raise StopIteration

            result = ('[%d]' % (self.pos), self.items[self.pos].address)

            self.pos += 1

            return result

        def next(self):
            return self.__next__()

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

        def __next__(self):
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

        def next(self):
            return self.__next__()

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
    T_BYTES                          = 0xc
    T_CALLBACK                       = 0xd
    T_ERROR_HANDLER                  = 0xe
    T_BREAK_ADDR                     = 0xf
    T_NULL                           = 0x10

    LVALUE_UNPROTECTED               = 0x00
    LVALUE_UNPROTECTED_CHAR          = 0x01
    LVALUE_UNPROTECTED_RANGE         = 0x02
    LVALUE_PROTECTED                 = 0x10
    LVALUE_PROTECTED_CHAR            = 0x11
    LVALUE_PROTECTED_RANGE           = 0x12

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
        T_BYTES:                          "T_BYTES",
        T_CALLBACK:                       "T_CALLBACK",
        T_ERROR_HANDLER:                  "T_ERROR_HANDLER",
        T_BREAK_ADDR:                     "T_BREAK_ADDR",
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
            ltype = val["x"]["lvalue_type"]

            if ltype == self.LVALUE_UNPROTECTED:
                return [("x.lvalue_type", "LVALUE_UNPROTECTED"),
                        ("u.lvalue", val["u"]["lvalue"])]
            elif ltype == self.LVALUE_UNPROTECTED_CHAR:
                lvalue = gdb.lookup_global_symbol("current_unprotected_char").value()
                return [("x.lvalue_type", "LVALUE_UNPROTECTED_CHAR"),
                        ("current_unprotected_char", lvalue)]
            elif ltype == self.LVALUE_UNPROTECTED_RANGE:
                lvalue = gdb.lookup_global_symbol("current_unprotected_range").value()
                return [("x.lvalue_type", "LVALUE_UNPROTECTED_RANGE"),
                        ("current_unprotected_range", lvalue)]
            elif ltype == self.LVALUE_PROTECTED:
                return [("x.lvalue_type", "LVALUE_PROTECTED"),
                        ("u.protected_lvalue", val["u"]["protected_lvalue"])]
            elif ltype == self.LVALUE_PROTECTED_CHAR:
                return [("x.lvalue_type", "LVALUE_PROTECTED_CHAR"),
                        ("u.protected_char_lvalue", val["u"]["protected_char_lvalue"])]
            elif ltype == self.LVALUE_PROTECTED_RANGE:
                return [("x.lvalue_type", "LVALUE_PROTECTED_RANGE"),
                        ("u.protected_range_lvalue", val["u"]["protected_range_lvalue"])]
            else:
                return [(".x.lvalue_type", ltype)]
        elif stype == self.T_NUMBER:
            return [(".u.number", val["u"]["number"])]
        elif stype == self.T_STRING or stype == self.T_BYTES:
            return [(".u.str", val["u"]["str"])]
        elif stype == self.T_POINTER:
            return [(".u.vec", val["u"]["vec"])]
        elif stype == self.T_OBJECT:
            return [(".u.ob", val["u"]["ob"])]
        elif stype == self.T_MAPPING:
            return [(".u.map", val["u"]["map"])]
        elif stype == self.T_FLOAT:
            if any([field.name == "float_number" for field in val["u"].type.fields()]):
                return [("u.float_number", val["u"]["float_number"])]
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
        elif stype == self.T_CALLBACK:
            return [(".u.cb", val["u"]["cb"]),
                    (".x.extern_args", val["x"]["extern_args"])]
        elif stype == self.T_ERROR_HANDLER:
            return [(".u.error_handler", val["u"]["error_handler"])]
        elif stype == self.T_BREAK_ADDR:
            return [(".u.break_addr", val["u"]["break_addr"])]
        else:
            return []

class ProtLvalPrinter:
    "Prints a protected lvalue"

    def __init__(self, val):
         self.val = val

    def to_string(self):
        (type,val) = unwind_ptr(self.val)

        return print_ptr(val.address)

    def children(self):
        (type,val) = unwind_ptr(self.val)
        if val.address == 0:
            return []

        return [(".ref", val["ref"]),
                (".val", val["val"].address)]

class ProtCharPrinter:
    "Prints a protected char lvalue"

    def __init__(self, val):
         self.val = val

    def to_string(self):
        (type,val) = unwind_ptr(self.val)

        return print_ptr(val.address)

    def children(self):
        (type,val) = unwind_ptr(self.val)
        if val.address == 0:
            return []

        return [(".ref", val["ref"]),
                (".str", val["str"]),
                (".charp", val["charp"])]

class ProtRangePrinter:
    "Prints a protected range lvalue"

    def __init__(self, val):
         self.val = val

    def to_string(self):
        (type,val) = unwind_ptr(self.val)

        return print_ptr(val.address)

    def children(self):
        (type,val) = unwind_ptr(self.val)
        if val.address == 0:
            return []

        return [(".ref", val["ref"]),
                (".index1", val["index1"]),
                (".index2", val["index2"]),
                (".vec", val["vec"].address),
                (".var", val["var"])]

class TypePrinter:
    "Prints a LPC type"

    TCLASS_PRIMARY    = 0
    TCLASS_STRUCT     = 1
    TCLASS_ARRAY      = 2
    TCLASS_UNION      = 3

    TYPE_UNKNOWN      =  0
    TYPE_NUMBER       =  1
    TYPE_STRING       =  2
    TYPE_VOID         =  3
    TYPE_OBJECT       =  4
    TYPE_MAPPING      =  5
    TYPE_FLOAT        =  6
    TYPE_ANY          =  7
    TYPE_CLOSURE      =  8
    TYPE_SYMBOL       =  9
    TYPE_QUOTED_ARRAY = 10

    type_names = {
        TYPE_UNKNOWN:      "unknown",
        TYPE_NUMBER:       "int",
        TYPE_STRING:       "string",
        TYPE_VOID:         "void",
        TYPE_OBJECT:       "object",
        TYPE_MAPPING:      "mapping",
        TYPE_FLOAT:        "float",
        TYPE_ANY:          "mixed",
        TYPE_CLOSURE:      "closure",
        TYPE_SYMBOL:       "symbol",
        TYPE_QUOTED_ARRAY: "quoted_array",
    }

    def __init__(self, val):
        self.val = val

    def calc_name(self, val):
        if val.address == 0:
            return "0x0"

        tclass = int(val["t_class"])

        if tclass == self.TCLASS_PRIMARY:
            primary = int(val["t_primary"])
            if primary in self.type_names:
                return self.type_names[primary]
            return "T_PRIMARY with unknown type %d" % (primary,)

        elif tclass == self.TCLASS_STRUCT:
            structinfo = val["t_struct"]["name"].dereference()
            if structinfo.address == 0:
                return "any struct"

            progname = structinfo["prog_name"]
            basename = structinfo["name"]

            return "struct %s:%s" % (
                progname["txt"].string(length = progname["size"]),
                basename["txt"].string(length = basename["size"]))

        elif tclass == self.TCLASS_ARRAY:
            return self.calc_name(val["t_array"]["base"].dereference()) + "*"*int(val["t_array"]["depth"])

        elif tclass == self.TCLASS_UNION:
            typestr = ""
            union = val
            while tclass == self.TCLASS_UNION:
                typestr += self.calc_name(union["t_union"]["member"].dereference()) + "|"
                union = union["t_union"]["head"].dereference()
                tclass = int(union["t_class"])
            typestr += self.calc_name(union)
            return "<" + typestr + ">"
        else:
            return "lpctype_t with unknown class %d" % (tclass,)

    def to_string(self):
        (type,val) = unwind_ptr(self.val)
        return print_ptr(val.address) + " " + self.calc_name(val)

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
    'lpctype_s':     TypePrinter,
    'protected_lvalue':       ProtLvalPrinter,
    'protected_char_lvalue':  ProtCharPrinter,
    'protected_range_lvalue': ProtRangePrinter,
}

direct_printers = {
    'svalue_s':      SValuePrinter,
}

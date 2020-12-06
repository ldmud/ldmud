import sys,unittest,gc
import ldmud

class TestModule(unittest.TestCase):
    def testMaster(self):
        ob = ldmud.Object("/master")
        self.assertEqual(ob, ldmud.get_master())

    def testSimulEfun(self):
        self.assertIsNone(ldmud.get_simul_efun())

class TestObject(unittest.TestCase):
    def testInitLoaded(self):
        ob = ldmud.Object("/master")
        self.assertIsNotNone(ob)
        self.assertTrue(ob)
        self.assertIn("master", repr(ob))
        self.assertEqual(ob, ldmud.Object("/master"))
        self.assertEqual(hash(ob), hash(ldmud.Object("/master")))

    def testInitLoad(self):
        oldob = ldmud.efuns.find_object("/testob")
        if oldob:
            ldmud.efuns.destruct(oldob)
        ob = ldmud.Object("/testob")
        self.assertIsNotNone(ob)
        self.assertEqual(ob.name, "/testob")
        self.assertIn("testob", repr(ob))

    def testInitNonExisting(self):
        with self.assertRaises(RuntimeError):
            ob = ldmud.Object("/imnotthere")

    def testFunctionInfo(self):
        ob = ldmud.Object("/testob")
        self.assertIsNotNone(ob)

        self.assertTrue('testfun' in dir(ob.functions))
        self.assertTrue('testfun' in ob.functions.__dict__)

        fun = ob.functions.testfun
        self.assertIsNotNone(fun)
        self.assertEqual(fun.name, "testfun")
        self.assertEqual(fun.file_name, "/testob.c")
        self.assertGreater(fun.line_number, 0)
        self.assertEqual(fun.return_type, int)
        self.assertEqual(fun.flags, ldmud.LF_NOMASK)
        self.assertEqual(fun.visibility, ldmud.VIS_PROTECTED)
        self.assertTrue("testfun" in repr(fun) and "testob" in repr(fun))
        self.assertEqual(hash(fun), hash(ob.functions.testfun))
        self.assertEqual(fun, ob.functions.testfun)
        self.assertNotEqual(fun, ob.functions.fun_testob)

        args = fun.arguments
        self.assertEqual(len(args), 2)
        self.assertEqual(args[0].position, 1)
        self.assertSetEqual(set(args[0].type), set((int, float,)))
        self.assertEqual(args[1].position, 2)
        self.assertEqual(args[1].flags, ldmud.LA_VARARGS)
        self.assertEqual(args[1].type, ldmud.Array)

        self.assertEqual(fun(10, "A", "B", "C"), 3)

    def testVariableInfo(self):
        ob = ldmud.Object("/testob")
        self.assertIsNotNone(ob)

        self.assertTrue('testvar' in dir(ob.variables))
        self.assertTrue('testvar' in ob.variables.__dict__)

        var = ob.variables.testvar
        self.assertIsNotNone(var)
        self.assertEqual(var.name, "testvar")
        self.assertEqual(var.value, 42)
        self.assertSetEqual(set(var.type), set((int, float,)))
        self.assertEqual(var.flags, ldmud.VF_NOSAVE)
        self.assertEqual(var.visibility, ldmud.VIS_PROTECTED)
        self.assertTrue("testvar" in repr(var) and "testob" in repr(var))
        self.assertEqual(hash(var), hash(ob.variables.testvar))
        self.assertEqual(var, ob.variables.testvar)
        self.assertNotEqual(var, ob.variables.var_testob)

        var.value = 84
        self.assertEqual(var.value, 84)

        with self.assertRaises(TypeError):
            var.value = "42"
        self.assertEqual(var.value, 84)

    def testDestructed(self):
        ob = ldmud.Object("/testob")
        ldmud.efuns.destruct(ob)
        self.assertFalse(ob)

    def testDestructedFunctionAndVariable(self):
        ob = ldmud.Object("/testob")
        lfun = ob.functions.testfun
        var = ob.variables.testvar

        ldmud.efuns.destruct(ob)
        self.assertFalse(lfun)
        self.assertFalse(var)

    def testDict(self):
        ob = ldmud.Object("/testob")
        ob.testvalue = "42"
        ob = None

        ob = ldmud.Object("/testob")
        self.assertEqual(ob.testvalue, "42")
        with self.assertRaises(AttributeError):
            ob.doesntExist

class TestLWObject(unittest.TestCase):
    def testInitLoaded(self):
        blueprint = ldmud.Object("/testob")
        lwob = ldmud.LWObject("/testob")
        self.assertIsNotNone(lwob)
        self.assertTrue(lwob)
        self.assertIn("testob", repr(lwob))

    def testInitLoad(self):
        oldob = ldmud.efuns.find_object("/testob")
        if oldob:
            ldmud.efuns.destruct(oldob)
        lwob = ldmud.LWObject("/testob")
        self.assertIsNotNone(lwob)
        self.assertEqual(lwob.program_name, "/testob.c")
        self.assertIn("testob", repr(lwob))

    def testInitNonExisting(self):
        with self.assertRaises(RuntimeError):
            lwob = ldmud.LWObject("/imnotthere")

    def testFunctionInfo(self):
        lwob = ldmud.LWObject("/testob")
        self.assertIsNotNone(lwob)

        self.assertTrue('testfun' in dir(lwob.functions))
        self.assertTrue('testfun' in lwob.functions.__dict__)

        fun = lwob.functions.testfun
        self.assertIsNotNone(fun)
        self.assertTrue(fun)
        self.assertEqual(fun.name, "testfun")
        self.assertEqual(fun.file_name, "/testob.c")
        self.assertGreater(fun.line_number, 0)
        self.assertEqual(fun.return_type, int)
        self.assertEqual(fun.flags, ldmud.LF_NOMASK)
        self.assertEqual(fun.visibility, ldmud.VIS_PROTECTED)
        self.assertTrue("testfun" in repr(fun) and "testob" in repr(fun))
        self.assertEqual(hash(fun), hash(lwob.functions.testfun))
        self.assertEqual(fun, lwob.functions.testfun)
        self.assertNotEqual(fun, lwob.functions.fun_testob)

        args = fun.arguments
        self.assertEqual(len(args), 2)
        self.assertEqual(args[0].position, 1)
        self.assertSetEqual(set(args[0].type), set((int, float,)))
        self.assertEqual(args[1].position, 2)
        self.assertEqual(args[1].flags, ldmud.LA_VARARGS)
        self.assertEqual(args[1].type, ldmud.Array)

        self.assertEqual(fun(10, "A", "B", "C"), 3)

    def testVariableInfo(self):
        lwob = ldmud.LWObject("/testob")
        self.assertIsNotNone(lwob)

        self.assertTrue('testvar' in dir(lwob.variables))
        self.assertTrue('testvar' in lwob.variables.__dict__)

        var = lwob.variables.testvar
        self.assertIsNotNone(var)
        self.assertTrue(var)
        self.assertEqual(var.name, "testvar")
        self.assertEqual(var.value, 42)
        self.assertSetEqual(set(var.type), set((int, float,)))
        self.assertEqual(var.flags, ldmud.VF_NOSAVE)
        self.assertEqual(var.visibility, ldmud.VIS_PROTECTED)
        self.assertTrue("testvar" in repr(var) and "testob" in repr(var))
        self.assertEqual(hash(var), hash(lwob.variables.testvar))
        self.assertEqual(var, lwob.variables.testvar)
        self.assertNotEqual(var, lwob.variables.var_testob)

        var.value = 84
        self.assertEqual(var.value, 84)

        with self.assertRaises(TypeError):
            var.value = "42"
        self.assertEqual(var.value, 84)

class TestArray(unittest.TestCase):
    def testInitEmpty(self):
        arr = ldmud.Array()
        self.assertIsNotNone(arr)
        self.assertEqual(len(arr), 0)

    def testInitSize(self):
        arr = ldmud.Array(size = 10)
        self.assertIsNotNone(arr)
        self.assertEqual(len(arr), 10)
        self.assertTrue(0 in arr)
        self.assertFalse(1 in arr)

    def testInitValues(self):
        arr = ldmud.Array([42, 1.5, "Hi"])
        self.assertIsNotNone(arr)
        self.assertEqual(len(arr), 3)
        self.assertFalse(0 in arr)
        self.assertTrue(42 in arr)
        self.assertTrue(1.5 in arr)
        self.assertTrue("Hi" in arr)

    def testInitIter(self):
        arr = ldmud.Array(range(10))
        self.assertIsNotNone(arr)
        self.assertEqual(len(arr), 10)
        self.assertTrue(0 in arr)
        self.assertTrue(9 in arr)
        self.assertFalse(10 in arr)

    def testConcat(self):
        arr = ldmud.Array([1,2,3]) + ldmud.Array([4,5,6])
        self.assertIsNotNone(arr)
        self.assertEqual(len(arr), 6)
        self.assertTrue(1 in arr)
        self.assertTrue(6 in arr)

    def testRepeat(self):
        arr = ldmud.Array([1,2,3]) * 10
        self.assertIsNotNone(arr)
        self.assertEqual(len(arr), 30)
        self.assertTrue(1 in arr)
        self.assertTrue(3 in arr)

    def testItemGetSet(self):
        arr = ldmud.Array(size = 4)
        arr[0] = 10
        arr[1] = 21
        arr[2] = 52
        arr[3] = "Me"
        self.assertEqual(arr[0], 10)
        self.assertEqual(arr[1], 21)
        self.assertEqual(arr[2], 52)
        self.assertEqual(arr[3], "Me")

    def testSlice(self):
        arr = ldmud.Array(range(8))
        self.assertEqual(list(arr[3:5]), [3,4])
        self.assertEqual(list(arr[5:3:-1]), [5,4])
        self.assertEqual(list(arr[::-2]), list(range(7,0,-2)))

        arr[::-1] = arr
        self.assertEqual(list(arr), list(range(7,-1,-1)))

        arr[5:10] = ldmud.Array([10])
        self.assertEqual(list(arr), [7, 6, 5, 4, 3, 10])

        arr[4:2:-1] = ldmud.Array([100,101,102,103,104])
        self.assertEqual(list(arr), [7, 6, 5, 104, 103, 102, 101, 100, 10])

    def testDeletion(self):
        parr = list(range(20))
        arr = ldmud.Array(range(20))

        del arr[5]
        del parr[5]
        self.assertEqual(list(arr), parr)

        del arr[1:3]
        del parr[1:3]
        self.assertEqual(list(arr), parr)

        del arr[3:1]
        del parr[3:1]
        self.assertEqual(list(arr), parr)

        del arr[4:2:-1]
        del parr[4:2:-1]
        self.assertEqual(list(arr), parr)

        del arr[0:10:3]
        del parr[0:10:3]
        self.assertEqual(list(arr), parr)

        del arr[10:0:-3]
        del parr[10:0:-3]
        self.assertEqual(list(arr), parr)


class TestMapping(unittest.TestCase):
    def testInitEmpty(self):
        m = ldmud.Mapping()
        self.assertIsNotNone(m)
        self.assertEqual(len(m), 0)
        self.assertEqual(m.width, 1)

    def testInitWidth(self):
        m = ldmud.Mapping(width=10)
        self.assertIsNotNone(m)
        self.assertEqual(len(m), 0)
        self.assertEqual(m.width, 10)

    def testInitDict(self):
        m = ldmud.Mapping({1:1, 2:4, 3:9, 4:16})
        self.assertIsNotNone(m)
        self.assertEqual(len(m), 4)
        self.assertEqual(m.width, 1)
        self.assertEqual(m[1], 1)
        self.assertEqual(m[2], 4)
        self.assertEqual(m[3], 9)
        self.assertEqual(m[4], 16)
        self.assertTrue(4 in m)
        self.assertFalse(5 in m)

    def testInitList(self):
        m = ldmud.Mapping([(1,2,3),(4,5,6),(7,8,9),(10,11,12),(13,14,15)])
        self.assertIsNotNone(m)
        self.assertEqual(len(m), 5)
        self.assertEqual(m.width, 2)
        self.assertEqual(m[1], 2)
        self.assertEqual(m[4,1], 6)
        self.assertEqual(m[7,0], 8)
        self.assertTrue(13 in m)
        self.assertFalse(14 in m)

    def testInitInvalid(self):
        with self.assertRaises(ValueError):
            m = ldmud.Mapping([(1,),(2,3)])
        with self.assertRaises(ValueError):
            m = ldmud.Mapping([(), ()])
        with self.assertRaises(ValueError):
            m = ldmud.Mapping(width=-1)

    def testGetSetSimple(self):
        m = ldmud.Mapping()
        self.assertIsNotNone(m)

        m['Hello'] = 1
        m['World'] = 2
        self.assertEqual(len(m), 2)
        self.assertEqual(m.width, 1)
        self.assertEqual(m['Hello'], 1)
        self.assertEqual(m['World'], 2)
        self.assertEqual(m['!'], 0)
        self.assertFalse('!' in m)

    def testDeletion(self):
        m = ldmud.Mapping( { "One": 1, "Two": 2, "Three": 3 } )
        self.assertIsNotNone(m)

        self.assertEqual(len(m), 3)
        self.assertEqual(m.width, 1)
        del m["Two"]

        self.assertEqual(len(m), 2)
        self.assertEqual(m.width, 1)
        self.assertFalse('Two' in m)
        self.assertEqual(m['One'], 1)
        self.assertEqual(m['Three'], 3)

    def testGetSetWide(self):
        m = ldmud.Mapping(width=2)
        self.assertIsNotNone(m)

        m['Hello',0] = 1
        m['Hello',1] = 42
        m['World'] = 2
        m['World',1] = 100

        self.assertEqual(len(m), 2)
        self.assertEqual(m.width, 2)
        self.assertEqual(m['Hello'], 1)
        self.assertEqual(m['Hello',0], 1)
        self.assertEqual(m['Hello',1], 42)
        self.assertEqual(m['World'], 2)
        self.assertEqual(m['World',0], 2)
        self.assertEqual(m['World',1], 100)

    def testGetSetInvalid(self):
        m = ldmud.Mapping(width=0)
        with self.assertRaises(Exception):
            m[10] = 1
        with self.assertRaises(Exception):
            val = m[10]

        m = ldmud.Mapping(width=2)
        with self.assertRaises(IndexError):
            m['Hi','There'] = 'Me'
        with self.assertRaises(IndexError):
            m['Hi',2] = 0

    def testIterator(self):
        m = ldmud.Mapping( { "One": 1, "Two": 2, "Three": 3 } );
        self.assertEqual(dict(m), { "One": 1, "Two": 2, "Three": 3})
        self.assertEqual(set(m.keys()), set(("One", "Two", "Three",)))
        self.assertEqual(set(m.values()), set((1,2,3,)))

class TestStruct(unittest.TestCase):
    def setUp(self):
        self.master = ldmud.efuns.find_object("/master")

    def testInitSimple(self):
        s = ldmud.Struct(self.master, "test_struct")
        self.assertIsNotNone(s)

    def testStructInfo(self):
        s = ldmud.Struct(self.master, "test_struct")
        self.assertEqual(s.name, "test_struct")
        self.assertEqual(s.program_name, "/master.c")

    def testMemberInfo(self):
        s = ldmud.Struct(self.master, "test_struct")
        self.assertIsNotNone(s)

        self.assertTrue('t_int' in dir(s.members))
        self.assertTrue('t_int' in s.members.__dict__)

        mem = s.members.t_int
        self.assertIsNotNone(mem)
        self.assertEqual(mem.name, "t_int")
        self.assertEqual(mem.type, int)

    def testInitValueTuple(self):
        s = ldmud.Struct(self.master, "test_struct", (42, 1.5, 'Hi',))
        self.assertIsNotNone(s)
        self.assertEqual(s.members.t_int.value, 42)
        self.assertEqual(s.members.t_float.value, 1.5)
        self.assertEqual(s.members.t_string.value, 'Hi')

    def testInitValueList(self):
        s = ldmud.Struct(self.master, "test_struct", [42, 1.5, 'Hi'])
        self.assertIsNotNone(s)
        self.assertEqual(s.members.t_int.value, 42)
        self.assertEqual(s.members.t_float.value, 1.5)
        self.assertEqual(s.members.t_string.value, 'Hi')

    def testInitValueMap(self):
        s = ldmud.Struct(self.master, "test_struct", { 't_int': 42, 't_float': 1.5, 't_string': 'Hi'})
        self.assertIsNotNone(s)
        self.assertEqual(s.members.t_int.value, 42)
        self.assertEqual(s.members.t_float.value, 1.5)
        self.assertEqual(s.members.t_string.value, 'Hi')

    def testSetValue(self):
        s = ldmud.Struct(self.master, "test_struct")
        self.assertIsNotNone(s)
        s.members.t_int.value = 123
        s.members.t_float.value = 5.5
        s.members.t_string.value = 'Hello!'
        self.assertEqual(s.members.t_int.value, 123)
        self.assertEqual(s.members.t_float.value, 5.5)
        self.assertEqual(s.members.t_string.value, 'Hello!')

class TestClosure(unittest.TestCase):
    def setUp(self):
        self.master = ldmud.get_master()

    def testEfun(self):
        s = ldmud.Closure(self.master, "this_object")
        self.assertIsNotNone(s)
        self.assertEqual(s(), self.master)
        s2 = ldmud.Closure(self.master, "this_object")
        self.assertEqual(s2, s)
        self.assertIn(s2, set((s,)))

    def testLWOEfun(self):
        lwob = ldmud.LWObject("/testob")
        s = ldmud.Closure(lwob, "this_object")
        self.assertIsNotNone(s)
        self.assertEqual(s(), lwob)
        s2 = ldmud.Closure(lwob, "this_object")
        self.assertEqual(s2, s)
        self.assertIn(s2, set((s,)))

    def testOperator(self):
        s = ldmud.Closure(self.master, ",")
        self.assertIsNotNone(s)
        with self.assertRaises(RuntimeError):
            s()
        s2 = ldmud.Closure(self.master, ",")
        self.assertEqual(s2, s)
        self.assertIn(s2, set((s,)))

    def testLfun(self):
        s = ldmud.Closure(self.master, "master_fun", self.master)
        self.assertIsNotNone(s)
        self.assertEqual(s(), 54321)
        s2 = ldmud.Closure(self.master, "master_fun", self.master)
        self.assertEqual(s2, s)
        self.assertIn(s2, set((s,)))

    def testDestructedLfun(self):
        ob = ldmud.Object("/testob")
        c = ldmud.Closure(ob, "testfun", ob)
        self.assertTrue(c)
        ldmud.efuns.destruct(ob)
        self.assertFalse(c)

    def testLWOLfun(self):
        lwob = ldmud.LWObject("/testob")
        s = ldmud.Closure(lwob, "testfun", lwob)
        self.assertIsNotNone(s)
        self.assertEqual(s(42, "A", "B", "C"), 3)
        s2 = ldmud.Closure(lwob, "testfun", lwob)
        self.assertEqual(s2, s)
        self.assertIn(s2, set((s,)))

    def testEmpty(self):
        s = ldmud.Closure.__new__(ldmud.Closure)
        with self.assertRaises(Exception):
            s()
        self.assertFalse(s)

class TestSymbol(unittest.TestCase):
    def testSymbolInit(self):
        s = ldmud.Symbol("sym")
        self.assertIsNotNone(s)
        self.assertEqual(s.name, "sym")
        self.assertEqual(s.quotes, 1)

    def testSymbolInitQuotes(self):
        s = ldmud.Symbol("sym", 5)
        self.assertIsNotNone(s)
        self.assertEqual(s.name, "sym")
        self.assertEqual(s.quotes, 5)

    def testSymbolEq(self):
        s1 = ldmud.Symbol("sym", 2)
        s2 = ldmud.Symbol("sym", 2)
        s3 = ldmud.Symbol("sym", 5)
        s4 = ldmud.Symbol("sym2", 2)
        self.assertIsNotNone(s1)
        self.assertIsNotNone(s2)
        self.assertIsNotNone(s3)
        self.assertIsNotNone(s4)
        self.assertEqual(s1, s2)
        self.assertNotEqual(s1, s3)
        self.assertNotEqual(s1, s4)

class TestQuotedArray(unittest.TestCase):
    def testQuotedArrayInit(self):
        qa = ldmud.QuotedArray(ldmud.Array([2, 1]))
        self.assertIsNotNone(qa)
        self.assertEqual(len(qa.array), 2)
        self.assertEqual(qa.array[0], 2)
        self.assertEqual(qa.array[1], 1)
        self.assertEqual(qa.quotes, 1)

    def testQuotedArrayInitQuotes(self):
        qa = ldmud.QuotedArray(ldmud.Array([2, 1]), quotes=5)
        self.assertIsNotNone(qa)
        self.assertEqual(len(qa.array), 2)
        self.assertEqual(qa.array[0], 2)
        self.assertEqual(qa.array[1], 1)
        self.assertEqual(qa.quotes, 5)

    def testQuotedArrayInitQuotedArray(self):
        qa1 = ldmud.QuotedArray(ldmud.Array([4, 3]), quotes=2)
        qa2 = ldmud.QuotedArray(qa1, quotes=3)
        self.assertIsNotNone(qa2)
        self.assertEqual(len(qa2.array), 2)
        self.assertEqual(qa2.array[0], 4)
        self.assertEqual(qa2.array[1], 3)
        self.assertEqual(qa2.quotes, 5)

    def testQuotedArrayInitInvalid(self):
        with self.assertRaises(TypeError):
            ldmud.QuotedArray(1.5, 1)

class TestLvalue(unittest.TestCase):
    def testLvalueInit(self):
        lv = ldmud.Lvalue(10)
        self.assertIsNotNone(lv)

    def testLvalueValue(self):
        lv = ldmud.Lvalue(10)
        self.assertEqual(lv.value, 10)
        lv.value = 20
        self.assertEqual(lv.value, 20)

    def testLvalueCompare(self):
        lv1 = ldmud.Lvalue(10)
        lv2 = ldmud.Lvalue(10)
        self.assertEqual(lv1, lv1)
        self.assertNotEqual(lv1, lv2)

    def testLvalueArrayItem(self):
        arr = ldmud.Array([10,11,12])
        lv = ldmud.Lvalue(arr)
        lv[2].value = 42
        self.assertEqual(arr[2], 42)
        arr[0] = 100
        self.assertEqual(lv[0].value, 100)

    def testLvalueStringItem(self):
        lv = ldmud.Lvalue("Test \U0001f4a5!")
        lv[5].value = 88
        self.assertEqual(lv.value, "Test X!")

    def testLvalueArrayRange(self):
        arr = ldmud.Array([10,11,12])
        lv = ldmud.Lvalue(arr)
        lv[1:2].value = ldmud.Array([111, 112])
        self.assertEqual(list(lv.value), [10, 111, 112, 12])

    def testLvalueStringRange(self):
        lv = ldmud.Lvalue("Teststring")
        lv[4:7].value = "\uff33\uff54\uff52"
        self.assertEqual(lv.value, "Test\uff33\uff54\uff52ing")
        lv[4:8].value = ""
        self.assertEqual(lv.value, "Testng")

    def testLvalueMappingItem(self):
        m = ldmud.Mapping({1:1, 2:4, 3:9, 4:16})
        lv = ldmud.Lvalue(m)
        lv[3].value = 27
        self.assertEqual(m[3], 27)
        m[3] = 81
        self.assertEqual(lv[3,0].value, 81)

    def testLvalueMappingNewItem(self):
        m = ldmud.Mapping()
        lv = ldmud.Lvalue(m)
        item = lv[3]
        self.assertEqual(item.value, 0)
        self.assertEqual(len(m), 0)
        item.value = 9
        self.assertEqual(len(m), 1)
        self.assertEqual(m[3], 9)
        self.assertEqual(item.value, 9)

    def testLvalueStructItem(self):
        s = ldmud.Struct(ldmud.get_master(), "test_struct", (10,))
        self.assertIsNotNone(s)

        lv = ldmud.Lvalue(s)
        self.assertEqual(lv.members.t_int.value, 10)
        lv.members.t_int.value = 100
        self.assertEqual(s.members.t_int.value, 100)
        s.members.t_int.value = 1000
        self.assertEqual(lv.members.t_int.value, 1000)

class TestEfuns(unittest.TestCase):
    def testDir(self):
        self.assertGreater(len(dir(ldmud.efuns)), 200)

    def testCalls(self):
        master = ldmud.efuns.find_object("/master")
        self.assertEqual(ldmud.efuns.call_other(master, "master_fun"), 54321)
        self.assertEqual(ldmud.efuns.object_name(master), "/master")

def python_test():
    """Run the python test cases."""

    suite = unittest.TestLoader().loadTestsFromName(__name__)
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)

    return result.wasSuccessful()

def python_return(val):
    """Return the given value to check the
    type conversion between Python and LPC."""
    return val

# Test of the garbage collection.
lpc_value = None
def python_set(val):
    """Remember the value <val>."""
    global lpc_value
    lpc_value = val

def python_get():
    """Return the remembered value."""
    return lpc_value

def python_error():
    """Make an exception."""
    raise Exception("Testing")

def python_gc():
    """Do a Python garbage collection."""
    gc.collect()

def python_typecheck(arg: str, arg2: str, *args: int):
    return arg

testob_lfun = None
testob_var = None
def python_remember_testob(ob: ldmud.Object) -> None:
    """Remember a lfun and variable object."""
    global testob_lfun, testob_var
    testob_lfun = ob.functions.fun_testob
    testob_var = ob.variables.var_testob

def python_check_testob() -> bool:
    """Verify the lfun and variable object from python_remember_testob()."""
    global testob_lfun, testob_var
    if testob_lfun() != "f_testob":
        print("testob->fun_testob() returned wrong result.\n", file=sys.stderr)
        return False
    if testob_var.value != "v_testob":
        print("testob->var_testob has wrong value.\n", file=sys.stderr)
        return False
    return True

ldmud.register_efun("python_test", python_test)
ldmud.register_efun("python_return", python_return)
ldmud.register_efun("python_get", python_get)
ldmud.register_efun("python_set", python_set)
ldmud.register_efun("python_error", python_error)
ldmud.register_efun("python_gc", python_gc)
ldmud.register_efun("python_typecheck", python_typecheck)
ldmud.register_efun("python_remember_testob", python_remember_testob)
ldmud.register_efun("python_check_testob", python_check_testob)

ldmud.register_efun("abs", lambda x: x*2)
ldmud.register_efun("unregister_abs", lambda: ldmud.unregister_efun("abs"))

# Test of the hooks
num_hb = 0
def hb_hook():
    global num_hb
    num_hb += 1

ob_list = []
def ob_created(ob):
    ob_list.append(ob)

def ob_destroyed(ob):
    ob_list.remove(ob)

def get_hook_info():
    return ldmud.Array((num_hb, ldmud.Array(ob_list),))

ldmud.register_hook(ldmud.ON_HEARTBEAT, hb_hook)
ldmud.register_hook(ldmud.ON_OBJECT_CREATED, ob_created)
ldmud.register_hook(ldmud.ON_OBJECT_DESTRUCTED, ob_destroyed)

ldmud.register_efun("python_get_hook_info", get_hook_info)

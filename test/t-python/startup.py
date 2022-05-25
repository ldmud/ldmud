import sys,unittest,gc,functools,random
import ldmud

class TestModule(unittest.TestCase):
    def testMaster(self):
        ob = ldmud.Object("/master")
        self.assertEqual(ob, ldmud.get_master())

    def testSimulEfun(self):
        self.assertIsNone(ldmud.get_simul_efun())

class TestObject(unittest.TestCase):
    def testEarlyObjectLoad(self):
        self.assertTrue(early_ob_worked)

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

    def testUninitializedObject(self):
        ob = ldmud.Object.__new__(ldmud.Object)
        self.assertEqual(ldmud.efuns.copy(ob), 0)

    def testSwappedObject(self):
        swap = getattr(ldmud.efuns, 'swap', None)
        if swap:
            ob = ldmud.Object("/testob")
            swap(ob)

            fun = ob.functions.testfun
            swap(ob)
            self.assertEqual(fun.file_name, "/testob.c")
            swap(ob)
            self.assertEqual(fun.arguments[1].type, ldmud.Array)
            swap(ob)
            self.assertEqual(fun(10, "A", "B", "C"), 3)

            swap(ob)
            var = ob.variables.testvar
            swap(ob)
            self.assertEqual(var.name, "testvar")
            swap(ob)
            var.value = 100
            swap(ob)
            self.assertEqual(var.value, 100)
            var.value = 42

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

    def testUninitializedLWObject(self):
        lwob = ldmud.LWObject.__new__(ldmud.LWObject)
        self.assertEqual(ldmud.efuns.copy(lwob), 0)

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

    def testUninitializedArray(self):
        arr = ldmud.Array.__new__(ldmud.Array)
        ca = ldmud.efuns.copy(arr)
        self.assertIsNotNone(ca)
        self.assertEqual(len(ca), 0)

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

        with self.assertRaises(RuntimeError):
            del m["One",1]

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

    def testGetSetRange(self):
        m = ldmud.Mapping((( "K", 0, 1, 2, 3, 4, 5, 6, 7 ),))

        self.assertIsNotNone(m)
        self.assertEqual(len(m), 1)
        self.assertEqual(m.width, 8)

        self.assertEqual(list(m["K",3:5]), [3,4])
        self.assertEqual(list(m["K",5:3:-1]), [5,4])
        self.assertEqual(list(m["K",::-2]), list(range(7,0,-2)))

        m["K",::-1] = ldmud.Array(range(8))
        self.assertEqual(list(m["K",:]), list(range(7,-1,-1)))

        m["K",5:6] = ldmud.Array([10])
        self.assertEqual(list(m["K",:]), [7, 6, 5, 4, 3, 10, 1, 0])

        m["K",4:2:-1] = ldmud.Array([100,101])
        self.assertEqual(list(m["K",:]), [7, 6, 5, 101, 100, 10, 1, 0])

        with self.assertRaises(ValueError):
            m["K",1:2] = ldmud.Array([-1,-2])
        with self.assertRaises(ValueError):
            m["K",1:2] = ldmud.Array([])
        with self.assertRaises(RuntimeError):
            del m["K",1:2]

    def testIterator(self):
        m = ldmud.Mapping( { "One": 1, "Two": 2, "Three": 3 } );
        self.assertEqual(dict(m), { "One": 1, "Two": 2, "Three": 3})
        self.assertEqual(set(m.keys()), set(("One", "Two", "Three",)))
        self.assertEqual(set(m.values()), set((1,2,3,)))

    def testUninitializedMapping(self):
        m = ldmud.Mapping.__new__(ldmud.Mapping)
        cm = ldmud.efuns.copy(m)
        self.assertIsNotNone(cm)
        self.assertEqual(len(cm), 0)

    def testInitLeak(self):
        DI_NUM_MAPPINGS = -415
        num_arr = ldmud.efuns.driver_info(DI_NUM_MAPPINGS)

        m = ldmud.Mapping({"A": ldmud.Mapping() })
        self.assertEqual(ldmud.efuns.driver_info(DI_NUM_MAPPINGS), num_arr+2)

        del m
        self.assertEqual(ldmud.efuns.driver_info(DI_NUM_MAPPINGS), num_arr)

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

    def testUninitializedStruct(self):
        s = ldmud.Struct.__new__(ldmud.Struct)
        self.assertEqual(ldmud.efuns.copy(s), 0)

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
        self.assertEqual(ldmud.efuns.copy(s), 0)

    def testSwappedObjectLfun(self):
        swap = getattr(ldmud.efuns, 'swap', None)
        if swap:
            ob = ldmud.Object("/testob")
            swap(ob)
            c = ldmud.Closure(ob, "testfun", ob)
            swap(ob)
            self.assertEqual(c(10, "A", "B", "C"), 3)
            ldmud.efuns.destruct(ob)

class TestCoroutine(unittest.TestCase):
    def setUp(self):
        self.ob = ldmud.Object("/testob");
        self.lwob = ldmud.LWObject("/testob")

    def testCRBlueprint(self):
        cr = self.ob.functions.testcoroutine("A", "B", "C")

        self.assertIn("testob", repr(cr))
        self.assertIn("testcoroutine", repr(cr))

        self.assertEqual(cr.object, self.ob)
        self.assertEqual(cr.program_name, "/testob.c")
        self.assertEqual(cr.function_name, "testcoroutine")
        self.assertEqual(cr.file_name, "/testob.c")
        self.assertEqual(cr.line_number, 26)
        self.assertIn('args', dir(cr.variables))
        self.assertIn('args', cr.variables.__dict__)
        self.assertEqual(list(cr.variables.args), [ "A", "B", "C" ])

        self.assertTrue(cr)
        self.assertEqual(cr(), 3);
        self.assertTrue(cr)
        self.assertEqual(cr.file_name, "/testob.c")
        self.assertEqual(cr.line_number, 28)
        self.assertIn('args', dir(cr.variables))
        self.assertIn('args', cr.variables.__dict__)
        self.assertEqual(list(cr.variables.args), [ "A", "B", "C" ])
        self.assertIn('local', dir(cr.variables))
        self.assertIn('local', cr.variables.__dict__)
        self.assertEqual(cr.variables.local, "X")
        cr.variables.local = "Y"
        self.assertEqual(cr.variables.local, "Y")

        self.assertEqual(list(cr("D")), ["A", "B", "C", "D", "Y"])
        self.assertFalse(cr)

    def testCRClone(self):
        ob = ldmud.efuns.clone_object(self.ob)
        cr = ob.functions.testcoroutine("A", "B", "C")

        self.assertIn("testob", repr(cr))
        self.assertIn("testcoroutine", repr(cr))

        self.assertEqual(cr.object, ob)
        self.assertEqual(cr.program_name, "/testob.c")
        self.assertEqual(cr.function_name, "testcoroutine")
        self.assertEqual(cr.file_name, "/testob.c")
        self.assertEqual(cr.line_number, 26)
        self.assertIn('args', dir(cr.variables))
        self.assertIn('args', cr.variables.__dict__)
        self.assertEqual(list(cr.variables.args), [ "A", "B", "C" ])

        ldmud.efuns.destruct(ob)
        self.assertFalse(cr)

    def testCRLWObject(self):
        cr = self.lwob.functions.testcoroutine("A", "B", "C")

        self.assertIn("testob", repr(cr))
        self.assertIn("testcoroutine", repr(cr))

        self.assertEqual(cr.object, self.lwob)
        self.assertEqual(cr.program_name, "/testob.c")
        self.assertEqual(cr.function_name, "testcoroutine")

        self.assertTrue(cr)
        self.assertEqual(cr(),3);
        self.assertTrue(cr)
        self.assertEqual(list(cr("D")), ["A", "B", "C", "D", "X"])
        self.assertFalse(cr)

    def testEmpty(self):
        c = ldmud.Coroutine.__new__(ldmud.Coroutine)
        with self.assertRaises(Exception):
            c()
        self.assertFalse(c)
        self.assertEqual(ldmud.efuns.copy(c), 0)

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

    def testUninitializedSymbol(self):
        s = ldmud.Symbol.__new__(ldmud.Symbol)
        self.assertEqual(ldmud.efuns.copy(s), 0)

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

    def testUninitializedQuotedArray(self):
        qa = ldmud.QuotedArray.__new__(ldmud.QuotedArray)
        self.assertEqual(ldmud.efuns.copy(qa), 0)

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

    def testLvalueMappingRange(self):
        m = ldmud.Mapping(((1,11,12,13,14,15), (2,21,22,23,24,25), (3,31,32,33,34,35),))
        lv = ldmud.Lvalue(m);

        lvr1 = lv[1,1:3]
        self.assertEqual(list(lvr1.value), [12,13])
        self.assertEqual(list(lvr1[:].value), [12,13])
        self.assertEqual(lvr1[0].value, 12)
        self.assertEqual(len(lvr1), 2)
        lvr1[0].value = 112
        self.assertEqual(m[1,1], 112)

        lvr2 = lv[2,2:0:-1]
        self.assertEqual(list(lvr2.value), [23,22])
        self.assertEqual(list(lvr2[:].value), [23,22])
        self.assertEqual(lvr2[0].value, 23)
        self.assertEqual(len(lvr2), 2)
        lvr2[0].value = 223
        self.assertEqual(m[2,2], 223)

    def testLvalueMappingNewRange(self):
        m = ldmud.Mapping(width = 5)
        lv = ldmud.Lvalue(m);

        lvr1 = lv[1,1:3]
        self.assertEqual(list(lvr1.value), [0,0])
        self.assertEqual(list(lvr1[:].value), [0,0])
        self.assertEqual(lvr1[0].value, 0)
        self.assertEqual(len(lvr1), 2)
        self.assertEqual(len(m), 0)
        lvr1[0].value = 112
        self.assertEqual(len(m), 1)
        self.assertEqual(list(m[1,:]), [0,112,0,0,0])

        lvr2 = lv[2,2:0:-1]
        self.assertEqual(list(lvr2.value), [0,0])
        self.assertEqual(list(lvr2[:].value), [0,0])
        self.assertEqual(lvr2[0].value, 0)
        self.assertEqual(len(lvr2), 2)
        lvr2[0].value = 223
        self.assertEqual(len(m), 2)
        self.assertEqual(list(m[2,:]), [0,0,223,0,0])

    def testLvalueStructItem(self):
        s = ldmud.Struct(ldmud.get_master(), "test_struct", (10,))
        self.assertIsNotNone(s)

        lv = ldmud.Lvalue(s)
        self.assertEqual(lv.members.t_int.value, 10)
        lv.members.t_int.value = 100
        self.assertEqual(s.members.t_int.value, 100)
        s.members.t_int.value = 1000
        self.assertEqual(lv.members.t_int.value, 1000)

    def testUninitializedLvalue(self):
        lv = ldmud.Lvalue.__new__(ldmud.Lvalue)
        self.assertEqual(ldmud.efuns.copy(lv), 0)

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

# Create a distinct class for use in LPC
class bigint(int):
    def __efun_to_int__(self) -> int:
        return int(self)

    def __efun_to_string__(self) -> str:
        return str(self)

    def __save__(self):
        return str(self)

    @staticmethod
    def __restore__(val):
        return bigint(val)

# Modify all operations to return bigint instead of int.
for funname in ( '__add__', '__radd__', '__sub__', '__rsub__', '__mul__', '__rmul__', '__truediv__', '__rtruediv__', '__mod__', '__rmod__', '__lshift__', '__rlshift__', '__rshift__', '__rrshift__', '__and__', '__rand__', '__or__', '__ror__', '__xor__', '__rxor__',):
    def get_replacement_fun(f):
        @functools.wraps(f)
        def replacement(self, other: (int, bigint)) -> bigint:
            return bigint(f(self, other))
        return replacement

    fun = getattr(bigint, funname, None)
    if fun:
        setattr(bigint, funname, get_replacement_fun(fun))

for funname in ( '__neg__', '__invert__',):
    def get_replacement_fun(f):
        @functools.wraps(f)
        def replacement(self) -> bigint:
            return bigint(f(self))
        return replacement

    fun = getattr(bigint, funname, None)
    if fun:
        setattr(bigint, funname, get_replacement_fun(fun))

# Add reverse comparison functions (and annotations to the existing one).
for funname, rfunname in (('__lt__','__rgt__',),('__le__','__rge__',),('__eq__','__req__',),('__ne__','__rne__',),('__gt__','__rlt__'),('__ge__','__rle__',),):
    def get_replacement_fun(f):
        @functools.wraps(f)
        def replacement(self, other: (int, bigint)) -> bool:
            return f(self, other)
        return replacement

    fun = getattr(bigint, funname, None)
    if fun:
        setattr(bigint, funname, get_replacement_fun(fun))
        setattr(bigint, rfunname, get_replacement_fun(fun))

# Efuns to create and read bigint.
def to_bigint(i: int) -> bigint:
    return bigint(i)

class random_generator(random.Random):
    # Implement call_strict() to call functions in this object.
    def __efun_call_strict__(self, funname: str, *args):
        if not funname.startswith("_") and hasattr(self, funname):
            return getattr(self, funname)(*args)
        raise AttributeError("Function %s() not found." % (funname,))

def create_random_generator(seed: int = None) -> random_generator:
    return random_generator(None if seed == 0 else seed)

class box:
    def __init__(self, val):
        self.value = val

    def get_value(self):
        return self.value

    def set_value(self, val):
        self.value = val

    def __efun_call_strict__(self, funname: str, *args):
        if not funname.startswith("_") and hasattr(self, funname):
            return getattr(self, funname)(*args)
        raise AttributeError("Function %s() not found." % (funname,))

    def __copy__(self):
        return box(self.value)

    def __save__(self):
        return self.value

    @staticmethod
    def __restore__(val):
        return box(val)

def create_box(value) -> box:
    return box(value)

ldmud.register_hook(ldmud.ON_HEARTBEAT, hb_hook)
ldmud.register_hook(ldmud.ON_OBJECT_CREATED, ob_created)
ldmud.register_hook(ldmud.ON_OBJECT_DESTRUCTED, ob_destroyed)

ldmud.register_efun("python_get_hook_info", get_hook_info)
ldmud.register_type("bigint", bigint)
ldmud.register_efun("to_bigint", to_bigint)
ldmud.register_type("random_generator", random_generator)
ldmud.register_efun("create_random_generator", create_random_generator)
ldmud.register_type("box", box)
ldmud.register_efun("create_box", create_box)

# Test loading objects at startup
early_ob = ldmud.Object("/testob")
if early_ob is not None and early_ob.name == "/testob":
    early_ob_worked = True
    ldmud.efuns.destruct(early_ob)
else:
    early_ob_worked = False

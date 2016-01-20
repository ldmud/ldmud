import sys,unittest
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

    def testInitLoad(self):
        oldob = ldmud.efuns.find_object("/testob")
        if oldob:
            ldmud.efuns.destruct(oldob)
        ob = ldmud.Object("/testob")
        self.assertIsNotNone(ob)

    def testInitNonExisting(self):
        with self.assertRaises(RuntimeError):
            ob = ldmud.Object("/imnotthere")

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

class TestStruct(unittest.TestCase):
    def setUp(self):
        self.master = ldmud.efuns.find_object("/master")

    def testInitSimple(self):
        s = ldmud.Struct(self.master, "test_struct")
        self.assertIsNotNone(s)

    def testInitValueTuple(self):
        s = ldmud.Struct(self.master, "test_struct", (42, 1.5, 'Hi',))
        self.assertIsNotNone(s)
        self.assertEqual(s.t_int, 42)
        self.assertEqual(s.t_float, 1.5)
        self.assertEqual(s.t_string, 'Hi')

    def testInitValueList(self):
        s = ldmud.Struct(self.master, "test_struct", [42, 1.5, 'Hi'])
        self.assertIsNotNone(s)
        self.assertEqual(s.t_int, 42)
        self.assertEqual(s.t_float, 1.5)
        self.assertEqual(s.t_string, 'Hi')

    def testInitValueMap(self):
        s = ldmud.Struct(self.master, "test_struct", { 't_int': 42, 't_float': 1.5, 't_string': 'Hi'})
        self.assertIsNotNone(s)
        self.assertEqual(s.t_int, 42)
        self.assertEqual(s.t_float, 1.5)
        self.assertEqual(s.t_string, 'Hi')

    def testSetValue(self):
        s = ldmud.Struct(self.master, "test_struct")
        self.assertIsNotNone(s)
        s.t_int = 123
        s.t_float = 5.5
        s.t_string = 'Hello!'
        self.assertEqual(s.t_int, 123)
        self.assertEqual(s.t_float, 5.5)
        self.assertEqual(s.t_string, 'Hello!')

class TestClosure(unittest.TestCase):
    def setUp(self):
        self.master = ldmud.get_master()

    def testEfun(self):
        s = ldmud.Closure(self.master, "this_object")
        self.assertIsNotNone(s)
        self.assertEqual(s(), self.master)

    def testOperator(self):
        s = ldmud.Closure(self.master, ",")
        self.assertIsNotNone(s)
        with self.assertRaises(RuntimeError):
            s()

    def testLfun(self):
        s = ldmud.Closure(self.master, "master_fun", self.master)
        self.assertIsNotNone(s)
        self.assertEqual(s(), 54321)

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

ldmud.register_efun("python_test", python_test)
ldmud.register_efun("python_return", python_return)
ldmud.register_efun("python_get", python_get)
ldmud.register_efun("python_set", python_set)

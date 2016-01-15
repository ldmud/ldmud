import sys,unittest
import ldmud

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

ldmud.register_efun("python_test", python_test)
ldmud.register_efun("python_return", python_return)

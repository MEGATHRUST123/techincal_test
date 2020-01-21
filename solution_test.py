import unittest
from solution import *

class MyTest(unittest.TestCase):
    def test(self):

        self.assertEqual(printStarHub(99.999), None)
        self.assertEqual(printStarHub(-5), None)
        self.assertEqual(printStarHub(13), '13')
        self.assertEqual(printStarHub(24), 'Star')
        self.assertEqual(printStarHub(49), 'Hub')
        self.assertEqual(printStarHub(28), 'StarHub')

if __name__ == '__main__':
    unittest.main()

import io
import sys
import unittest
import math
from solution import print_star_hub


class testsolution(unittest.TestCase):
    """
    # Possible test cases:
        1. count the number of output - must be 100
        2. check if the printed output matches expected results (sample unit cases)
        3. Count the number of "star" printed
        4. Count the number of "Hub" printed
        5. Count the number of "StarHub" printed
    """

    def test_fun_100(self):
        capturedOutput = io.StringIO()               
        sys.stdout = capturedOutput   
        print_star_hub().print_100()
        sys.stdout = sys.__stdout__
        string = capturedOutput.getvalue()                 
        return [v for v in string.split('\n') if v != '']
        
    def test_count(self):
        """
            Count the number of output - must be 100
        """
        self.assertEqual(len(self.test_fun_100()), 100)

    def test_multi_2(self):
        """
            Count the number of "Star" printed
        """
        self.assertEqual(len([v for v in self.test_fun_100() if v == 'Star']), 50 - math.floor(100/14))

    def test_multi_7(self):
        """
            Count the number of "Hub" printed
        """
        self.assertEqual(len([v for v in self.test_fun_100() if v == 'Hub']), math.floor(100/7) - math.floor(100/14))

    def test_multi_14(self):
        """
            Count the number of "StarHub" printed
        """
        self.assertEqual(len([v for v in self.test_fun_100() if v == 'StarHub']), math.floor(100/14))

    def test_each(self):
        """
            Check if the printed output matches expected results (sample unit cases)
        """
        self.assertEqual(self.test_fun_100()[4], '5')
        self.assertEqual(self.test_fun_100()[6], 'Hub')
        self.assertEqual(self.test_fun_100()[27], 'StarHub')
        self.assertEqual(self.test_fun_100()[39], 'Star')

if __name__ == '__main__':
    unittest.main()

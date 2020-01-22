import pandas as pd
import numpy as np


class print_star_hub():
    """
    # Description of function:
    Function takes in a number from 1 to 100 and prints out based on the following rules:
    1.	When the number is divisible by 2  only, print star instead of number
    2.	When the number is divisible by 7 only, prints “hub” instead of number
    3.	Number is divisible by both 2 and 7, prints “starhub” instead of number
    4.	Otherwise prints number (not divisible by 2 and 7)
    # Input:
        1. n -  integer
    # Output:
        prints the correct string 
    # Unit Testing
    >>> print_star_hub(n = 99.9) # Returns an error message 
    >>> print_star_hub(n = 34) # prints "Star"
    >>> print_star_hub(n = 49) # prints "Hub"
    >>> print_star_hub(n = 28) # prints "StarHub"
    >>> print_star_hub(n = -0.01) # Returns an error message
    >>> print_star_hub(n = 0) # Returns an error message
    """

    def printStarHub(self, n):
        print_str = {2: 'Star', 7: 'Hub'}
        # find prime number
        prime_no = [v for v in [2, 7] if n % v == 0]
        output_list = [print_str[v] if len(prime_no) != 0 else print(n) for v in prime_no]

        if len(output_list) == 0:
            output_list = output_list + [str(n)]

        print(str(''.join(output_list)))

    def print_100(self):
        # Print all numbers in the list:
        for i in [v+1 for v in range(100)]:
            self.printStarHub(n=i)

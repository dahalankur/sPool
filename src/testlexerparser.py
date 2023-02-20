# runtests.py
# Run the tests for sPool
# Team Nautilus

import sys
import os
import glob
from subprocess import Popen, PIPE

SCRIPT_DIR = os.path.dirname(os.path.realpath(__file__))
EXECUTABLE = SCRIPT_DIR + "/toplevel.native"
FAILED = False

def run_test(test):
    global FAILED
    
    print("Testing " + test.split("/")[-1] + ":\n")

    success_tests = sorted(glob.glob(test + "/test-*.sP"))
    success_expected = sorted(glob.glob(test + "/test-*.out"))
    fail_tests = sorted(glob.glob(test + "/fail-*.sP"))
    fail_expected = sorted(glob.glob(test + "/fail-*.err"))

    # First run the tests that should succeed
    for test, expected in zip(success_tests, success_expected):
        name = test.split("/")[-1]
        output = Popen([EXECUTABLE], stderr=PIPE, stdout=PIPE, stdin=PIPE).communicate(input=open(test, "rb").read())[0].decode("utf-8")
        with open(expected, "r") as f: expected_output = f.read()
        if output != expected_output:
            print(f"Test {name} FAILED. Expected output: {expected_output}, got: {output}")
            FAILED = True
        else:
            print(f"Test {name} PASSED.")
    
    # Run the tests that should fail
    for test, expected in zip(fail_tests, fail_expected):
        name = test.split("/")[-1]
        
        # read from stderr instead of stdout
        output = Popen([EXECUTABLE], stderr=PIPE, stdout=PIPE, stdin=PIPE).communicate(input=open(test, "rb").read())[1].decode("utf-8")
        with open(expected, "r") as f: expected_output = f.read()
        
        # empty stderr indicates the test expected to fail, but didn't
        if output == "" or output != expected_output:
            print(f"Test {name} FAILED. Expected output: {expected_output}, got: {output}")
            FAILED = True
        else:
            print(f"Test {name} PASSED.")
    
    print("-----------------\n")



def main(to_test):
    tests = []
    tests_dir = os.path.dirname(os.path.realpath(__file__)) + "/../tests/"

    if to_test == "all":
        print("Running all tests\n")
        tests = glob.glob(tests_dir + "*")
    else:
        if os.path.exists(tests_dir + to_test):
            tests.append(tests_dir + to_test)

    if not os.path.exists(EXECUTABLE):
        print("toplevel.native not found. Please run 'make' first in the src directory.")
        sys.exit(1)

    for test in tests:
        run_test(test)
    
    if FAILED:
        print("Some tests failed. Please check the output above.")
        sys.exit(1)
    else:
        print("All tests passed.")
    
if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: python3 runtests.py [all|testname]")
        sys.exit(1)
    main(sys.argv[1])
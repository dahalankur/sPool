#  testlexerparser.py
#  Run the tests for sPool
#  This script can be run as a standalone script, or from the Makefile
#  Usage: python3 runtests.py dir [all|testname], where testname is the name of a 
#                                             test directory in tests/ and 
#                                             dir is the directory containing
#                                             the subdirectories of tests
#  Written by Team Nautilus on 02/20/2023

import sys
import os
import glob
from subprocess import Popen, PIPE

SCRIPT_DIR = os.path.dirname(os.path.realpath(__file__))
EXECUTABLE = SCRIPT_DIR + "/toplevel.native"
ARGS = ""
FAILED = False

# Takes a string as an argument, the path to a test directory
# Runs the tests in the directory and reports the results
def run_test(codegen, test):
    global FAILED
    
    print("Testing " + test.split("/")[-1] + ":\n")

    if ".sP" in test:
        # individual testing
        if "test-" in test:
            success_tests = [test]
            success_expected = [test.replace(".sP", ".out")]
            fail_tests = []
            fail_expected = []
        else:
            success_tests = []
            success_expected = []
            fail_tests = [test]
            fail_expected = [test.replace(".sP", ".err")]
    else:
        # batch testing
        success_tests = sorted(glob.glob(test + "/test-*.sP"))
        success_expected = sorted(glob.glob(test + "/test-*.out"))
        fail_tests = sorted(glob.glob(test + "/fail-*.sP"))
        fail_expected = sorted(glob.glob(test + "/fail-*.err"))

    # Run the tests that should succeed
    for test, expected in zip(success_tests, success_expected):
        name = test.split("/")[-1]
        
        output = Popen([EXECUTABLE, ARGS], stderr=PIPE, stdout=PIPE, stdin=PIPE).\
            communicate(input=open(test, "rb").read())
        stdout = output[0].decode("utf-8")
        stderr = output[1].decode("utf-8")
        
        if stderr != "":
            print(f"\033[91mTest {name} FAILED.\033[0m Expected no errors, got:\n{stderr}")
            FAILED = True
            continue

        if codegen:
            # run compile.sh script on input file to get the executable
            output = Popen([f"{SCRIPT_DIR}/compile.sh", test, name + ".exe"], stderr=PIPE, stdout=PIPE).communicate()

            # run the executable and get the output
            output = Popen([f"{SCRIPT_DIR}/{name}.exe"], stderr=PIPE, stdout=PIPE).communicate()
            stdout = output[0].decode("utf-8")
            stderr = output[1].decode("utf-8")
            if stderr != "":
                print(f"\033[91mTest {name} FAILED.\033[0m Expected no errors, got:\n{stderr}")
                FAILED = True
                continue

        with open(expected, "r") as f: expected_output = f.read()
        
        # diff expected and actual output
        if stdout != expected_output:
            print(f"\033[91mTest {name} FAILED.\033[0m Expected output: {expected_output}, got:\n{stdout}")
            FAILED = True
        else:
            print(f"\033[92mTest {name} PASSED.\033[0m")
    
    # Run the tests that should fail
    for test, expected in zip(fail_tests, fail_expected):
        name = test.split("/")[-1]
        
        # read from stderr instead of stdout
        output = Popen([EXECUTABLE, ARGS], stderr=PIPE, stdout=PIPE, stdin=PIPE).\
            communicate(input=open(test, "rb").read())[1].decode("utf-8")
        with open(expected, "r") as f: expected_output = f.read()
        
        # empty stderr indicates the test was expected to fail, but didn't
        if output == "" or output != expected_output:
            
            print(f"\033[91mTest {name} FAILED.\033[0m Expected output: {expected_output}, got:\n{output}")
            FAILED = True
        else:
            # print this in GREEN color in the terminal
            print(f"\033[92mTest {name} PASSED.\033[0m")

    
    print("\n" + "-----------------" * 3 + "\n")

def cleanup(base_dir):
    # remove all the .ll and .exe files that were created
    for f in glob.glob(f"{base_dir}/*.ll", recursive=True):
        os.remove(f)
    for f in glob.glob(f"{base_dir}/*.exe", recursive=True):
        os.remove(f)
    for f in glob.glob(f"{base_dir}/*.s", recursive=True):
        os.remove(f)

# Takes a string as an argument, either "all" or the name of a test directory
# in tests/ and runs and reports the results of the tests
def main(to_test):
    global ARGS

    base_test_dir = to_test[0]
    to_test = to_test[1:]
    
    tests = []
    tests_dir = os.path.dirname(os.path.realpath(__file__)) + f"/../tests/{base_test_dir}/"

    if to_test[0] == "all":
        if to_test[1] == "_":
            print("Running all tests\n")
            tests = glob.glob(tests_dir + "*")
        else:
            if os.path.exists(tests_dir + to_test[1]):
                tests = glob.glob(tests_dir + to_test[1])
            else:
                print(f"\033[91mThe requested directory does not exist.\033[0m\n")
                sys.exit(1)
    else:
        if os.path.exists(tests_dir + to_test[0] + "/" + to_test[1] + ".sP"):
            filename = tests_dir + to_test[0] + "/" + to_test[1] + ".sP"
            tests.append(filename)
        else:
            print("The requested test does not exist. Please ensure you typed the correct name. \nTo double check the name of the test, look at the tests under the tests/ directory.\n\nFor example, for running codegen tests on the test-if1.sP file, run python3 runtests.py codegen if test-if1\n")

    if not os.path.exists(EXECUTABLE):
        print("toplevel.native not found. Please run 'make' first in the src directory.")
        sys.exit(1)

    # send correct arguments to toplevel.native
    if base_test_dir == "lexerparser":
        ARGS += "-a"
    elif base_test_dir == "codegen":
        ARGS += "-c"

    for test in tests:
        run_test(base_test_dir == "codegen", test)
    
    cleanup(SCRIPT_DIR) # clean up intermediate files and executables generated by the tests

    if FAILED:
        print(f"\033[91mSome tests failed. Please check the test output above.\033[0m")
        sys.exit(1)
    else:
       print(f"\033[92mAll tests passed.\033[0m")


if __name__ == '__main__':
    if len(sys.argv) != 4:
        print("Usage: python3 runtests.py testdir [all|testname] [filename]")
        # if all is given, filename is ignored
        # if testname is given, filename is the individual test file to run
        sys.exit(1)
    main(sys.argv[1:])
The sPool Programming Language
       Team Nautilus
  
Yuma Takahashi (yuma.takahashi@tufts.edu)
Max Mitchell   (maxwell.mitchell@tufts.edu)
Ankur Dahal    (ankur.dahal@tufts.edu)
Etha Hua       (tianze.hua@tufts.edu)

--------------------------------------------------------------------------------

For more information about the sPool programming language, please see the 
[project report](./docs/report/Nautilus.pdf).

Scripts provided:

NOTE: For all scripts mentioned below, please ensure that you are within the `src` 
directory.

- Compile the compiler:
       A Makefile is provided which compiles the compiler into an executable. 
       When present in the `src` directory, type `make` to generate the executable 
       file named `toplevel.native`. To successfully compile the sPool compiler, it 
       is expected that the tools required to compile the MicroC compiler are 
       present on the host machine, i.e., basic OCaml toolchain (ocaml, ocamlbuild, 
       ocamllex, ocamlyacc, ocamlfind, opam) as well as LLVM (LLVM on the host 
       machine and the Ocaml LLVM API installed using opam) must be present. 
       Other than these tools and Python3 (tested on versions 3.8.10 and higher, 
       although lower versions should still work), no additional packages/tools 
       are needed to compile the sPool compiler or to run any of the 
       included tests.

- Run all tests:
       To run all tests in this integration testing suite, type:
       
              `make testcodegen` or `make testparser`
       depending on if you want to run tests for codegen or lexer/parser
       respectively.

       This should automatically compile the compiler and run our `runtests.py` python script
       to run all tests in the testsuite.

- Run all tests in a specific directory:
        To run all tests in a testname directory type:

            `python3 runtests.py <TESTPHASE> all <TESTDIR>` 
        
        where <TESTPHASE> is the name of the directory in sPool/tests/
        and <TESTDIR> is the name of the directory in sPool/tests/<TESTPHASE>/

- Run a single test:

       To run a single test, type:
              `python3 runtests.py <TESTPHASE> <TESTDIR> <TESTNAME>` 

       where <TESTPHASE> is the name of the directory in sPool/tests/,       
       <TESTDIR> is the name of the directory in sPool/tests/codegen/ and <TESTNAME> is the 
       name of the test without the extension `.sP` in sPool/test/codegen/<TESTDIR>/
       
       For example, if you want to run an individual test on the test-list8.sP file under 
       sPool/tests/codegen/list, type:
              `python3 runtests.py codegen list test-list8`
       
       Similarly, if you want to run the test on the fail-strings1.sP file 
       under sPool/tests/codegen/strings, type:
              `python3 runtests.py codegen strings fail-strings1`

- Run the compiler on a given source file:

       To compile a specific sPool source file, type
              `./compile.sh [-stdlib] <file> <executable>`
       
       where <file> is the sPool src file to be compiled, and the <executable>
       is the name of the executable generated. [-stdlib] is an optional flag
       which, if included, will include all standard library functions in 
       compilation for use by the program. The intermediate files generated 
       in the process will be present in the same directory from which the 
       compile script was run. There are two files generated for each sPool 
       source file compiled -- the LLVM IR (having .ll extension) and the 
       assembly file (having .s extension).

       The sPool source code for functions in the sPool standard library can 
       be found in the stdlib/ directory.

       For instance, if you want to compile a sPool source file hello.sP, run:
              `./compile.sh hello.sP a.out`
       The resulting executable will be named a.out, so running `./a.out` will 
       execute the program.

--------------------------------------------------------------------------------

The sPool integration testing suite:

- Programs present in the testing suite:
  Our testing suite follows the format of the testing suite we submitted for 
  our extended testsuite deliverable: there are directories under tests/
  that contain a bunch of tests. Tests that are expected to succeed are 
  named with test-<testname[n]>.sP with their corresponding gold-standard output 
  stored in test-<testname[n]>.out, whereas tests that are expected to fail 
  are named with fail-<testname[n]>.sP with their corresponding gold-standard 
  error output stored in fail-<testname[n]>.err. The description of the 
  programs present in our testing suite is given below:
      LEXER AND PARSER: 
      These tests are present in the tests/lexerparser directory.

       - fail-assign1 : Checks that LHS must be a name, not a literal value
       - fail-assign2 : Checks that LHS cannot be a type
       - fail-assign3 : Checks that only an expression may be assigned to a name
       - fail-assign4 : Checks that names may not have spaces 
       - test-assign1 : Checks basic assignments (name = var, name = literal, etc)
       - test-assign2 : Checks that lambda, call, and expr may be assigned to names
       - test-assign3 : Checks various self-assignment cases
       - test-assign4 : Checks lists, floats, strings
       - test-assign5 : Stress testing assignment on more complex cases
       
       - fail-binop1 : May not have two binops in a row; need expressions between them
       - fail-binop2 : Parentheses must match
       - fail-binop3 : Binops must have one expression on each side 
       - fail-binop4 : Binops may not have more than one expression on each side
       - fail-binop5 : Cannot combine assignment and binop (ala C++'s `+=`)
       - test-binop1 : Addition and subtraction
       - test-binop2 : Multiplication, division, and modulo
       - test-binop3 : Equality and comparison
       - test-binop4 : Parentheses for arithmetic
       - test-binop5 : Parentheses for equality and comparison
       
       - fail-call1 : Arguments to functions must be an expression
       - fail-call2 : Parsing error due to unmatched parentheses in function call
       - fail-call3 : Wrong number of parens while calling a function leads to an error
       - fail-call4 : Function names may not begin with a number
       - fail-call5 : Layered parens are not allowed while calling functions
       - fail-call6 : Empty argument list with commas fails for function calls
       - test-call1 : Multiple function calls with well-formed arguments
       - test-call2 : Function calls made as the RHS of a variable definition
       - test-call3 : Nested function calls with multiple layers are tested
       - test-call4 : Testing function calls with expressions that are not literals

       - fail-define1 : May not use reserved words (types) as variable names
       - fail-define2 : Must use valid type before name in definition
       - fail-define3 : Arrow types must have at least one argument type
       - fail-define4 : Arrow types must have one return type
       - test-define1 : Int, bool, float, and quack definition (quack definition is 
                        syntactically valid but semantically invalid)
       - test-define2 : String, mutex, and thread definitions
       - test-define3 : List and function definitions
       - test-define4 : Nested lists definitions
       - test-define5 : Many argument lambda definitions
       - test-define6 : Lambda definition stress tests
       - test-define7 : List of lambdas definition

       - fail-fundef1 : Assignment of fundef to variables should fail
       - fail-fundef2 : Functions with no return types are invalid
       - fail-fundef3 : Functions that do not specify parameter names are invalid
       - test-fundef1 : Checks that nested function definitions work
       - test-fundef2 : Checks that a recursive function with the store keyword is valid
       - test-fundef3 : Checks empty function body and empty params/return values
       - test-fundef4 : A higher order function that takes in a function and returns a function is valid

       - fail-if1 : If statement with empty conditional is invalid
       - fail-if2 : Semicolons not closing the if body is an error
       - fail-if3 : If statement with no colon is a parsing error
       - fail-if4 : If statement with no parens in conditional is invalid
       - fail-if5 : an else clause with a colon after it is a parsing error
       - fail-if6 : if is closed with semicolon and else proceeds it immediately; it is a parsing error
       - fail-if7 : Testing that the if conditional is an expression and not a statement
       - test-if1 : Testing a simple if statement is valid
       - test-if2 : If used with an else is valid
       - test-if3 : Testing if with an empty body
       - test-if4 : Testing the dangling else problem

       - fail-lambda1 : Lambda must have return type declared
       - fail-lambda2 : Lambda must have formal types declared
       - fail-lambda3 : Formals must have parentheses around them
       - fail-lambda4 : Lambdas must end with a semicolon
       - test-lambda1 : Any number of formals is valid
       - test-lambda2 : Checks a variety of syntactically valid bodies
       - test-lambda3 : Nested lambdas
       - test-lambda4 : Stress test lambda with many statement types
       - test-lambda5 : Lambda as an argument to a function

       - fail-literal1 : Integers with characters are invalid
       - fail-literal2 : Floating point literals have to start with a digit
       - fail-literal3 : Two literals on the same line are invalid
       - fail-literal4 : Strings without explicit newlines are invalid
       - fail-literal5 : Unmatched parentheses for lists
       - test-literal1 : Some simple integer literals
       - test-literal2 : Some simple float literals
       - test-literal3 : Some simple bool literals with other non-bool names
       - test-literal4 : Some string literals with newlines
       - test-literal5 : Well-typed list literals are valid

       - fail-return1 : Can only return an expression
       - fail-return2 : Can't return a return expression
       - fail-return3 : Can't assign a return expression to a name
       - fail-return4 : Return may not be used in formals
       - fail-return5 : Return may not be parenthesized
       - test-return1 : Returning basic expressions
       - test-return2 : Returning no expression (for quack functions)
       - test-return3 : Multiple returns (syntactically valid, semantically invalid)
       - test-return4 : Returns with composite expressions
       - test-return5 : Returning a lambda from a lambda

       - fail-thread1 : detects that statements can not be on the same line
       - test-thread1 : Nested threads on the same line with empty body are valid
       - test-thread2 : An empty thread with no body is valid
       - test-thread3 : A non-empty thread body is valid
       
       - fail-unop1 : Unary operator after the expression is invalid
       - fail-unop2 : Same as fail-unop1, but for negation
       - test-unop1 : Testing n numbers of negation on boolean values
       - test-unop2 : Testing unary operators on booleans inside parentheses
       - test-unop3 : Testing arithmetic negation
       - test-unop4 : Same as unop3, but with parentheses and nested expressions
       - test-unop5 : Testing mixture of unary operators
       
       - fail-var1 : detect that valid variable names can not begin with a number
       - fail-var2 : detect that valid variable names can not begin with an underscore
       - fail-var3 : detect that valid variable names can not use special characters except _
       - fail-var4 : detect that variable names can not use exclamation marks
       - fail-var5 : detect that variable names can not use spaces
       - test-var1 : using lowercase alphabets in variable names is valid
       - test-var2 : using uppercase alphabets in variable names is valid
       - test-var3 : using numbers in variable names is valid, as long as they don't start with numbers
       - test-var4 : using underscores in variable names is valid, as long as they don't start with underscores
       - test-var5 : testing long valid variable names is valid

       - fail-while1 : Must have some expression as the condition
       - fail-while2 : Must open body with a colon
       - fail-while3 : Must close body with a semicolon
       - fail-while4 : Must wrap condition in parentheses
       - fail-while5 : Condition must be an expression
       - test-while1 : Simple infinite loop
       - test-while2 : Nested while loops
       - test-while3 : Testing an if statement in the body of a while statement is valid
       - test-while4 : The body of a while statement can syntactically be empty
       
    CODEGEN:
    These tests are present in the tests/codegen directory.

       - test-closure1 : Closure of non-shared variables
       - test-closure2 : Closure of shared variables
       - test-closure3 : Closure for nested functions
       - test-closure4 : Single line lambda definition and return
       - test-closure5 : Repeatedly defining a function in a loop
       - test-closure6 : Stress testing closures and nested closures
       - test-closure7 : Wildly nested functions and threads; 
                         tests that the variables are unpacked correctly in their 
                         closures while generating function body 
       - test-closure8 : Lambdas as arguments, modifying shared variables, and complex closures
       - test-closure9 : Capturing function pointers in nested layers of functions works as expected

       - test-function1 : Recursive function
       - test-function2 : Using store to implement idempotence
       - test-function3 : Downgrading a store function when passsed as a parameter
       - test-function4 : Exhausting a function's store with shared variables to break idempotence
       - test-function5 : Testing that a store function that returns INT_MIN will not cache!

       - test-hofs1 : Reassign function variable to a new function value
       - test-hofs2 : Nested functions and capturing function with closure
       - test-hofs3 : Test passing functions as parameters
       - test-hofs4 : Test passing lambdas as parameters
       - test-hofs5 : Capturing reassigned function with closure inside another function
       - test-hofs6 : Returning a function from another function

       - fail-if1 : detect the semantic error where the conditional of an if statement
                    is a string value instead of a boolean value
       - fail-if2 : detect the semantic error where an integer value is 
                     being "anded" with a boolean value     
       - test-if1 : Testing that if the condition for if is true, then the program  
                    goes into the if block and executes that section. The conditional
                    is an equality check between two integers.
       - test-if2 : Testing that if the condition for if is false, then the program
                    goes into the else block and executes that section.

       - fail-list1 : Testing that a creation of heterogeneous list fails
       - fail-list2 : Testing that the List_insert built-in function fails by inserting
                      a type different from that of elements in the supplied list literal
       - test-list1 :  Testing the List_insert built-in function with inserting elements
                       to the front, middle and back of a list.
       - test-list2 :  Testing the List_insert built-in function with inserting elements
                       to an empty list.
       - test-list3 :  Testing the polymorphic feature of list via spawning 
                       lists of integers, strings, booleans, float, threads 
                       as well as a list of lists of integers. 
       - test-list4 :  Testing the List_replace built-in function with replacing
                       an element at the front, in the middle and at the back.
       - test-list5 :  Testing the List_remove built-in function with removing
                       an element at the front, in the middle and at the back.
       - test-list6 :  Testing the List_at built-in function with accessing
                       an element at the front, in the middle and at the back.
       - test-list7 :  Testing the List_len built-in function with printing out
                       the length of a list before and after removing and inserting 
                       elements from/to it.
       - test-list8 :  Testing lists to be passed and stored as references. Multiple list 
                       variables point to the same list, changes are being made to the list 
                       by calling list-modifying functions with different list variables, 
                       while the change is preserved when the same list is accessed via all 
                       three list variables.
       - test-list9 : Testing nested lists and dealing with lists inside lists using the 
                      List_at builtin function. We iterate over each nested list and print 
                      the individual elements to stdout.
       - test-list10 : This tests that a list is captured properly by functions, and any 
                       modifications made to the captured list is reflected everywhere 
                       the list is accessed. Moreover, it also tests passing lists by 
                       reference to functions and testing if changes made within the 
                       function body is reflected everywhere.
       - test-list11 : Testing that list assignment preserves reference to the initially 
                       assigned list literal. Removal of the element at the head after re-assigning
                       lists work as expected
       - test-list12 : Testing that assigning a new list literal to a list 
                       variable in a new scope preserves the assignment
       - test-list13 : Testing list capture in closure and modification and seeing if 
                       it is reflected in the outer scope. Moreover, making modifications
                       to the captured list in different layers of scopes works as 
                       expected and preserves the changes made to the shared list
                       variable
       - test-list14 : Testing lists as formal parameters and modifying them in functions and threads
       - test-list15 : Testing scenarios where list literals are returned from functions, including 
                       nested list literals
       
       - fail-print1 : Testing a semantic error related to the print built-in function 
                       by supplying a non-string integer argument to it.
       - fail-print2 : Testing a semantic error related to the print built-in function 
                       by supplying a non-string boolean argument to it.
       - test-print1 : Testing the println built-in function with a true boolean value.
       - test-print2 : Testing the println built-in function with a "Hello, world!" string.
       - test-print3 : Testing the println built-in function with a multi-line
                       string value.

       - fail-strings1 : Testing that the String_substr built-in function fails on an
                         argument with the incorrect type.
       - test-strings1 : Testing the String_substr built-in function by printing 
                         the length of a string
       - test-strings2 : Testing the String_eq and String_concat built-in functions by
                         concatenating two strings if those two strings are not equivalent

       - test-thread1 : Testing waiting for an asynchronous thread to finish 
                        executing works as expected. The main thread only 
                        exits after the expected outputs have been printed 
                        to stdout
       - test-thread2 : Similar to test-thread1, but now there are ten threads 
                        concurrently printing to stdout. This test waits for 
                        all threads to be done before the main thread exits, 
                        ensuring that the output is always deterministic
       - test-thread3 : Integration test that captures lists, and tests calls 
                        to function that modifies the captured list from within
                        a thread
       - test-thread4 : Testing mutexes prevent concurrent threads from executing 
                        the function body at the same time

       - test-integration1 : An integration test that implements a BFS algorithm
                             to walk through a 2D array.
       - test-integration2 : An integration test that implements a "store" 
                             fibonacci function and within a loop, prints the 
                             first 46 fibonacci numbers. Moreover, this file
                             implements a HOF named flip and tests it.

--------------------------------------------------------------------------------

How testing works:

- Successful compilation and output validation:
       If the compilation was successful, then it would exhibit no errors. 
       The compilation step involves three stages: compiling the sPool source 
       file to LLVM IR, compiling the LLVM IR to assembly, and then linking 
       this assembly file with the external object file written in C to produce
       the final executable. The testing script does all this to produce an 
       executable for every test that is expected to pass. If there were any 
       errors in any of these steps, the process of testing is stopped and the 
       error is reported. Otherwise, the executable is run for each test whose
       output is compared with what we expect (aka the "gold standard", present 
       in the .out files). If they are equivalent the test passes with the 
       message `Test <test.sP> PASSED`, where `test.sP` is the test file. 
       Any errors would show the expected output and the output we got on the 
       test report screen.

- Unsuccessful compilation:
       If the compilation was unsuccessful, it could be due to a semantic error, 
       a lexing error, or a parsing error. In this case, an error will be raised
       terminating compilation. Our compilation error strategy is to halt 
       compilation at the time of the first error and display the error message
       to the user.

       The testing script will, for tests that are expected to fail, compare 
       the expected error message against the one we received while trying 
       to compile the program and determine if the compilation process 
       was successful or not.

--------------------------------------------------------------------------------

Some fun demos written in the sPool programming language:

We include a folder of demos of the sPool language. These can all be found
within the `demos` directory:
     - sPool/demos/fibonacci.sP
     - sPool/demos/flip.sP
     - sPool/demos/idempotence.sP
     - sPool/demos/mutex.sP
     - sPool/demos/nondeterminism.sP
     - sPool/demos/sort.sP
     - sPool/demos/storedStringFunc.sP
     - sPool/demos/unblackedges.sP

Each file contains information on how to run the demo and what it does.
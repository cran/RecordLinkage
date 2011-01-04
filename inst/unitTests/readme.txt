Tests for functions in package Record Linkage

Tests are conducted via package RUnit, which has to be installed on the system
the tests are run on. Also, the package RecordLinkage itself has to be 
installed and the installed version is tested.

The tests are conducted by running the R script "runtest.r" from the test 
directory. HTML output is placed in the file "protocol.html".

Every test file corresponds to one source file in the package and is named
after it, with the prefix "runit." added. For example, "runit.strcmp.r" holds
the test functions for file "strcmp.r" in the "R" directory of the package.

Within each file, every function defined in the corresponding source file is
tested by one function which has the prefix "test." attached to the name of
the tested function, for example "test.jarowinkler" holds the tests for function
"jarowinkler".

Details regarding the test cases are explained by comments in the test files.
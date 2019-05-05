# OCaml-s-Type-Inference-using-Prolog
OCaml like Python is able to infer datatypes of declared variables. This project is an implementation of Type Inference in OCaml using Prolog made to understand the concepts of Functional Programming.  
This project is completed by Akshat Bajpai and Timon Angerhofer.

Running the code
================

A) Start the SWI prolog interpreter (run swipl 
on Lixnux and swipl.exe on Windows).

[akshat@localhost typeInf]$ swipl
Welcome to SWI-Prolog (threaded, 64 bits, version 7.6.4)
-------
?-

B) Load your code by typing the location of your .pl file inside Square Brackets "[]". Absolute path works best.
We're assuming that the typeInf file is in the same directory. So...

?- [typeInf].

Writing and running unit tests
==============================

Running tests
    1. Run all tests
?- consult("typeInf.plt"), run_tests().

    2. Run one specific tests
?- consult("typeInf.plt"), run_tests(typeInf:testname).

We ran around 29 tests (including infer) and they worked well.
We have the following things implemented:-
1. Basic functions to provide interesting code (fType predicate)
2. Statement types. At a minimum you need to cover:

    a. global variables with expression initialization  (e.g. let x = 3)
    
    b. global function definitions (let add x y = x+y)
        last expression is an implicit return. return statement also possible
        
    c. expression computation (as a statement)
    
    d. if statements
    
    e. "let in" statement for local variables
    
    f. for statements
    
    g. code blocks (separated by ;)
    

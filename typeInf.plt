:- begin_tests(typeInf).
:- include(typeInf). 

%:- dynamic gvar/2.

/* Note: when writing tests keep in mind that 
    the use of of global variable and function definitions
    define facts for gvar() predicate. Either test
    directy infer() predicate or call
    delegeGVars() predicate to clean up gvar().
*/

% tests for typeExp
test(typeExp_iplus) :- 
    typeExp(iplus(int,int), int).
test(typeExp_isub) :- 
    typeExp(isub(int,int), int).
test(typeExp_fplus) :- 
    typeExp(fplus(float,float), float).
test(typeExp_imul) :-
    typeExp(imul(int,int),int).
test(typeExp_fsub) :- 
    typeExp(fsub(float,float), float).
test(typeExp_greaterthan) :-
    typeExp(greaterthan(float,float),bool).
test(typeExp_lessthan) :-
    typeExp(lessthan(float,float),bool).

% this test should fail
test(typeExp_iplus_F, [fail]) :-
    typeExp(iplus(int, int), float).

test(typeExp_imul_T, [true(T == int)]) :-
    typeExp(imul(int, int), T).
test(typeExp_iplus_T, [true(T == int)]) :-
    typeExp(iplus(int, int), T).
test(typeExp_isub_T, [true(T == int)]) :-
    typeExp(isub(int, int), T).
test(typeExp_fplus_T, [true(T == float)]) :-
    typeExp(fplus(float, float), T).
test(typeExp_fsub_T, [true(T == float)]) :-
    typeExp(fsub(float, float), T).
test(typeExp_greaterthan_T,[true(T == bool)]):-
    typeExp(greaterthan(float,float), T).
test(typeExp_lessthan_T,[true(T == bool)]) :-
    typeExp(lessthan(float,float), T).

% NOTE: use nondet as option to test if the test is nondeterministic

% test for statement with state cleaning
test(typeStatement_gvar, [nondet, true(T == int)]) :- % should succeed with T=int
    deleteGVars(), /* clean up variables */
    typeStatement(gvLet(v, T, iplus(X, Y)), unit),
    assertion(X == int), assertion( Y == int), % make sure the types are int
    gvar(v, int), % make sure the global variable is defined
    typeStatement(gvGet(v, int), unit).

% same test as above but with infer 
test(infer_gvar, [nondet]) :-
    infer([gvLet(v, T, iplus(X, Y))], unit),
    assertion(T==int), assertion(X==int), assertion(Y=int),
    gvar(v,int).

test(infer_test1, [nondet, true(T==int)]):-
    infer(
        [
            if(greaterthan(C1,C2), [iplus(A,B),iplus(C,D)], [int])
        ],
        T),
    assertion(C1 == float),
    assertion(C2 == float),
    assertion(A == int),
    assertion(B == int),
    assertion(C == int),
    assertion(D == int).

test(infer_test2, [nondet]) :-
    infer(
        [
            gfLet(foo, [string, char, float], [bool]),
            gvLet(v, int, int),
            for(Y, int, int, 
                [
                    gfGet(foo, Foo),
                    if(Foo, [isub(int, Int)], [int]),
                    gvGet(v, V),
                    iplus(V, int),
                    print(V),
                    fToInt(float),
                    unit
                ])
        ],
        T),
        assertion(Y==int),
        assertion(Int==int),
        assertion(T==unit).

test(infer_test3, [nondet, true(T == unit)]):-
    infer(
        [
             for(iplus(imul(isub(fToInt(iToFloat(A)),B),C),D),int,int,[
                for(fToInt(fplus(F1,F2)),imod(A,B),C,[
                    print(something),
                    letin(int, imod(int,int))                    
                ])
             ])
        ],T),
   
        assertion(A == int),
        assertion(B == int),
        assertion(C == int),
        assertion(D == int),
        assertion(F1 == float),
        assertion(F2 == float).

test(infer_test4,[nondet, true(T == float)]):-
    infer(
        [
            return(float),
            int
        ]
        ,T).

        
% test custom function with mocked definition
test(mockedFct, [nondet]) :-
   deleteGVars(), % clean up variables since we cannot use infer
   asserta(gvar(my_fct, [int, float])), % add my_fct(int)-> float to the gloval variables
   typeExp(my_fct(X), T), % infer type of expression using or function
   assertion(X==int), assertion(T==float). % make sure the types infered are correct

test(typeStatement_gflet, [nondet, true(T==int)]) :-
    deleteGVars(),
    typeStatement(gfLet(foo, [float, float, int], [imod(M1,M2)]), T),
    typeStatement(gfGet(foo, T), unit),
    assertion(M1 == int), 
    assertion(M2 == int).

test(typeStatement_return, [nondet, true(T==float)]) :-
    deleteGVars(),
    typeStatement(gfLet(foo, [], [int, float, return(float), int]), T),
    typeStatement(gfGet(foo, T), unit).

test(typeStatement_letin, [nondet, true(X==int)]) :-
    deleteGVars(),
    typeStatement(letin(X, imod(int,int)), unit),
    typeExp(imod(X,int), int).      % check that X is the right type

test(typeStatement_for, [nondet]) :-
    deleteGVars(),
    typeStatement(for(X, int, int, [imod(int,int), fdiv(float,float), unit]), unit),
    assertion(X==int).

%If Statement:- (Hopefully should Pass!)
test(typeStatement_if,[nondet, true(T == float)]):-
    deleteGVars(),
    typeStatement(if(greaterthan(C1,C2), [imod(P1,P2), fdiv(M1,M2)], [iToFloat(I)]), T),
    assertion(C1 == float),
    assertion(C2 == float),
    assertion(M1 == float),
    assertion(M2 == float),
    assertion(P1 == int), 
    assertion(P2 == int), 
    assertion(I == int).

test(typeCode_codeBlock, [nondet, true(T==int)]):-
    deleteGVars(),
    typeCode([imod(int,int), fdiv(float,float), letin(F, iToFloat(I)), print(unit), fToInt(F)], T),
    assertion(F==float),
    assertion(I==int).
    
:-end_tests(typeInf).

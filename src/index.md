\title The Ciao programming language

Ciao is a programming language that builds up from a logic-based
simple kernel, and is designed to be extensible and modular. Its supports:

 - **constraint** logic programming (and, in particular, **Prolog**, supporting the ISO-Prolog standard), 
 - different levels of modularity (from small to large scale):
   - **modules** as (analysis-friendly) compilation units,
   - **bundles** as collections of modules, 
 - **packages** as modules implementing language extensions
   (syntactic definitions, compilation options, compiler plugins), 
 - **assertions** (as a homogeneous framework that allows static and
   dynamic verification to work cooperatively in a unified way), 
 - **multiparadigm** constructs (meta-programming, higher-order,
   mutables, concurrency, functions, etc.) and interfacing with
   **foreign** code.

The system implements some advanced features such as separate and
incremental compilation, global program analysis and static debugging
and optimization (via source to source program transformation,
CiaoPP preprocessor), a build automation system, documentation
generator, debugger, and integrated development environment.

```ciao_runnable
:- module(_, _, [fsyntax, rtchecks, assertions]).
%! \begin{focus}
select(X, [X|Ys], Ys).
select(X, [Y|Ys], [Y|Zs]):-
    select(X, Ys, Zs).

% perm([], []).
% perm([X|Xs], [R|Rs]) :-
%     select(R, [X|Xs], Ys),
%     perm(Ys, Rs).
:- pred perm(Xs,Ys) : list(Xs) => list(Ys).
perm([]) := [].
perm([X|Xs]) := [R| ~perm(Ys)] :-
    select(R, [X|Xs], Ys).

:- use_module(library(between)).
i(X) :- between(-10,10,X).
%! \end{focus}
```
```ciao_runnable
?- perm([p,r,o,l,o,g],Xs).
```
Solutions for dyophantine equations
```ciao_runnable
?- i(X),i(Y),i(Z),3*X*X-2*X*Y-Y*Y*Z-7=:=0.
```
```ciao_runnable
:- module(_,_,[clpfd]).

:- use_module(library(format)).
%! \begin{focus}
send_more_money(Vars) :-
    Vars = [S,E,N,D,M,O,R,Y], 
    domain(Vars, 0, 9),
    0 #< S, 0 #< M,
    all_different(Vars),
    S*1000 + E*100 + N*10 + D +
    M*1000 + O*100 + R*10 + E #=
    M*10000 + O*1000 + N*100 + E*10 + Y,
    labeling([], Vars),
    print(Vars).

print([S,E,N,D,M,O,R,Y]) :-
    format(  "   ~w ~w ~w ~w~n",  [S,E,N,D]),
    format(  " + ~w ~w ~w ~w~n",  [M,O,R,E]),
    format(  " ---------~n",             []),
    format( " ~w ~w ~w ~w ~w~n",[M,O,N,E,Y]).
%! \end{focus}
```
```ciao_runnable
?- send_more_money(Vars).
```

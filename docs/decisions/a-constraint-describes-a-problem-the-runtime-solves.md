# A constraint describes a problem the runtime solves

Date: 2026-10-22, revised 2026-10-28: a class the library defines. Status: accepted

## Context

LRM clause 18 makes `randomize()` choose values for a class's random variables, at random from the
object's own generator, so that every constraint in force holds -- uniformly over the assignments
that do, unless `solve ... before` orders them (18.5.9), and leaving every variable as it was where
none does (18.6.3). What is in force is decided while the program runs: a constraint reads the
values of variables it does not choose, `rand_mode()` and `constraint_mode()` switch variables and
blocks per object (18.8, 18.9), a call may list what it chooses (18.11) or add constraints of its
own (18.7), and a random handle brings the object it reaches into the call (18.4). The size of an
array a constraint names is chosen before its elements (18.5.7.1), and a variable a function in a
constraint is handed is chosen before the constraints the call's answer enters (18.5.11).

A class extending another adds variables and blocks, and a block of the name of one it inherits
replaces it (18.5.2). The two are often in different compilation units: a verification library's
base classes and the test extending them.

## Decision

**A constraint is code that describes a problem to a solver in the runtime, as a C++ program using a
constraint library writes it; MIR has no node for a constraint or for a random variable.**

- **A problem** is a runtime object made by each call. A variable enters it through a reference to
  its storage, so what a random variable is is the storage it names, and an object reached twice is
  one variable. A value a constraint reads that names no storage enters as a constant. Each operator
  is a call answering a term; each constraint is a requirement or a preference on one. A random
  array enters element by element, walked by the runtime from the value's type, so the compiler
  states a property once whatever its shape.
- **Every class object starts with the part the runtime library declares for it**, whatever defines
  its class, which holds the object's generator beside the modes every object keeps, and its class's
  definition states five bodies: declare the random variables, state the constraints, name the
  random objects the handles reach, and the two hooks. Each source class states the first three,
  doing what the class it extends does first -- calling that class's body directly, whichever unit
  declares it -- and then adding its own. A class the library defines, a process, states none of the
  five, so randomizing one chooses nothing and checks only what the call writes with it. A
  constraint block is a virtual method of its class, called where its mode is on, so a block of the
  same name in an extension replaces it by the ordinary means. A class declaring `pre_randomize` or
  `post_randomize` overrides the hook with a call to it; one that does not inherits its base's. A
  variable's mode and a `randc` variable's cycle are kept for the variable the class declares and
  the positions of an element within it, never for the element's storage, which moves when an array
  holding it is resized and is then free for another variable.
- **A call of `randomize()`** runs `pre_randomize` on the object and on each object a random handle
  reaches, has every object declare its variables before any states a constraint, adds the inline
  constraints, settles what has to be chosen first, and solves. Settling chooses array sizes and the
  variables functions are handed, by the constraints those reach, then resizes and writes them and
  states the description again with them held; a call with neither states it once. A call that fails
  puts back what settling changed.
- **The solver** builds an and-inverter circuit of the terms. Variables that no constraint ties
  together are drawn apart. A group's bits are decided in one fixed order -- its variables as
  declared, each from its most significant bit -- so what a call draws follows from the problem and
  the object's generator alone, never from what any call drew or built before (18.14). Where the
  condition fits a decision diagram of a bounded number of live nodes, built in that order with no
  reordering, the diagram counts every completion exactly and the draw is exactly uniform (18.5.9);
  a diagram is kept by the condition it holds and reused by the next call stating the same one.
  Where it does not fit, each variable tries a few values drawn at random and keeps the first a
  satisfiability solver says something completes, and a bit still open takes a random side among
  those something completes: every answer meets the constraints and none is out of reach, but where
  one side of a bit holds far more completions than the other the two are taken equally often. It
  finds an answer wherever one exists. A `randc` variable takes the next value of a keyed
  permutation of its range, or, where constraints reach it, a value drawn among those they allow
  that the permutation has not yet taken; a new permutation begins when none is left, and when the
  constraints on the variable differ from the previous call's. A variable of an enumerated type that
  the call chooses is required to equal one of the members its enumeration names, which the compiler
  states beside its declaration, and a `randc` one permutes the members' positions. A distribution
  chooses an item by weight among those the constraints leave some value of, and a value uniformly
  within it.

## Why this one

The survey was of systems that randomize under constraints, asking where the description is made,
what solves it, and how a random answer is drawn.

- **Verilator** (`V3Randomize.cpp`, `verilated_random.cpp`) generates C++ that writes each call's
  constraints as SMT-LIB2 text, with the current values of what is not chosen spliced in, and pipes
  it to an external solver process; with none installed, constrained randomization is unavailable.
- **CRAVE** (Haedicke et al., FDL 2012; DVCon 2014) builds the constraints as C++ objects at run
  time through expression templates and solves them with decision diagrams and an SMT portfolio,
  drawing uniformly from a diagram and noting that uniformity is much harder through an SMT solver.
- **SCV** (Grosse et al., GLSVLSI 2007) solves run-time expression objects with decision diagrams,
  weighting the walk to the true terminal so that each solution is equally likely.
- **PyVSC** builds run-time Python objects and solves with Boolector, randomizing by assumptions.
- **UniGen** (Chakraborty, Meel and Vardi, DAC 2014) draws near-uniformly from a satisfiability
  solver's solutions by cutting them with random parity constraints and enumerating one small cell.
- **clang and rustc** have no construct for a constraint: a solver is a library, and a constraint is
  code that calls it (the z3 C++ API is the common example).

Every one of them makes the description at run time, by code, from the run's state, which is the
part the standard forces. Where they differ is the solver, and two conditions of ours decide it. A
built program runs with nothing installed beside it, which rules out an external solver process. And
drawing uniformly among the solutions of an arbitrary condition is #P-hard, so no solver meets
18.5.9 for every constraint; a decision diagram meets it exactly where it fits. Where it does not,
none of the open tools above draws uniformly either, and the one guarantee the field offers
(UniGen's) needs a solver with native parity constraints and tens to hundreds of solver calls a
draw. Lyra keeps the one property they all leave out and the standard asks for -- an object's values
depend on its own generator alone -- by deciding every bit by whether something completes it, never
by which completion a solver happened to find.

## Rejected alternatives

- **A MIR node for a constraint, and one for a random variable.** Neither is a construct of a
  general-purpose language, and a backend meeting one would have to decide how it is solved.
- **Solving at compile time.** What a constraint reads, which variables are chosen and which blocks
  are on are all values of the running program.
- **An external solver process.** It has to be installed wherever a program runs, and its answers
  are not drawn uniformly. The runtime links CUDD and CaDiCaL instead, built from source by this
  repository's own build files so that a program optimized together with the runtime compiles them
  too.
- **Reordering a diagram's variables to keep it small.** A diagram is then drawn in an order other
  than the one its bits are decided in, which costs counts over the whole diagram for every bit, and
  whether it fits a budget comes to depend on what the package did before; built in the deciding
  order it may be larger, but a draw is one walk down it and its size follows from the condition.
- **Random parity cuts past the budget** (UniGen's way). Close to uniform, but it needs a solver
  with native parity constraints and tens to hundreds of solver calls a draw.
- **Returning the assignment a solver finds, from random values given it to aim at** (CRAVE's and
  PyVSC's way). The solver's choice depends on what it learned answering earlier questions, so one
  object's values would depend on what every other object asked.
- **A compiled table of each constraint, read by an interpreter in the runtime.** It needs a second
  evaluator of every expression a constraint may contain; code that calls the problem reuses the
  lowering of each one.
- **A user `pre_randomize` as the override of the hook itself.** A function takes a report of what
  it reads and answers a completion, so its signature is not the hook's; and the standard says the
  hooks are not virtual, only reached as though they were (18.6.2), which a hook calling the one its
  class declares gives.

## Consequences

- Every class object holds the 32 bytes of its generator, as
  `a-generator-is-split-from-the-one-that-made-it.md` foresaw, and for an object `new` makes the
  generator that made it advances by four draws; a process's is its thread's, which nothing splits
  again. Randomizing a process, which declares no random variable, draws nothing from that
  generator, a choice the standard leaves to the tool. It also holds a pointer to its modes, which
  are made the first time anything reads or sets one.
- Each source class states three more bodies in its definition, whether or not it declares anything
  random, so a class of another unit can call its base's without knowing what the base declares.
- A real random variable, an associative array declared random and an array of structures declared
  random are refused when a call reaches them; the solver has integral terms only.
- A program carries CUDD (BSD-3-Clause) and CaDiCaL (MIT) inside its runtime, with their notices,
  and a program built to optimize at the link compiles both again, once per cache.
- A condition too large for a diagram is drawn with a known bias toward the side of a bit holding
  fewer completions, which the run reports the first time a call draws so; the run's command line
  may raise the bound.

## Cross-references

- LRM 18.3 -- 18.12, 18.14
- `docs/decisions/a-generator-is-split-from-the-one-that-made-it.md` (the object's generator)

# A formal's default is evaluated by its callee

## Date

2026-10-26

## Status

Accepted.

## Requirement

A call omitting an argument takes the formal's default, which "is evaluated in the scope containing
the subroutine declaration each time a call using the default is made"; a call passing the argument
evaluates nothing (LRM 13.5.3).

## Facts it rests on

1. The front end fills an omitted argument with the formal's default expression, bound in the
   subroutine's own scope, by placing that very expression in the call: an argument that is the
   formal's default expression is one the call omitted.
2. A default may read the callee's own storage -- a variable of the instance declaring the
   subroutine, a property of the object a method is called on -- which is reached from the callee
   and means nothing where the caller stands. Lowered as the caller's text, it was an internal error
   for a call into another instance and read the wrong object for a method (run 2026-10-26, both
   backends).
3. A port's default (LRM 23.2.2.4) is already a function of the child that the connection calls.

## Decision

**D1. The callee evaluates the default.** Whatever declares a subroutine with a defaulted input
declares, per such formal, a function evaluating the default: the scope or namespace a subroutine
belongs to, or the class of a method. The function is published beside the subroutine and named for
it and the formal, and what a call reads of the callee names it beside the formal.

**D2. The call states only that the default applies.** An omitted argument stands in the call as the
callee's default and nothing of the expression is lowered where the call is, so a read analysis of
the caller does not see it either. Lowering the call asks the evaluating function on the same object
the callee is made on, which is evaluated once and stands at both. A method's default is the one of
the method the call names, whichever body its object's class runs.

**D3. What the default reads is part of what the call reads.** The evaluating function states its
reads, as any function does, and a call followed by an implicit list asks it too (LRM 9.2.2.2.1).

## Survey

clang keeps a parameter's default on the parameter's declaration and refers to it at the call
(`CXXDefaultArgExpr`), which suffices because a C++ default cannot read `this` or another parameter.
Swift has the defining module emit a default argument generator that the caller calls (recalled).
The condition that differs is fact 2: a SystemVerilog default may read the callee's own state, so it
needs the callee's receiver, which is Swift's shape with a receiver.

## Limits, each with its cause

- A DPI-C import's default is still lowered where the call stands: a call of an import is made
  through its own boundary, not the one a user subroutine's call takes, so a default reading the
  importing instance's storage, called from another instance, still fails.
- A constructor's default is still lowered where the call stands. A class is constructed only where
  it is visible, which puts the default's names in reach of the call, so it reads the same storage;
  the class has no object yet to evaluate it on.
- A default reading an earlier formal (`int b = a`) is refused, as before: the evaluating function
  takes no formals.

## Rejected

- **The callee evaluating its own defaults in a prologue, told by a hidden argument which formals
  were passed.** Every subroutine with a default would take a hidden argument, and an override would
  evaluate its own default where the call names another's.
- **Lowering the default as the caller's text, resolved from the callee's scope.** It evaluates the
  callee's text in the caller's body, so the caller compiles the callee's expression and a change to
  a default rebuilds every caller with nothing on a signature recording why.

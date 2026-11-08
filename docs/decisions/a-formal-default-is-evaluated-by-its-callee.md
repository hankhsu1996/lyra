# A formal's default is evaluated by its callee

## Date

2026-10-26, rewritten 2026-11-02 when defaults naming earlier formals, the defaults of formals that
bind places, of constructors and of DPI-C imports were brought under one rule. D2 extended the same
day: it said the object is evaluated once without saying how, and a call made through a class
property read the handle once per default and could be made on an object a default had let go of.

## Status

Accepted. Revised 2026-11-07: D1 named the evaluating function by a name made for it, which a call
then looked up in the callee's declarer below the front end's layer; the declarer now states the
function's position beside each formal.

## Requirement

A call omitting an argument takes the formal's default, which "is evaluated in the scope containing
the subroutine declaration each time a call using the default is made"; a call passing the argument
evaluates nothing (LRM 13.5.3). The call binds what the default gives as it binds an actual it
wrote: `m.t1()` is `m.t1(m.a)` and copies back into `m.a`. Constructor arguments follow the same
convention (LRM 8.7), and an import declaration "is equivalent to defining a subroutine of that name
in the SystemVerilog scope in which the import declaration occurs" (LRM 35.5.4).

## Facts it rests on

1. The front end fills an omitted argument with the formal's default expression, bound in the
   subroutine's own scope after the formal, by placing that very expression in the call: an argument
   that is the formal's default expression is one the call omitted. So a default may name an earlier
   formal, which LRM 13.5.3 read alone does not reach, and UVM's `uvm_report` does. For a method
   defined outside its class, the default is bound among the prototype's formals.
2. A default may read the callee's own storage -- a variable of the instance declaring the
   subroutine, a property of the object a method is called on -- which is reached from the callee
   and means nothing where the caller stands.
3. A port's default (LRM 23.2.2.4) is already a function of the child that the connection calls.
4. A static subroutine's formal is storage that keeps its value between calls (LRM 13.4.2), so a
   default reading the storage reads the previous call.
5. The default of an `output`, `inout` or `ref` formal names a place, which is what the call binds.
6. A construction evaluates arguments before any constructor runs, and an object of a class a scope
   declares records its instance only as that class's constructor begins.

## Decision

**D1. The callee evaluates the default.** Whatever declares a subroutine with a defaulted formal
declares, per such formal, a function evaluating the default: the scope or namespace a subroutine or
an import belongs to, or the class of a method or a constructor. The function is published beside
the subroutine, and the declarer states beside each formal which of its own callables evaluates it,
by that callable's position; a call reads that position off the callee's declaration. The function's
name, made from the subroutine and the formal, is spelling only.

**D2. The call states only that the default applies.** An omitted argument stands in the call as the
callee's default and nothing of the expression is lowered where the call is, so a read analysis of
the caller does not see it either. Lowering the call asks the evaluating function on the same object
the callee is made on, which is evaluated once and stands at both. A method's default is the one of
the method the call names, whichever body its object's class runs.

The object is the call's first binding, so a call that omits an argument holds it as it holds the
formals (D4): after the arguments the source wrote and before the first default, it reads the class
handle the source named once into a share of its own, opens it once, and enters every default and
the callee on what that opening answers. The share is what keeps the object while the defaults are
evaluated and the callee runs, so a default that points the named handle at another object, or lets
go of every other handle to it, leaves the call on the object it began with. What the call holds
lives in a scope of the call's own, a block expression or, for a task, the body the enabler awaits,
and ends with it, as a C++ temporary ends with its full-expression. The object is therefore the one
the handle names once the written arguments have run, which is when an ordinary call forms its
object too
([an-evaluation-runs-its-operands-first-to-last](an-evaluation-runs-its-operands-first-to-last.md)
D1). Scala's rewrite of such a call binds its prefix the same way, `val q = p` ahead of every
default getter called on `q` (SLS 6.6.1).

**D3. What the default reads is part of what the call reads.** The evaluating function of an `input`
states its reads, as any function does, and a call an implicit list follows asks it too (LRM
9.2.2.2.1). Any other default names a place, and a call the list does not follow has no function the
list asks, so there the list reads the default's text, where a name of an earlier formal stands for
what the call binds it, which the list read where the call wrote it.

**D4. The function takes the formals before the one it evaluates, each by reference to what the call
binds that formal.** A default is an inserted actual, and the names in one are what the call binds:
this is Lyra's definition of a text the standard does not reach, and it is why a static subroutine's
formal is a parameter of the function and never its storage. An import's open array is no such
parameter: only a call fixes its shape (LRM 35.6.1.1). A call that omits an argument holds what it
binds each formal in a place of its own for as long as it runs -- the value of an `input` or an
`inout`, the starting value of an `output` (LRM 13.3.2), the reference of a `ref` -- evaluated once,
so one evaluation serves the callee and every default reading it. A call evaluates what the source
wrote, left to right, then what it left to a default, left to right, within the freedom LRM 13.5
leaves, so a default may read one an earlier default gave. By reference, because a default may
answer a place, and the one it names may be such a formal: it must outlive the function.

**D5. A default binding a place is passed back by reference.** The function of an `output`, `inout`
or `ref` formal's default passes back the place the default names rather than a value, read-only for
a `const ref`. HIR says only, beside the result's type, the direction of the formal whose actual the
result stands as -- `input` for every function the source writes -- because SystemVerilog spells how
a value is bound as a direction keyword and has no reference type; the lowering to MIR turns that
direction into passing by value or by reference (LRM 13.5.1, 13.5.2), the one word it also turns a
formal's direction into, and MIR, which has reference types, states the result as one, so a call of
it is a place as a C++ call of a function returning `T&` is an lvalue. The function is a function
like any other: its one return binds the place as an actual is bound to a `ref` formal, and it has
no result variable to fall through to. The call treats what it passes back as the place of an actual
it wrote: written back for an `output`, read and written back for an `inout`, handed on for a `ref`.

**D6. A constructor's default is evaluated on the object, ahead of its constructor.** A construction
evaluates the arguments the source wrote, creates the object, evaluates each omitted one on it in
order, and enters the constructor. The function runs as the constructor does: handed what the
constructor is handed ahead of its formals, since the object records none of that yet. What it reads
of a property is what the property holds before initialization, which LRM 8.7 leaves undefined. Such
a construction is written in MIR as its two steps, a creation and a call of the constructor by name,
as a constructor enters its base's; one omitting nothing stays one node. A base constructor left to
its defaults is the same, on the object already being built, whichever unit declares the base. The
function records the instance on the object as the constructor does, so a method of the class it
calls reaches it.

**D7. An import's default is the declaring scope's.** The foreign symbol stays global and takes no
receiver; what belongs to the scope is the evaluating function and, for a `context` import, the
scope it observes. A call names the scope as it would to call any subroutine of it: this scope's
own, the object of another instance, or none for a namespace.

## Survey

Scala generates a method per defaulted parameter, `f$default$N`, taking the preceding parameter
lists, and rewrites an omitting call to pass its answer (SID-1, Named and Default Arguments). That
is this shape; SystemVerilog's front end only widens the parameters to those earlier in the same
list. Kotlin's JVM backend generates one `f$default` per function with an omission mask, which fills
what was left out and calls the body; every site knows what it omitted, so the mask decides again at
run time what is settled, a task's would be a coroutine awaiting the task, and a virtual method's
would have to dispatch half of itself. Swift's default argument generators take no parameters, a
default may not read another, and letting it stalled on a stable ABI (Swift forums, "Allow default
argument expressions to reference earlier parameters"), a condition Lyra's rebuilt signatures do not
have. clang refers to a parameter's default at the call (`CXXDefaultArgExpr`) and refuses one naming
another parameter or `this` (`CheckDefaultArgumentVisitor`), which suffices for C++ and not here:
fact 2. For a call binding a place, clang's result is an lvalue of a function returning `T&`
(`VK_LValue`) and rustc's THIR has a call typed `&mut T` under a `Deref`.

For the order of a construction, the JVM allocates before evaluating a constructor's arguments (JLS
15.9.4) and C++17 sequences the allocation before the initializer ([expr.new]); neither lets an
argument name the object, because neither language gives a default a way to, and SystemVerilog's
front end does. Creating an object and then entering its constructor by name is Python's
`object.__new__(C)` followed by `__init__`, and the JVM's `new` followed by `invokespecial`.

For a function passing back a place, clang classifies a call as an lvalue exactly where the return
type is an lvalue reference (`ExprClassification.cpp`, [expr.call]p10), and rustc's THIR reads the
place through `Deref` of a call typed `&mut T`: in both, reference-ness is the result's type and no
second kind of function exists.

## Limits, each with its cause

- An interface class a design element declares has no code in any body, so its default, a constant
  the same for every class implementing it (LRM 8.26.8), is lowered where the call stands.
- A default of an `output` or `inout` formal naming part of a packed value is refused: the
  evaluating function passes back a reference to a variable, and such a part is not one.
- A class in `$unit` whose constructor's default names a variable of a design element is refused, as
  any method of such a class naming one is.
- A method of a base class that a constructor's default or a base's arguments call runs before that
  base has recorded its instance on the object, so reaching the instance through it fails.
- Where an implicit list reads a default's text, it cannot reach that text from another instance,
  and a function call in the text naming an earlier formal is refused.
- A default naming an import's open array is refused, since its function does not take it.
- A `default` constructor argument (LRM 8.17) shares the base's expression, whose earlier formals
  are the base's, so one naming an earlier formal is refused.

## Rejected

- **The callee evaluating its own defaults in a prologue, told by a hidden argument which formals
  were passed.** Every subroutine with a default would take a hidden argument, and an override would
  evaluate its own default where the call names another's.
- **Lowering the default as the caller's text, resolved from the callee's scope.** It evaluates the
  callee's text in the caller's body, so the caller compiles the callee's expression and a change to
  a default rebuilds every caller with nothing on a signature recording why.
- **A static function for a constructor's default, refusing one naming the object.** It rejects a
  text the front end accepts and the standard gives a meaning, an undefined value, and "arguments
  before the object" was this project's own choice, not a condition.
- **Earlier formals by value.** A default answering a place naming one would answer a reference into
  its own frame.
- **A kind of subroutine for a function passing back a place.** It put value category into the call
  protocol: every reader of the kind gained an arm meaning "a function", and the one mapping back to
  a type lost whether the reference was read-only.

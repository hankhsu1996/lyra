# An object is created whole, then constructed

Date: 2026-10-22 Status: accepted.

## Context

SystemVerilog's classes are garbage collected "as in Java" (LRM 8.7), and the standard orders their
construction and their dispatch the way Java does. A derived class's `new` first calls its base's,
then initializes each of its own properties, then runs the rest of its body (LRM 8.7, 8.17). A
virtual method called from a constructor is the one the object's class implements, and "properties
the method refers to may not have been initialized" yet (LRM 8.17 NOTE 2, 8.20). The base
constructor's arguments may read a property of the class being constructed before that property's
initializer has run -- the clause's own example passes `d3` to `super.new` and states the result
(LRM 8.7). A cast inside a base constructor tests the object (LRM 8.16), an abstract class's
constructor runs only as part of an object of a class extending it (LRM 8.21), an object lives while
anything refers to it and "a newly created object is strongly reachable by the process that created
it" (LRM 8.29), and every member of an object has the object's lifetime from `new` on (LRM 37.3.7).

Lyra built an object the way C++ does. Each constructor entered its base's constructor, then gave
the object its own class's tables and brought its own members into existence, then ran its body; a
construction an exception left ended the parts it had built and gave the storage back. So a base
constructor dispatched as the base, a cast to the derived class failed inside it, an abstract base
calling its pure virtual method aborted, LRM 8.7's own example did not compile on either backend
(the derived property it reads did not exist yet), and an object that had handed `this` on before
its construction was left was freed under the handle holding it.

## Decision

**D1. Creating an object and constructing it are two operations, and creating it runs no code of the
design.** Creation allocates the storage of the complete object, builds the part of it the runtime
library defines, brings every member of every class of its lineage into existence at the value its
type starts with, and gives the object its own class's tables -- its lineage's and those of its
interface class parts -- once. Construction is the class's constructor entered on that object. From
creation on, the object is a value of its class, so a dispatch and a cast made during construction
answer for its class (LRM 8.17 NOTE 2, 8.16, 8.21), and a property of any class of the lineage can
be read before its initializer runs, holding its type's start value, which is one of the values LRM
8.7 leaves undefined.

**D2. A constructor is an ordinary body of its class, and its base's construction is a statement of
it.** It first records what Lyra itself keeps for the object -- the instance a class declared inside
one belongs to, and a scope's place in the hierarchy -- then calls its base's constructor directly
on the same object with the arguments the class states for it (LRM 8.7, 8.17), then gives each of
its own properties its start value and then its initializer, in declaration order (LRM 8.7), then
runs its body. A class extending the library's root makes no base call. The base's arguments are
expressions of that body, so nothing stands outside it.

**D3. `new` evaluates its arguments, creates the object, takes the creator's reference to it, and
constructs it.** A construction left partway -- its process killed (LRM 9.7), or a run-time error --
ends that reference as any value the body owns is ended on that way out, and nothing else. The
object ends when nothing else refers to it, and otherwise goes on existing with what its
construction had given it (LRM 8.29). Creation does not raise; only running out of memory could stop
it.

**D4. An instance of the design hierarchy is created and constructed the same way.** Its owner is
unique rather than counted, and ending it ends the object through the end its class's definition
names. The library's part of a scope is built without arguments, its class's definition is written
when it is created, and the scope's constructor gives it its parent and its segment first, as D2's
record of what Lyra keeps.

**D5. Each backend realizes the two operations itself.** The execution backend gives each class a
body creating it -- taking its storage, the library's part through the library's own entry, every
member of each class from the root down, then the class's definition -- emitted by the declaring
unit and called by symbol from any other, so a unit creating an object of another unit's class reads
nothing of its size. The C++ backend renders a class's C++ constructor as creation only, with no
code of the design in it, and the SV constructor as a member function; a base's construction is a
qualified call of the base's, and a `new` is a library helper that creates, takes the reference and
constructs. Clang compiles that rendering to what the execution backend does.

What stays as the earlier record had it is that where every part of an object sits and which body
answers each behavior are fixed when the program is compiled; a method that is not virtual is chosen
by the class of the handle (LRM 8.14); a parameterized class has its own static properties per
specialization (LRM 8.25).

## Why

**The standard's model is Java's, and the order and dispatch it states are Java's to the clause.**
JLS 12.5 allocates "room for all the instance variables declared in the class and ... in each
superclass", sets them to their defaults, then runs the superclass constructor, the class's
initializers and the body, and says "Unlike C++, the Java programming language does not specify
altered rules for method dispatch during the creation of a new class instance"; its example 12.5-2
is LRM 8.17 NOTE 2 in another language. Every runtime with a collector or a reference count fixes an
object's type once, at allocation: HotSpot installs the class word in `MemAllocator::finish`, the
CLR's `AllocateObject` sets the method table, CPython's `_PyObject_Init` sets `ob_type`, and Swift's
`swift_allocObject` builds the header from the most-derived metadata. C++ is the one that changes it
per level (Itanium C++ ABI 2.6.1, [class.cdtor]), which is the rule LRM 8.17 NOTE 2 does not have.

**A construction left partway drops the creator's reference, because the object is referred to.**
CPython's `type_call` answers a failed `tp_init` with `Py_SETREF(obj, NULL)`, which drops the
creator's one reference, and the object is reclaimed only if nothing else holds it. That is LRM
8.29's reachability under a reference count, which is how Lyra holds an object. Ending what was
built, as a C++ constructor does when an exception leaves it, frees an object a handle may still
name, which LRM 8.29 rules out ("no dangling references").

**What Lyra keeps for an object is recorded before the base is constructed, because an override the
base calls may read it.** A method of a class declared inside an instance reaches that instance
through a member the object records, so a derived override running inside the base's constructor
needs it set. Java answers the same need the same way. Javac stores an inner class's outer instance
before the `super()` call, and the verifier allows a constructor exactly that before its superclass
is initialized, assigning fields its own class declares (JVMS 4.10.2.4).

**Arguments first, because a departure there then owes nothing for an object.** The JVM creates the
object before evaluating the constructor's arguments. Evaluating them first leaves no object to
account for when one of them is left partway, and keeps where an object's own random generator is
seeded (LRM 18.14.1) after the arguments, where it was.

**The C++ backend's constructor builds only, because a C++ constructor brings C++'s rules with it.**
Rendering an SV constructor as a C++ constructor gave the program C++'s dispatch during construction
and C++'s unwinding of a construction, neither of which MIR states or the standard asks for.
Verilator, read as a second opinion, renders a class's `new` as a member function `init` and creates
with `new T` before calling it (its runtime's `VlClassRef` constructor).

## How the earlier answer was reached

The C++ backend first rendered a class's constructor as a C++ constructor and SV's `virtual` as
C++'s, and with them came C++'s answer for dispatch during construction; nothing asked whether that
was SystemVerilog's. When both backends were given one object model, the execution backend took the
same answer as "what clang does with the C++ backend's rendering"
(an-object-model-fixed-at-compile-time D7, "A base being constructed thus dispatches as itself, as
C++ gives it through construction virtual tables"), with no requirement named and no clause of the
standard read; that record's survey asked when dispatch positions are laid out, a question on which
construction never comes up. The rule that the execution backend does what clang makes of the C++
backend's output holds below MIR and assumes the rendering states nothing MIR did not, which a
host-language constructor does.

## Rejected

- **Keeping C++'s construction and telling a base's constructor whether it builds a base part**, a
  hidden argument letting a base reach the derived class's choice. It answers one call site at a
  time what the object's type answers everywhere, and leaves the escaped `this` freed.
- **Initializers before the base's construction**, as C# runs them. LRM 8.7 orders them after it.
- **Ending what was built when a construction is left**, as C++ does. It frees an object something
  may still refer to.
- **Forbidding the object's use before it is fully initialized**, as Swift's definite initialization
  does. The standard allows the calls (LRM 8.17 NOTE 2) and warns only about what they read.
- **Creation handed the values Lyra records**, a scope's place and a class's declaring instance, by
  whoever writes the `new`. Creation would then depend on the creator's values while no code of the
  design may run in it; recording them first in the constructor needs nothing new.

## Supersedes

- [an-object-model-fixed-at-compile-time](an-object-model-fixed-at-compile-time.md) D7's account of
  how an object is built, in which each constructor built its base and then its tables and members,
  a base being constructed dispatched as itself, the creator's reference went to the handle after
  the constructor, and a construction left partway ended what it built. That an instance of the
  design hierarchy is built the way a `new` object is, and that no construction is entered through a
  constant, stand. D9's last clause, that a constructor builds its base, takes its class's table and
  builds its members before its body runs, goes with it.
- [entering-a-class-construction](entering-a-class-construction.md) D3, that the C++ backend spells
  a construction with the host language's own constructor. Its D1, that the allocation answers an
  object whose properties hold their default before any body runs, is what D1 here restores.
- [a-new-expression-allocates-then-initializes](a-new-expression-allocates-then-initializes.md), for
  an object. A class's `new` creates and then constructs, and no storage is given back around a
  construction. A box's `new T(init)` stands as it was.

# A Subroutine an Interface Leaves to a Module Is a Callback

## Date

2026-11-01

## Status

Accepted. Extends `calling-a-subroutine-on-another-units-object.md` to a subroutine the interface
declares and does not define, reversing nothing in it.

## Why this decision matters

LRM 25.7 lets an interface declare a task or function and leave the body to a module connected to
it. `function int b.Get(); ...` in a module reads like a member of `b`, and what it does is give the
interface instance reached through `b` the code to run. Callers write `bus.Get()` and never name the
module.

Who defines the subroutine is a fact of each interface instance. One `Consumer` body serves
interfaces bound to different modules, and one `Bus` body serves instances defined by different
modules, so neither body may know the other. North star 2 rules out a caller's code naming the
definer's class.

## Decisions

### D1. HIR states what the source states

The interface's subroutine is a subroutine with no body, published as a callable like any other. The
scope also publishes which of its callables are left to a connected module, and whether each takes
one definition or any number (`extern forkjoin`). A caller is unchanged.

The definition is the module's own subroutine, and the module records that it defines that published
subroutine on the object a route reaches. The route is the same as for any name through a port or an
instance the module holds.

### D2. MIR holds a callback, in generic words

For each such subroutine there is one interface with one behavior, `invoke`. The published class
gains one compiler-made method, `<name> bind`, taking a handle of that interface, and the
realization keeps what it is handed: one handle, or a queue of them for `extern forkjoin`. The
subroutine itself calls what is kept.

The definer has a small class for each definition. It implements the interface, holds the object it
was made for, and its `invoke` calls the definition on that object. A module class does not
implement the interface itself, since one module may define `b1.Get` and `b2.Get`, two bodies for
one contract.

No MIR node or field says `extern`. The interface, the virtual call, the handle and the queue are
what a class-based program already uses. Established languages call this a callback: an object
stored behind an interface and registered at setup (a C++ abstract class, a Rust `Box<dyn Fn>`, a C#
delegate), with the call being a virtual call through it (`InstanceKind::Virtual` in rustc).

### D3. A definer binds in its own resolve

Every object is built before any resolve runs, and the parent's resolve binds a child's interface
port, so the definer has a route to the interface once its own routes are installed. It binds there:
it makes the callback object and calls `<name> bind` on what the route reaches. Everything is bound
before any initial value or process runs.

The callback object's class is rooted at the plain managed object and not at a class object. A class
object takes a random generator from the process running it (LRM 18.14), and nothing runs during
resolve. MIR names this root as its own alternative of a class reference.

### D4. The interface decides how a definition is kept

Replacing one or adding one to a queue is the interface's choice, made in its `bind`, because which
form applies is stated by the interface's declaration. The definer does not know.

### D5. A task runs as a body of the task

An `extern` task's body is written as a task body, not as a forwarding call, so it has the task's
disable extent, its `output` values and its completion. `disable bus.Ping` ends it as it ends any
task, and every process it started goes with it.

`disable slow.b.Ping` names the task through a module instance, and LRM 25.7.4 says that ends only
that module's definition. The definition is the module's own task with its own disable target, so
the name resolves to that target and no new mechanism is needed. A module instance defining no
`extern forkjoin` task has no call of it, so such a `disable` ends nothing.

### D6. `extern forkjoin` starts a process that owns the definitions

LRM 25.7.4 says a call behaves as a `fork ... join` of every definition. The number of definitions
is known only once the design is built, and the fork MIR has takes a list fixed at compile time. So
the body is one branch of a `fork ... join`, and that branch starts one `join_none` branch for each
definition, then does `wait fork`. It is a new process, so the definitions are the only children
`wait fork` waits for, and any process the caller already started is left alone.

No connected module defining the task is a run-time error, reported at the call, and the call
returns at once. The body is the tool's, so it reports at its caller as any body of the tool's does.
LRM 25.7.4 says it returns with no effect, and that holds except for an `output` formal, which the
caller writes back from the completion as it does for any call, so it receives the formal's default.
Not writing it needs a call that can complete without a completion, and the program has already
reported an error.

A branch started by `join_none` may run past the frame starting it, so it cannot refer to that
frame's storage. The formals of such a task have their lifetime extended (LRM 6.21), as a formal
read by a `join_none` branch in the source has. Each definition then reads its inputs and writes its
outputs as it runs, on one set of formals, which matches `fork ... join` of the calls: a definition
hands back its `output` and `inout` formals as it ends, and the last to end is what the caller
finds.

A `ref` formal of such a task cannot be extended this way, since it refers to the caller's storage.
It is not yet supported. A fork whose branches are counted at run time would carry it.

## Consequences

- The front end has to show a definition as a member of the module declaring it. That is what makes
  it lowered, have a scope and be published like any subroutine, and it is why Lyra pins a front end
  where that holds.
- A callback adds one small class per definition per body. Nothing is added per instance.
- A call of an `extern` subroutine costs a virtual call on the published class and one on the
  callback.

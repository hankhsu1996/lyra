# Assertion control is state of the statement, read where an execution begins

Date: 2026-11-02. Status: accepted, realized on both backends for every task of LRM 20.11 and every
statement it controls: the immediate assertions, simple and deferred, the declarative concurrent
assertions, the expect statement, and the violation reports of `unique`, `unique0` and `priority`. A
task's list may name anything the front end binds there except a subroutine of a package or a class
named from another body, a class, a package, a checker, an anonymous program and a production of a
`randsequence`, which are not yet supported.

## Why this decision matters

The assertion control tasks select what they act on by where it stands in the hierarchy, by name, by
type and by directive, and the integers that say so are the running program's. What they act on is
state that outlives every execution of a statement -- a control turned off stays off -- while much
of what they must not touch is an execution already under way. So the questions are where that state
lives, how a task finds it, at which moment a statement reads it, and how a Kill reaches work that
has already begun without the scheduler learning what an assertion is.

## What the standard requires

- **R1. A task selects by type, directive, scope and name** (LRM 20.11, Tables 20-6 and 20-7): a
  list of scopes or assertions, `levels` below each named instance as `$dumpvars` counts them (LRM
  21.7.1.2), and a statement label naming an assertion because it is a named block (LRM 16.3).
  `directive_type` is checked only for assertions. The short tasks are `$assertcontrol` with fixed
  arguments, 15 for `$asserton`, `$assertoff` and `$assertkill` and 31 for the action tasks, so
  `$assertoff` reaches no violation report and `$assertcontrol(4)` reaches every one.
- **R2. Off is admission.** No new attempt begins; attempts executing, and their pass and fail
  statements, are unaffected; a queued deferred report or violation report still matures, and no new
  one is queued. On, Off and Kill do not reach an expect statement.
- **R3. Kill aborts.** It aborts the attempts executing and flushes queued reports that have not
  matured, then stops checking until On.
- **R4. The action controls choose what an outcome runs**, the default `$error` included, for
  assertions and expect statements and not for violation reports; an assertion already executing,
  its action included, is unaffected. A vacuous and a nonvacuous success are told apart (LRM
  16.14.8), and these tasks do not change any counter.
- **R5. Lock** keeps what it locks from every change until Unlock.

## The decisions

**D1. Every statement the tasks select owns a control, as static storage of the statement.** It is
declared by the scope the statement stands in, as a static variable is, and lives where that
statement's other static storage lives: one per instance of the declaring scope, and one for a class
or the program where a class method or a package subroutine declares it
([variable-lifetime-storage](variable-lifetime-storage.md)). Two instances' assertions are told
apart by the tasks as a hierarchical name tells them apart, and a statement a loop or a reentrant
task runs many times is one control. LRM clause 39 gives each assertion the same standing: a handle,
iterated per design or per instance, with enable, disable, reset and lock callbacks that are the
tasks' own.

**D2. A control is registered where the design is built, and a task walks the registry.** Each
control registers under the instance holding it, or under none for a class's or a package's, and
under every named block and task it stands in, a label's included. A task naming nothing walks the
instances from the tops and takes every control no instance holds; naming an instance or a generate
block it walks the instances `levels` from it down, a block counting as no level, and a count below
0 asks for none; naming a block or a task it takes what registered within it. A name may stand for
several scopes: an array of instances is each instance, which the declaration fixed (LRM 23.3.2),
and a loop generate construct's name is each block the loop built in the object holding it, which is
that object's construction (LRM 27.4), so the loop's blocks are found as the task runs. A name of a
loop's block without a path selects it by its own index. A scope where no controlled statement can
stand -- a clocking block, a declaration of a sequence, a property or a type -- selects nothing, and
a list whose every name selects nothing is not a call naming no list, which is the whole design.
Registration happens before any process runs, so a task at time zero reaches a statement no process
has reached. Applying a control twice leaves what once does, so a call naming overlapping scopes
needs no deduplication.

**D3. What an execution may do is read once, where it begins, and kept.** An immediate assertion
reads it as the statement is reached; a deferred one decides there which action it queues; a
concurrent attempt records it as it opens; an expect records it as its wait begins. This one rule is
R2's "executing is not affected" and R4's "already executing is not affected", for every statement
alike, and nothing re-reads the control while an execution runs.

**D4. Kill reaches what has begun by two means, chosen by who holds the work.** The control holds
its assertion's attempts, so a Kill discards them where they are. A report the region holds is not
the control's to find: it records the control's generation as it is queued, a Kill advances the
generation, and the report acts at maturity only while it still stands -- the commit-point
validation [deferred-report-queue](deferred-report-queue.md) already uses for every other source
that withdraws a report. On afterwards does not bring either back.

**D5. A statement's control is where its pending reports are kept.** A deferred assertion's action
and a violation report are queued through the control, which adds its generation to what the report
records. A deferred assertion reached while checking is off is not checked, so it queues nothing,
and a violation report raised then is dropped as it is made; nothing else about the queue changes.

**D6. Vacuity is computed beside the answer, operator by operator.** Each term the automaton answers
a part of the property with also says whether the evaluation is nonvacuous, by LRM 16.14.8's rule
for its operator: a sequence is, a negation is where its operand is, a conjunction, a disjunction
and `iff` are where either operand is, `implies` is where its left operand holds and both are, and
an implication -- every `if`, `case` and followed-by among them -- is where one of its instances is.
So `implies` and `iff`, which answer as other operators do but differ in vacuity, are terms of their
own. Whether an operand holds is read on the trace so far, as every answer is
([concurrent-assertion-evaluation](concurrent-assertion-evaluation.md) D1, Annex F.5.3.1): a success
is reported at the tick it is decided (LRM 16.14.3), and the right operand of `implies` can decide
one while the left is still live, so a weak left operand still able to match holds there and a
strong one does not. Annex F.5.3.3 states vacuity over the whole word; a longer trace than the one
an attempt completed on is one no action of that attempt reads, since what has begun is not affected
(LRM 20.11). An immediate assertion has no vacuous success.

**D7. Every layer above the runtime holds what it does for any other static cell and any other
call.** HIR declares each controlled statement in its scope with the type and directive a task
selects it by, and writes each name of a task's list as what it names: an object, the objects of an
array, the blocks of a loop, or the extent of a block or a subroutine. HIR-to-MIR gives the control
a cell, registers it at bring-up, reads it where an execution begins and lowers a task, a statement,
to one runtime call per scope it names; MIR holds a library type, calls on it and arithmetic on the
word it answers. The tables' meanings, Lock, the walk and the generation are the runtime's.

## Consequences

- A design that calls no task pays one cell and one registration per controlled statement and one
  read where each execution begins.
- A task's cost follows the part of the hierarchy it selects, not the attempts in flight.
- Under `--assertions skip` the only controlled statements left are violation reports, and a task
  acts on those.
- A control type Table 20-5 does not give selects nothing: the call is ignored with a warning, as a
  dump task's argument it cannot use is, and the run goes on. The masks are read bit by bit and a
  bit the tables give no meaning to selects nothing. Every argument is read as the 64-bit integer
  every other task's integer argument is read as.

## Rejected alternatives

- **One state per type and directive for the whole run**, as a mask the context holds and each
  statement tests. It answers a task naming no scope and cannot express one naming an instance, a
  block or an assertion, which R1 requires.
- **A control per scope, inherited by the statements in it.** A task naming one assertion inside a
  scope another task turned off would need an override beside the inherited state, and Lock would
  have to say which of the two it protects; a control per statement has one answer to each.
- **Finding the controls by walking bodies when a task runs.** Nothing at run time holds a body's
  statements, and a scope's controlled statements are a fact the lowering already has, so recording
  them where the design is built costs nothing a walk would not.
- **Reading the control at each step of an execution.** It would let a change reach an execution
  under way, which R2 and R4 forbid, and Kill, the one control that must, is answered by D4.
- **A Kill that finds the reports it withdraws.** The region's lists hold every pending effect of
  the run and know nothing of assertions; validating at maturity reaches each report without a
  search, as every other withdrawal of one already does.

## Cross-references

- [concurrent-assertion-evaluation](concurrent-assertion-evaluation.md) -- the attempts a control
  holds and the answers vacuity is computed beside.
- [deferred-report-queue](deferred-report-queue.md) -- the validity a report records, which gains
  its control's generation.
- [variable-lifetime-storage](variable-lifetime-storage.md) -- where a statement's static storage
  lives.
- [qualified-statement-violation-check](qualified-statement-violation-check.md) -- the violation
  reports a control keeps.

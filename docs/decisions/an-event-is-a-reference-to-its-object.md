# An event is a reference to the object it names

Date: 2026-10-21 Status: accepted

## Why this decision matters

An event was a waiter list and the time of its last trigger, laid out in place where a design
element declared it and never read as a value. That served `event e; -> e; @e;` and nothing else:
every other way the language uses an event was refused or crashed, measured on both backends before
this was designed -- assigning one, `null`, comparing two, testing one, an event local to a task, an
element of any array, a member of a structure, a property of a class (a crash at construction on the
execution backend), an argument, a result, an event among the entries of an event list, and an event
in a queue (a crash on the execution backend and a C++ compile error). Each was one position of one
question, what an event variable holds.

LRM 6.17 answers it: "The event data type provides a handle to a synchronization object", and "When
assigned another event variable, both event variables refer to the same synchronization object. When
assigned null, the association between the synchronization object and the event variable is broken."
LRM 15.5 puts the waiters in the object: "When a process waits for an event to be triggered, the
process is put on a queue maintained within the synchronization object."

## The decisions

```text
D1. An event's value is a counted reference to a synchronization object the runtime defines, or to
    none. In MIR it is the managed reference to a runtime class that a process handle already is,
    so it is held, copied, compared, nulled, stored in a container and passed as that reference is;
    there is no event type of MIR's own. Trigger, the triggered query, a new object, and what a wait
    on one is put on are runtime functions handed the reference. The object lives as long as
    anything refers to it (LRM 15.5.5.2).

D2. A declaration that writes no initializer gives each event it declares a new object -- the event
    itself, each element of a fixed-size array of them, each member of a structure whose
    declaration gives it none (LRM 6.17, Table 6-7) -- and so do the elements `new[N]` makes (LRM
    7.5.1). Reading an entry an array does not have answers `null` (Table 7-1). The type's one
    default value is the second: it is copied wherever a value of the type stands for many
    positions at once, and a copy of one reference is one object, never several. The first is
    stated by the lowering where a declaration or `new[N]` brings positions into existence, one
    object per position.

D3. A wait on an event evaluates the event where the wait begins and is put on the object it names
    then, holding a share of that reference while it waits. An assignment to the event afterwards
    reaches only a later wait, and naming another element of an array of events is no occurrence
    (LRM 15.5.5.1, 15.5.1). What decides the wait is how many times that object has been triggered
    since the wait began, a value its watch compares as any watch does; so an event stands beside
    value changes in one event list, an `iff` qualifier holds it back the same way, and where the
    waiting process decides it asks every entry alike.

D4. A nonblocking trigger (`->>`) reads its event where the statement stands, as every deferred
    effect reads its operands there; the object triggered where it lands is that one.

D5. Triggering an event that names no object does nothing (LRM 15.5.5.2). A wait on one is put on
    nothing and waits for as long as the run lasts, one of the outcomes the clause leaves open, and
    its `triggered` answers 0.

D6. A dump of an event variable records the time steps in which the object it names then is
    triggered (LRM 21.7.2.2): the watch is on the variable and on that object, holding a share of
    it as a wait does, and moves to another object where the variable is given one.
```

## Rejected alternatives

- **Keep the object in place and wrap it only where a container needs a copy.** That is the row's
  own diagnosis, and it answers the one position it was found in: the first assignment, `null`, or
  wait through a handle needs the reference anyway, and then two representations of one event meet.
- **An event in place where nothing ever assigns it, a reference elsewhere.** That is an
  optimization chosen by whether the whole design assigns the event, the one question the layer
  stating what an event is cannot answer from the declaration in front of it; a layer below may take
  it where it can show it holds.
- **One default for both tables.** A new object as the type's default value, copied into every
  element of a fixed array, is one object for all of them; `null` as the declaration's value leaves
  every declared event unable to be triggered. The two tables answer two questions.
- **A wait on an event decided by being reached.** It is correct alone and wrong in a list the
  waiting process decides, which asks every entry after any wake and would hear the event entry say
  yes for a wake another entry caused.

## Consequences

- A queue, a dynamic array, an associative array, a structure, a class and a mailbox hold events as
  they hold any value, on both backends.
- A takeover (`assign`, `force`) of an event holds it to the object its source names (LRM 10.6).
- A clocking event of an assertion, a sampled value function or an `expect` may name an event, as
  any event control may (LRM 16.5).
- Waiting in a given order (`wait_order`, LRM 15.5.4) is still refused; it is its own construct over
  this representation.

## Cross-references

- `variable-initialization.md` -- an event's declaration gets an init statement like any value.
- `event-control-unification.md` -- an event's wait is the one wait shape, its leaf the object.
- `update-due-on-an-event.md` -- what a nonblocking trigger freezes.
- IEEE 1800-2023: 6.17 (the event data type), Table 6-7 and Table 7-1 (the two defaults), 7.5.1
  (`new[N]`), 15.5 (named events), 15.5.1 and 15.5.2 (trigger and wait), 15.5.5 (assigning,
  reclaiming and comparing events), 21.7.2.2 (dumping an event).

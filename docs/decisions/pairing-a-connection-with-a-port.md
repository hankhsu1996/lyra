# Pairing a connection's objects with a port's positions

## Date

2026-09-11

## Status

Accepted. Answers the question `instance-array-multiplicity.md` D4 leaves open -- it settles that
each element of an array port binds to its own instance and does not say which element binds where.
It reverses no recorded decision.

## Why this decision matters

An interface port may carry a range, and the actual on a connection to it may be an interface
instance, an array of them, or a higher level interface port (LRM 23.3.3.4). Whichever it is, the
port stands for several objects and the connection has to say which object stands at which of its
positions. That question has one answer in the standard and had none here: the case where the actual
is another port was refused, and the case where it is an array was answered by a helper of the front
end rather than by anything this compiler states.

The refusal is the visible half. The invisible half is that nothing here had ever written down the
pairing rule, so the one path that worked, worked because a front-end helper happened to apply it.

## The tension this addresses

Three facts have to hold together, and the third is what makes the first two awkward.

- **The pairing is fixed by the standard and by the declarations alone.** LRM 23.3.3.5 pairs the two
  sides left index to left index, right index to right index. So it is a function of the two
  declared ranges: nothing about the elaborated design enters it, and it is the same for every
  instantiation of the module the connection sits in.
- **Each side counts its positions from the lower end of its own range.** That is what a name
  resolving a coordinate spends the declared range for, and it is what the coordinates a route
  carries have always meant. Lower-based counting and left-to-left pairing disagree exactly when the
  two ranges run in opposite directions, which is legal and which the front end accepts.
- **Neither side can compute the other's half.** Where the actual is a local array, only the front
  end can say which instance an element is; where it is a forwarding port, only this unit can say
  which coordinate of its own member to take, because the objects behind that port belong to
  whatever bound it and differ per instantiation.

## Decisions

### D1. A connection binds one object at each position the port stands for, and the port's shape says how many

How many objects a connection binds is the child's promise, read off the published type of the port
it binds. A port standing for one instance stands at one position with no coordinate, which is the
same statement over an empty list of dimensions rather than a case of its own -- so a connection to
a scalar port and a connection to a ranged one are one operation, and no consumer asks which it was.

### D2. Which object stands at a position is decided by the two declared ranges, never by the elaborated design

The pairing runs through each side's offset from the left end of its own declared range: a position
is converted to that offset where it is counted, and back from it where it is used. Two ranges that
run the same way pair position to position; two that disagree pair position to its mirror. Nothing
else is consulted.

This is the rule LRM 7.6 already states for the elements of an array, and which this codebase
already applies to values: whole-value movement is position-wise in left-to-right order. An array of
objects is the same rule over objects, so the second axis is expressed the way the first already is
rather than growing a form of its own.

What it rules out is the shortcut. Where the actual is a forwarding port, the front end can be asked
what that port is connected to, and it answers with the objects at the far end, already paired with
the child's positions. Those objects belong to one instantiation of the module being compiled; the
module has one compiled body and may have many instantiations, and may be the top, with no
instantiation at all. Reading the pairing off them would compute a per-unit fact from a per-instance
artifact.

### D3. What the actual names is a member and a region of it, so whole, part, and one element are one shape

An actual reaches a member that stands for objects and keeps some of its coordinates. A coordinate
the actual fixed settles a dimension; one it did not leaves every position of that dimension open; a
part select leaves a contiguous span of them. Naming the whole of a member is the case where the
span is the whole dimension, and naming one element is the case where the dimension is settled -- so
the three spellings the language admits are one statement at three widths, and the walk that reads a
name through a port is the same walk that reads a connection's actual.

### D4. The agreement answers, not either side

The child's shape comes from the type it published; the actual's comes from the declarations of the
unit the connection sits in. The two are compared dimension by dimension, and a disagreement is
refused.

**What that comparison produces is the only thing entitled to say how many objects the connection
binds and which one stands at each position.** Neither side may be asked directly. This is the
substance of the decision rather than a way of writing it down: a count read off one side agrees
with itself whatever the other says, so a connection sized by the child's promise and then checked
against the child's promise has a check that cannot fail. The two sides would appear to meet while
only one of them ever spoke, and the dependency the boundary exists to declare would be undeclared
in the one respect that matters.

**Both sides keep the shape of what they came from, so a reading cannot be mistaken for the other.**
A promise read off a signature and a reach built by a walk are different things and stay different
things where the two are brought together. Compatibility between two shapes is symmetric, so a
comparison alone cannot tell which of them was the promise; the pairing that follows is not
symmetric, and would silently invert. What separates them is where each came from, so that is what
is carried rather than a shape stripped of it.

The failure this guards is the quiet one: a connection that pairs positions which do not correspond
produces a design that elaborates, runs, and is wrong, with nothing able to report it.

**The question that found this** -- and the part of it that transfers -- is not "is this checked"
but **"which side is entitled to answer this?"** Both sides knowing a fact is not the same as either
being entitled to act on it alone.

## Rejected alternatives

- **Bind the port to the actual's sequence rather than to its objects one at a time.** One store
  instead of N, and no pairing at the connection at all. Rejected because the child's member holds
  its own handles: a part select, and two sides whose ranges disagree, both make the child's
  sequence a different sequence from the actual's, so the elements have to be placed rather than
  shared -- and placing them is the pairing this entry is about, moved to whoever builds the
  sequence.

- **Take the pairing from the front end for every actual form.** It is already correct for one of
  them, and it would remove the second statement of the rule. Rejected under D2: for a forwarding
  port the front end's answer is the far objects of one instantiation, which is per-instance
  information, and the unit may have no instantiation.

- **Record the pairing as a rule the two sides share, computed once.** Rejected because the two
  sides cannot share it: one answers "which instance", which only the front end knows, and the other
  answers "which coordinate of my own member", which only this unit knows. The rule is applied twice
  because the question it answers is asked of two different things, and what keeps the two honest is
  a conformance case that puts a direction mismatch on each -- not a shared helper, which would have
  to be given the answer it is meant to compute.

- **Carry the declared ranges on the port's own declaration beside its type.** The connection needs
  which way each range runs, and the declaration records only how many elements each dimension
  holds. Rejected because the port's type already states the whole of it: a width is what is left of
  a range after the direction is dropped, and the reader that needed the direction is evidence that
  dropping it was the error, not that a second place should hold it.

## Consequences

- A module that hands its interface port on is compiled from its own declarations alone, so it
  compiles the same whether it is instantiated once, many times, or not at all.
- The route vocabulary is unchanged. A step onto a port or onto a published member already carries
  the coordinates that pick one object out of several, and the connection writes the paired
  coordinate onto the end of the walk's own route.
- A name and a connection's actual are read by one walk. A name settles every coordinate because it
  reaches one object; an actual may leave some open, and that is the only difference between them.
- The count a connection is sized by and the coordinates it writes come from one place, so the two
  cannot drift and no later reader can reach for either without the comparison having happened.
- What a port carries to a child it forwards to is the objects the outermost connection bound, at
  any depth, because each link pairs its own two declared ranges and no link consults the design.

## Cross-references

- `instance-array-multiplicity.md` -- one member whose type carries the multiplicity, and D4's
  element-at-a-time binding, whose open question this entry answers.
- `interface-port-binding.md` -- what an interface port's member holds and who fills it.
- `publishing-an-owned-instance.md` -- the published instance an actual may name through a port,
  which D3 reads with the same walk.
- `selector-coordinate-resolution.md` -- D4 there states the left-to-right element correspondence
  for values that D2 here states for objects.

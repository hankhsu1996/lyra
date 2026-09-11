# A Net Type Is a Fold and a Contribution of Its Own

## Date

2026-09-11

## Status

Accepted. Refines [net-driver-resolution](net-driver-resolution.md): its decision 8 put strength on
the contribution and this says why that is exact, and its decision 4 gave the single-driver
constraint to the Seal barrier, which F3 below argues belongs to neither that barrier nor this layer
at all.

## Why this decision matters

Half the built-in net types resolve and half are refused, and the half that is refused looks like
six separate features: a pulldown, a pullup, two power supplies, a charge store, and a single-driver
wire. Taking them one at a time produces six resolvers, each defensible on its own and none of them
the language's own shape. The choice underneath is what a net type is allowed to say, and it decides
two things that are expensive to get wrong: whether drive strength becomes a property of every net's
value, which every design in the corpus would then pay for on every driver update; and whether the
constraint a single-driver net carries needs a design-global barrier before it can be checked.

## What the standard requires

Read from IEEE 1800-2023 rather than recalled, because the sentence that settles the shape is in the
declaration clause rather than in either of the clauses about net types or strengths.

- **6.7.1**: "The default initialization value for a net shall be the value `z`. Nets with drivers
  shall assume the output value of their drivers. The `trireg` net is an exception. The `trireg` net
  shall default to the value `x`, with the strength specified in the net declaration (`small`,
  `medium`, or `large`)." A net type therefore states a **value and a strength**, not a value.
- **6.6.5**: "A `tri0` net is equivalent to a wire net with a continuous 0 value of `pull` strength
  driving it... When there are drivers on a `tri0` or `tri1` net, the drivers combine with the
  strength `pull` value implicitly driven on the net to determine the net's value." The same pair,
  said as a driver the net type supplies -- and one the clause does not count as a driver, since it
  also writes "when no driver drives a `tri0` net".
- **6.6.6**: `supply0` and `supply1` "shall have `supply` strengths", which is the same pair at the
  top of the scale.
- **6.6.4**: a `trireg` is in the driven state while any driver holds 1, 0, or x, and in the
  capacitive state, retaining its last driven value, when every driver is at z.
- **6.6.2** calls `uwire` "an unresolved or unidriver wire", and **6.6.7** states the general rule
  behind that pairing: a nettype "may be declared without a resolution function, in which case it
  shall be an error for a net of that nettype to have multiple drivers". Unresolved and
  single-driver are one fact under two names.
- **28.11** Table 28-7: one scale of eight levels, `supply` 7 down to `highz` 0, and a strength
  specification is written **on a driver** -- on a continuous assignment or a gate instance -- as a
  strength for the value's 0 part and one for its 1 part.
- **28.12.1** and **28.12.4** decompose resolution: between drivers of unequal strength "the
  stronger signal shall dominate all the weaker drivers and determine the result", and the wired net
  types "shall resolve conflicts when multiple drivers have the same strength" by treating the
  signals as inputs of a logic function. Strength decides between levels; the truth table decides
  within one.
- **28.12**: "Nets with user-defined nettypes shall not have strength levels", and the resolution
  function of 6.6.7 takes only the drivers' values. The language's own generalization of a net type
  keeps the fold and drops the strength.

## What the modelled subset forces, before any lower contract is consulted

Two consequences follow from what Lyra does and does not simulate, and everything below is
downstream of them.

**A resolved net value is never an input to another resolution.** The three constructs that would
make it one are a switch, a gate primitive, and net collapse; the first two are not modelled, and
the third (LRM 23.3.3.7) merges two nets into one resolution node, so the drivers of both sides join
one fold rather than one net's result reaching the other. So the resolved value needs no strength,
and the strength levels a contribution can carry stay unambiguous -- the ambiguous strength ranges
of 28.12.2 are produced by three-state gates with unknown control and by charge sharing across
switches, and by nothing else.

**A contribution's strength is a property of its driver, not of its bits.** It is written on the
assignment that creates the driver, or fixed by the net type that supplies it. Per-bit strength
exists in the standard because a switch network produces it, so a model with no switches carries one
strength per contribution and loses nothing.

## Findings that shaped the design

### F1. Twelve keywords are two independent facts

Written out, the built-in net types are a product of a fold and a contribution the net type makes to
its own resolution, with no cell that needs anything else:

| Net type         | Fold            | Own contribution                  |
| ---------------- | --------------- | --------------------------------- |
| `wire`, `tri`    | tri-state       | z at `highz`                      |
| `wand`, `triand` | wired-and       | z at `highz`                      |
| `wor`, `trior`   | wired-or        | z at `highz`                      |
| `tri0`           | tri-state       | 0 at `pull`                       |
| `tri1`           | tri-state       | 1 at `pull`                       |
| `supply0`        | tri-state       | 0 at `supply`                     |
| `supply1`        | tri-state       | 1 at `supply`                     |
| `uwire`          | tri-state       | z at `highz`                      |
| `trireg`         | charge-retained | x at the declared charge strength |

A contribution at `highz` can decide nothing, so the three ordinary rows contribute nothing and the
net with no drivers resolves to the value every fold starts from. The undriven case is therefore not
a case.

### F2. Strength between levels and the truth table within one is the whole algorithm

28.12.1 and 28.12.4 give the resolution directly, and it needs no strength arithmetic beyond
comparison:

```
resolve(contributions):            # each contribution is a value and one level
  result = high-impedance
  for level from supply down to small:
      group = contributions at that level
      if group is empty: continue
      g = fold(group) under the net type's truth table
      result = g where result is still high-impedance, else result
  return result
```

Where every contribution sits at one level -- every design that writes no strength specification and
declares no pull, supply, or charge net -- the loop runs its body once and the result is the fold
this codebase already performs, so the general form costs the ordinary case nothing.

The check that the model is the standard's own is Table 6-5, which gives `tri0` resolved against two
`strong` drivers. Running the algorithm against it agrees in every cell, including the two that
separate a strength model from a value-only one: `(1, z)` is 1, because the strong driver outranks
the pull contribution, and `(z, z)` is 0, because nothing outranks it.

### F3. The single-driver constraint belongs to the front end, which already enforces it

[net-driver-resolution](net-driver-resolution.md) decision 4 places the single-driver constraint at
the Seal barrier, reasoning that a count is meaningful only once every attachment across the design
exists. Both the barrier and the count are answers to a question this layer does not own. Which
drivers a net may have is semantic resolution over the elaborated design, which
[front-end-semantic-boundary](front-end-semantic-boundary.md) puts in the front end, and the front
end's design-wide driver analysis already answers it -- naming the net, the driver that broke the
constraint, and the one that was there first, including where that one arrives through a port
connection authored in another unit. Measured against the two programs this decision was written
from, the front end reports both and Lyra reports neither, because Lyra runs elaboration and not
that analysis.

What follows for the net type is that `uwire` needs nothing below the front end at all. With at most
one driver admitted, a fold over one contribution is that contribution, so `uwire` is the tri-state
fold under the ordinary contribution, and a fold alternative meaning "unresolved" would be an arm no
accepted program can reach. The same holds for the general rule 6.6.7 states, which is the same
check on the same analysis.

### F4. Two decorations the front end hands over and nothing reads

A net declaration may carry a drive strength, a charge strength, and a delay. A continuous
assignment statement carrying a drive strength is refused by name; the same strength written on a
net declaration is dropped, and so is a net delay. Measured on the tree this decision was written
against: `wire (weak0, weak1) w = 1'b0;` beside `assign w = 1'b1;` resolves to x where 28.12.1
requires 1, and a `#10` net delay changes the value at time 1. Both are silent wrong answers rather
than refusals, which is what makes them this decision's to close: a net type that states its own
contribution states it in the same vocabulary a driver's strength is written in, so the strength has
somewhere to go the moment the vocabulary exists.

## The decision

1. **A net's construction states two things: the fold its net type performs, and the contribution
   that net type makes to the net's own resolution.** The contribution is a value and a strength
   level. Nothing downstream recovers a net type keyword, and no net type is a resolver of its own.

2. **Resolution is domination between strength levels and the net type's truth table within a
   level.** A contribution at the high-impedance level takes no part, which is what makes the
   undriven value the fold's own starting point rather than a case anyone writes.

3. **A constraint on which drivers a net may have is the front end's, and Lyra's part is to run the
   analysis that decides it and report what it says.** No net type is unresolved below that
   boundary, and no layer here counts drivers.

4. **The charge-retaining fold's own contribution takes the resolved value at every position a
   driver decided.** That is the whole of the driven state and the capacitive state: a position some
   level above the charge decided is refreshed, and one nothing decided is answered by the
   contribution, which still holds what it was last given.

5. **A driver's contribution carries the strength its source states**, defaulting to `strong` where
   the source states none, and a net-declaration assignment's strength is its driver's like any
   other. A specification whose two halves differ is refused by name: at a position where such a
   driver is unknown it occupies a range of levels rather than one, and a range is what this model
   does not carry.

6. **A net's resolved value carries no strength.** What the subset forces, above, is the reason; the
   boundary is a switch or a gate primitive, so that is where this has to be revisited rather than
   at the next net type.

## Consequences

- The six refused net types need one new fold between them. Four are the tri-state fold under a
  different contribution, one is the tri-state fold exactly, and only the charge-retaining type
  resolves differently.
- Lyra runs the front end's design-wide analysis and reports what it finds, which is how the
  single-driver rule arrives and how every other rule that analysis decides arrives with it.
- Drive strength on a continuous assignment stops being a refusal and becomes what a driver carries,
  because the fold that consumes it exists for another reason.
- A design that writes no strength anywhere resolves through the same single pass as before, and the
  net carries one small integer per contribution.
- The Seal barrier stays unmaterialized. Nothing in this decision needs it, and the endpoint
  validation that does is unchanged.
- A net delay is refused by name rather than dropped, which also refuses charge decay (LRM 6.6.4.2),
  whose specification is a delay on the declaration.

## Rejected

- **A strength on the net's resolved value, per bit.** This is the faithful model and it is what
  Icarus Verilog implements, with one byte of strength per bit and resolution performed bit by bit;
  it is also why that simulator carries two net representations, one that preserves strength through
  a node and one that reduces it to four-valued logic, so that only the nets that need the first pay
  for it. Rejected here for the reason the subset forces: nothing in the modelled language reads the
  strength of a resolved value, so the faithful model would be paid for by every net and observed by
  none. The condition that would reverse it is switch-level modelling, not another net type.

- **A value-only fold per net type**, with `tri0` spelled as "resolve tri-state, then answer 0 where
  the result is high impedance". It is correct for exactly the programs that write no strength, and
  the shape is the problem rather than the coverage: it is the comparison between `pull` and
  `strong` already performed and then frozen into a rule, so `supply0` needs the opposite rule and a
  design that writes one explicit strength is outside all of them.

- **Resolving strengths at compile time.** Verilator's tristate pass walks the whole design from
  child to parent, resolves the strongest driver statically where it can, and expands what is left
  into a value and an enable, which is why its own notes record that two equally strong drivers with
  different values cannot produce x. The pass needs the design's whole driver set at compile time.
  Under compile-per-unit with elaboration at runtime a net's drivers are not knowable when its unit
  compiles -- a driver may be a port connection authored in a unit that has not been seen -- which
  [net-driver-resolution](net-driver-resolution.md) F2 establishes and which rules this out here
  however cheap it is there.

- **A single-driver check of Lyra's own, whether at the Seal barrier or at the attachment that
  breaks the constraint.** Both are a second authority for a rule the front end decides, and the
  second one loses on its own terms as well: to name the driver that was already there, every driver
  on every net in the design would carry a provenance record, so that one diagnostic could name a
  conflict the front end names for nothing.

- **Taking the bidirectional port with these net types.** An `inout` port connection is "a
  non-strength-reducing transistor connection" (LRM 23.3.3) between two nets, which 23.3.3.7 settles
  by merging them into one simulated net whose type Table 23-1 selects. It is a question about which
  nets are one resolution domain, and it is answered without deciding anything about strength -- the
  merged node folds the drivers of both sides exactly as it folds its own.

## Cross-references

- [net-driver-resolution](net-driver-resolution.md) -- the driver, contribution, and resolution
  model this states the net type's half of.
- [procedural-continuous-assignment](procedural-continuous-assignment.md) -- why a forced value is
  not a driver, and therefore carries no strength to compare.
- [runtime-entry-naming](runtime-entry-naming.md) -- the rule that splits these two: a form
  differing by which request is meant is its own entry, which is what a fold is, while one differing
  by an argument is one entry whose caller supplies it, which is what a contribution is.

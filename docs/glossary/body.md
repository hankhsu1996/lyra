# Body

**Definition.** The lowerings of one [unit](unit.md) whose HIR comes out equal, compiled once and
run by every instance they serve; named by the unit's name for the unit's first body in the design's
order, and by the unit's name and its ordinal after.

**Contrast.** Not the front end's elaborated body (`InstanceBodySymbol`), which is one per instance
it did not share and is always written "the elaborated body". Not a unit: a unit's bodies differ in
their code and agree on what they publish.

**Usage notes.** Everything after HIR is per body. Which body each instance and generate block is,
is construction data the design root states, never code a parent compiles.

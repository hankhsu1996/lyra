# Only a Base Links Two Signatures

## Date

2026-09-16

## Status

Accepted. Realizes `../architecture/emission_model.md` invariant 1 and
[unit-signature](unit-signature.md) D6 in the C++ backend, and reverses neither.

## Why this decision matters

A unit's declarations and the bodies realizing them shared one emitted file. A referrer therefore
took on everything the unit it referenced had itself referenced, and two units referencing each
other produced a pair of files no order could satisfy: whichever the compiler entered first met a
body naming a unit it had not seen.

That the two must be separate artifacts is settled elsewhere. What is not written down anywhere is
how much the separation buys -- which turns out to be almost everything, because once the bodies are
out, the declarations depend on another unit in exactly one construct. Without that written down the
fix reads like a coincidence of the case that found the defect, and the next reader re-runs the
experiments to find out whether it holds in general.

## Decisions

### D1. A unit's declarations and its bodies are separate, and the program is compiled per translation unit and linked

Each unit specialization emits the declarations a referrer compiles against and, separately, the
translation unit realizing them. Every translation unit is compiled on its own and the results are
linked, so no unit's bodies are read while another is compiled. The program entry is one more
translation unit with no standing of its own.

### D2. A declaration reaches another unit through a pointer, so it names the class and not the file

Apart from D3's one construct, a unit's declarations name no other unit's file. Every external name
they carry is reached through a pointer, which an incomplete type serves, so the external class is
declared without its contents beside this unit's own declarations.

The reason that covers every case, rather than the case at hand, is that -- D3's base aside -- a
declaration has only three ways left to carry another unit's name, and none of them needs a complete
type:

- **A handle to a class another unit declares** carries no class name at all. A handle is an opaque
  object reference, whose identity is compared and carried without naming what it points at.
- **A type another unit declares** is not a cross-unit name. A referrer interns the type into its
  own types, so what the declaration carries is this unit's own spelling of it.
- **The object of a unit this one instantiates, connects a port to, or holds a published member of**
  is reached through a pointer to that object.

Everything needing a complete type -- constructing an instance, reaching a member, casting to the
class -- is an operation rather than a declaration, and operations are in the other file.

### D3. A base is the one name that needs a complete type, so a class is what a file holds

A base must be complete where the derived class is declared (LRM 8.13, and 8.26 for an interface
class), so a class extending a class of another unit is the single cross-unit name a declaration
cannot reach through a pointer: the edge exists exactly where a base crosses and nowhere else.

**A file is read once, so a file is the unit an order can be given to.** What actually has to be
ordered is one class against the class it rests on. Where a file holds several classes, an order
over classes that exists can fail to have a corresponding order over files -- whichever file is
entered first reaches a base the second entry cannot supply, because that file is already open. The
circle is manufactured by the packaging; the design never had one.

So each class a unit promised is written in a file of its own, alongside one file for what the unit
declares that rests on no class at all. The include graph is then the class graph, which is acyclic
because a class may not be its own ancestor -- and the front end checks that, so this backend
inherits the guarantee instead of restating it. A referrer names the file from the pair naming the
class, which both sides have, so no list says where a class was put.

**This reverses an earlier reading, which refused such a design.** That reading was derived with the
per-unit file taken as given, and concluded that "a cycle in those edges is the one thing this shape
cannot carry" and that the party assembling the program should detect one and report it. Two things
answer that rationale. The program is well-formed -- no class in such a circle is its own ancestor,
and the standard's only ordering rule that could reach it is LRM 26.3, whose normative sentence
requires a package's compilation to precede scopes in which the package is _imported_, while a
qualified reference is resolved where it is written; LRM 3.12 paraphrases that rule more widely but
cites it rather than adding one, and says in the same breath that a package is a design element and
that the standard "does not normally specify requirements regarding the order of compilation for
design elements". And `../architecture/north_star.md` inv 3 forbids rejecting a correct program
because a sharing cannot be applied, which is what refusing it was. The per-unit file was never a
requirement; it was the shape that happened to be there when the question was first asked.

**Interleaving includes with class definitions inside one per-unit header is the cheaper-looking
answer and does not work.** A valid order can require entering one file's later part after another
file's later part, which the guard forbids, so it succeeds from one translation unit's entry point
and fails from another's.

### D4. A symbol several units each define takes a merge rule that keeps an unreferenced definition

Every unit declaring a scope that exports a foreign name defines that name's symbol, and the linker
keeps one. The rule chosen must be one that keeps a definition nothing in the emitting language
references, because such a symbol is reached only from outside that language (LRM 35.4, 35.7); a
rule permitting the drop drops it from every artifact and the program fails to link.

## Rejected alternatives

- **Ordering one file's contents so the includes sit between the declarations and the bodies.** It
  resolves the same cycle: each unit's declarations are in scope before any other unit's file is
  entered, and the bodies meet each other afterwards. It is cheaper than this entry's shape and it
  is what to reach for if the shape were blocked. Rejected because it answers a symptom: every body
  still sits in a file every referrer includes, so editing one body still recompiles every unit that
  reaches it, and the independence the unit boundary exists for is no closer.

- **A third artifact per unit carrying only forward declarations**, on the model of a standard
  library's forward-declaration header. It is the general answer where declarations may cycle.
  Rejected because D2 leaves only D3's construct needing one, and a base needs the contents rather
  than the name -- so the extra artifact would serve none of the cases and miss the one that is
  left. D3's own answer splits the contents instead, which is why it reaches the case this one
  misses.

- **A referrer naming the declaring unit's file for every external name.** Simpler to emit, and it
  is what the single-file shape did. Rejected because it makes one unit's declarations depend on
  another's wherever a name crosses, which is both the cycle and a dependency on facts the referrer
  never uses: what the declaring unit itself referenced arrives with it.

- **Publishing the unit's cells as definitions in the declarations, merged by the linker.** A cell
  value-initialized in a header is one definition however many artifacts include it. Rejected
  because each referrer's object file then carries storage belonging to the unit it reached, which
  is a unit's contents crossing the boundary in the one direction nothing checks.

## Consequences

- The declarations complete without any other unit's file except where a base crosses, so what a
  referrer compiles against is bounded by what the unit it references promised rather than by what
  that unit consumed.
- A design whose units extend each other in a circle emits and runs. Nothing detects the circle,
  because the shape has none to detect: the files are the size of the thing an order is over, and a
  program that could be ordered at all can be ordered here.
- Incremental reuse over the declarations is per class rather than per unit, which is finer than the
  boundary this entry set out to draw and falls out of the same change.
- Every build still compiles every unit: nothing records which artifact a change invalidated. The
  boundary makes incremental compilation available and does not perform it; compiling several units
  at once is spent, and [a-build-is-told-how-wide-to-run](a-build-is-told-how-wide-to-run.md)
  settles who says how many.
- A cell the unit owns is declared in the declarations and defined once in the bodies, so the
  program holds one cell per declaration and no referrer's artifact carries it.
- A constant the declarations themselves name -- a type description a class's own constant reads in
  its initializer -- stays with them. Two constants of one file are initialized in the order that
  file writes them, and that is a guarantee which ends at the file boundary.

## Cross-references

- `../architecture/emission_model.md` -- the artifact rules D1 realizes, and the forbidden shape a
  single file matched.
- [unit-signature](unit-signature.md) -- what a unit publishes, and D6 there, which requires the
  split this entry realizes.
- [program-facts-belong-after-compilation](program-facts-belong-after-compilation.md) -- D3 there
  settles that a foreign symbol several units define takes a merge rule rather than an owner; D4
  here names the party and the constraint on the rule.
- [reaching-past-a-published-class](reaching-past-a-published-class.md) -- what a class promises
  about the class it extends, which is what a referrer walks; D3 here is the emission-side cost of
  that edge crossing a unit boundary.
- [declarations-before-bodies](declarations-before-bodies.md) -- the same ordering one layer up, in
  the lowering rather than in what it emits. Its rejection of two artifacts is about the IR a
  consumer reads, and does not reach what a backend writes.

# HIR

The code does not meet invariant 2 everywhere: what still carries a name a later layer finds a
declaration by is listed by the architecture policy's rules A029 and A030
(`tools/policy/check_architecture.py`), and the list only shrinks.

## Purpose

HIR is a SystemVerilog-faithful semantic IR. Its job is to mirror SV's language structure in a form
the compiler owns: declarations, expressions, statements, processes, assertions, functions, tasks,
parameters, ports, and generate constructs all appear as first-class HIR nodes carrying their
LRM-level shape. HIR is the compiler's first canonical representation and the cutover from frontend
identity to compiler identity -- slang's symbol ids are discarded at AST-to-HIR; HIR's typed ids
replace them with no loss of LRM-level information.

HIR is not a generic programming-language IR -- that role belongs to MIR. SV-specific concepts
(signals, non-blocking assignments and the deferred scheduling they imply, event-control semantics,
generate-as-elaboration, ports-as-aliases) all live here verbatim, awaiting translation into a
generic programming-language vocabulary at HIR-to-MIR. Loops are loops, assertions are assertions,
classes are classes; the abstraction level is the language the user wrote, not what the compiler
later turns it into.

HIR's role between AST and MIR mirrors the role a language's own semantic IR plays in other
compilers -- rustc's HIR and THIR, clang's AST ([`compiler_overview.md`](compiler_overview.md) maps
every layer): a language-faithful representation the compiler owns, with frontend syntactic noise
removed and canonical compiler-owned identity in its place, but with the language's vocabulary still
visible. The translation away from that vocabulary is the next layer's job, not HIR's.

## Owns

- Language constructs as first-class nodes: declarations, expressions, statements, processes,
  assertions, functions, tasks, parameters, ports.
- Compilation-unit-local declarations with typed identities, one id kind per kind of declaration
  (variables, nets, parameters, instance members, interface ports). An interface port is a kind of
  its own rather than an instance member, because the scope is handed the instance it names instead
  of building one (LRM 25.3), so a route stepping through it continues into whatever unit that
  instance is of.
- The unit's type pool, a type graph whose every edge is a type identity. A packed array is one
  declared dimension over an element named by its type, so an array of a packed structure carries
  that structure, and the nest bottoms out at a scalar bit, a leaf type of its own (LRM 7.4.1);
  `int` and the packed array it equals are one type (LRM 6.11.1), so the spelling is not recorded.
  An unpacked array is likewise one dimension per node, nested through its element type, because a
  multidimensional array is an array of arrays (LRM 7.4.4). A width and whether a value can hold x
  or z are worked out from the pool, never stored beside a node, so they cannot disagree with it.
- The generate-region tree as a first-class structure inside each module compilation unit.
- Each procedural body's lexical scope tree, beside its statement tree. Every body has one root;
  every `begin ... end`, `fork ... join`, task and function body, and every loop declaring its own
  variables (an implicit block, LRM 12.7.1, 12.7.3) opens a scope nested in the one around it. A
  scope records the construct that opened it, the identifier the source gave it where it gave one,
  the variables it declares directly and the scopes nested directly inside it. A block-shaped
  statement names the scope it opens; nothing in the scope tree points back into the statements.
- The canonical association between a declaration and its references within the same compilation
  unit.
- Source locations for every construct that may produce a diagnostic.

## Does Not Own

- Control-flow graphs, basic blocks, or phi nodes.
- Execution scheduling, dirty tracking, or subscription wiring.
- Low-level storage layout or ABI decisions.
- Hierarchical paths beyond the compilation unit's own generate tree (see
  [`hierarchy_and_generate.md`](hierarchy_and_generate.md)).
- A generic programming-language vocabulary. SV's vocabulary survives at HIR; the translation into a
  generic vocabulary is MIR's identity, performed at HIR-to-MIR.
- Where a variable of static lifetime is stored and which object holds it
  ([`lifetime.md`](lifetime.md)), and what a procedural scope keeps at run time, which is no object
  ([`hierarchy_and_generate.md`](hierarchy_and_generate.md)).

## Core Invariants

Each invariant below is a direct consequence of the identity stated in Purpose -- HIR is the
compiler's first canonical, SV-faithful representation. An invariant that cannot be re-derived from
that identity is the suspect, not the analysis.

1. Every compilation-unit-local declaration has a typed id. References to that declaration carry the
   typed id, never a frontend symbol id. _Source-faithful consequence: HIR is the layer's source of
   identity; frontend identity ends at AST-to-HIR._
2. A declaration another unit publishes -- a class, a covergroup, the class of a scope, an
   enumeration, a structure or a union -- is known across units by its stable key, the declaring
   unit and the name it was published under, the only identity a signature carries; for a class that
   name is its specialization's. A unit turns a key into its own typed id for the declaration once,
   the first time the key enters it, and a key naming the unit itself -- a hierarchical name leaving
   the instance and reaching another instance of the same module (LRM 23.6), or a signature naming
   this unit -- lands on the unit's own declaration, so one declaration has one id in a unit. What a
   unit knows of another unit's declaration is what that unit published, read back with every key in
   it turned into the reader's ids, and it knows that of every class a construct of it reads rather
   than only names; the unit's verification refuses one that does not. A namespace (LRM 26.3) is
   such a declaration: a unit holds one entry for each namespace it names, its own included, with
   what the namespace declares, and names a member of it by the entry and the member's position
   there, as it names a class's member; the verification refuses a reference to an entry the unit
   does not hold. Below this layer a name is spelling and nothing finds a declaration by it.
   _Source-faithful consequence: SV's compilation-unit boundary is where a key becomes an id, and
   nowhere else._
3. HIR preserves LRM-level constructs without flattening them into lower-level primitives. Loops are
   loops, assertions are assertions, classes are classes; a packed type is its declared nest and not
   a width, and the streaming operator is itself -- its operands, the `with` range of each, and its
   block size, zero for `>>`, which performs no re-ordering (LRM 11.4.14.2) -- with no type of its
   own, since the stream between packing and unpacking is never visible (LRM 11.4.14), and no
   direction, since where it stands says which it is. _Source-faithful consequence: the abstraction
   level is the language the user wrote; flattening into a generic vocabulary is MIR's job._
4. Each compilation unit is self-contained: resolving any intra-unit reference requires no data
   outside that unit. _Source-faithful consequence: independent compilation is preserved at the
   layer where SV's unit boundary lives._
5. The generate tree lives inside the module compilation unit and is the sole representation of
   generate-region structure at HIR level. _Source-faithful consequence: SV's generate construct is
   carried verbatim, not pre-elaborated._
6. HIR is the canonical semantic identity of the compiler. No frontend id survives past AST-to-HIR.
   Downstream layers consume HIR identity, never frontend identity. _Source-faithful consequence:
   the cutover from frontend to compiler-owned identity happens at one boundary, not by gradual
   propagation._
7. An expression the source wrote once is one node, held by one construct. A construct whose meaning
   uses an operand several times -- `$` in a queue's index, a `foreach` over an array, a net joined
   to several others, a name a view joins several declarations under -- states the operand once and
   says what the construct is; working out how often and where the operand is then read is the
   translation's job. A description of what a body reads or waits on names expressions the body
   already holds and is not a second holder. The unit's verification refuses a unit that breaks
   this, where the unit is produced. _Source-faithful consequence: HIR is a tree because the source
   is one, and a node with two holders is a translation done a layer early._
8. A type's identity in a unit is what the type is, and nothing the frontend did. The pool is an
   interner: a second request for one type answers the identity the first got, whichever route
   asked. A type naming a declaration -- a class handle, an enumeration, an unpacked structure or
   union -- is keyed by that declaration, so two declared alike are two types (LRM 6.22.1); every
   other type by its structure. A type arriving from another unit's signature is interned on
   arrival, so it lands on the entry the unit already held, and an id means one type within its unit
   and nothing across units. _Source-faithful consequence: equality of ids is equality of types at
   the one layer that publishes types across a boundary, and a unit's identities depend on nothing
   outside it, so units lower in parallel and a cached unit is read back with its own._
9. A part of a value is the select the source wrote. A select whose index is a genvar or a parameter
   stays an ordinary select of that expression, evaluated by each construction with its own value,
   and so does what a body reads or waits on: the longest static prefixes its text writes. A
   part-select's type is a vector as wide as the select, numbered from zero in the direction of the
   value it selects from, since LRM 11.5.1 holds only its width constant; which bits it reached
   belongs to the select. A pattern's key is a designator naming a position, as a structure
   pattern's key names a member, and HIR carries the position. _Source-faithful consequence: one
   body serves every construction that wrote the same text, and a wait on a part and a read of it
   are turned into a position by the same step, so they cannot disagree about where it lies._
10. Containment in a procedural body's scope tree is stated once, downward, from the body's root. A
    variable of static lifetime written in a scope is no declaration of the body: it belongs to the
    structural scope, class or namespace around the body and records the scope it is written in, as
    clang separates a declaration's semantic context from its lexical one. A scope's identity is
    minted before any body lowers exactly where a name in another body can reach it -- a subroutine,
    a block the source named -- or where a static is written in it, and every scope minted is one
    some body goes on to open. _Source-faithful consequence: a `disable` or a hierarchical name in a
    body that lowers first names the scope or the static by the one id its declaration has, and a
    reader that wants what a scope owns descends the scope tree and never the statements._
11. HIR equality is derived and exact: two pieces of HIR are equal only where every field is, and a
    real value is held as the bits that represent it, since 0.0 and -0.0 compare equal as numbers
    while dividing by each differs, and a NaN compares unequal to itself. Lowerings whose HIR is
    equal are compiled as one body ([`specialization_model.md`](specialization_model.md) invariant
    6), so an equality that merged two values differing in what a program computes would run one
    lowering's code for the other. _Source-faithful consequence: equality of HIR is identity of the
    source's meaning, field by field, and nothing a comparison chose to overlook._

## Boundary to Adjacent Layers

- Consumes slang's AST. AST-to-HIR is a cutover, not a translation: frontend ids are discarded at
  this boundary. HIR establishes the first canonical semantic identity owned by the compiler. Which
  unit declares a class the front end hands over is worked out in one place, the translator that
  turns a front-end class into the unit's identity for it; a class of this unit is minted by its
  declaration pass, which every scope of the unit that can declare one is visited by before any body
  lowers.
- Produces input to MIR. HIR-to-MIR is the layer where SV's vocabulary gets translated into a
  generic programming-language vocabulary. Every SV-specific concept that does not appear in MIR is
  translated at this boundary or rejected as unsupported. HIR-to-MIR holds the SV knowledge; MIR
  does not. It turns a packed type into a width, a signedness and a state domain, a select into a
  position, and the streaming operator into the calls that pack, re-order and unpack a stream.
- HIR-to-MIR's output is consumed by backends through [`backend_contract.md`](backend_contract.md).
  If a backend would need decision logic to render the produced MIR, HIR-to-MIR is failing to emit
  the right combination of MIR primitives. The lowering's correctness is validated against the
  mechanical-translation invariant: the MIR it produces must be translatable by a mechanical LLVM IR
  backend without payload-driven branches. When extending HIR-to-MIR, read
  [`backend_contract.md`](backend_contract.md) before adding a lowering rule that produces a MIR
  node a backend cannot realize mechanically.

## Forbidden Shapes

Each entry is the inverse of a property a source-faithful, compiler-identity-owning IR implies, and
names it.

- **A frontend id -- `SymbolId` or any frontend-global id -- as the identity of a declaration
  reference, or carried into HIR or below**, fails invariants 1 and 6: frontend identity ends at
  AST-to-HIR.
- **A resolver keyed on `(instance_id, local_symbol)` at HIR level** fails the compile-time /
  runtime split: per-instance identity belongs to the runtime object graph, and HIR is class-level.
- **CFG-shaped nodes** -- basic blocks, successor lists, phi nodes -- fail invariant 3: HIR is
  structured because the source is; a CFG is LIR's vocabulary.
- **A generate-region representation outside the compilation unit it belongs to** fails invariant 5:
  lifting generate out of the unit flattens the unit boundary.
- **Coordinate or ordinal identity for a generate region** fails invariant 1: identity is typed and
  carried by HIR's own id kinds.
- **Ownership of a declaration reconstructed from names or paths through a side table, or a local
  reference resolved through a global lookup table**, fails invariant 4: ownership is structural and
  a local reference resolves inside the unit.
- **Two reference forms for one kind of declaration, one naming this unit's by id and one naming
  another unit's by key**, fails invariant 2: a declaration has one id in a unit, and which unit
  declares it is a fact of the id.
- **Type identity that reproduces whichever types the frontend happened to merge** -- an identity
  keyed by the frontend's type pointer, which merges integral vectors and gives each structure,
  enumeration and typedef a symbol of its own -- fails invariant 8. A frontend key may shortcut
  translation work; it may not decide which types are the same, and the frontend's pointer is
  design-global and dies with the frontend, which independent and incremental compilation forbid
  ([`north_star.md`](north_star.md)).
- **One type pool for the whole design** fails invariant 4 and the north star's parallel and
  incremental build: every unit would write one table, and the numbering would depend on every unit.
- **A packed type as one node of an atom and a list of dimensions** fails invariant 3 -- it is two
  flattenings a layer early -- and invariant 8's typed graph: its element is not a type, so a packed
  array of a packed structure cannot be stated and the scalar has no identity of its own.
- **A part a construction value selects carried as the range the frontend settled for one
  elaboration** fails invariant 9: every construction then states different text, so one body cannot
  serve them.
- **A back-link in the scope tree** -- a parent on a scope, a declaring scope on a variable -- fails
  invariant 10: every reader descends containment, so a back-link is stored backwards and someone
  pays a scan to invert it. Deriving the nesting from the statement tree fails it too: it makes
  every declaration-side reader walk execution structure to recover a fact the scope tree states.
- **An identity minted for a scope no pass goes on to fill**, or a second identity for a static so a
  reference can name what its declaration mints too late, fails invariant 10: one declaration has
  one id, minted at the stage a name can reach it.
- **A node whose equality is written by hand, or a real held as the number it denotes**, fails
  invariant 11: a comparison that overlooks a field, or calls 0.0 and -0.0 one value, merges two
  lowerings into one body that computes differently for one of them.

## Notes / Examples

A reference to a local variable carries a `VariableId` in the compilation unit's arena. A reference
to a package-level entity or to another unit's declaration names the unit's own entry for it, the
same kind of id, and which unit declares it is a fact of that entry (invariant 2).

A SystemVerilog `always_comb` block, a non-blocking assignment, an event control, and a `generate`
region all appear in HIR as themselves -- a process kind, an NBA statement, an event-control
expression, a generate node. HIR-to-MIR is where each gets translated into MIR's generic
programming-language vocabulary (a process becomes a callable invoked by the scheduler; an NBA
becomes a deferred closure submission; an event control becomes a coroutine suspension; a generate
becomes constructor-time logic). The SV vocabulary is HIR's; the generic vocabulary is MIR's.

Types, as the current pool states them:

```text
logic [3:0][7:0] -> Packed{[3:0], elem: Packed{[7:0], elem: ScalarBit{logic}}}
int              -> Packed{[31:0], elem: ScalarBit{bit}, signed}
byte_t [1:0]     -> Packed{[1:0], elem: <the packed struct byte_t>}
int a [4][8]     -> Unpacked{[0:3], elem: Unpacked{[0:7], elem: int}}
```

An assignment pattern `'{...}` is an expression, as the front end makes it: the destination's type
reaches it from outside (LRM 10.9.1), whether it initializes a declaration or stands on the right of
an assignment.

A `with` clause's `item` and `item.index` (LRM 7.12.4) are one iteration binding of the method's
loop, naming which of its two roles a reference reads; `item.index` is no call. An enumeration's
methods (LRM 6.19.5) keep the call shape the source wrote, over a method identity only HIR holds,
because answering one needs the member list in its lowered form, which HIR-to-MIR builds.

A body with no block at all (`initial #7 f();`) still has its root scope, the one scope kind
SystemVerilog does not itself define (LRM 23.9 lists the `begin ... end`, not the `always` or
`initial` around it), so the root is a plain id rather than an optional every reader branches on. A
block the source neither named nor declared anything in still opens a scope, of the kind the
compiler opens for its own declarations; whether the source named a scope is the presence of its
identifier, never a kind, so a `begin ... end` is one construct with or without a label, and a scope
a path can name is exactly one the source gave an identifier (LRM 23.9). A static written in a
labelled block of a task is reached from a sibling process as `t.blk.x`, which names the static's
own declaration; the blocks on the way are segments of the path text and no step of the access.

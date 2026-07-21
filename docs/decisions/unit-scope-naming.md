# Anonymous `$unit`-scope unit naming

## Date

2026-07-20

## Status

Accepted

## Why this decision matters

The `$unit` compilation-unit scope (LRM 3.12.1) holds declarations that lie outside any design
element. Lyra models that scope as an ordinary namespace unit -- the same rootless unit a package
is, with no source name of its own. Every other unit publishes a name a referrer recomputes from the
same slang facts with no shared table: a package its declared name, a module its specialization name
(`specialization-identity.md`). The anonymous unit has neither a declared name nor parameters, so it
needs its own naming rule. This entry pins that rule and records why the tempting alternative --
give the unit a design-wide id -- is the forbidden shape, not the fix, so the question does not get
re-litigated at the next cross-unit-identity site.

## The tension this addresses

Two `$unit` scopes must be distinguishable in the linked program (their namespaces and emitted
headers must not collide), yet a cross-unit name must be recomputed identically by the producer (the
unit emitting the declaration) and every consumer (a design element reaching a `$unit` member by
name) with no shared table -- `compilation_unit_model.md` invariant 8 and `north_star.md` invariant
5 forbid any design-wide identifier space two units both depend on.

The anonymous unit exposes no intrinsic naming property except one: the compilation-unit input it
belongs to. slang assigns it an empty name and gives its symbol no source location; only its members
carry a location, and they all share the one source buffer of their input. That input grouping is
not an incidental packaging fact for this unit the way a file is incidental to a module (a module
`M` is `M` in any file): LRM 3.12.1 _defines_ the `$unit` scope boundary _by_ the compilation-unit
input. The input grouping is the anonymous unit's identity.

## The decision

### D1. The anonymous `$unit` scope is a namespace unit named by its compilation-unit input identity

At AST-to-HIR, each slang `CompilationUnitSymbol` that declares namespace-level content is collected
as an anonymous namespace unit, lowered through the same rootless-unit path a package uses. Its
published name is a pure function of the unit's own source-input identity: the resolved path of the
source buffer its declarations live in, folded to a stable digest. One naming function serves every
unit kind -- a package's declared name, a module's specialization name, and the anonymous unit's
input-derived name -- so the producer and every consumer compute the same name from the same slang
unit symbol with no shared table. This holds whether the file set compiles as one unit (a single
`$unit` visible across every file) or per file (each file its own `$unit`); the count of anonymous
units is just N, handled uniformly, with the single-unit case as N = 1.

### D2. The name derives from the input identity, never from a design-wide unit id

There is no design-wide unit identity in Lyra: a unit's cross-boundary handle is its name string,
and cross-unit references carry that name, not an id. The anonymous unit's name is therefore derived
from an intrinsic, table-free property of its own compilation-unit input (its source buffer), the
same way a module's specialization name is derived from its own definition name and bindings.
Deriving from the input keeps the name stable across edits to the scope's body (the property a
module's name has), which is what incremental compilation (`north_star.md` invariant 4) requires.

## Rejected alternatives

- **A single fixed name for every anonymous unit.** Correct only when at most one anonymous unit has
  content. Two files each declaring `$unit`-scope content would emit the same namespace and the same
  header filename, colliding at link time. Rejected: it special-cases N = 1 and breaks at N > 1.

- **A design-wide unit id (a `CompilationUnitSymbol -> id`, or `-> name`, registry the producer and
  every consumer consult).** This is the same design-wide precomputed map
  `cross-unit-class-translation.md` already rejected for classes: it violates
  `compilation_unit_model.md` invariant 8 (units share no identifier space) even when the key is
  slang-side, because it introduces a design-wide shared table where independent per-unit
  compilation was the target, and it forecloses compiling one unit without a global pass that sees
  the others. A cross-unit reference is a name resolved against an interface, never a shared
  position in a global table. Lyra has no unit id for any unit kind for exactly this reason; the
  anonymous unit does not get a special one.

- **A name derived from the unit's position in the design-wide compilation-unit list.** A
  design-wide ordinal two units both depend on -- the forbidden shape named directly in
  `compilation_unit_model.md`. It also renames a unit when another is added, breaking incremental
  stability.

- **A name derived from the scope's content (a digest of its members).** Table-free and intrinsic,
  but it renames the unit on every edit to its body, breaking the incremental stability D2 requires.
  The input identity is stable across body edits; the content is not.

## Consequences

- The anonymous `$unit` scope rides every mechanism a named package already has -- rootless-unit
  lowering, by-name cross-unit value and callable references, one-program-global storage, and the
  two design-wide initialization passes -- with no second scope system and no new IR vocabulary.
- The naming function is the single source of truth for a unit's published name across all unit
  kinds; the producer's collection and every consumer's boundary classifier call it, so they cannot
  disagree.
- A class declared at `$unit` scope is not covered here; like a class declared in a package, it
  rides the class workstream.

## Relation to existing decisions

- `cross-unit-class-translation.md` -- the class-side companion. It rejects the design-wide
  `slang -> name` map for class references; this entry applies the same rejection to the anonymous
  unit's own name and records why the input-derived name is the table-free analogue of a module's
  specialization name.
- `front-end-semantic-boundary.md` -- slang owns resolution, Lyra owns translation. The anonymous
  unit's owner is read from slang's already-resolved scope, never re-derived.
- `specialization-identity.md` -- a module's specialization name is likewise a pure function both
  the producer and the consumer compute from the same bindings with no shared table; the anonymous
  unit's name is the same shape keyed on the compilation-unit input.
- `../architecture/compilation_unit_model.md` -- the term "compilation unit" here is Lyra's (a
  namespace unit); the LRM's `$unit` file-set scope is what this unit models. That doc's Notes warn
  the two concepts are unrelated; this entry is where they meet.

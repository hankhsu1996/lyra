# A unit takes what another holds from what it publishes

## Date

2026-10-27

## Status

Accepted.

## Requirement

Of another unit, a unit takes from the front end only what its own text resolved to: which instance,
which declaration, which block, which index, and the constants and types the language had it fold.
Everything about what that resolved to comes from what the other unit states for others, asked for
by that unit's identity and the item. So each such fact is one read a build can record, and no read
reaches a unit's surroundings through a front-end pointer.

## Facts it rests on

1. The front end has resolved every name in the whole design, and folded every constant the language
   requires, before any unit is lowered. It is not a query system.
2. Every unit's signature is derived before any unit's bodies are lowered, from that unit's own
   declarations alone, and units may name each other in a cycle.
3. An enumeration, an unpacked structure and an unpacked union are identified by their declaration
   (LRM 6.22.1), and a signature names other units' such types often: a package structure holding
   another package's enumeration, a port typed by a package typedef.
4. Specializations of one unit publish one structure and may hold different parameter values.
5. A sequence, property or `let` instance is expanded by the front end inside the referrer: each
   instance binds the declaration's syntax afresh with its actuals (LRM 16.8, F.4: the flattened
   body is what the instance means, and its legality is judged after substitution).
6. A sequence or property declared in a clocking block is clocked by that block's event (LRM 14.13,
   16.16), and another unit reaches such a declaration through a name.

## Decision

**D1. Identity from the front end, content from the declarer.** A unit reading another unit's
content asks that unit's published statement for it, by the unit's or specialization's identity and
the item. What the front end resolved the reader's own text to -- an instance, a declaration, a
block, an index, a structural type value -- is identity and is taken as the front end gives it.

**D2. A declared type is its declaration; its definition is published.** An enumeration, an unpacked
structure and an unpacked union are named by their declaration and own no copy of what it declares.
Each unit holds the definitions of the declared types its types name: its own, worked out from its
declarations, and another unit's, as that unit's signature states it, taken once every signature
exists. A signature names another unit's type by declaration alone, so deriving it reads no other
unit. This is what rustc's `AdtDef` does (fields by `DefId`, their types by query) and what clang's
module reader does (declarations loaded on demand).

**D3. Values belong to the specialization.** Each specialization publishes the value of every value
parameter it declares, apart from its unit's signature, and a unit folding a parameter of another
specialization reads that specialization's published value.

**D4. A clocking block is published structure.** Its clocking event crosses as, per event, the edge,
the callable the declaring scope evaluates the operand in, the callable evaluating its qualifier,
and the members the operand reads -- the shape a view's computed name already has. An instance of a
sequence or property declared in another instance's clocking block is clocked by that.

**D5. A type the front end worked out for the reader's expression is the reader's; a type a
declaration states is the declarer's.** The type of `a.x[3]` is the front end's analysis of the
reader's own text and is taken as given. The type and storage of `x`, and the type of a formal a
call leaves to its default (LRM 13.5.3), are stated by whoever declares `x` or the subroutine, and
are read from what it published. Specializations of one unit publish one structure (fact 4), so a
member whose type differs between two specializations puts them in different units, and a read of
another specialization's content names the specialization it reached.

**D6. What the front end's elaboration consumed is a dependency the published boundary does not
see.** A constant the language required folded inside the reader's own declarations, the binding of
a name, `$typename` of the reader's own operand, and every expansion of a sequence, property or
`let` (fact 5) are the front end's work on the reader's text. They depend on other units, and a
query-based build has to track them where the front end runs or run the front end again; recording
reads at the published boundary does not cover them.

## Survey

rustc's metadata keeps per item kind what a dependent reads (`rustc_metadata/src/rmeta/encoder.rs`:
`should_encode_type`, `should_encode_const`, `should_encode_mir`), and a dependent reaches it only
through queries on a `DefId`. Clang modules write a variable's evaluated value beside its
initializer (`ASTRecordWriter::AddVarDeclInit`) and load declarations lazily (`ASTReader`); a
template is instantiated in the importing translation unit. Both keep an expression's type apart
from a declaration's: rustc's `type_of` answers a declaration by `DefId`, while
`TypeckResults::expr_ty` (`rustc_middle/src/ty/typeck_results.rs`) holds what type checking the body
worked out; clang's `CXXDefaultArgExpr` takes its type from the callee's `ParmVarDecl`
(`clang/include/clang/AST/ExprCXX.h`), which under modules is the declaration the reader loaded. Two
conditions differ: the front end here has already resolved and elaborated the whole design (fact 1),
and units may name each other in a cycle with every signature derived first (fact 2), which is why a
definition is taken after the signature stage and never during it.

## Rejected

- **A reader importing another unit's type in full while it publishes.** The other unit's signature
  may not exist yet, and reading it would order the signature stage (fact 2).
- **Keeping the expanded body of a sequence as a read of another unit's front-end objects, declared
  as an exception.** The expansion is the reader's own elaboration (fact 5); the one read back of
  the declarer it made was a clocking block's event, which D4 publishes.
- **A published template of a sequence or property the reader instantiates.** Its meaning is the
  flattened body, typed and judged after substitution, which is the front end's analysis (fact 5).
- **Parameter values on the unit's signature.** A unit groups specializations that may hold
  different values (fact 4).

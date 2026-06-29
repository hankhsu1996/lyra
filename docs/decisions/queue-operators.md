# Queue operator lowering

## Date

2026-06-18

## Status

Accepted

## Context

A queue (LRM 7.10) is a variable-size unpacked array, so beyond its native methods it also supports
the unpacked-array operator surface: indexing with `$`, the slice `q[a:b]`, unpacked array
concatenation `{...}` (LRM 10.10), aggregate equality, the `q[$+1]` append, and the bounded form
`q[$:N]` (LRM 7.10.5). The question this records is how those operators are represented in MIR. MIR
is a small primitive expression set shaped by the downstream optimizer, not a place to add a node
per SystemVerilog construct; a queue's bounds are runtime values and its element storage is an
unpacked container, so the operators cannot reuse the packed-array operator lowering verbatim.

## Decisions

1. **A queue access operator is a built-in method call, not a select node.** Indexed and slice
   access on a queue have no native C++ expression -- each realizes as a method call on the runtime
   queue value. They therefore lower to the existing call primitive: a call whose callee is a
   built-in method identifier (the same first-class home as `size` / `push_back`), with the receiver
   as the first argument and the index or bounds as the remaining arguments. The element-access
   entries carry no SystemVerilog syntax -- they are compiler-internal -- but they sit at the same
   level as the SV-callable queue methods because both are function calls. The backend renders
   through the one generic built-in-method-call rule; no queue-specific access node or render path
   exists. A dedicated MIR node was rejected: it would duplicate an existing primitive (a select, or
   a call) and freeze one receiver type's argument shape -- a packed part-select's
   `(offset, compile-time width)` -- into the IR, which is exactly what does not fit a queue's
   runtime `(lo, hi)` bounds. The LRM 7.10.1 clamping and the append-vs-discard rules live in the
   runtime method, the same way packed bit-extraction lives in the packed array's slice method; each
   backend realizes the method per receiver type.

2. **Read and write are different methods, chosen at lowering.** A read takes the default-on-miss
   element accessor; a write (and the `q[$+1]` append) takes the append-aware write-side accessor.
   The distinction is resolved where it is known -- the assignment lowering marks its left-hand side
   as a write target, and the queue element-select picks the write-side method under that flag and
   the read-side otherwise. Keeping the choice at lowering leaves the backend render purely
   mechanical: it emits the method the node names and never re-decides read versus write.

3. **Packed and string concatenation are the value-build primitive; a queue concatenation is a
   runtime builder call.** A packed or string `{...}` is the `ConcatExpr` primitive realized per
   result type -- a bit join or a string join -- whose result shape is carried entirely by the
   result type, so the operands are joined directly. A queue `{...}` is not the same primitive:
   because the queue value is an ordinary value that carries its own declared representation, a
   queue concatenation must produce a value already holding its element shape and LRM 7.10.5 bound,
   and an empty `{}` cannot recover an element shape from its parts. It therefore lowers to a call
   against the queue builder whose element-default and bound arguments are produced by lowering --
   the same channel every other shaped container construction uses -- ahead of the concatenation
   parts; the backend derives element-vs-spread from each part's type (an unpacked-container part is
   spliced, anything else is one element), so the call needs no per-part tag. Carrying the element
   shape as a payload field on the shared concatenation node was rejected: the element default is
   ordinary construction data the result type already implies, supplied through the call's
   arguments, not a field only one result type reads.

4. **A queue's element shape and bound are declared-type properties the store boundary conforms to,
   not a shape-preserving assignment.** A queue's element shape (its out-of-bounds shield slot, see
   `runtime-shape-and-default-value.md`) and its LRM 7.10.5 bound are fixed by its declared type,
   while its length and elements are value state. The queue value is an ordinary value -- copy,
   move, and assignment all replace it whole -- and the declared element shape and bound are
   established at construction and re-applied at every semantic store, not preserved by the value's
   own assignment operator. So a concatenation value carries its element default and bound (an empty
   `{}` therefore matches its declared type rather than relying on the destination to keep one), and
   assigning a differently-bounded source conforms it to the destination's bound at the store
   boundary. This keeps the "the cell owns the declared type, the store converts to it" discipline
   uniform with every other value family, rather than letting a queue's assignment operator silently
   keep the destination's shape.

5. **`$` resolves to `size - 1` at the indexing site.** Slang models `$` as an unbounded literal
   with no value of its own; it means "the last index" of the queue being indexed. The AST-to-HIR
   walk threads that queue base through the walk frame, and `$` lowers to `size(base) - 1` against
   it -- so `$`, `$-1`, and the `q[1:$]` / `q[0:$-1]` pop idioms (LRM 7.10.4) all become ordinary
   index or bound arguments to the access method calls above.

## Consequences

- Queue access operators are built-in method calls today; the packed part-select, packed struct
  field access, unpacked slice, and fixed/dynamic element access still use the dedicated select
  nodes. This cut is the role model for migrating those to the same call form when that work is
  taken up; until then the two forms coexist deliberately.
- All three queue slice forms (`q[a:b]`, `q[base+:w]`, `q[base-:w]`) are supported: they reduce to a
  low / high pair fed to the same `Slice` method call, so the indexed forms cost no extra machinery.
- An unpacked concatenation whose result is a dynamic or fixed array rather than a queue is rejected
  with a diagnostic; it extends the same builder mechanism when a consumer needs it.
- `$` is supported in procedural contexts, where every documented queue idiom lives. A `$` in a
  structural context (a continuous assignment) surfaces as unsupported rather than silently
  mis-lowering.

## Cross-references

- LRM 7.10 (queues), 7.10.1 (operators / invalid index / `q[$+1]`), 7.10.4 (push / pop / insert via
  assignment and concatenation), 7.10.5 (bounded queues), 10.10 (unpacked array concatenation),
  11.2.2 / 11.4.5 (aggregate equality).
- `runtime-shape-and-default-value.md` -- the element-shape shield slot a queue carries.
- `builtin-call-identity.md` -- the built-in method-call mechanism these operators reuse rather than
  adding select nodes beside.

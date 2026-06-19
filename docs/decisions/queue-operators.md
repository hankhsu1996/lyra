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

1. **A queue access operator is a built-in method call, not a select node.** `q[i]` and `q[a:b]`
   have no native C++ expression -- each realizes as `receiver.Method(args)` on the runtime
   `Queue<T>` (`ElementAt`, `WriteRef`, `Slice`). So they lower to the existing call primitive: a
   `CallExpr` whose callee is a `QueueMethodKind` (the same first-class home as `size` /
   `push_back`), with the receiver as `arguments[0]` and the index or bounds as the remaining
   arguments. The element-access methods carry no SystemVerilog syntax -- they are compiler-internal
   -- but they sit at the same level as the SV-callable queue methods because both are function
   calls. The backend render is the one generic built-in-method-call render
   (`(arg0).Name(arg1, ...)`); no queue-specific access node or render path exists. A dedicated MIR
   node was rejected: it would duplicate an existing primitive (a select, or a call) and freeze one
   receiver type's argument shape -- packed part-select's `(offset, compile-time width)` -- into the
   IR, which is exactly what does not fit a queue's runtime `(lo, hi)` bounds. The LRM 7.10.1
   clamping and the append-vs-discard rules live in the runtime method, the same way packed
   bit-extraction lives in `PackedArray::Slice`; each backend realizes the method per receiver type.

2. **Read and write are different methods, chosen at lowering.** A read takes the default-on-miss
   `ElementAt`; a write (and the `q[$+1]` append) takes the append-aware `WriteRef`. The distinction
   is resolved where it is known -- the assignment lowering marks its left-hand side as a write
   target, and the queue element-select lowers to `WriteRef` under that flag and `ElementAt`
   otherwise. Keeping the choice at lowering leaves the backend render purely mechanical: it emits
   the method the node names and never re-decides read versus write.

3. **Unpacked concatenation reuses the concatenation value-build primitive.** Packed, string, and
   queue `{...}` are the same `ConcatExpr` primitive, realized per result type -- a bit join, a
   string join, or the runtime queue builder. The backend derives element-vs-spread from each
   operand's type (an unpacked-container operand is spliced, anything else is one element), so the
   node needs no per-operand tag. A second concatenation node was rejected for the same reason as a
   dedicated access node: concatenation is one primitive, and the result type already states which
   realization applies.

4. **A queue's element shape and bound are declared-type properties, preserved across assignment.**
   `value-assignment-and-moved-from.md` established that a shape-bearing value keeps the
   destination's declared type across assignment and copies only the value in. A queue's element
   shape (its out-of-bounds shield slot, see `runtime-shape-and-default-value.md`) and its LRM
   7.10.5 bound are that kind of property: `q = rhs` keeps `q`'s shape and bound and copies the
   elements, rather than adopting the source's. Because a declared queue is default-constructed and
   then initialized by assignment, the value type adopts the source's shape (and bound) on that
   first store and preserves thereafter -- so assigning the empty `{}` or an unbounded concatenation
   result keeps the variable's shape, and the concatenation value therefore carries no element
   default. This is what removed an earlier element-default field that had been compensating for an
   assignment that wrongly adopted the source's shape.

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
  with a diagnostic; it extends the same `ConcatExpr` mechanism when a consumer needs it.
- `$` is supported in procedural contexts, where every documented queue idiom lives. A `$` in a
  structural context (a continuous assignment) surfaces as unsupported rather than silently
  mis-lowering.

## Cross-references

- LRM 7.10 (queues), 7.10.1 (operators / invalid index / `q[$+1]`), 7.10.4 (push / pop / insert via
  assignment and concatenation), 7.10.5 (bounded queues), 10.10 (unpacked array concatenation),
  11.2.2 / 11.4.5 (aggregate equality).
- `value-assignment-and-moved-from.md` -- the type-vs-value distinction the queue's shape and bound
  follow.
- `runtime-shape-and-default-value.md` -- the element-shape shield slot a queue carries.
- `array-method-dispatch.md` -- the built-in method-call mechanism these operators reuse rather than
  adding select nodes beside.

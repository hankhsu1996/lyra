# A product is laid out from its components, and crosses to the runtime erased

Date: 2026-09-25 Status: accepted. Supersedes
[jit-aggregate-realization](jit-aggregate-realization.md) for products; unions and containers keep
the realization that entry chose.

## Context

A callable's answer is a product: its result and then every `output` and `inout` argument, which the
return passes back to the caller (LRM 13.5). A function with no outputs answers a one-component
product. The C++ backend renders it as a template record that clang lays out inline and returns
through `sret`, so an answer costs what moving its values costs.

The execution backend realized every aggregate as one erased runtime object. Returning built a boxed
copy of each component, collected the boxes into a heap vector, and ended them; the caller reached
each component through a runtime call. On call-chain (`--release`, callgrind) that was about a fifth
of the run: `tuple_make` 9.7%, the destroy 5.0%, `extract` 2.6%, the boxing 2.1%, the move 1.9%.

That realization was chosen when every execution-backend value was an opaque handle to a runtime
allocation. Since [a-value-lives-in-its-makers-frame](a-value-lives-in-its-makers-frame.md), every
value is an object of fixed size in its maker's frame, and the product was the one domain whose
object was still a header over heap-held boxes.

## Decision

**A product is laid out by the generated code itself, as a record of its components' objects, the
way a C compiler lays out a struct: each component at the first offset its alignment allows after
the one before, the whole rounded to the widest alignment among them.** Its layout is derived at
code generation from its component types; nothing above the backend states offsets.

- **Building, copying, moving and ending a product are its components', one by one**, as clang
  synthesizes a record's special members from its fields'. Reading a component is an address
  computation and a copy of that component; replacing one is a copy of the product and of the new
  component into its slot.
- **An answer is built directly in the storage the caller gives**, so a return moves nothing and a
  caller reading a component reads it where it lies.
- **Reading a product's component, or a union's member, is one extraction in LIR**, whichever MIR
  call reached it, so the backend has one place that realizes it.
- **What the runtime holds or computes over keeps a product erased**, because the runtime is
  compiled once and each of its value families is written over the domain's one type. A product
  handed to it -- stored in a variable's cell, a net or a driver, compared, streamed, put into a
  container or a union -- is rebuilt in that form for the call, and one it answers with is laid out
  from it. A body only the runtime calls, a closure's, takes and answers products in that form too.

## Rejected

- **Laying out only the answer and keeping every other product erased.** Nothing separates an answer
  from a struct value except where it came from, so two realizations of one type would each need
  every operation, and every consumer would ask which it holds.
- **Keeping the erased product and making it cheaper.** Its cost is the boxing and the heap vector,
  which exist because the runtime cannot hold components of a type it was not compiled for. The
  record removes them where the runtime is not involved; nothing short of it does.
- **Moving components out of a product the runtime answers with, instead of copying them.** Measured
  on a struct-variable loop, it was slower (567.6 M instructions against 538.0 M): a small packed
  value copies as cheaply as it moves, and taking one leaves a moved-from object behind to destroy.

## Consequences

- An answer no longer touches the runtime. Call-chain went from 1,044.7 M to 895.5 M instructions
  (`--release`, callgrind), and Ibex ran with an identical trace in 52.5 s against 54.5 s, one run
  each.
- A struct variable pays the conversion on every access, because every declared variable's storage
  is runtime-held. A loop reading and writing one went from 427.1 M to 538.0 M instructions. The
  conversion disappears when the runtime holds a product laid out, which needs its value families to
  reach a product's components through a description the unit states rather than through a type they
  were compiled for -- the answer Swift's runtime gives with type metadata.
- A handle to a runtime cell is typed as a pointer to the value it holds, never as that value: once
  a value's type decides how it is laid out, a handle typed as the value would be laid out as one.

## Cross-references

- [a-value-lives-in-its-makers-frame](a-value-lives-in-its-makers-frame.md) -- a value is an object
  in its maker's frame; this extends "sized by its domain" to "sized by its components" for a
  product.
- [jit-aggregate-realization](jit-aggregate-realization.md) -- the erased realization, which unions
  and containers keep.
- [unpacked-struct-representation](unpacked-struct-representation.md) -- a struct is a value
  product, which is what makes it one realization with a callable's answer.
- `../architecture/lir.md` -- physical layout is derived below LIR.

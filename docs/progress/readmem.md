# Memory load ($readmemh / $readmemb)

Tracks LRM 21.4 memory-array load from a text file. `$readmemh` reads hexadecimal words, `$readmemb`
binary; both fill an unpacked memory from a file of whitespace-separated words with optional
`@address` directives, comments, and a supplied address range. This is the running inventory of what
is supported and what remains.

## Done

- [x] One-dimensional unpacked memory of packed integral elements, for both radixes. File format:
      whitespace- and newline-separated words, `@hex` address directives, `//` line and `/* */`
      block comments, per-digit `x` / `z` / `?`, and `_` digit separators. Elements of any width
      (including wider than 64 bits) and any declared range (non-zero-based or descending) load by
      declared index.
- [x] Addressing (LRM 21.4). With no range the load fills from the lowest declared index upward; a
      `start`-only call fills upward from `start`; a `start` / `finish` call fills from `start`
      toward `finish`, descending when `start > finish`. An `@address` in the file repositions the
      write cursor. Words the file never addresses keep their prior value. A 2-state element
      collapses `x` / `z` to 0 (LRM 21.4.2).
- [x] Statement-position use in any procedural context (an `initial` / `always` / `final` block, a
      task, or a function). The task carries an output argument and has no value, so the frontend
      rejects it in a continuous assignment or any other expression position -- that is the language
      rule, not a gap.
- [x] Error / warning behaviour. A missing file, an `@address` outside the active range, and a
      malformed word or address each stop the load and report an error; the simulation continues (a
      failed load is not fatal). A `start` / `finish` range whose word count does not match the
      file, with no in-file `@address`, reports a warning. A missing file is reported as an error
      (LRM 21.4 does not fix the severity; treating a silently-uninitialized memory as an error is
      the deliberate choice here).

## Not yet supported

Each form below is rejected today with a clear diagnostic (never silently mis-loaded), and stands
open until the corresponding behaviour lands.

- [ ] Multidimensional unpacked memory (LRM 21.4.3). The file is organized row-major with the lowest
      dimension varying fastest; a memory whose element is itself an unpacked array is rejected
      today.
- [ ] Associative-array memory (LRM 21.4.1). Loading an address creates the entry, and indices must
      be integral (an enumerated index maps by ordinal value). Rejected today.
- [ ] Dynamic-array and queue memory (LRM 21.4.1). The container's current size is fixed and is not
      resized by the load. Rejected today.
- [ ] A packed struct / union element (LRM 21.4.1: each packed element is loaded as its vector
      equivalent). Only bit-vector elements load today.

## Related

- `$writememh` / `$writememb` (LRM 21.5), the dump counterpart, is not modelled.
- The Ibex bring-up consumer that first drove this feature is tracked in `ibex.md`.

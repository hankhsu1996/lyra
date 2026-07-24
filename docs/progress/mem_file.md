# Memory file load and dump ($readmem / $writemem)

Tracks LRM 21.4 memory-array load from a text file and LRM 21.5 dump back to one. `$readmemh` /
`$readmemb` read hexadecimal / binary words into an unpacked memory; `$writememh` / `$writememb`
write a memory out in a form the matching `$readmem` reads back. Both sides share one file-format
contract (whitespace-separated words, optional `@address` directives, comments, a supplied address
range). This is the running inventory of what is supported and what remains.

## Done

- [x] One-dimensional unpacked memory of packed elements, for both radixes and both directions. A
      packed element is any that reduces to a single vector (LRM 21.4.1 / 21.5.1): a bit vector, or
      a packed struct / union / enum, each loaded or dumped as its vector equivalent. Elements of
      any width (including wider than 64 bits) and any declared range (non-zero-based or descending)
      load and dump by declared index.
- [x] Load file format (LRM 21.4). Whitespace- and newline-separated radix words, `@hex` address
      directives, `//` line and `/* */` block comments, per-digit `x` / `z` / `?`, and `_` digit
      separators. A 2-state element collapses `x` / `z` to 0 (LRM 21.4.2).
- [x] Dump file format (LRM 21.5). One radix word per line at full element width, with per-digit `x`
      / `z` preserved so a 4-state value round-trips through `$writemem` and back. An existing file
      is overwritten (no append). No `@address` is written for an unpacked array (LRM 21.5.3).
- [x] Addressing (LRM 21.4 / 21.5), shared by load and dump. With no range the operation spans the
      whole declared memory from the lowest index upward; a `start`-only call spans upward from
      `start`; a `start` / `finish` call spans from `start` toward `finish`, descending when
      `start > finish`. On load, an `@address` in the file repositions the write cursor and words
      the file never addresses keep their prior value.
- [x] Statement-position use in any procedural context (an `initial` / `always` / `final` block, a
      task, or a function). Both tasks carry a memory argument and have no value, so the frontend
      rejects them in a continuous assignment or any other expression position -- that is the
      language rule, not a gap.
- [x] Load error / warning behaviour. A missing file, an `@address` outside the active range, and a
      malformed word or address each stop the load and report an error; the simulation continues (a
      failed load is not fatal). A `start` / `finish` range whose word count does not match the
      file, with no in-file `@address`, reports a warning. A missing file is reported as an error
      (LRM 21.4 does not fix the severity; treating a silently-uninitialized memory as an error is
      the deliberate choice here).

## Not yet supported

Each form below is rejected today with a clear diagnostic (never silently mis-loaded or dumped), and
stands open until the corresponding behaviour lands. All three are the same **container-shape**
axis: the memory is something other than a one-dimensional fixed-size unpacked array, and the axis
cuts across load and dump identically (LRM 21.4.3 names both `$readmem` and `$writemem`), so it
lands as one cut covering both directions.

- [ ] Multidimensional unpacked memory (LRM 21.4.3). The file is organized row-major with the lowest
      dimension varying fastest; an `@address` addresses the highest dimension's words, and an
      incompletely filled highest-dimension word leaves its remaining subwords unchanged. A memory
      whose element is itself an unpacked array is rejected today.
- [ ] Associative-array memory (LRM 21.4.1). Loading an address creates the entry, and indices must
      be integral (an enumerated index maps by ordinal value); a dump writes an `@index` per entry
      (LRM 21.5.3). Rejected today.
- [ ] Dynamic-array and queue memory (LRM 21.4.1). The container's current size is fixed and is not
      resized by the load. Rejected today.

## Related

- The Ibex bring-up consumer that first drove the load path is tracked in `ibex.md`; it needs only
  the one-dimensional form.

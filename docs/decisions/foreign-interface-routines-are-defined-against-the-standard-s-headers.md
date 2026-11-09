# The DPI and VPI routines are defined against the standard's own headers

Date: 2026-11-07 Status: accepted. Revised 2026-11-09: the DPI routines too.

## Why this decision matters

Foreign code is C written against the standard's headers -- `svdpi.h` for DPI (LRM Annex I), and
`vpi_user.h` and `sv_vpi_user.h` for VPI (Annexes K and M) -- and linked into the program that runs
the design (LRM 35.4, 36.2). The library it calls is the tool's, and Lyra provides it routine by
routine, so the questions this settles come back with every routine added: what the definitions are
checked against, what a routine not provided yet does, where the routines whose prototype is
variadic live, and when a control asked for from C is carried out.

## The decisions

```text
D1. The headers are the standard's listings, shipped beside every program, and the runtime
    defines each routine against them. They were taken from Annexes I, K, L and M and corrected
    only where the typesetting broke a line or a character, or the listing contradicts its own
    clause; each correction is recorded in the headers' own README. Defining against them means
    the compiler holds every definition to the prototype the application was compiled against,
    which no copy of the declarations could promise.

D2. A routine is defined only once what it acts on exists in the runtime. One not provided is not
    defined at all, so an application calling it fails to link and the linker names the routine.
    A definition that compiled and then answered "not supported" would let the program build and
    lose the answer at run time, and the routine's own error report (LRM 38.2) is a status the
    application may never read.

D3. Save and restart (`vpi_get_data`, `vpi_put_data`) are never defined. LRM 38.36.3 lets a tool
    lack save and restart, and Lyra has no such feature for those routines to serve. `vpi_control`
    asked for `vpiReset` or `vpiSetInteractiveScope` answers 0 and reports an error, for the same
    reason: there is no `$reset` and no interactive mode.

D4. The routines whose prototype is variadic -- the printing routines and `vpi_control` -- are
    defined in C, and hand what they have read to the runtime as ordinary arguments. Reading a
    variable argument list is C's own facility, and every compiler builds a C translation unit for
    it as the application's own code is built; the runtime's C++ never sees a `va_list`.

D5. `vpi_control(vpiFinish)` and `vpi_control(vpiStop)` are carried out upon return of the
    application routine (LRM 38.4), which is the context import that called them, innermost where
    imports nest through an export. The request is recorded on that call and done where the call
    returns, as the design's own `$finish` would be done where the call is written; nothing is
    raised among the foreign frames, and the execution leaves at the import's boundary
    (`a-departure-stops-at-a-foreign-frame.md` D5). A request where no context import's call is
    running -- from a plain import called from SystemVerilog, or with no import at all -- has no
    call to be done upon, and answers 0 with an error. A plain import called from inside a context
    import's call is uninstrumented (LRM 35.5.3), so its request is recorded on that call and done
    where it returns; LRM 35.5 leaves what VPI does from a non-context import unpredictable.

D6. Every version-bound name Annex L gives a routine Lyra defines is defined beside the routine,
    so an application compiled for any compatibility mode (LRM 36.12.2.1) links unmodified. Where
    the routine's behaviour does not differ between the versions Lyra provides it for, each bound
    name does what the unbound one does.

D7. The runtime holds the canonical representation of a packed value (Annex H.10.1.2) in the
    types `svdpi.h` declares, and in no copy of them, so the header ships among the runtime's own
    headers as well as beside a program's foreign sources. The two are one file, the one the standard
    defines for every tool (Annex H.10.1).

D8. `svGetCallerInfo` answers where the design wrote the context import whose call is running,
    which the call chain keeps for every such call. LRM Annex H.9.3 leaves to the tool whether it
    has that place; Lyra has it wherever the design wrote the call, and answers false with nothing
    written where no context import's call is running or the call has no place in the source.
    `svDpiVersion` answers "1800-2005", the string Annex H.10.1.3 gives the VPI-based canonical
    representation that "DPI-C" passes packed values in. The SV3.1a routines Annex H.14 leaves
    out of what an IEEE 1800 tool owes are not defined (D2).
```

## Rejected alternatives

- **Stub every routine the headers declare, answering an error.** It is what lets an application
  link against a partial library. It moves the failure from the link, where the routine is named, to
  a run, where only a caller that checks `vpi_chk_error` learns of it -- and the routines most often
  called unchecked are the ones whose answers matter.
- **Define the variadic routines in C++ with `<cstdarg>`.** It works, and it puts the one C facility
  C++ treats as unsafe into the runtime's own code, where nothing else needs it.
- **Carry out `vpi_control` where it is called.** The call is inside foreign code, whose frames no
  departure crosses, and LRM 38.4 already says the task waits for the routine's return.
- **Keep the runtime's own copy of the canonical types, so the design's C++ never reads `svdpi.h`.**
  The copy was there so an emitted program would compile without the standard header, which every
  program now ships with. What it still keeps out is every name the header declares at file scope --
  its macros (`SV_MASK`), its types (`svBit`) and its routines (`svSize`) -- which collide with a
  package, module, class or task of the same name only because the C++ backend writes those by their
  bare source names, the defect that already breaks a package named `EOF`.

## Cross-references

- [a-departure-stops-at-a-foreign-frame](a-departure-stops-at-a-foreign-frame.md) -- the boundary a
  requested control is carried out at, and why nothing crosses a foreign frame.
- [foreign-code-is-linked-not-loaded](foreign-code-is-linked-not-loaded.md) -- why an undefined
  routine is a link failure of the program, named by the linker.
- [dpi-foreign-boundary](dpi-foreign-boundary.md) -- the context import a VPI call is made from.
- IEEE 1800-2023: 35.4 (foreign code), 36.2 (the VPI library), 36.12.2.1 (compatibility modes), 38.2
  (`vpi_chk_error`), 38.4 (`vpi_control`), 38.36.3 (what a tool may lack), Annex H.9.3 (context
  utilities), H.10.1 (the header), H.11.5 (canonical utilities), H.14 (SV3.1a compatibility),
  Annexes I, K, L and M.

# Packages

Tracks the SystemVerilog `package` (LRM 26) and the `$unit` compilation-unit scope (LRM 3.12.1).

A package is a namespace: a compilation unit that owns a set of named declarations -- parameters and
localparams, typedefs and enums, functions and tasks, and variables -- with no instances, no
receiver, and no construction. Its C++ peer is a `namespace`, where a module's peer is a `class`. A
package is referenced by name from other units (`pkg::item`, or a name brought into scope by
`import`). The reference behaves like the equivalent C++ name: a compile-time constant or a type
folds or interns at elaboration in the referencing unit (as a `constexpr` value or a `using` alias
does), so it manifests no cross-unit entity; a function or a variable is a real cross-unit symbol
the referencing unit reaches by name, and it is these that require the package to be a first-class
unit with a body to emit. `$unit`-scope declarations -- those lying outside any design element --
are the same concept without a name: an anonymous package.

Done when the LRM 26 surface reproduces on both backends: a package declaration and each item kind
it may hold; the reference forms (explicit `pkg::item` scope resolution, explicit
`import pkg::item`, and wildcard `import pkg::*`); and `$unit`-scope declarations. Classes declared
inside a package ride on the class workstream and are out of scope here.

## Actionable

PK1 (compile-time contents), PK2 (the package as a first-class unit, carrying functions), and PK3
(package variables) are done on the C++ backend: a package variable is declared with an initializer,
initialized once at time zero before the top modules, and read and written from another unit by
name, including from a package function or task and including waking a process on its change. A
package task enabled from another unit suspends its caller until it completes. PK4 (the import forms
and the LRM 26.3 name-search rules) is the next actionable step.

## Sub-Steps

The `PK*` IDs are stable references. They do not impose a total order; the inline notes record the
real dependencies. PK2 establishes the package as a lyra compilation unit, which PK3 and the
callable-bearing part of PK4 reuse; PK1 is independent of it.

- [x] PK1 -- Package compile-time contents referenced from another unit: a localparam / parameter, a
      typedef, and an enum constant, reached by explicit `pkg::item` or brought into scope by
      `import`. These fold to a value or intern as a type at elaboration in the referencing unit,
      the way a C++ `constexpr` or `using` does, so they manifest no cross-unit entity and need no
      package unit. The item locks this behavior with coverage across the reference forms and the
      item kinds.
- [x] PK2 -- A package becomes a first-class lyra compilation unit, and package functions and tasks
      are called from other units (LRM 26.3). A `pkg::func()` is the first cross-unit subroutine
      call: it targets a receiver-less (static) callable owned by another unit's namespace, resolved
      and linked by name, exactly as an instantiated child names its unit by name. Establishes the
      package-as-unit collection and lowering that PK3 reuses. The C++ backend emits a package as a
      namespace of free functions, and a package function whose signature or body names a
      package-declared enum or typedef resolves it against the namespace's own type declarations,
      emitted into the namespace. Input arguments and the return value work, and one package
      function calls a sibling in the same package. Remaining increments: a package function with an
      output / inout / ref argument; and a DPI-C import declared inside a package (LRM 35.4), which
      is rejected with a diagnostic rather than mis-emitted.
- [x] PK3 -- Package variables. A package variable is static storage owned by the namespace -- one
      program-global cell, shared, not a member of any instance -- read and written from other units
      by name. It is the same type-associated storage a class static property uses, not a
      package-specific singleton object. The variable is reached by name uniformly (a referrer in
      another unit and the package's own function agree on the by-name form, since neither has a
      receiver), and a process may wake on its change. A package function or task both reads and
      writes the variable, and a package task enabled from another unit suspends its caller until it
      completes. Initialization (LRM 10.5) runs at time zero, driven by the design root before the
      top modules initialize, in two design-wide passes: every package's cells are installed with
      their declared type and default first, then every package's value initializers run, so an
      initializer that reads another package's variable always reaches installed storage. The
      relative order of initializers is unspecified by the LRM; the design root picks a stable,
      best-effort order (a dependency before its dependent where a direct read makes it known) as a
      quality-of-implementation choice, and an unknown or cyclic dependency degrades to reading a
      default, never a crash. Remaining increments: an initializer read reached through a called
      function does not yet contribute to the preferred order; and two packages that reference each
      other's symbols still produce circular emitted headers (a header-only emission limit, shared
      with cross-package subroutine calls, not specific to variables).
- [ ] PK4 -- The `import` forms for the remaining item kinds and the LRM 26.3 name-search and
      shadowing rules. An import brings a name into a scope's lookup; it does not create a new kind
      of reference, so a name resolved through an import lowers identically to the explicit
      `pkg::item` form. Import of compile-time contents is covered by PK1; this item covers import
      of the callable and variable entities once PK2 and PK3 exist, plus the search-order and
      shadowing rules.
- [ ] PK5 -- `$unit`-scope declarations (LRM 3.12.1): declarations outside any design element,
      visible across the compilation-unit file set. Modeled as an implicit anonymous package so they
      ride the same unit, by-name reference, storage, and callable mechanisms as a named package; no
      second scope system.

## Blocked

Nothing blocked. PK4 is actionable now.

## Out of Scope

- Classes declared inside a package (LRM 26.2). The class is an ordinary type-level member of the
  namespace; its own semantics ride the class workstream, not this one.
- Package / `$unit`-scoped DPI export wrappers. The export makes a package-scoped subroutine
  callable from foreign C; it rides on PK1-PK2 for the package unit and the callable, and is tracked
  as the next C++-backend deliverable in `dpi.md`.

## Cross-references

- LRM anchors: 26.1 (overview), 26.2 (package declarations), 26.3 (referencing package contents;
  `import`; search order), 26.4 (search order precedence); 3.12.1 (`$unit` compilation-unit scope).
- Architecture contracts the work must satisfy: `../architecture/compilation_unit_model.md` (a
  package is a compilation unit; its interface is its set of exported declarations, generalizing the
  module-centric "parameters and ports" wording), `../architecture/object_model.md` (a namespace is
  the type-associated member scope, here standing alone as a unit root rather than attached to an
  object type), `../architecture/reference_resolution.md` and `../architecture/emission_model.md`
  (by-name cross-unit resolution and per-unit emission), `../architecture/north_star.md` (the
  cross-unit dependency is explicitly declared, per the incremental / parallel constraints).
- Unblocks: `ibex.md` (the Ibex design leans on `ibex_pkg`), `dpi.md` D4 (package / `$unit`-scoped
  export hangs off the package unit and callable established here).

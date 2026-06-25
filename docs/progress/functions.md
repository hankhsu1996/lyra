# Subroutines (tasks and functions)

Tracks user-defined `function` and `task` declarations and the calls that invoke them (LRM 13).
Functions and tasks are the same construct -- a _subroutine_ -- differing only in whether the body
may suspend and whether it yields a value, so they are tracked together.

The semantic surface that scopes this workstream is LRM 13, not any single reference corpus. The
archived `functions/*` cases are drawn on for inspiration where they fit, but coverage is judged
against the LRM concept space below and the tests authored alongside each cut.

Done when the LRM 13 subroutine surface reproduces: value-returning and void functions, tasks
(including suspending tasks), all five argument directions, by-value and by-reference passing,
default and named arguments, return-by-name, automatic lifetime / recursion, local default
initialization, and subroutine boundaries for every data-type family Lyra otherwise supports
(integral, packed, string, container, chandle, struct / union). Multi-instance and the
elaboration-time / foreign / class-method surfaces are out of scope here; see [Blocked](#blocked)
and [Out of Scope](#out-of-scope).

## Actionable

F1 is the entry point: the value-returning function call and return path. Most other items build on
it -- the dependencies are stated inline. Tasks that suspend (T2) wait on the timing machinery owned
by `processes.md`.

## Sub-Steps

The `F*` / `T*` IDs are stable references. They do **not** impose a total order, but F1 precedes
everything and the inline "rides on" notes record the real dependencies.

### Functions: call and return

- [x] F1 -- Value-returning and void functions with `input` by-value arguments (LRM 13.4, 13.5.1).
      `return expr`; nonvoid call as an expression operand and as a nested operand; void call as a
      statement; `void'(f())` to discard a nonvoid result. Sequential body statements, early /
      conditional return, and function-local variable declarations.
- [x] F2 -- Return-by-name via the implicit variable that shares the function's name (LRM 13.4.1).
      `return` overrides a prior name-assignment; an empty body returns the implicit variable's
      current value; the implicit variable is default-initialized so a read-before-write sees a
      defined value.
- [x] F11 -- Struct / union return values (LRM 13.4.1). A hierarchical name beginning with the
      function name denotes a member of the return value inside the body; member access on the
      returned aggregate at the call site.

### Lifetime and recursion

- [x] F3 -- `automatic` lifetime and recursion (LRM 13.3.1, 13.3.2, 13.4.2). A subroutine local
      follows its resolved lifetime: an automatic local is reinitialized on entry and lives only for
      the activation, while a static local (the module / package default) has one per-instance copy
      that retains its value between calls and is default-initialized once. Per-variable `automatic`
      / `static` overrides are honored, so both lifetimes coexist in one body, and static locals
      that share a name across sibling or nested blocks are kept distinct. Calls resolve regardless
      of source order, so direct recursion, mutual recursion, and forward references to a
      later-defined subroutine all work for automatic-lifetime subroutines. Not yet: separate static
      storage across multiple instances of a module (rides on module hierarchy); and static-lifetime
      formal arguments and the implicit result variable, which still behave as automatic.

### Arguments

- [x] F4 -- `output` and `inout` arguments (LRM 13.5). `output` copies out at return, `inout` copies
      in at call and out at return; multiple outputs; mixed directions. Default direction is `input`
      and subsequent formals inherit the previous direction and data type. Copy-in-copy-out is
      observable: a body that also reaches an actual passed as `output` sees the LRM-defined value,
      and the copy-out goes through the actual's own write path. Supported where the call stands in
      statement position (a bare call or the whole right side of a blocking assignment); a call with
      `output` / `inout` actuals nested inside a larger expression is not yet supported.
- [x] F6 -- Default argument values (LRM 13.5.3), binding by name (LRM 13.5.4), and the optional
      empty argument list for no-argument / all-defaulted subroutines (LRM 13.5.5). Default
      expressions evaluate in the declaration scope at each defaulting call.
- [x] F10 -- `ref` and `const ref` arguments (LRM 13.5.2). A reference aliases the caller's
      variable; reads and writes go through that variable's own access path, so a write is visible
      immediately and, when the variable is observable, wakes its event subscribers (LRM 4.3 update
      event). `const ref` is read-only. Legal only for automatic-lifetime subroutines and for a
      variable or unpacked-array element; mixes with `output` / `inout` in one call. Not yet:
      compound, partial, or increment / decrement writes through a reference formal (whole-variable
      reads and writes are supported); class properties and unpacked-struct members (those data
      types); and the outdated-reference rules for resized / deleted container elements, which ride
      on F8.

### Local storage

- [x] F5 -- Function-local default initialization (LRM 13.3.2 default-init, 6.8). 2-state locals
      default to 0, 4-state locals to X (four-state suite), unpacked arrays element-wise, and string
      locals to empty; the same default applies to every other data-type family Lyra supports.
      Container and unpacked-union locals follow once those data types exist (containers ride on
      F8).

### Data types across the boundary

- [x] F7 -- `string` arguments and return values. Managed lifecycle across the call boundary;
      concatenated and computed string returns.
- [ ] F8 -- Container arguments and return values: dynamic array, queue, associative array. Managed
      ownership and lifecycle across the boundary, including `output` / `inout` containers and the
      outdated-reference rules of LRM 13.5.2. The container value types now exist (`aggregate.md`),
      so the subroutine ABI carries them by value through the same path as any other type; what
      remains is the cross-boundary lifecycle and the LRM 13.5.2 outdated-reference rules.
- [ ] F9 -- `chandle` arguments and return values (LRM 6.14). Identity round-trip and `null`
      handling through the subroutine boundary.

### Tasks

- [x] T1 -- Tasks without timing controls (LRM 13.3). Enabled as a statement; results returned
      through `output` / `inout` arguments; `return` exits before `endtask`; an empty body is a
      no-op. A task may enable other tasks and functions (LRM 13.2); a function enabling a task is
      rejected. Behaves like a void function. Task locals follow the static / automatic lifetime
      distinction established by F3.
- [x] T2 -- Tasks containing time-controlling statements (LRM 13.3). `#`, `@(...)`, and `wait`
      inside a task suspend the enabling process and resume it later, so a task enable can span
      multiple time steps; control returns to the enabler only after the task completes. A task may
      enable another time-consuming task and the suspension propagates through the whole enable
      chain. `output` / `inout` / `ref` arguments written after a suspension flow back when the task
      resumes. Rides on the timing-control machinery tracked in `processes.md`.

## Blocked

| Item                                                    | Blocked on                                                                                                                                                                                                                                                                      |
| ------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Module-scoped subroutine accessing per-instance storage | Module hierarchy. A subroutine declared in a module reads and writes the storage of the specific instance that called it; correct per-instance addressing needs the instantiation / object-tree model that does not yet exist. Tracked under `architecture-reset.md` hierarchy. |

## Out of Scope

- DPI import / export subroutines (LRM 13.6). Foreign-language boundary; a separate workstream.
- Class methods (LRM 8.6). Always automatic; ride on the class workstream, not this one.
- Parameterized tasks and functions (LRM 13.8), which the LRM realizes through static methods of
  parameterized classes. Needs classes.
- Constant functions (LRM 13.4.3). Evaluated at elaboration time inside constant expressions, a
  distinct evaluation context from the runtime call path tracked here.
- `fork`-`join_none` spawned inside a function (LRM 13.4.4). Concurrency; rides on `fork` / `join`
  (`processes.md` P8).

## Cross-references

- LRM anchors: 13.2 (overview), 13.3 (tasks), 13.3.1 / 13.3.2 (static vs automatic, concurrent
  activation), 13.4 (functions), 13.4.1 (return values and void functions), 13.4.2 (static vs
  automatic functions), 13.4.4 (background processes), 13.5 (calls and argument passing), 13.5.1
  (pass by value), 13.5.2 (pass by reference), 13.5.3 (default argument values), 13.5.4 (binding by
  name), 13.5.5 (optional argument list); 6.8 (default initialization), 6.14 (chandle).
- Rides on: `control-flow.md` (return / conditional / loop within bodies), `processes.md` timing
  controls (T2), `datatypes.md` (string, container, chandle boundary types), `packed.md` and
  `integral.md` (packed / integral boundary types).
- Blocks / unblocks: module hierarchy (`architecture-reset.md`) unblocks the per-instance subroutine
  case.

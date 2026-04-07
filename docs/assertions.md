# Assertions

Assertion architecture overview, with deferred immediate assertions as the main architectural focus. Concurrent assertions (A5 series) are noted for taxonomy but their architecture is not detailed here.

## Assertion Taxonomy

The LRM defines several assertion forms. They differ in evaluation timing, maturity region, and action execution region:

| Form                     | Condition evaluation   | Maturity  | Action execution            | LRM   |
| ------------------------ | ---------------------- | --------- | --------------------------- | ----- |
| Simple immediate         | Inline in process flow | --        | Immediately after condition | 16.3  |
| Observed deferred (`#0`) | Inline in process flow | Observed  | Reactive                    | 16.4  |
| Final deferred (`final`) | Inline in process flow | Postponed | Postponed                   | 16.4  |
| Concurrent               | Sampled in Observed    | Observed  | Reactive                    | 16.5+ |

All immediate assertions (simple and deferred) share the same condition-evaluation model: the expression is evaluated at the point in procedural flow where the assertion appears. They differ only in when the resulting action executes.

Concurrent assertions are fundamentally different -- they evaluate against sampled values in the Observed region and have multi-cycle temporal semantics. Their architecture is a separate concern (A5 series) and is not covered in this document.

## Assertion Kinds

Three assertion kinds exist across all forms:

| Kind     | Pass behavior                                       | Fail behavior                                      |
| -------- | --------------------------------------------------- | -------------------------------------------------- |
| `assert` | Optional pass action (default: nothing)             | Required fail path (user action or default report) |
| `assume` | Optional pass action (default: nothing)             | Required fail path (user action or default report) |
| `cover`  | Required success path (user action or built-in hit) | Nothing (false means "not covered")                |

The distinction between assert and assume is semantic (simulation vs formal verification intent), not behavioral in simulation. Cover has inverted disposition semantics: the "interesting" path is pass, not fail.

## Immediate Assertions

Simple immediate assertions (`assert`, `assume`, `cover` without timing qualifier) evaluate the condition and execute the action in a single procedural step. No deferral, no capture, no scheduling interaction. The condition result directly selects the pass or fail action, which executes inline.

## Deferred Immediate Assertions

Deferred immediate assertions split evaluation from action execution. The condition is evaluated inline in the process, but the resulting action is deferred to a later scheduling boundary. Critically, deferred assertions are still immediate assertions -- condition evaluation always happens at the point of encounter, regardless of timing qualifier.

### Observed Deferred (`#0`)

When a process encounters `assert #0 (cond) pass_call; else fail_call;`:

1. **Condition evaluation happens immediately.** The expression `cond` is evaluated in the Active region, at the point in procedural flow where the assertion appears. The result (pass/fail) is determined now.

2. **By-value actuals are fully evaluated at encounter time.** Every argument expression in the action call that is passed by value is evaluated when the assertion is processed, not later. The resulting values are captured.

3. **The pending report matures in the Observed region.** After the Active group converges, the Observed region determines which pending deferred assertions have survived (not invalidated by flush). Mature reports proceed to action execution.

4. **Action execution happens in the Reactive region.** The action subroutine call executes in the Reactive region, consuming the previously captured by-value arguments and any live reference bindings.

5. **`ref`/`const ref` actuals are not captured by value.** They remain bound to the underlying variable, and reads/writes observe the current value in the Reactive region.

6. **Action blocks are restricted to a single subroutine call.** No output or inout arguments are permitted. Deferred assertion actions shall reject automatic or dynamic variables as actuals to `ref` or `const ref` formals. This is a deferred-assertion-specific legality rule (LRM 16.4), stricter than ordinary subroutine ref passing (13.5.2).

7. **Flush semantics are process-associated.** If a process re-triggers (combinational sensitivity re-evaluation, resumption from wait), previously enqueued deferred assertions for that process are invalidated. Only assertions from the most recent activation survive to maturity.

### Final Deferred (`final`)

When a process encounters `assert final (cond)`:

1. **Condition evaluation happens immediately** -- at the point of encounter in procedural flow, just like all immediate assertions. The result is captured as a pending report.

2. **The pending report matures and executes in the Postponed region** -- after all final blocks have completed.

The action artifact model (outlined routine + capture payload) applies equally to final deferred assertions, but the scheduling trigger is different from observed deferred.

### The Key Distinction: Encounter vs Execution

The deferred action is not a "delayed call." It is an action artifact assembled at encounter time, then executed later. The two phases have fundamentally different access to state:

| Phase     | When                                                 | What it does                                                                          |
| --------- | ---------------------------------------------------- | ------------------------------------------------------------------------------------- |
| Encounter | Active region, inline in process flow                | Evaluate condition, evaluate by-value actuals, capture results, determine disposition |
| Execution | Reactive region (observed `#0`) or Postponed (final) | Invoke the action routine with captured values and live reference bindings            |

This split is the central architectural constraint for deferred assertion actions.

### Intuition

```
-- Encounter (Active region, inline in process) --
result = evaluate(cond)
if result requires deferred action:
    payload = evaluate_and_capture(by_value_actuals)
    ref_bindings = bind_live_ref_actuals()
    enqueue(site_id, disposition, payload, ref_bindings)

-- Maturity (Observed or Postponed) --
for each pending record that survived flush:
    mark as mature

-- Execution (Reactive or Postponed) --
for each mature record:
    site.action_routine(record.payload, record.ref_bindings)
```

This is conceptual only -- not a specification of internal structure.

## Deferred Action Architecture

### Site-Specialized Action Artifacts

Each deferred assertion site in the source produces a fixed, known action shape at compile time. The compiler can determine:

- Which action routines exist (pass, fail, or both)
- What the capture payload contains (number, types, and layout of by-value actuals)
- What reference bindings are needed (which formals are `ref`/`const ref`)

This information is site-static. It does not vary across executions of the same assertion. The architecture treats each site as a specialization point:

- **Each site has its own outlined action routine(s).** The action call body is extracted into a standalone routine during compilation, not interpreted at runtime.
- **Each site has a fixed capture layout.** The typed layout of captured by-value actuals is determined at compile time and is invariant for the site.
- **Runtime dispatch is by site and disposition.** The runtime looks up site metadata and invokes the pre-compiled action routine. It does not parse, plan, or interpret assertion semantics.

### Outlined Routines, Not Deferred Calls

The action routine is a distinct compiled artifact, not a copy of the original call lowering path executed later:

- Its **inputs** are the captured by-value payload plus any directly bound live ref/const-ref bindings.
- Its **body** contains the single subroutine call with arguments wired from the payload (by-value) and the bound references (by-ref).
- Its **lifetime** is independent of the originating process's frame. Captured values are owned by the deferred record, not borrowed from the process.

The outlined routine does not depend on the originating process frame being live at execution time. By-value data has already been copied out. Reference bindings are bound to live storage at encounter time and dereferenced at execution time.

### Capture Payload

Each site/disposition pair has a statically known payload type for by-value actuals. Encounter-time lowering writes directly into that typed payload. The thunk casts the erased payload pointer back to its site-local type and consumes it directly. No runtime schema walking or dynamic field decoding is involved.

- **Owned storage.** The payload is part of the deferred assertion record. It does not alias the originating process frame.
- **Evaluated eagerly.** All by-value fields are fully evaluated at encounter time. No deferred expression evaluation occurs.

### Reference Bindings

`ref` and `const ref` actuals are bound to live storage at encounter time and used directly at deferred execution time, observing the current value of the underlying variable in the Reactive/Postponed region. There is no late symbolic re-resolution. Legal ref-actual shapes follow the general rules of 13.5.2; deferred assertions additionally reject automatic or dynamic variables as `ref`/`const ref` actuals (LRM 16.4). `const ref` uses the same binding model as `ref` -- only mutability differs.

### Typed Payload and Erased Thunk Model

**By-value arguments use typed copied payload; `ref`/`const ref` arguments use typed bound live storage.**

The key insight that makes this architecture static and cheap: each site/disposition pair has its own payload shape, and the runtime never interprets payload contents.

**Each site/disposition has a fixed payload shape.** A single assertion site with both pass and fail actions may have two different payload shapes -- the pass thunk and fail thunk capture different arguments. The layout is fully determined at compile time per (site, disposition) pair.

**Execution crosses an erased ABI boundary.** The runtime stores a uniform record: site ID, disposition, payload pointer, and any bound ref pointers. It does not know the payload's internal structure. All thunks share a single erased entry signature.

**The site thunk reinterprets the payload as its own typed shape.** Inside the thunk, the opaque payload pointer is cast to the site-specific payload type. The thunk then performs the final subroutine call with arguments wired from the typed payload (by-value) and the stored ref bindings (by-ref).

```
-- Compile time --
site 7 / fail payload type:  { x: int32, msg_id: int8 }
site 7 / fail ref bindings:  { r0: int32* }

-- Runtime record --
{ site_id: 7, disposition: fail, payload_ptr: ..., ref_bindings: [r0] }

-- Dispatch --
thunk = site_table[record.site_id].thunk[record.disposition]
thunk(record.payload_ptr, record.ref_bindings)

-- Inside site 7 / fail thunk --
p = payload_ptr as site_7_fail_payload*
call fail_fn(p.x, p.msg_id, *record.ref_bindings.r0)
```

This means:

- The runtime is a thin dispatcher. It indexes into a site table and calls through a function pointer. No schema walking, no dynamic field decoding, no type introspection.
- Payload decode cost is zero beyond what the thunk's own compiled code does. The "decoder" is the thunk itself.
- Adding a new assertion site adds a new thunk and a new table entry. No runtime code changes.

### Disposition-Oriented Metadata

Each deferred assertion site carries metadata describing what happens for each disposition:

- An assert/assume site describes its **fail path**: either a default failure report or a user-supplied fail action routine.
- A cover site describes its **success path**: either a built-in cover hit or a user-supplied pass action routine.
- Metadata is validated at compile time for mutual exclusivity and completeness.

This metadata drives runtime dispatch. The runtime reads the disposition, looks up the corresponding pre-compiled artifact, and invokes it.

## Runtime Role

The architecture separates assertion-specific semantics from the execution substrate:

**Feature-specific semantics** (assertion/compiler layer):

- Deciding what disposition applies (pass vs fail, based on condition evaluation)
- Deciding what to capture (by-value actuals evaluated at encounter time)
- Deciding what ref bindings to store (which formals bind to which live variables)
- Assembling the deferred record with captured payload, ref bindings, and site identity

**Execution substrate** (runtime layer):

- Storing pending deferred records
- Managing per-process flush/invalidation state
- Scheduling maturity at the correct region boundary
- Invoking pre-compiled routines with stored payload and ref bindings

The substrate is potentially shareable with other deferred-execution mechanisms (e.g., concurrent assertion pass actions, which also execute in the Reactive region). The assertion-specific semantics are not -- they encode LRM rules about what is evaluated when and what access model the deferred action has.

The runtime is an **executor of pre-compiled action artifacts**, not an interpreter of assertion semantics.

## Observable Behavior and Implementation Freedom

The LRM defines observable behavior: when assertion results are determined, when actions execute, what values they see. Within that constraint, implementation may optimize freely:

- **Flush invalidation** may use generation counters, lazy pruning, or eager deletion -- as long as invalidated assertions never execute their actions.
- **Payload storage** may use inline buffers, arena allocation, or any allocation strategy -- as long as captured values are correct and outlive the deferred record.
- **Maturity scheduling** may batch, reorder, or coalesce within a single maturity boundary -- as long as all mature actions execute before the next region begins.
- **Reference bindings** may be stored in any implementation-defined typed form, as long as execution observes the current value of the originally bound underlying variable and deferred legality restrictions are enforced.
- **Region surrogate** -- before full Observed/Reactive region support, a settle-boundary surrogate may combine maturity and execution into a single step, as long as the observable ordering is preserved.

Preserve LRM-observable semantics, allow internal optimization.

## Scheduling Region Integration

Currently, observed deferred assertions use a settle-boundary surrogate: maturity and execution happen after the Active group converges, before any would-be Reactive-region work. When the Observed and Reactive regions are implemented (A3 series), maturity moves to Observed and action execution moves to Reactive.

This migration should be transparent to the action artifact model. Outlined routines, capture payloads, and dispatch metadata do not depend on which region triggers execution -- only that it happens at the correct point in the scheduling order.

## LRM References

- IEEE 1800-2023 Section 16.3: Immediate assertion statements
- IEEE 1800-2023 Section 16.4: Deferred immediate assertions
- IEEE 1800-2023 Section 16.5-16.14: Concurrent assertions
- IEEE 1800-2023 Section 4.4.2.3: Observed region (maturity point for `#0`)
- IEEE 1800-2023 Section 4.4.2.4: Reactive region (action execution for `#0` and concurrent)
- IEEE 1800-2023 Section 4.4.2.6: Postponed region (maturity/execution for `final`)
- IEEE 1800-2023 Section 13.5.2: Pass by reference restrictions

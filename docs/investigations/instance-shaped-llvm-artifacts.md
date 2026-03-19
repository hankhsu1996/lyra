# Instance-Shaped LLVM Artifacts Investigation

Investigation of per-instance LLVM code generation that violates the specialization boundary rule: instance count should affect runtime construction work, not LLVM codegen shape.

## Symptom

LLVM instruction count and compile time scale linearly with module instance count, even when all instances share one specialization (value-only parameter differences). Measured on `module-count` fixture: 976 LLVM insts at 4 instances, 1790 at 16 -- a clear linear relationship. CI nightly confirms: 5,088 insts (64 instances) vs 35,552 (512 instances).

## Root Cause

The `LyraProcessFunc` ABI is `void(*)(void* state, uint32_t resume, ProcessOutcome* out)` -- a 3-arg signature. The shared body function has a 7-arg signature adding `this_ptr`, `inst_id`, `signal_id_offset`, `unstable_offsets`. A per-instance LLVM wrapper function bridges the gap by baking instance-specific constants. The runtime dispatches through `procs[proc_idx](state, resume, &outcome)` with no mechanism to pass a descriptor alongside the call.

## Current Dispatch Paths

### Process dispatch

1. `BuildLayout` (layout.cpp) builds `scheduled_processes[]` -- flat array with one entry per (instance, process) pair
2. `CompileDesignProcesses` Phase 4 (lower.cpp) compiles one shared body per specialization
3. Phase 5 (lower.cpp:729-823) generates one `process_N` wrapper per instance-process via `GenerateProcessWrapper` (process.cpp:1910-1953)
4. `EmitProcessFuncArray` (emit_design_main.cpp:516-542) stores wrapper ptrs in `__lyra_module_funcs`
5. Runtime: `AotProcessDispatch` (simulation.cpp:304-347) calls `procs[proc_idx](state, resume, &outcome)`
6. Wrapper: loads design_ptr, computes `this_ptr = design_ptr + base_byte_offset`, calls shared body with 7 args

### Comb dispatch

1. `EmitCombWrappers` (emit_design_main.cpp:647-723) generates `__lyra_comb_wrapper_N` per comb kernel
2. Each comb wrapper allocates local outcome buffer, calls the underlying process wrapper, discards outcome
3. Chain: `comb_wrapper -> process_wrapper -> shared_body` -- two levels of per-instance indirection
4. Runtime: `Engine::InitCombKernels` stores `{func, state}` per kernel, calls `kernel.func(state, 0)`

### Unstable-offset path

1. `CompileDesignProcesses` Phase 4b (lower.cpp:696-724) emits `inst_N_unstable_offsets` globals per instance
2. Each is a constant `[num_unstable x i64]` array of instance-specific byte offsets
3. Passed through wrapper to shared body arg 5; body loads offsets by unstable ordinal
4. Only emitted for parameterized designs with different slot sizes -- zero for homogeneous designs

## Full Inventory of Instance-Shaped LLVM Artifacts

### 1. Per-instance process wrappers (`process_N`)

- **Produced**: `GenerateProcessWrapper` at process.cpp:1910-1953, called from lower.cpp:802-804
- **Count**: one per (instance, module-process) pair
- **Carries**: base_byte_offset (i64), instance_id (i32), base_slot_id (i32), unstable_offsets ptr
- **Consumer**: `__lyra_module_funcs` array -> `AotProcessDispatch`
- **Required for correctness**: no -- exists only because runtime ABI lacks descriptor slot
- **Body**: ~10 LLVM instructions

### 2. Per-comb-kernel wrappers (`__lyra_comb_wrapper_N`)

- **Produced**: `EmitCombWrappers` at emit_design_main.cpp:683-708
- **Count**: one per comb kernel entry (subset of module processes)
- **Carries**: reference to underlying process wrapper function
- **Consumer**: `__lyra_comb_funcs` array -> engine comb dispatch
- **Required for correctness**: no -- exists because comb ABI `void(ptr, i32)` lacks outcome ptr
- **Body**: ~5 LLVM instructions

### 3. Per-instance unstable-offset globals (`inst_N_unstable_offsets`)

- **Produced**: lower.cpp:700-724
- **Count**: one per instance with unstable slots (0 for homogeneous designs)
- **Carries**: `[num_unstable x i64]` byte offset array
- **Consumer**: passed through wrapper to shared body
- **Required for correctness**: the data is required; the LLVM global representation is not

### 4. Per-scheduled-process LLVM types (`ProcessStateN`, `ProcessFrameN`)

- **Produced**: `BuildProcessStateType` / `BuildFrameLayout` at layout.cpp:1302-1326
- **Count**: one named struct type per scheduled process
- **Carries**: structurally identical layout, only name differs
- **Impact**: minimal IR size, but conceptually instance-shaped; should be per-body

### 5. `__lyra_module_funcs` global array

- **Produced**: `EmitProcessFuncArray` at emit_design_main.cpp:516-542
- **Count**: 1 global, but size = num_module_processes (instance-shaped)
- **Carries**: array of per-instance wrapper function pointers

### 6. `__lyra_proc_offsets` global array

- **Produced**: `EmitPackedStateInit` at emit_design_main.cpp:436-439
- **Count**: 1 global, size = num_module_processes
- **Carries**: byte offsets into packed process state buffer

### 7. Per-process state init in `main()`

- **Produced**: `EmitPackedStateInit` at emit_design_main.cpp:400-514
- **Impact**: 4-state init loops iterate over all instances; scales linearly

## Runtime ABI Constraints That Force These Artifacts

The single root constraint: `LyraProcessFunc = void(*)(void*, uint32_t, ProcessOutcome*)` has no descriptor argument. The runtime dispatches through `procs[proc_idx](state, resume, &outcome)` -- a bare function pointer call. All instance-specific binding must be baked into the function itself.

Secondary: the comb ABI `void(*)(void*, uint32_t)` differs from the process ABI by lacking the outcome pointer, requiring an adapter wrapper.

## Clean Replacement Boundary

### Process descriptor (runtime-owned, one per instance-process):

```
struct ProcessDescriptor {
  SharedBodyFn* body;               // 7-arg shared body function ptr
  uint64_t      base_byte_offset;   // design_state offset to instance base
  uint32_t      instance_id;        // for %m path lookup
  uint32_t      base_slot_id;       // signal_id_offset for module-local signals
  const uint64_t* unstable_offsets; // nullptr if all stable
};
```

### Dispatch options

**Option A (recommended): Runtime-side descriptor dispatch**. Change `AotProcessDispatch` to read a descriptor table and call the shared body directly. No LLVM trampoline needed. The `__lyra_module_funcs` array becomes a descriptor table (or the descriptors are built at construction time from metadata). Shared body function pointers are stored once per body in a small array.

**Option B: Universal trampoline**. One LLVM function per shared body that loads descriptor fields and calls the body. Per-body, not per-instance. Requires runtime to pass descriptor pointer alongside dispatch.

**Option C: Modified dispatch ABI**. Add descriptor argument to `ProcessDispatchFn`. Runtime owns descriptor table, passes descriptor to dispatch callback.

### Comb path

Either: (a) runtime allocates outcome buffer and calls process wrapper directly (eliminating comb wrappers), or (b) unify comb and process ABIs.

### Unstable offsets

Move from per-instance LLVM globals to runtime-owned memory populated at construction time. Descriptor carries pointer to runtime-allocated offset table.

## What's Already Correct vs. Bridge

### Correct (long-term shape)

- Shared body functions with 7-arg ABI (one per body, instance-independent)
- MIR-level body deduplication (BuildSpecCompilationUnits groups by body_id)
- Body compilation isolation (per-body arena scope)
- Spec slot layout stable/unstable classification (per-body analysis)
- Connection kernelization (data-driven, no per-instance LLVM functions)
- Design metadata (word arrays, string pools -- data tables, not code)
- DesignState byte arena (flat byte storage, canonical contract)

### Bridge (needs migration)

- Per-instance wrapper functions -- bridge for 3-arg ABI gap
- Per-comb-kernel wrapper functions -- bridge for missing outcome ptr
- Per-instance unstable-offset globals -- data needed, LLVM global form is not
- Per-scheduled-process LLVM named types -- cosmetic, should be per-body
- `__lyra_module_funcs` as function pointer array -- would become descriptor table
- `__lyra_proc_offsets` global -- process state layout could move to runtime
- Per-process state init in `main()` -- scales with instances

## Hidden Dependencies

- **No pointer identity comparisons** on wrapper function pointers found
- **No ordering assumptions** beyond `scheduled_processes[]` deterministic order (preserved by descriptors)
- **No tests** assert per-instance wrapper existence (YAML behavioral tests only)
- **Debug naming**: wrapper function names (`process_N`) visible in LLVM dumps and debugger backtraces; minor debuggability regression, mitigated by descriptor-carried name strings
- **`comb_kernel_flags_`** uses `proc_idx` as index -- works with descriptors if flat index preserved
- **`AotProcessDispatchContext`** couples `procs[]` and `states[]` by parallel index -- moves to descriptor table naturally
- **JIT path**: same `LyraRunSimulation` entry point, same dispatch model. Descriptor migration benefits both AOT and JIT equally

## Open Unknowns

1. **Scale of LLVM overhead**: For ibex (~1116 processes), ~1116 wrappers x ~10 LLVM insts each = ~11K instructions of wrapper overhead. Each requires LLVM function creation, verification, and machine code emission. At thousands of modules, this fraction grows.

2. **Process state allocation**: Currently `EmitPackedStateInit` emits per-process LLVM code. With descriptors, process state layout computation could move to runtime construction, but the packed allocation scheme may have constraints.

3. **ABI version**: `kRuntimeAbiVersion` provides explicit versioning. Both codegen and runtime are in-tree and co-versioned, so no external compatibility constraint.

4. **Constructor-time offset computation**: Unstable offsets are currently computed at LLVM codegen time from `GetInstanceRelByteOffsets`. This data must be available at constructor time, which means the realization path must carry per-instance slot offset information into runtime construction.

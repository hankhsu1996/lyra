# SSA Block Params Implementation Review

**Date**: 2026-02-03
**Scope**: MIR Block Args for SSA Temps at Joins (Ternary / If-Else Only)

---

## 1. Invariants (Non-Negotiable Contracts)

### 1.1 Each `temp_id` is bound exactly once in LLVM lowering

**Enforcement**: `src/lyra/llvm_backend/context.cpp:303-311`

```cpp
void Context::BindTemp(int temp_id, llvm::Value* v, TypeId type) {
  auto [it, inserted] = temp_values_.try_emplace(temp_id, v);
  if (!inserted) {
    throw common::InternalError(
        "BindTemp",
        std::format("temp {} already bound", temp_id));
  }
  temp_types_.emplace(temp_id, type);
}
```

**Status**: ENFORCED via `try_emplace` + `InternalError` on duplicate.

**Counterexample**: If MIR emitted the same `temp_id` as a block param in two different blocks, the second `BindTemp` would throw.

### 1.2 `ReadTemp` is never called before binding

**Enforcement**: `src/lyra/llvm_backend/context.cpp:313-321`

```cpp
auto Context::ReadTemp(int temp_id) const -> llvm::Value* {
  auto it = temp_values_.find(temp_id);
  if (it == temp_values_.end()) {
    throw common::InternalError(
        "ReadTemp",
        std::format("temp {} not bound", temp_id));
  }
  return it->second;
}
```

**Status**: ENFORCED via `InternalError` on miss.

**Counterexample**: If an operand referenced `UseTemp(5)` but temp 5 was never defined as a block param, `ReadTemp` throws.

### 1.3 `GetTempType` is always available for any `UseTemp` encountered

**Enforcement**: `src/lyra/llvm_backend/context.cpp:323-331`

```cpp
auto Context::GetTempType(int temp_id) const -> TypeId {
  auto it = temp_types_.find(temp_id);
  if (it == temp_types_.end()) {
    throw common::InternalError(
        "GetTempType",
        std::format("temp {} not bound", temp_id));
  }
  return it->second;
}
```

**Status**: ENFORCED. Both maps are populated atomically in `BindTemp`.

**Counterexample**: Same as 1.2 - referencing an unbound temp.

### 1.4 Edge args do not require instruction emission during PHI wiring

**Enforcement**: `src/lyra/llvm_backend/process.cpp:106-119`

```cpp
auto LowerEdgeArgs(Context& context, const std::vector<mir::Operand>& args)
    -> Result<std::vector<llvm::Value*>> {
  std::vector<llvm::Value*> llvm_args;
  llvm_args.reserve(args.size());
  for (const auto& arg : args) {
    auto val_or_err = LowerOperandRaw(context, arg);
    // ...
  }
  return llvm_args;
}
```

**Status**: PARTIALLY ENFORCED. Edge args are lowered BEFORE the terminator is created (line 127-132 for Jump, line 156-168 for Branch), ensuring loads happen in the correct block. However, there's no compile-time enforcement that edge args are "immediate" (Const/UseTemp only).

**Counterexample**: If an edge arg were `Use(PlaceId)` pointing to a complex projected place requiring GEP chains, those GEPs would be emitted in the predecessor block - which is correct but could be expensive. A stricter invariant would restrict edge args to Const/UseTemp only.

**ISSUE**: No static validation that edge args are restricted to Const/UseTemp.

### 1.5 Every CFG predecessor of a block with PHIs has incoming edges wired

**Enforcement**: `src/lyra/llvm_backend/process.cpp:640-646` (and similar at 1203-1207, 1685-1691)

```cpp
for (const auto& edge : phi_state.pending_edges) {
  for (size_t j = 0; j < edge.args.size(); ++j) {
    auto* phi = phi_state.phis[PhiWiringState::MakeKey(edge.succ_idx, j)];
    phi->addIncoming(edge.args[j], edge.pred);
  }
}
```

**Status**: NOT ENFORCED. The wiring relies on MIR correctness - if a predecessor doesn't record a pending edge, the PHI will have missing incoming values.

**Counterexample**: If MIR had a Jump to a block with params but `jump.args` was empty, no pending edge would be recorded, and the PHI would have fewer incoming values than predecessors.

**ISSUE**: No validation that PHI incoming count matches predecessor count.

### 1.6 Resume switch does NOT introduce spurious predecessors for PHI blocks

**Enforcement**: `src/lyra/llvm_backend/process.cpp:568-586`

```cpp
// Assert: resume target blocks (and bb0) must NOT have block params.
if (!process.blocks[0].params.empty()) {
  throw common::InternalError(
      "GenerateProcessFunction",
      std::format(
          "entry block 0 has {} params; entry block must have no block params",
          process.blocks[0].params.size()));
}
for (size_t target : resume_targets) {
  if (!process.blocks[target].params.empty()) {
    throw common::InternalError(
        "GenerateProcessFunction",
        std::format(
            "resume target block {} has {} params; resume targets must have "
            "no block params",
            target, process.blocks[target].params.size()));
  }
}
```

**Status**: ENFORCED via `InternalError`. Entry block (bb0) and all resume targets (blocks jumped to after Delay/Wait) must have no params.

**Counterexample**: If a ternary's merge block were also a resume target, this would throw.

---

## 2. Architecture / Symmetry Check

### 2.1 Process Lowering (`GenerateProcessFunction`)

**Location**: `src/lyra/llvm_backend/process.cpp:486-659`

| Step                 | Location                                    | Description                                        |
| -------------------- | ------------------------------------------- | -------------------------------------------------- |
| `ClearTemps()`       | Line 601                                    | Clear stale bindings from previous functions       |
| PHI pre-creation     | Lines 603-621                               | Iterate blocks, create PHIs for params             |
| Bind param temps     | Line 618                                    | `context.BindTemp(param.temp_id, phi, param.type)` |
| Record pending edges | Lines 624-637 via `LowerJump`/`LowerBranch` | During terminator lowering                         |
| Backpatch PHIs       | Lines 640-646                               | After all blocks lowered                           |

### 2.2 Monitor Check Thunk (`DefineMonitorCheckThunk`)

**Location**: `src/lyra/llvm_backend/process.cpp:935-1210`

| Step                 | Location                                      | Description                                        |
| -------------------- | --------------------------------------------- | -------------------------------------------------- |
| `ClearTemps()`       | Line 1031                                     | Clear stale bindings                               |
| PHI pre-creation     | Lines 1033-1051                               | Identical pattern                                  |
| Bind param temps     | Line 1048                                     | `context.BindTemp(param.temp_id, phi, param.type)` |
| Record pending edges | Lines 1059-1083 via `LowerJump`/`LowerBranch` | During terminator lowering                         |
| Backpatch PHIs       | Lines 1203-1207                               | After all blocks lowered                           |

### 2.3 User Function (`DefineUserFunction`)

**Location**: `src/lyra/llvm_backend/process.cpp:1390-1738`

| Step                 | Location                                      | Description                                        |
| -------------------- | --------------------------------------------- | -------------------------------------------------- |
| `ClearTemps()`       | Line 1586                                     | Clear stale bindings                               |
| PHI pre-creation     | Lines 1588-1606                               | Identical pattern                                  |
| Bind param temps     | Line 1603                                     | `context.BindTemp(param.temp_id, phi, param.type)` |
| Record pending edges | Lines 1613-1682 via `LowerJump`/`LowerBranch` | During terminator lowering                         |
| Backpatch PHIs       | Lines 1685-1691                               | After all blocks lowered                           |

### 2.4 Symmetry Assessment

**Status**: GOOD. All three entry points follow the exact same pattern:

1. ClearTemps at start
2. Pre-create PHIs and bind temps
3. Lower blocks (terminators record pending edges)
4. Backpatch PHIs at end

**Code duplication**: The PHI setup logic (lines 603-621, 1033-1051, 1588-1606) is copy-pasted three times. This is acceptable for now but could be extracted into a helper.

**Difference**: Process lowering has resume switch validation (lines 568-586) that others don't need.

---

## 3. Type Correctness for UseTemp

### 3.1 Where `temp_id -> TypeId` is created

| Definition Site         | Location            | How TypeId is Provided           |
| ----------------------- | ------------------- | -------------------------------- |
| Block params (Process)  | `process.cpp:618`   | `param.type` from MIR BlockParam |
| Block params (Monitor)  | `process.cpp:1048`  | `param.type` from MIR BlockParam |
| Block params (UserFunc) | `process.cpp:1603`  | `param.type` from MIR BlockParam |
| Statement-defined temps | **NOT IMPLEMENTED** | N/A                              |

**ISSUE**: Statement-defined temps (e.g., Assign to kTemp place) do NOT use `BindTemp`. They still use Place-based storage with allocas. See Section 9.

### 3.2 Failure modes if temp_id used without type

`GetTempType` throws `InternalError` if the temp_id is not in the map. This catches:

- UseTemp referencing a temp_id that was never defined
- UseTemp referencing a temp_id before its defining block is reached

### 3.3 TempId handling in operand/type queries

| Function                          | File:Line              | TempId Handling                      |
| --------------------------------- | ---------------------- | ------------------------------------ |
| `LowerOperandRaw`                 | `operand.cpp:131-133`  | `context.ReadTemp(temp_id.value)`    |
| `GetOperandTypeId` (math)         | `math.cpp:51-52`       | `context.GetTempType(temp_id.value)` |
| `GetOperandTypeId` (real)         | `real.cpp:50-51`       | `context.GetTempType(temp_id.value)` |
| `GetOperandTypeId` (string)       | `string.cpp:58-59`     | `context.GetTempType(temp_id.value)` |
| `GetOperandTypeId` (two_state)    | `two_state.cpp:50-51`  | `context.GetTempType(temp_id.value)` |
| `IsOperandFourState` (four_state) | `four_state.cpp:57-60` | `context.GetTempType(temp_id.value)` |
| `IsOperandFourState` (math)       | `math.cpp:88-91`       | `context.GetTempType(temp_id.value)` |
| `LoadFourStateOperand`            | `cast.cpp:109-112`     | `context.GetTempType(temp_id.value)` |
| `GetOperandPackedWidth`           | `ops.cpp:191-193`      | **THROWS ERROR** (see Section 5)     |

---

## 4. PHI Wiring Implementation Details

### 4.1 Data Structures

**PhiWiringState**: `src/lyra/llvm_backend/process.cpp:49-69`

```cpp
struct PhiWiringState {
  // Map from (block_index, param_index) to PHINode*
  std::unordered_map<uint64_t, llvm::PHINode*> phis;

  struct PendingEdge {
    llvm::BasicBlock* pred;
    size_t succ_idx;
    std::vector<llvm::Value*> args;
  };
  std::vector<PendingEdge> pending_edges;

  static auto MakeKey(size_t block_idx, size_t param_idx) -> uint64_t {
    return (static_cast<uint64_t>(block_idx) << 32) |
           static_cast<uint64_t>(param_idx);
  }
};
```

**Key design**: Composite key `(block_idx << 32) | param_idx` enables O(1) PHI lookup without scanning block instructions.

### 4.2 PHI Creation

**Location**: `process.cpp:603-621` (and equivalent in other entry points)

```cpp
for (size_t i = 0; i < process.blocks.size(); ++i) {
  const auto& block = process.blocks[i];
  if (!block.params.empty()) {
    builder.SetInsertPoint(llvm_blocks[i], llvm_blocks[i]->begin());
    for (size_t j = 0; j < block.params.size(); ++j) {
      const auto& param = block.params[j];
      auto llvm_ty_or_err = GetLlvmTypeForType(context, param.type);
      // ...
      auto* phi = builder.CreatePHI(*llvm_ty_or_err, 2, std::format("phi{}", j));
      phi_state.phis[PhiWiringState::MakeKey(i, j)] = phi;
      context.BindTemp(param.temp_id, phi, param.type);
    }
  }
}
```

**Note**: PHIs are created with initial capacity 2 (typical for ternary/if-else).

### 4.3 Edge Recording

**LowerJump**: `process.cpp:121-146`

```cpp
// Lower edge args BEFORE creating the terminator
std::vector<llvm::Value*> arg_values;
if (!jump.args.empty()) {
  auto args_or_err = LowerEdgeArgs(context, jump.args);
  // ...
}

// Create the branch terminator
context.GetBuilder().CreateBr(blocks[jump.target.value]);

// Record edge args for PHI wiring
if (!arg_values.empty()) {
  phi_state.pending_edges.push_back({
    .pred = current_bb,
    .succ_idx = static_cast<size_t>(jump.target.value),
    .args = std::move(arg_values)
  });
}
```

**Critical**: Args are lowered BEFORE terminator creation to ensure loads happen in the correct block.

### 4.4 PHI Backpatching

**Location**: `process.cpp:640-646`

```cpp
for (const auto& edge : phi_state.pending_edges) {
  for (size_t j = 0; j < edge.args.size(); ++j) {
    auto* phi = phi_state.phis[PhiWiringState::MakeKey(edge.succ_idx, j)];
    phi->addIncoming(edge.args[j], edge.pred);
  }
}
```

**Guarantee**: Backpatching is wiring-only (no IR emission) because `edge.args` contains pre-lowered `llvm::Value*`.

### 4.5 Minimal IR Example: Ternary in User Function

**MIR** (conceptual):

```
bb0:
  %cond = ...
  Branch %cond -> bb1, bb2

bb1:
  %then_val = ...
  Jump bb3(%then_val)

bb2:
  %else_val = ...
  Jump bb3(%else_val)

bb3(param %result):
  Return %result
```

**LLVM IR** (after lowering):

```llvm
bb0:
  %cond = ...
  br i1 %cond, label %bb1, label %bb2

bb1:
  %then_val = ...
  br label %bb3

bb2:
  %else_val = ...
  br label %bb3

bb3:
  %phi0 = phi i32 [ %then_val, %bb1 ], [ %else_val, %bb2 ]
  ret i32 %phi0
```

### 4.6 Resume Switch Handling

**Resume targets collected**: `process.cpp:551-566`

```cpp
std::vector<size_t> resume_targets;
for (const auto& block : process.blocks) {
  std::visit(
      [&](const auto& term) {
        using T = std::decay_t<decltype(term)>;
        if constexpr (std::is_same_v<T, mir::Delay>) {
          resume_targets.push_back(term.resume.value);
        } else if constexpr (std::is_same_v<T, mir::Wait>) {
          resume_targets.push_back(term.resume.value);
        }
      },
      block.terminator.data);
}
```

**Switch creation**: `process.cpp:588-598`

```cpp
auto* sw = builder.CreateSwitch(
    resume_block_arg, llvm_blocks[0],
    static_cast<unsigned>(resume_targets.size()));

for (size_t target : resume_targets) {
  sw->addCase(
      llvm::ConstantInt::get(i32_ty, static_cast<uint64_t>(target)),
      llvm_blocks[target]);
}
```

**Design**: bb0 is the default case. Resume targets are added as explicit cases. Blocks with params cannot be resume targets (enforced).

---

## 5. Dangerous Patterns / Likely Hacks

### 5.1 Remaining "UseTemp not supported" Paths

**Status**: **FIXED**. All TempId handlers now implemented.

**Fixed location**: `src/lyra/llvm_backend/compute/ops.cpp:191-193`

```cpp
[&](mir::TempId temp_id) -> TypeId {
  return context.GetTempType(temp_id.value);
},
```

**Verification**: `grep -r "UseTemp.*not.*support" src/lyra/llvm_backend` returns no matches.

### 5.2 LLVM Type Shape Checks for 2-state vs 4-state

**FOUND**: `src/lyra/llvm_backend/process.cpp:86-91`

```cpp
// 4-state struct: extract known-true bits (a & ~b)
if (cond_type->isStructTy()) {
  auto* a = builder.CreateExtractValue(cond_val, 0, "cond.a");
  auto* b = builder.CreateExtractValue(cond_val, 1, "cond.b");
  // ...
}
```

**Status**: ACCEPTABLE. This is in `LoadConditionAsI1` which loads from a Place (not a temp). The LLVM type check is used to dispatch between 2-state and 4-state loading, and is correct because the Place type determines the LLVM representation.

**However**: If Branch condition were changed to Operand (as planned), this would need to use MIR type, not LLVM type.

### 5.3 Code That Scans Block Instructions for PHIs

**FOUND**: None. The implementation uses `PhiWiringState::phis` map with direct lookup via `MakeKey(block_idx, param_idx)`.

### 5.4 Order-Dependent PHI Wiring

**Status**: NOT FRAGILE. The wiring loop iterates `pending_edges` in insertion order, but PHI `addIncoming` order doesn't affect semantics - only which predecessor appears first in the textual IR.

---

## 6. Validation and Debug Instrumentation

### 6.1 MIR Validation

**Status**: **IMPLEMENTED**. Static MIR validation in `src/lyra/mir/verify.cpp`.

**Invariants checked** (documented in `include/lyra/mir/verify.hpp:9-27`):

- param_local_slots invariants (existing)
- Return value invariants (existing)
- **NEW**: BlockParam temp_ids are unique across all blocks
- **NEW**: For Jump/Branch: edge arg count == target block param count
- **NEW**: For Jump/Branch: edge arg types match target block param types
- **NEW**: All UseTemp operands in edge args reference defined temp_ids

**Key functions**:

- `VerifyBlockParamsAndEdgeArgs()`: `src/lyra/mir/verify.cpp:179-234`
- `VerifyEdgeArgs()`: `src/lyra/mir/verify.cpp:127-152`
- `VerifyUniqueBlockParamTempIds()`: `src/lyra/mir/verify.cpp:155-168`
- `VerifyUseTempDefined()`: `src/lyra/mir/verify.cpp:171-182`

### 6.2 LLVM Verification

**Status**: **IMPLEMENTED**. `llvm::verifyFunction` called after each function definition.

**Locations**:

- Process: `src/lyra/llvm_backend/process.cpp:705-713`
- Monitor thunk: `src/lyra/llvm_backend/process.cpp:1311-1320`
- User function: `src/lyra/llvm_backend/process.cpp:1823-1832`

```cpp
// Verify LLVM IR
std::string err_str;
llvm::raw_string_ostream err_stream(err_str);
if (llvm::verifyFunction(*func, &err_stream)) {
  throw common::InternalError(
      "GenerateProcessFunction",
      std::format("LLVM IR verification failed for process '{}': {}",
                  func->getName().str(), err_str));
}
```

### 6.3 PHI Wiring Validation

**Status**: **IMPLEMENTED**. `PhiWiringState::ValidatePhiWiring()` at `src/lyra/llvm_backend/process.cpp:75-115`.

For each PHI:

- Counts LLVM predecessors via `llvm::pred_begin/pred_end`
- Compares to `phi->getNumIncomingValues()`
- Throws `InternalError` with detailed message if mismatch

**Locations called**:

- Process: `src/lyra/llvm_backend/process.cpp:697`
- Monitor thunk: `src/lyra/llvm_backend/process.cpp:1275`
- User function: `src/lyra/llvm_backend/process.cpp:1775`

---

## 7. Performance Evidence (Before/After Metrics)

**Status**: NOT COLLECTED.

The implementation does NOT remove kTemp from Place storage allocation. See Section 9.

**Ibex smoke test**: Blocked by remaining `GetOperandPackedWidth` error (Section 5.1).

**Recommendation**: After fixing Section 5.1, collect:

```bash
# Count allocas in LLVM IR dump
lyra dump llvm --top ibex_simple_system -f lyra.f 2>&1 | grep -c "alloca"

# Count loads/stores
lyra dump llvm ... | grep -c "load\|store"
```

---

## 8. Patch List

### Original SSA Block Params Implementation

| File                                                 | Change                                                                         |
| ---------------------------------------------------- | ------------------------------------------------------------------------------ |
| `include/lyra/llvm_backend/context.hpp:347-359`      | Added `BindTemp(temp_id, value, type)`, `GetTempType(temp_id)`, `ClearTemps()` |
| `include/lyra/llvm_backend/context.hpp:515-519`      | Added `temp_values_` and `temp_types_` maps                                    |
| `src/lyra/llvm_backend/context.cpp:303-336`          | Implemented `BindTemp`, `ReadTemp`, `GetTempType`, `ClearTemps`                |
| `src/lyra/llvm_backend/process.cpp:49-69`            | Added `PhiWiringState` struct                                                  |
| `src/lyra/llvm_backend/process.cpp:106-188`          | Added `LowerEdgeArgs`, `LowerJump`, `LowerBranch` with edge arg handling       |
| `src/lyra/llvm_backend/process.cpp:551-598`          | Resume switch: collect targets, validate no params, create switch              |
| `src/lyra/llvm_backend/process.cpp:601-646`          | Process: ClearTemps, PHI setup, backpatch                                      |
| `src/lyra/llvm_backend/process.cpp:1031-1051`        | Monitor thunk: ClearTemps, PHI setup                                           |
| `src/lyra/llvm_backend/process.cpp:1203-1207`        | Monitor thunk: PHI backpatch                                                   |
| `src/lyra/llvm_backend/process.cpp:1586-1606`        | User function: ClearTemps, PHI setup                                           |
| `src/lyra/llvm_backend/process.cpp:1685-1691`        | User function: PHI backpatch                                                   |
| `src/lyra/llvm_backend/compute/cast.cpp:109-112`     | `LoadFourStateOperand`: TempId via `GetTempType`                               |
| `src/lyra/llvm_backend/compute/four_state.cpp:57-60` | `IsOperandFourState`: TempId via `GetTempType`                                 |
| `src/lyra/llvm_backend/compute/math.cpp:51-52`       | `GetOperandTypeId`: TempId via `GetTempType`                                   |
| `src/lyra/llvm_backend/compute/math.cpp:88-91`       | `IsOperandFourState`: TempId via `GetTempType`                                 |
| `src/lyra/llvm_backend/compute/real.cpp:50-51`       | `GetOperandTypeId`: TempId via `GetTempType`                                   |
| `src/lyra/llvm_backend/compute/string.cpp:58-59`     | `GetOperandTypeId`: TempId via `GetTempType`                                   |
| `src/lyra/llvm_backend/compute/two_state.cpp:50-51`  | `GetOperandTypeId`: TempId via `GetTempType`                                   |
| `src/lyra/llvm_backend/compute/operand.cpp:131-133`  | `LowerOperandRaw`: TempId via `ReadTemp`                                       |
| `include/lyra/mir/basic_block.hpp:11-23`             | Added `BlockParam` struct with `temp_id` and `type`                            |
| `include/lyra/mir/terminator.hpp:18-28`              | Added `Jump::args`, `Branch::then_args`, `Branch::else_args`                   |
| `include/lyra/mir/operand.hpp:13-42`                 | Added `TempId` struct and `Operand::UseTemp()`                                 |

### P0 Fixes (This Review)

| File                                            | Change                                                               |
| ----------------------------------------------- | -------------------------------------------------------------------- |
| `src/lyra/llvm_backend/compute/ops.cpp:191-193` | Fixed `GetOperandPackedWidth` TempId handler                         |
| `src/lyra/llvm_backend/process.cpp:23-26`       | Added `llvm/IR/Verifier.h` and `llvm/Support/raw_ostream.h` includes |
| `src/lyra/llvm_backend/process.cpp:705-713`     | Added LLVM verification for process functions                        |
| `src/lyra/llvm_backend/process.cpp:1311-1320`   | Added LLVM verification for monitor thunks                           |
| `src/lyra/llvm_backend/process.cpp:1823-1832`   | Added LLVM verification for user functions                           |

### P1 Fixes (This Review)

| File                                       | Change                                                                |
| ------------------------------------------ | --------------------------------------------------------------------- |
| `include/lyra/mir/verify.hpp:9-27`         | Updated docs with new invariants                                      |
| `src/lyra/mir/verify.cpp:88-250`           | Added block param/edge arg validation functions                       |
| `src/lyra/llvm_backend/process.cpp:75-137` | Added `PhiWiringState::ValidatePhiWiring()` and `WireIncomingEdges()` |

### Pre-commit Cleanup (This Review)

| File                                          | Change                                                    |
| --------------------------------------------- | --------------------------------------------------------- |
| `src/lyra/llvm_backend/process.cpp:52-88`     | Added doc comment explaining PHI wiring lifecycle         |
| `src/lyra/llvm_backend/process.cpp:140-175`   | Added `SetupBlockParamPhis()` helper                      |
| `src/lyra/llvm_backend/process.cpp:177-188`   | Added `VerifyLlvmFunction()` helper                       |
| `src/lyra/llvm_backend/process.cpp:722-744`   | Refactored GenerateProcessFunction to use helpers         |
| `src/lyra/llvm_backend/process.cpp:1130-1135` | Refactored DefineMonitorCheckThunk to use helpers         |
| `src/lyra/llvm_backend/process.cpp:1666-1671` | Refactored DefineUserFunction to use helpers              |
| `src/lyra/mir/verify.cpp:108-126`             | Added `VerifyEdgeArgKind()` with documented allowed kinds |

---

## 9. Open Issues / Follow-ups

### 9.1 Temp Model: UseTemp Restricted to Block Params Only (DESIGN DECISION)

**Decision**: The current implementation uses a hybrid model where:

- **Block params**: Use SSA (BindTemp/ReadTemp, PHI nodes)
- **Statement-defined temps (kTemp places)**: Use storage-backed allocas

This is **Option B** from the plan. We consciously chose NOT to implement full SSA temps (Option A) in this iteration.

**Rationale**:

1. Block params already provide SSA semantics for CFG join values (ternary, if-else)
2. Full SSA for all temps would require significant MIR builder changes
3. Current model is consistent and well-tested
4. Performance benefits from full SSA can be evaluated later

**Enforcement**: `include/lyra/mir/operand.hpp:38-42`

```cpp
// UseTemp: reference an SSA temp by its id.
// Used for block params and temps that don't need Place storage.
static auto UseTemp(int temp_id) -> Operand {
  return {.kind = Kind::kUseTemp, .payload = TempId{temp_id}};
}
```

The comment documents that UseTemp is for block params. MIR verification (`src/lyra/mir/verify.cpp`) validates that all UseTemp references resolve to defined block param temp_ids.

**Future work** (if needed for performance):

1. Statement-defined temps could call `BindTemp` instead of storing to Place
2. `PlaceCollector` could exclude kTemp
3. MIR builder could emit UseTemp for statement temps

### 9.2 GetOperandPackedWidth TempId Support

**Status**: **FIXED** in `src/lyra/llvm_backend/compute/ops.cpp:191-193`.

### 9.3 Branch.condition is PlaceId, Not Operand

**Issue**: `Branch::condition` is `PlaceId`, not `Operand`. This forces condition values through Place storage.

**Evidence**: `include/lyra/mir/terminator.hpp:23`

```cpp
struct Branch {
  PlaceId condition;  // Should be Operand
  // ...
};
```

**Impact**: Prevents conditions from being direct UseTemp operands.

**Required work**: Change to `Operand condition` and update `LoadConditionAsI1` to handle Const/UseTemp.

### 9.4 MIR Validation

**Status**: **IMPLEMENTED** in `src/lyra/mir/verify.cpp`. See Section 6.1.

### 9.5 LLVM Verification

**Status**: **IMPLEMENTED** in `src/lyra/llvm_backend/process.cpp`. See Section 6.2.

### 9.6 Ternary SSA Status Unknown

**Issue**: It's unclear whether HIR->MIR lowering actually emits block params for ternaries.

**Evidence needed**: Dump MIR for a ternary expression and verify it uses block params, not place-based temps.

### 9.7 Edge Args Not Restricted to Const/UseTemp

**Issue**: Any Operand (including complex Place projections) can be an edge arg.

**Risk**: Complex places require GEP chains in predecessor blocks, which works but may be inefficient.

**Required work**: Either validate edge args are immediate, or document that complex args are acceptable.

---

## Summary

| Category              | Status                                   |
| --------------------- | ---------------------------------------- |
| Invariant enforcement | PARTIAL - runtime checks only            |
| Architecture symmetry | GOOD - three entry points are consistent |
| Type correctness      | GOOD - MIR TypeId is source of truth     |
| PHI wiring            | GOOD - direct map, no scanning           |
| Dangerous patterns    | ONE FOUND - `GetOperandPackedWidth`      |
| Validation            | MISSING - no MIR or LLVM verification    |
| Performance metrics   | NOT COLLECTED                            |
| kTemp storage removal | NOT IMPLEMENTED                          |

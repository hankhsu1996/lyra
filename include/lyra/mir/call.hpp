#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/common/dpi_types.hpp"
#include "lyra/common/parameter_direction.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/system_tf.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::mir {

// Explicit cross-domain reference to a design-global callable.
// Used when body MIR calls any design-global function (package functions,
// design-level generated functions). The SymbolId identifies the callable
// in the design-global function namespace. The backend resolves this to
// a pre-compiled LLVM Function* by symbol identity.
struct DesignFunctionRef {
  SymbolId symbol;
};

// DPI ABI passing convention at the foreign boundary.
enum class DpiPassingMode : uint8_t {
  kByValue,    // Input: T
  kByPointer,  // Output/inout: T*
};

// DPI return convention at the MIR level.
enum class DpiReturnKind : uint8_t {
  kVoid,
  kDirectValue,
};

// Per-parameter descriptor for a DPI-C import signature.
// Carries the full foreign-boundary contract for one parameter.
struct DpiParamDesc {
  TypeId sv_type;
  DpiAbiTypeClass abi_type = DpiAbiTypeClass::kInvalid;
  ParameterDirection direction = ParameterDirection::kInput;
  DpiPassingMode passing = DpiPassingMode::kByValue;
};

// Return descriptor for a DPI-C import signature.
struct DpiReturnDesc {
  TypeId sv_type;
  DpiAbiTypeClass abi_type = DpiAbiTypeClass::kInvalid;
  DpiReturnKind kind = DpiReturnKind::kVoid;
};

// Frozen DPI-C import signature with per-parameter descriptors.
// Separate from FunctionSignature; never participates in Lyra-internal call
// ABI.
struct DpiSignature {
  DpiReturnDesc result;
  std::vector<DpiParamDesc> params;
};

// Explicit reference to a DPI-C imported function.
// Carries only foreign-boundary data. No DesignState*, Engine*, or
// Lyra-internal frame data. No fake MIR body.
struct DpiImportRef {
  SymbolId symbol;     // SV declaration identity (for diagnostics/debug)
  std::string c_name;  // External C symbol name
  DpiSignature signature;
  // True for `import "DPI-C" context ...`.
  // The hidden svScope call-context parameter is NOT represented in
  // DpiSignature; it is added separately by LLVM import declaration and
  // call emission. DpiSignature describes the user-visible DPI ABI only.
  bool is_context = false;
};

// Per-argument binding at a DPI call site.
// Input: input_value present, writeback_dest absent.
// Output: input_value absent, writeback_dest present.
// Inout: both present (copy-in value + writeback destination).
struct DpiArgBinding {
  std::optional<Operand> input_value;
  std::optional<PlaceId> writeback_dest;
};

// Callee discriminator for non-DPI calls: body-local function,
// design-global function, or system TF. DPI calls use DpiCall instead.
using Callee = std::variant<FunctionId, DesignFunctionRef, SystemTfOpcode>;

// Argument passing mode for writeback parameters only.
enum class PassMode : uint8_t {
  kOut,    // Output only (write-only)
  kInOut,  // Input + output (read-modify-write)
  kRef,    // Reference (aliasing) - fail-loud until implemented
};

// Return output binding: stages return value through explicit temp.
//
// Two forms based on usage context:
//
// EXPRESSION FORM (dest == nullopt):
//   Call emits to tmp, returns Use(tmp) as Operand.
//   Caller emits Assign(actual_dest, Use(tmp)) which goes through CommitValue.
//   Used when call result feeds into larger expression or explicit assignment.
//
// STATEMENT FORM (dest.has_value()):
//   Call emits to tmp, then internally commits to dest.
//   Used when call has explicit target (e.g., system TF with known dest).
//
// Contract:
// - tmp: MIR-visible PlaceId where call writes return value (staging)
// - dest: optional final destination (nullopt = expression form)
// - type: return type for dispatch and ownership handling
//
// Execution flow:
// 1. Call writes return to tmp (register return or sret out-param)
// 2. If dest: CommitValue(dest, Load(tmp), type, kMove)
//    Else: caller handles commit via Assign(dest, Use(tmp))
struct CallReturn {
  PlaceId tmp;                  // Staging temp (MIR-visible, preallocated)
  std::optional<PlaceId> dest;  // nullopt = expression form (caller assigns)
  TypeId type;                  // Return type
};

// How the backend handles data flow for a writeback parameter.
enum class WritebackKind : uint8_t {
  // Default: call writes to tmp staging area, then backend commits tmp->dest.
  // Used for most system TFs ($value$plusargs, $fgets, $fscanf).
  kStaged,

  // Runtime writes directly to dest via raw pointer; no tmp alloca, no commit.
  // Used for bulk memory operations ($fread memory variant) where staging
  // through tmp would be wasteful and the operation follows the $readmemh
  // pattern of direct backing-store mutation.
  kDirectToDest,
};

// Writeback binding: routes an output parameter to its destination.
//
// Two modes controlled by WritebackKind:
//
// STAGED (default):
//   Call writes to tmp (via out-param pointer).
//   After call: CommitValue(dest, Load(tmp), type, kMove).
//   Backend allocates tmp as local storage.
//   Invariant: tmp.has_value() == true.
//
// DIRECT-TO-DEST:
//   Runtime receives dest pointer directly and writes in-place.
//   No tmp is allocated; no commit step.
//   Follows the bulk-write pattern used by $readmemh/$writemem.
//   Invariant: tmp == std::nullopt.
//
// Common fields:
// - dest: final destination Place for the output
// - type: output type for dispatch and ownership handling
// - mode: Out or InOut (Ref fails loud)
// - arg_index: formal parameter index (for ordering and debugging)
struct CallWriteback {
  std::optional<PlaceId> tmp;      // Staging temp (nullopt for kDirectToDest)
  PlaceId dest;                    // Final destination
  TypeId type;                     // Output type
  PassMode mode = PassMode::kOut;  // Out or InOut
  WritebackKind kind = WritebackKind::kStaged;  // Staging vs direct write
  int32_t arg_index = 0;  // Formal param index (for commit ordering)
};

// Scope classification for DPI export wrappers.
enum class DpiExportScopeKind : uint8_t {
  kPackage,
  kModule,
};

// Stable cross-session identity for a module-scoped export callee.
// Body-local FunctionIds are 0-based per body and can collide across bodies.
// Pairing with ModuleBodyId makes the identity design-global and unique.
struct ModuleExportCalleeKey {
  ModuleBodyId body_id;
  FunctionId function_id;
  auto operator==(const ModuleExportCalleeKey&) const -> bool = default;
};

struct ModuleExportCalleeKeyHash {
  auto operator()(const ModuleExportCalleeKey& k) const -> size_t {
    return std::hash<uint64_t>{}(
        (static_cast<uint64_t>(k.body_id.value) << 32) | k.function_id.value);
  }
};

// Resolved export target for wrapper emission.
// Package path uses package_symbol (design-global callable, resolved by
// SymbolId through the design function registry).
// Module path uses module_target (body_id + body-local function_id),
// resolved through the module-scoped function accumulator after Phase 4.
struct DpiExportTarget {
  DpiExportScopeKind scope_kind = DpiExportScopeKind::kPackage;
  SymbolId package_symbol;
  ModuleExportCalleeKey module_target;
};

// DPI export wrapper descriptor for LLVM backend emission.
// Binds a visible C name to the canonical DpiSignature and a resolved
// export target. Populated during MIR design lowering,
// deterministically sorted by c_name.
struct DpiExportWrapperDesc {
  std::string c_name;
  DpiSignature signature;
  DpiExportTarget target;
};

}  // namespace lyra::mir

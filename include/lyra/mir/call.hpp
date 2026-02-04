#pragma once

#include <cstdint>
#include <optional>
#include <variant>

#include "lyra/common/system_tf.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::mir {

// Callee discriminator: user function vs system TF.
// Uses existing SystemTfOpcode to avoid parallel dispatch universe.
using Callee = std::variant<FunctionId, SystemTfOpcode>;

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

}  // namespace lyra::mir

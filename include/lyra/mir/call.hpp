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

// Writeback binding: stages an output parameter through explicit temp.
//
// Contract:
// - tmp: MIR-visible PlaceId where call writes output (staging)
// - dest: final destination Place for the output
// - type: output type for dispatch and ownership handling
// - mode: Out or InOut (Ref fails loud)
// - arg_index: formal parameter index (for ordering and debugging)
//
// Execution flow:
// 1. Call writes to tmp (via out-param pointer)
// 2. After call: CommitValue(dest, Load(tmp), type, kMove)
struct CallWriteback {
  PlaceId tmp;                     // Staging temp (MIR-visible, preallocated)
  PlaceId dest;                    // Final destination
  TypeId type;                     // Output type
  PassMode mode = PassMode::kOut;  // Out or InOut
  int32_t arg_index = 0;           // Formal param index (for commit ordering)
};

}  // namespace lyra::mir

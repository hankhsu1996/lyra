#pragma once

#include <cstdint>
#include <variant>
#include <vector>

#include "lyra/mir/expr_id.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::mir {

// LRM 21.3.4.3 `$sscanf` / `$fscanf`. The runtime call is positioned
// inside a closure body synthesized by the lowering; each slot's `temp`
// references a procedural-local sink in that body. Lvalue semantics
// (observable structural vars, partial selectors, etc.) belong to the
// closure body's conditional writeback assignments, NOT to this call --
// the runtime sees only plain temp pointers plus per-slot type metadata
// and the matched-conversion count.

struct IntegralScanSlot {
  ExprId temp{};  // ProceduralVarRef to a PackedArray temp
  std::uint32_t bit_width{};
  bool is_signed{};
  bool is_four_state{};
};

struct StringScanSlot {
  ExprId temp{};  // ProceduralVarRef to a String temp
};

using ScanSlotDesc = std::variant<IntegralScanSlot, StringScanSlot>;

struct RuntimeScanCall {
  support::ScanSourceKind source_kind{};
  ExprId source{};
  ExprId format{};
  std::vector<ScanSlotDesc> slots;
};

}  // namespace lyra::mir

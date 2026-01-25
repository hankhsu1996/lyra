#include "lyra/mir/verify.hpp"

#include <cstdint>
#include <format>
#include <unordered_set>
#include <variant>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::mir {

namespace {

// Verify Return invariants in a list of basic blocks.
// is_void: true if the routine is void (no return value expected)
// routine_kind: "function" or "process" for error messages
void VerifyReturnInvariants(
    const std::vector<BasicBlock>& blocks, bool is_void,
    std::string_view routine_kind) {
  for (size_t i = 0; i < blocks.size(); ++i) {
    const auto& block = blocks[i];
    if (const auto* ret = std::get_if<Return>(&block.terminator.data)) {
      if (is_void) {
        if (ret->value.has_value()) {
          throw common::InternalError(
              "MIR verify", std::format(
                                "block {}: {} is void but Return has value", i,
                                routine_kind));
        }
      } else {
        if (!ret->value.has_value()) {
          throw common::InternalError(
              "MIR verify",
              std::format(
                  "block {}: {} is non-void but Return has no value", i,
                  routine_kind));
        }
      }
    }
  }
}

// Verify param_local_slots invariants for a function.
void VerifyParamLocalSlots(const Function& func) {
  if (func.param_local_slots.size() != func.signature.params.size()) {
    throw common::InternalError(
        "MIR verify",
        std::format(
            "param_local_slots.size() ({}) != signature.params.size() ({})",
            func.param_local_slots.size(), func.signature.params.size()));
  }

  std::unordered_set<uint32_t> seen_slots;
  for (size_t i = 0; i < func.param_local_slots.size(); ++i) {
    uint32_t slot = func.param_local_slots[i];

    if (slot >= func.local_types.size()) {
      throw common::InternalError(
          "MIR verify",
          std::format(
              "param_local_slots[{}] = {} is out of range (local_types.size() "
              "= {})",
              i, slot, func.local_types.size()));
    }

    if (seen_slots.contains(slot)) {
      throw common::InternalError(
          "MIR verify",
          std::format(
              "param_local_slots[{}] = {} is a duplicate (aliasing not "
              "allowed)",
              i, slot));
    }
    seen_slots.insert(slot);
  }
}

}  // namespace

void VerifyFunction(
    const Function& func, const Arena& /*arena*/, const TypeArena& types) {
  VerifyParamLocalSlots(func);

  const Type& ret_type = types[func.signature.return_type];
  bool is_void = ret_type.Kind() == TypeKind::kVoid;
  VerifyReturnInvariants(func.blocks, is_void, "function");
}

void VerifyProcess(
    const Process& proc, const Arena& /*arena*/, const TypeArena& /*types*/) {
  VerifyReturnInvariants(proc.blocks, true, "process");
}

}  // namespace lyra::mir

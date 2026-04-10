#include "lyra/mir/effect.hpp"

#include <variant>

#include "lyra/common/overloaded.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::mir {

auto GetEffectExecutionRequirement(const EffectOp& op)
    -> BodyExecutionRequirement {
  return std::visit(
      common::Overloaded{
          [](const RecordDecisionObservation&) {
            return BodyExecutionRequirement::kDeferredCheckOwnerRequired;
          },
          [](const RecordDecisionObservationDynamic&) {
            return BodyExecutionRequirement::kDeferredCheckOwnerRequired;
          },
          [](const EnqueueDeferredAssertionEffect&) {
            return BodyExecutionRequirement::kDeferredCheckOwnerRequired;
          },
          [](const auto&) {
            return BodyExecutionRequirement::kGenericCallable;
          },
      },
      op);
}

auto ComputeBodyExecutionRequirement(const Function& func)
    -> BodyExecutionRequirement {
  for (const auto& block : func.blocks) {
    for (const auto& stmt : block.statements) {
      const auto* effect = std::get_if<Effect>(&stmt.data);
      if (effect == nullptr) continue;
      if (GetEffectExecutionRequirement(effect->op) ==
          BodyExecutionRequirement::kDeferredCheckOwnerRequired) {
        return BodyExecutionRequirement::kDeferredCheckOwnerRequired;
      }
    }
  }
  return BodyExecutionRequirement::kGenericCallable;
}

auto BuildCallableAbiContract(const Function& func) -> CallableAbiContract {
  CallableAbiContract contract;
  if (IsObserverProgram(func.runtime_meta)) {
    return contract;
  }
  // Seed: accept decision owner iff this body directly contains
  // deferred-check-owner-required effects. PropagateDeferredOwnerAbi()
  // extends this to callees that transitively need it.
  if (func.body_requirement ==
      BodyExecutionRequirement::kDeferredCheckOwnerRequired) {
    contract.accepts_decision_owner = true;
  }
  return contract;
}

auto CallsCalleeAcceptingDecisionOwner(
    const Function& func, const std::vector<Function>& local_functions,
    const Arena* design_arena) -> bool {
  for (const auto& block : func.blocks) {
    for (const auto& stmt : block.statements) {
      const auto* call = std::get_if<Call>(&stmt.data);
      if (call == nullptr) continue;

      // Check local FunctionId edges.
      const auto* local_id = std::get_if<FunctionId>(&call->callee);
      if (local_id != nullptr) {
        if (local_id->value < local_functions.size() &&
            local_functions[local_id->value]
                .abi_contract.accepts_decision_owner) {
          return true;
        }
        continue;
      }

      // Check design-global DesignFunctionRef edges.
      if (design_arena != nullptr) {
        const auto* ref = std::get_if<DesignFunctionRef>(&call->callee);
        if (ref != nullptr) {
          // Look up the design-global function by scanning for matching
          // canonical_symbol. Design arena functions are indexed by FunctionId
          // but we only have SymbolId here.
          for (uint32_t i = 0; i < design_arena->FunctionCount(); ++i) {
            const auto& design_func = (*design_arena)[FunctionId{i}];
            if (design_func.canonical_symbol == ref->symbol &&
                design_func.abi_contract.accepts_decision_owner) {
              return true;
            }
          }
        }
      }
    }
  }
  return false;
}

}  // namespace lyra::mir

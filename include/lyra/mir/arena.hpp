#pragma once

#include <vector>

#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::mir {

class Arena final {
 public:
  Arena() = default;
  ~Arena() = default;

  Arena(const Arena&) = delete;
  auto operator=(const Arena&) -> Arena& = delete;

  Arena(Arena&&) = default;
  auto operator=(Arena&&) -> Arena& = default;

  auto AddPlace(Place place) -> PlaceId {
    PlaceId id{static_cast<uint32_t>(places_.size())};
    places_.push_back(std::move(place));
    return id;
  }

  // Creates a derived place by appending a projection to a resolved base.
  // Caller resolves the base Place from the correct arena. The derived
  // result is allocated in this arena. This avoids cross-arena PlaceId
  // indexing -- the caller is responsible for domain-aware resolution.
  auto DerivePlace(const Place& base_place, Projection proj) -> PlaceId {
    Place new_place{
        .root = base_place.root,
        .projections = base_place.projections,
    };
    new_place.projections.push_back(std::move(proj));
    return AddPlace(std::move(new_place));
  }

  auto AddProcess(Process proc) -> ProcessId {
    ProcessId id{static_cast<uint32_t>(processes_.size())};
    processes_.push_back(std::move(proc));
    return id;
  }

  // Reserve a ProcessId for pre-allocation.
  auto ReserveProcess() -> ProcessId {
    ProcessId id{static_cast<uint32_t>(processes_.size())};
    processes_.emplace_back();
    return id;
  }

  // Fill in a previously reserved process's body.
  void SetProcessBody(ProcessId id, Process proc) {
    processes_[id.value] = std::move(proc);
  }

  auto AddFunction(Function func) -> FunctionId {
    FunctionId id{static_cast<uint32_t>(functions_.size())};
    functions_.push_back(std::move(func));
    return id;
  }

  // Reserve a FunctionId with a frozen signature (pre-allocation for
  // recursion). The signature is immutable after this point.
  // canonical_symbol identifies design-global callables for backend dispatch.
  auto ReserveFunction(
      FunctionSignature signature, SymbolId canonical_symbol = kInvalidSymbolId)
      -> FunctionId {
    FunctionId id{static_cast<uint32_t>(functions_.size())};
    Function placeholder;
    placeholder.signature = std::move(signature);
    placeholder.canonical_symbol = canonical_symbol;
    functions_.push_back(std::move(placeholder));
    return id;
  }

  // Fill in a previously reserved function's body. Preserves the frozen
  // signature and canonical_symbol from ReserveFunction.
  // Computes intrinsic body_requirement from this function's own effects only.
  // ABI contract is seeded here but may need propagation via
  // PropagateDeferredOwnerAbi() after all bodies are set.
  void SetFunctionBody(FunctionId id, Function func) {
    func.signature = std::move(functions_[id.value].signature);
    func.canonical_symbol = functions_[id.value].canonical_symbol;
    func.body_requirement = ComputeBodyExecutionRequirement(func);
    func.abi_contract = BuildCallableAbiContract(func);
    functions_[id.value] = std::move(func);
  }

  // Mark a function as module-scoped. Called for all functions in a module
  // body after their bodies are set. Sets needs_module_binding on the
  // function's ABI contract so the backend can read it directly from the arena.
  void MarkModuleScoped(FunctionId id) {
    functions_[id.value].abi_contract.needs_module_binding = true;
  }

  // Propagate accepts_decision_owner through internal call graph.
  // If function A calls function B (local or design-global), and B accepts
  // a decision owner, then A must also accept (to carry decision_owner_id).
  // body_requirement is NOT mutated -- it stays intrinsic.
  // design_arena: optional, for resolving DesignFunctionRef cross-arena edges.
  // Pass nullptr for the design arena's own propagation (no cross-arena refs).
  void PropagateDeferredOwnerAbi(const Arena* design_arena = nullptr) {
    bool changed = true;
    while (changed) {
      changed = false;
      for (auto& func : functions_) {
        if (func.abi_contract.accepts_decision_owner) continue;
        if (IsObserverProgram(func.runtime_kind)) continue;
        if (CallsCalleeAcceptingDecisionOwner(func, functions_, design_arena)) {
          func.abi_contract.accepts_decision_owner = true;
          changed = true;
        }
      }
    }
  }

  [[nodiscard]] auto operator[](PlaceId id) const -> const Place& {
    return places_[id.value];
  }

  [[nodiscard]] auto operator[](ProcessId id) const -> const Process& {
    return processes_[id.value];
  }

  // Mutable access for MIR optimization passes.
  [[nodiscard]] auto GetMutableProcess(ProcessId id) -> Process& {
    return processes_[id.value];
  }

  [[nodiscard]] auto operator[](FunctionId id) const -> const Function& {
    return functions_[id.value];
  }

  // Mutable access for MIR optimization passes.
  [[nodiscard]] auto GetMutableFunction(FunctionId id) -> Function& {
    return functions_[id.value];
  }

  [[nodiscard]] auto PlaceCount() const -> size_t {
    return places_.size();
  }
  [[nodiscard]] auto FunctionCount() const -> size_t {
    return functions_.size();
  }

 private:
  std::vector<Place> places_;
  std::vector<Process> processes_;
  std::vector<Function> functions_;
};

}  // namespace lyra::mir

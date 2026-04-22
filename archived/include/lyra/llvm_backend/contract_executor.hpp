#pragma once

#include <cstdint>
#include <unordered_map>
#include <vector>

#include "lyra/llvm_backend/activation_local.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/llvm_backend/slot_access.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;

// Executes the activation-local contract for one process.
// Owns resolver creation, block-to-segment mapping, and per-statement
// contract execution. Process lowering calls the executor API instead
// of manually interpreting the contract plan.
class ContractExecutor {
 public:
  ContractExecutor(
      Context& ctx, const CuFacts& facts, const ProcessActivationPlan& plan,
      std::vector<ManagedSlotStorage> storage);

  // Get the resolver and lifecycle for a given block.
  // Returns canonical resolver/lifecycle for blocks not in any
  // managed segment.
  auto GetResolver(uint32_t block_index) -> SlotAccessResolver&;
  auto GetLifecycle(uint32_t block_index) -> SegmentLifecycle&;

  // Execute block-entry actions from the contract.
  // Call at the start of each block, after SetInsertPoint.
  void ExecuteBlockEntry(uint32_t block_index);

  // Execute pre-statement contract action (sync), if any.
  // Call before lowering each statement.
  void ExecutePreStatement(uint32_t block_index, uint32_t statement_index);

  // Execute post-statement contract action (reload), if any.
  // Call after lowering each statement.
  void ExecutePostStatement(uint32_t block_index, uint32_t statement_index);

  // Execute block-exit actions from the contract.
  // Call before lowering the terminator.
  void ExecuteBlockExit(uint32_t block_index);

 private:
  auto FindSegmentIndex(uint32_t block_index) const -> std::optional<size_t>;
  auto FindStatementContract(uint32_t block_index, uint32_t statement_index)
      const -> const StatementContractPlan*;
  auto GetOrCreateResolver(size_t segment_index) -> ActivationLocalSlotAccess&;

  Context& ctx_;
  const CuFacts& facts_;
  const ProcessActivationPlan& plan_;
  std::vector<ManagedSlotStorage> storage_;
  CanonicalSlotAccess canonical_;
  std::unordered_map<uint32_t, size_t> block_to_segment_;
  std::unordered_map<size_t, ActivationLocalSlotAccess> segment_resolvers_;
};

}  // namespace lyra::lowering::mir_to_llvm

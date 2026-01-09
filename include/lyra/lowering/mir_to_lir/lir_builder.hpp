#pragma once

#include <memory>
#include <string>
#include <vector>

#include <fmt/core.h>

#include "lyra/common/literal.hpp"
#include "lyra/lir/basic_block.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/module.hpp"
#include "lyra/lir/process.hpp"

namespace lyra::lowering::mir_to_lir {

/// Procedural context in which code is being lowered. Determines how local
/// variables and control flow are handled.
enum class ProceduralContext {
  kModule,    // Module-level (not in any procedural block)
  kProcess,   // Inside a process (initial/always block)
  kFunction,  // Inside a user-defined function
  // Future: kTask, kForkJoin
};

class LirBuilder {
 public:
  explicit LirBuilder(
      std::string module_name, std::shared_ptr<lir::LirContext> context);

  // Module interface
  void BeginModule();
  void AddModuleVariable(const common::Variable& variable);
  void AddPort(const common::Variable& variable, lir::PortDirection direction);
  void AddSubmodule(const lir::SubmoduleInstance& submodule);
  auto EndModule() -> std::unique_ptr<lir::Module>;

  // Process interface
  void BeginProcess(const std::string& name);
  void EndProcess();

  // Function interface (for user-defined functions)
  void BeginFunction(const std::string& name);
  void EndFunction();
  auto TakeFunctionBlocks() -> std::vector<std::unique_ptr<lir::BasicBlock>>;

  // Context-aware variable registration
  // In process context: registers variable in process's local_variables
  // In function context: no-op (function locals are pre-registered in MIR)
  void RegisterLocalVariable(const common::Variable& variable);

  // Query current procedural context
  [[nodiscard]] auto GetProceduralContext() const -> ProceduralContext {
    return procedural_context_;
  }

  // Basic block interface
  void StartBlock(lir::LabelRef label);
  void EndBlock();

  // Instruction interface
  void AddInstruction(lir::Instruction instruction);
  auto AllocateTemp(const std::string& hint, common::Type type) -> lir::TempRef;
  auto InternLiteral(const common::Literal& literal) -> lir::LiteralRef;
  [[nodiscard]] auto GetContext() const -> std::shared_ptr<lir::LirContext> {
    return context_;
  }

  // Additional helpers
  auto MakeLabel(const std::string& hint) -> lir::LabelRef;

  // Add a monitor expression block and return its index.
  // Used by $monitor to store re-evaluable expressions.
  auto AddMonitorExpressionBlock(lir::MonitorExpressionBlock block) -> size_t;

  // Get the current number of instructions in the current block.
  // Used to mark the start of an expression for capture.
  [[nodiscard]] auto GetCurrentBlockInstructionCount() const -> size_t;

  // Copy instructions added since the given start index.
  // Used to capture instructions for MonitorExpressionBlocks.
  [[nodiscard]] auto CopyInstructionsSince(size_t start_index) const
      -> std::vector<lir::Instruction>;

 private:
  auto InternLabel(const std::string& name) -> lir::LabelRef;

  std::string module_name_;
  std::unique_ptr<lir::Module> module_;
  std::shared_ptr<lir::LirContext> context_;

  std::vector<std::shared_ptr<lir::Process>> processes_;
  std::shared_ptr<lir::Process> current_process_;

  std::unique_ptr<lir::BasicBlock> current_block_;
  std::vector<std::unique_ptr<lir::BasicBlock>> current_blocks_;

  int label_counter_ = 0;
  int temp_counter_ = 0;

  // Current procedural context
  ProceduralContext procedural_context_ = ProceduralContext::kModule;
  std::vector<std::unique_ptr<lir::BasicBlock>> function_blocks_;
};

}  // namespace lyra::lowering::mir_to_lir

#include "lyra/lowering/mir_to_lir/link.hpp"

#include <memory>
#include <span>
#include <string>
#include <unordered_map>

#include "lyra/common/internal_error.hpp"
#include "lyra/lir/basic_block.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/module.hpp"
#include "lyra/lir/process.hpp"

namespace lyra::lowering::mir_to_lir {

namespace {

/// Resolves kCall instructions in a basic block.
void LinkBlockCalls(
    lir::BasicBlock& block, const lir::Module& module,
    const std::unordered_map<std::string, const lir::Function*>&
        package_functions) {
  for (auto& instr : block.instructions) {
    if (instr.kind != lir::InstructionKind::kCall) {
      continue;
    }
    if (instr.callee != nullptr) {
      continue;  // Already resolved
    }

    const auto& name = instr.called_function_name;

    // If name contains "::", it's a package function
    if (name.find("::") != std::string::npos) {
      auto it = package_functions.find(name);
      if (it == package_functions.end()) {
        throw common::InternalError(
            "LinkFunctionCalls",
            std::string("package function '") + name + "' not found");
      }
      instr.callee = it->second;
    } else {
      // Module-local function
      instr.callee = module.FindFunction(name);
      if (instr.callee == nullptr) {
        throw common::InternalError(
            "LinkFunctionCalls", std::string("function '") + name +
                                     "' not found in module '" + module.name +
                                     "'");
      }
    }
  }
}

/// Resolves kCall instructions in all blocks of a process.
void LinkProcessCalls(
    lir::Process& process, const lir::Module& module,
    const std::unordered_map<std::string, const lir::Function*>&
        package_functions) {
  for (auto& block : process.blocks) {
    LinkBlockCalls(*block, module, package_functions);
  }
}

/// Resolves kCall instructions in all blocks of a module function.
void LinkModuleFunctionCalls(
    lir::Function& function, const lir::Module& module,
    const std::unordered_map<std::string, const lir::Function*>&
        package_functions) {
  for (auto& block : function.blocks) {
    LinkBlockCalls(*block, module, package_functions);
  }
}

/// Resolves kCall instructions in a basic block (package function context).
/// Only looks up in package_functions, not module functions.
void LinkBlockCallsInPackage(
    lir::BasicBlock& block,
    const std::unordered_map<std::string, const lir::Function*>&
        package_functions) {
  for (auto& instr : block.instructions) {
    if (instr.kind != lir::InstructionKind::kCall) {
      continue;
    }
    if (instr.callee != nullptr) {
      continue;  // Already resolved
    }

    const auto& name = instr.called_function_name;

    // In package functions, all calls are to other package functions
    auto it = package_functions.find(name);
    if (it == package_functions.end()) {
      throw common::InternalError(
          "LinkFunctionCalls",
          std::string("package function '") + name + "' not found");
    }
    instr.callee = it->second;
  }
}

/// Resolves kCall instructions in all blocks of a package function.
void LinkPackageFunctionCalls(
    lir::Function& function,
    const std::unordered_map<std::string, const lir::Function*>&
        package_functions) {
  for (auto& block : function.blocks) {
    LinkBlockCallsInPackage(*block, package_functions);
  }
}

}  // namespace

void LinkFunctionCalls(
    std::span<const std::unique_ptr<lir::Module>> modules,
    std::span<const std::unique_ptr<lir::Function>> package_functions) {
  // Build package function lookup map
  std::unordered_map<std::string, const lir::Function*> pkg_func_map;
  for (const auto& func : package_functions) {
    pkg_func_map[func->name] = func.get();
  }

  // Resolve function calls in package functions themselves
  for (const auto& func : package_functions) {
    LinkPackageFunctionCalls(*func, pkg_func_map);
  }

  // Resolve function calls in all modules
  for (const auto& module : modules) {
    // Link calls in processes
    for (auto& process : module->processes) {
      LinkProcessCalls(*process, *module, pkg_func_map);
    }

    // Link calls in module-local functions
    for (auto& function : module->functions) {
      LinkModuleFunctionCalls(function, *module, pkg_func_map);
    }
  }
}

void LinkSubmodules(std::span<const std::unique_ptr<lir::Module>> modules) {
  // Build signature → module lookup map
  std::unordered_map<std::string, const lir::Module*> module_map;
  for (const auto& module : modules) {
    module_map[module->signature] = module.get();
  }

  // Resolve submodule references in all modules
  for (const auto& module : modules) {
    for (auto& submod : module->submodules) {
      auto it = module_map.find(submod.module_signature);
      if (it == module_map.end()) {
        throw common::InternalError(
            "LinkSubmodules",
            std::string("module '") + submod.module_signature + "' not found");
      }
      submod.child_module = it->second;
    }
  }
}

}  // namespace lyra::lowering::mir_to_lir

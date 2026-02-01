#pragma once

#include <format>
#include <vector>

#include <llvm/IR/Function.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/raw_ostream.h>

#include "lyra/common/internal_error.hpp"

namespace lyra::lowering::mir_to_llvm {

// Debug-only helper to verify call ABI before LLVM's internal assert.
// Guarded by NDEBUG so release builds have zero overhead.
inline void VerifyCallAbi(
    [[maybe_unused]] llvm::Function* callee,
    [[maybe_unused]] const std::vector<llvm::Value*>& args,
    [[maybe_unused]] const char* debug_tag) {
#ifndef NDEBUG
  auto* fn_type = callee->getFunctionType();

  // Print full diagnostic on any mismatch
  auto print_diagnostic = [&](const char* reason, int mismatch_idx) {
    llvm::errs() << "=== ABI MISMATCH [" << debug_tag << "] ===\n";
    llvm::errs() << "Reason: " << reason << "\n";
    llvm::errs() << "Mismatch index: " << mismatch_idx << "\n";
    llvm::errs() << "Callee: " << callee->getName() << "\n";

    // Print FunctionType on single line
    llvm::errs() << "FunctionType: ";
    fn_type->print(llvm::errs());
    llvm::errs() << "\n";

    // Print each arg type
    llvm::errs() << "Args provided (" << args.size() << "):\n";
    for (size_t i = 0; i < args.size(); ++i) {
      llvm::errs() << "  [" << i << "]: ";
      args[i]->getType()->print(llvm::errs());
      llvm::errs() << "\n";
    }

    // Print expected param types
    llvm::errs() << "Expected params (" << fn_type->getNumParams() << "):\n";
    for (unsigned i = 0; i < fn_type->getNumParams(); ++i) {
      llvm::errs() << "  [" << i << "]: ";
      fn_type->getParamType(i)->print(llvm::errs());
      llvm::errs() << "\n";
    }
  };

  // Check arg count
  if (args.size() != fn_type->getNumParams()) {
    print_diagnostic("arg count mismatch", 0);
    throw common::InternalError(
        "VerifyCallAbi", std::format(
                             "arg count mismatch: got {}, expected {}",
                             args.size(), fn_type->getNumParams()));
  }

  // Check each arg type
  for (size_t i = 0; i < args.size(); ++i) {
    if (args[i]->getType() != fn_type->getParamType(i)) {
      print_diagnostic("arg type mismatch", static_cast<int>(i));
      throw common::InternalError(
          "VerifyCallAbi", std::format("arg type mismatch at index {}", i));
    }
  }
#endif
}

}  // namespace lyra::lowering::mir_to_llvm

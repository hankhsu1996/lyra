#pragma once

#include <optional>

#include "lyra/support/machine_signature.hpp"

namespace llvm {
class Function;
class LLVMContext;
class Type;
}  // namespace llvm

namespace lyra::backend::llvm_backend {

// The LLVM type a value of `kind` is held in.
[[nodiscard]] auto ToLlvmType(
    llvm::LLVMContext& context, support::MachineKind kind) -> llvm::Type*;

// The machine kind a value of the LLVM type `type` crosses as, where it is one
// of them.
[[nodiscard]] auto MachineKindOf(llvm::Type* type)
    -> std::optional<support::MachineKind>;

// A predicate crosses between generated code and C++ as a `bool`, which C++
// reads as a whole byte, so every machine boolean `function` takes or answers
// is marked zero-extended, as clang marks a `bool` wherever it declares a
// function.
void MarkBoolsAsCpp(llvm::Function* function);

}  // namespace lyra::backend::llvm_backend

#pragma once

#include <optional>
#include <variant>
#include <vector>

#include "lyra/backend/llvm/fn_abi.hpp"
#include "lyra/backend/llvm/target_lowering.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type_id.hpp"

namespace llvm {
class Type;
}  // namespace llvm

namespace lyra::backend::llvm_backend {

class CodeGenModule;
class CodeGenTypes;

// One argument of a call at machine types: the type its value crosses at, and
// what crosses with it.
struct LlvmArgAbi {
  llvm::Type* type;
  PassMode mode;
};

// How a callee's answer comes back, the role clang gives the `ABIArgInfo` of a
// function's return: as the machine value it is, or built in storage the
// caller gives, which the callee is handed last. A callee building its answer
// there returns a machine value too: the address of that storage, or a count
// where the entry answers one beside the value it lays out.
struct ReturnDirect {
  llvm::Type* type;
};
struct ReturnIndirect {
  llvm::Type* returned;
};
using ReturnInfo = std::variant<ReturnDirect, ReturnIndirect>;

// How one call crosses to its callee at machine types, the role of clang's
// `CGFunctionInfo`: an `FnAbi` with the machine type of each argument and how
// the answer comes back. The machine type a callee is declared at is a function
// of this and of nothing else.
struct LlvmFnAbi {
  ImplicitArg implicit_arg;
  std::optional<NamedPartArg> named_part;
  std::vector<LlvmArgAbi> args;
  std::vector<TypeArg> type_args;
  ReturnInfo ret;
};

// `abi` at machine types, its answer coming back as `ret`.
auto AtMachineTypes(CodeGenModule& module, FnAbi abi, ReturnInfo ret)
    -> LlvmFnAbi;

// Arranges calls at machine types, apart from emitting them, as clang keeps
// `CodeGenTypes::arrange*` apart from `CodeGenFunction::EmitCall`. Nothing is
// emitted and no value is read.
class CallArranger {
 public:
  CallArranger(CodeGenModule& module, const TargetLowering& lowering);

  // How `call`, answering a value of `result_type`, crosses at machine types.
  [[nodiscard]] auto LlvmFnAbiOf(
      const lir::CallInstr& call, lir::TypeId result_type) const
      -> diag::Result<LlvmFnAbi>;
  // A call this target makes on its own account, stating no call of the layer
  // above: `args` in order, answering as `ret` says, with nothing led by and
  // nothing told of a type.
  [[nodiscard]] static auto Arrange(
      std::vector<LlvmArgAbi> args, ReturnInfo ret) -> LlvmFnAbi;
  // An argument that is the machine value of `type` it is: an address this
  // target computed, or a count it states.
  [[nodiscard]] static auto Direct(llvm::Type* type) -> LlvmArgAbi;
  // How a callee making a value of `type` hands it back, the one return
  // arrangement its declaration, its body and every call of it read, as
  // clang's `CGFunctionInfo` is: built in storage the caller gives where the
  // value lies in storage, and as the machine value it is otherwise.
  [[nodiscard]] static auto ReturnOfValue(CodeGenTypes& types, lir::TypeId type)
      -> ReturnInfo;

 private:
  // How the answer of `call`, a value of `result_type`, comes back: built in
  // storage the caller gives where the call makes a value its caller owns,
  // and as the machine value it is otherwise.
  [[nodiscard]] auto ReturnOf(
      const lir::CallInstr& call, lir::TypeId result_type) const -> ReturnInfo;
  CodeGenModule* module_;
  const TargetLowering* lowering_;
};

}  // namespace lyra::backend::llvm_backend

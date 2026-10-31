#pragma once

#include <cstdint>
#include <functional>
#include <map>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

#include <llvm/ADT/ArrayRef.h>
#include <llvm/IR/GlobalValue.h>

#include "lyra/backend/llvm/codegen_types.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/machine_signature.hpp"
#include "lyra/support/value_operation.hpp"
#include "lyra/value/layout.hpp"

namespace llvm {
class Constant;
class Function;
class GlobalVariable;
class IRBuilderBase;
class Type;
class Value;
}  // namespace llvm

namespace lyra::lir {
struct CompilationUnit;
}  // namespace lyra::lir

namespace lyra::backend::llvm_backend {

class CodeGenModule;

// The constant stating to the runtime each type a module lays out whose
// operations the design compiles: a product, a value made of several
// components at once -- an unpacked structure (LRM 7.2), or a tuple a lowering
// composes, such as a call's answer, its result together with its `output` and
// `inout` arguments (LRM 13.5) -- a union or a tagged union, one storage its
// members overlay (LRM 7.3), and a fixed-size unpacked array (LRM 7.4.2). The
// type is how the runtime, compiled before any of these types existed, reaches
// what every holder of a value may ask of it: the steps of its life the unit
// states for it, and whether a new value differs from the one held. Every
// other operation the language defines on the whole value -- comparing and
// streaming it, folding it in a net, taking some of its positions -- is handed
// to the routine that needs it. Each is a method a record's declaration states
// or a body a unit states for an array. Such a value's bytes state nothing of
// it, so whatever hands one to the runtime hands the type beside it. It is laid
// out as the record the library declares for its kind, opening with the address
// of a table of this module's bodies, one where the table's declaration lays
// out each function.
//
// The type of a struct or a union is its declaring unit's, one in the
// program, which every other module refers to. An array's and an integral
// type's are stated by every module naming them under one name, of which the
// linker keeps one; a tuple's is each module's own.
class CodeGenValueTypes {
 public:
  CodeGenValueTypes(
      CodeGenModule& module, CodeGenTypes& types,
      const lir::CompilationUnit& unit);

  // At `b`, every position above an integral's width in the value of `type`
  // lying at `at` made clear: inline for an integral, and otherwise a call of
  // the function the type has for it, which clears each component, each
  // element, or the member a tagged union's tag names in turn. A type that
  // keeps no position clear emits nothing.
  void EmitClearAboveWidths(
      llvm::IRBuilderBase& b, lir::TypeId type, llvm::Value* at);

  // The constant stating `type` to the runtime, for a type laid out by the
  // design.
  auto GetAddrOfLaidOutType(lir::TypeId type) -> llvm::GlobalVariable*;
  // The constant stating the integral type `type` to the runtime: the record
  // the library declares for an integral type, naming the table every
  // integral type answers through and stating the width, signedness and states
  // that table's functions read. One integral type is one constant in the
  // program, as one array type is.
  auto GetAddrOfIntegralType(lir::TypeId type) -> llvm::Constant*;
  // The constant stating how a net folds values of `type`, a type laid out by
  // the design: the type's own function for each fold, where the library's
  // declaration lays the fold out. A net of the type is built holding it, and
  // nothing else asks for it.
  auto GetAddrOfNetFolds(lir::TypeId type) -> llvm::Constant*;
  // The function taking some positions of a value of `type`, a type laid out
  // by the design, from another (LRM 10.6.2), as the library's declaration of
  // it takes them: the type's own function, entered with the type first. A
  // takeover is handed it where it begins.
  auto GetAddrOfTakePositions(lir::TypeId type) -> llvm::Constant*;
  // The operations of `type` the library compares and streams a value of it
  // by: the functions, the type, and a container's element's operations. A
  // library kind no other type parameterizes states its own; every other type
  // a call names has them stated here, as data naming the functions of its
  // kind or, for a type laid out by the design, the functions this module
  // states for it.
  auto GetAddrOfOperations(lir::TypeId type) -> diag::Result<llvm::Constant*>;

  // The type the library asks of a value of `type`: one this module lays out,
  // for a type laid out by the design or an integral type, or the library's
  // own for a value of one of its kinds.
  auto GetAddrOfValueType(lir::TypeId type) -> diag::Result<llvm::Constant*>;
  // The constant that states `element` to the library as the type of an
  // array's elements, or of a value a holder keeps: the type, and the function
  // that builds the value one starts as (LRM Table 6-7, 7-1). A record's is its
  // declaring unit's, since its default is its declaration's; the module
  // defines one per other such type it hands over.
  auto GetAddrOfElementType(lir::TypeId element)
      -> diag::Result<llvm::Constant*>;
  // The constant that states the queue type `queue` to the library: the
  // constant stating its elements' type, and the bound it declares (LRM
  // 7.10.5). The module defines one per queue type it hands over.
  auto GetAddrOfQueueType(lir::TypeId queue) -> diag::Result<llvm::Constant*>;

  // The type of every struct and union this unit declares, and what another
  // unit hands the library of one -- its operations, how a value of it starts,
  // how a net folds values of it, how a takeover takes some of its positions --
  // which another unit may refer to whether or not this one builds a value of
  // it.
  auto EmitDeclared() -> void;

 private:
  // The type constant `named` stands for, defined as `content` at `align`, as
  // the library's record of it is laid out: it takes the name and every use of
  // `named`, which a body emitted while filling it may hold, and `named` goes.
  auto Defined(
      const std::string& key, llvm::GlobalVariable* named,
      llvm::Constant* content, std::size_t align) -> llvm::GlobalVariable*;
  // How what this module states of `type` -- its type, its operations, how a
  // value of it starts, how a net folds values of it, how a takeover takes some
  // of its positions -- is linked. A struct's or a union's is its declaring
  // unit's, linked as its declaration is, since that unit states its methods.
  // An array is its count and its element wherever it is named, so every
  // module naming it states the same constants under the same names and the
  // linker keeps one, as it keeps one of a template's static data member. A
  // tuple's is each module's own. Nothing where another unit declares the
  // type, which this one only refers to.
  [[nodiscard]] auto LinkageOfItsOperations(lir::TypeId type) const
      -> std::optional<llvm::GlobalValue::LinkageTypes>;
  // The name a type has in every unit, which tells one type's code and
  // constants from another's.
  auto KeyOf(lir::TypeId type) -> const std::string&;
  // A function of this unit answering one operation of the type, and the
  // operation it answers.
  struct OwnOperation {
    support::ValueOperation answers;
    lir::FunctionId function;
  };
  // The functions the type answers its operations with: a record's methods,
  // and the bodies the unit states for an array or a tuple.
  [[nodiscard]] auto OwnOperations(lir::TypeId type) const
      -> std::vector<OwnOperation>;
  // The function of this unit answering `operation` for `type`, where the type
  // states one.
  [[nodiscard]] auto OwnOperationOf(
      lir::TypeId type, support::ValueOperation operation) const
      -> std::optional<lir::FunctionId>;
  // At `b`, a branch per one of `members` of a tagged union lying at `value`
  // with its tag `placed`, each emitting what `held` emits for the member the
  // tag names, and nothing where it names none; `b` is left after the branches
  // meet.
  static void ForHeldMember(
      llvm::IRBuilderBase& b, std::span<const lir::TypeId> members,
      const value::TagPlacement& placed, llvm::Value* value,
      const std::function<void(llvm::IRBuilderBase&, lir::TypeId)>& held);
  // A body for the witness `witness` of signature `stands_in_for`, declared
  // and left for its caller to fill: it takes the type it is handed and then
  // what that witness takes, and answers what it answers.
  auto DeclareBody(
      lir::TypeId type, std::string_view witness,
      const support::MachineSignature& stands_in_for) -> llvm::Function*;
  // The body the type's table holds as the witness `witness`, which does what
  // `filling` does, or the library's failing of an operation the language
  // does not define for the type where nothing fills it.
  auto Body(
      lir::TypeId type, std::string_view witness,
      const support::MachineSignature& stands_in_for, llvm::Function* filling)
      -> llvm::Constant*;
  // A body doing what a type's own function does where the two differ in how
  // one integral value crosses, opened and left for its caller to fill: it
  // holds storage for a value of the integral type `stated`, which the function
  // takes or answers.
  struct ThunkBody;
  // The type of the value the type's own function `method` takes its parameter
  // `index` as a reference to.
  [[nodiscard]] auto ParamReferent(
      lir::FunctionId method, std::size_t index) const -> lir::TypeId;
  auto OpenThunkBody(
      lir::TypeId type, std::string_view witness,
      const support::MachineSignature& stands_in_for, lir::TypeId stated)
      -> ThunkBody;
  // What a witness whose method answers a comparison answers: the scalar the
  // comparison came to, or whether it holds where it is never unknown (LRM
  // 11.4.5).
  enum class ComparisonAnswered : std::uint8_t {
    kAsItsScalar,
    kAsWhetherItHolds,
  };
  // The body of a witness whose method answers a comparison; of the one whose
  // method answers the value's stream of bits, and of the one whose method
  // takes a stream, each at a width the type fixes or as a run of bits; and of
  // the one whose method takes a count's control bits.
  auto ScalarAnswerBody(
      lir::TypeId type, std::string_view witness,
      const support::MachineSignature& stands_in_for, lir::FunctionId method,
      ComparisonAnswered answered) -> llvm::Constant*;
  auto StreamWriteBody(
      lir::TypeId type, std::string_view witness,
      const support::MachineSignature& stands_in_for, lir::FunctionId method)
      -> llvm::Constant*;
  auto StreamReadBody(
      lir::TypeId type, std::string_view witness,
      const support::MachineSignature& stands_in_for, lir::FunctionId method)
      -> llvm::Constant*;
  auto RunWriteBody(
      lir::TypeId type, std::string_view witness,
      const support::MachineSignature& stands_in_for, lir::FunctionId method)
      -> llvm::Constant*;
  auto RunBuildBody(
      lir::TypeId type, std::string_view witness,
      const support::MachineSignature& stands_in_for, lir::FunctionId method)
      -> llvm::Constant*;
  auto ControlReadBody(
      lir::TypeId type, std::string_view witness,
      const support::MachineSignature& stands_in_for, lir::FunctionId method)
      -> llvm::Constant*;
  // The function clearing every position above a width in a value of `type`,
  // made once per type.
  auto ClearFunction(lir::TypeId type) -> llvm::Function*;
  // At `b`, a loop over every element of the array `array` lying at `value`,
  // in position order, each emitting what `each` emits for the element's
  // address; `b` is left after the loop.
  void ForEachElement(
      llvm::IRBuilderBase& b, lir::TypeId array, llvm::Value* value,
      const std::function<void(llvm::IRBuilderBase&, llvm::Value*)>& each);
  // The integral value of type `read` that the stream `stream` holds below
  // its `taken` most significant positions, laid out at `laid_out`, answering
  // how many positions are taken after it.
  auto ReadOutOfStream(
      llvm::IRBuilderBase& b, llvm::Value* stream, llvm::Value* taken,
      llvm::Value* laid_out, lir::TypeId read) -> llvm::Value*;
  // A call of the library entry `op` with `args` as they stand, answering a
  // value of `answers`.
  auto ForwardToEntry(
      llvm::IRBuilderBase& b, RuntimeOp op, llvm::Type* answers,
      llvm::ArrayRef<llvm::Value*> args) -> llvm::Value*;

  CodeGenModule* owner_;
  CodeGenTypes* types_;
  const lir::CompilationUnit* unit_;
  std::unordered_map<lir::TypeId, std::string> keys_;
  std::map<std::string, llvm::Function*> clear_functions_;
  std::map<std::string, llvm::GlobalVariable*> types_of_;
  std::map<std::string, llvm::GlobalVariable*> integral_types_;
  std::map<std::string, llvm::Constant*> net_folds_of_;
  std::map<std::string, llvm::Constant*> take_positions_of_;
  std::map<std::string, llvm::GlobalVariable*> operation_functions_of_;
  std::map<std::string, llvm::Constant*> operations_of_;
  std::unordered_map<lir::TypeId, llvm::Constant*> element_types_;
  std::unordered_map<lir::TypeId, llvm::GlobalVariable*> queue_types_;
};

}  // namespace lyra::backend::llvm_backend

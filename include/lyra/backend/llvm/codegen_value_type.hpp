#pragma once

#include <cstdint>
#include <functional>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

#include <llvm/ADT/ArrayRef.h>

#include "lyra/backend/llvm/codegen_types.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_operation.hpp"

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

// The machine type one parameter or the answer of a function of a type's table
// has where a body of this module fills that function's place.
enum class MachineKind : std::uint8_t {
  kVoid,
  kAddress,
  kTruth,
  kInt8,
  kInt32,
  kInt64,
};

// What a function of a type's table takes after the type it is handed, and
// what it answers, read off the table's declaration of it.
struct WitnessSignature {
  MachineKind returns;
  std::vector<MachineKind> takes;
};

// The lifecycle of each type a module lays out whose operations the design
// compiles, and the constant stating the type to the runtime: a tuple, a
// value made of several components at once -- an unpacked structure (LRM
// 7.2), or a call's answer, its result together with its `output` and `inout`
// arguments (LRM 13.5) -- a union or a tagged union, one storage its members
// overlay (LRM 7.3), and a fixed-size unpacked array (LRM 7.4.2). Each step of
// a tuple's life applies the same step to every component, the way clang
// emits a class's implicit special members, and an array's to every element;
// a tagged union's applies it to the member its tag names, and an untagged
// union's members own nothing, so its steps are its bytes'. The type is how
// the runtime, compiled before any of these types existed, reaches what a
// value it holds can do: the lifecycle compiled here, and every operation the
// language defines on the whole value, which are the methods a record's
// declaration states and the bodies a unit states for an array. Such
// a value's bytes state nothing of it, so whatever hands one to the runtime
// hands the type beside it. It is laid out as the record the library declares
// for its kind, opening with the address of a table of this module's bodies,
// one where the table's declaration lays out each function.
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

  auto Function(lir::TypeId type, LifecycleStep step) -> llvm::Function*;

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

  // The type of every struct and union this unit declares, which another unit
  // may refer to whether or not this one builds a value of it.
  auto EmitDeclared() -> void;

 private:
  auto ComponentKey(lir::TypeId component) -> std::string;
  // The type constant `named` stands for, defined as `content` at `align`, as
  // the library's record of it is laid out: it takes the name and every use of
  // `named`, which a body emitted while filling it may hold, and `named` goes.
  auto Defined(
      const std::string& key, llvm::GlobalVariable* named,
      llvm::Constant* content, std::size_t align) -> llvm::GlobalVariable*;
  // The declaration of the struct or the union `type` is, or nothing for any
  // other type.
  [[nodiscard]] auto DeclarationOf(lir::TypeId type) const
      -> std::optional<lir::TypeDeclarationRef>;
  // A name the type's components or members settle, which tells one type's
  // code from another's when reading the module.
  auto KeyOf(lir::TypeId type) -> const std::string&;
  auto Emit(lir::TypeId type, LifecycleStep step, llvm::Function* fn) -> void;
  // A function of this unit answering one operation of the type, and the
  // operation it answers.
  struct OwnOperation {
    support::ValueOperation answers;
    lir::FunctionId function;
  };
  // The functions the type answers its operations with: a record's methods,
  // the bodies the unit states for an array, and none for a tuple.
  [[nodiscard]] auto OwnOperations(lir::TypeId type) const
      -> std::vector<OwnOperation>;
  // At `b`, a branch per member of the tagged union `tagged` lying at
  // `value`, each emitting what `held` emits for the member the tag names,
  // and nothing where it names none; `b` is left after the branches meet.
  void ForHeldMember(
      llvm::IRBuilderBase& b, lir::TypeId tagged, llvm::Value* value,
      const std::function<void(llvm::IRBuilderBase&, lir::TypeId)>& held);
  [[nodiscard]] auto MachineTypeOf(MachineKind kind) const -> llvm::Type*;
  // A body for the witness `witness` of signature `stands_in_for`, declared
  // and left for its caller to fill: it takes the type it is handed and then
  // what that witness takes, and answers what it answers.
  auto DeclareBody(
      lir::TypeId type, std::string_view witness,
      const WitnessSignature& stands_in_for) -> llvm::Function*;
  // The body the type's table holds as the witness `witness`, which does what
  // `filling` does, or the library's failing of an operation the language
  // does not define for the type where nothing fills it.
  auto Body(
      lir::TypeId type, std::string_view witness,
      const WitnessSignature& stands_in_for, llvm::Function* filling)
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
      const WitnessSignature& stands_in_for, lir::TypeId stated) -> ThunkBody;
  // What a witness whose method answers a comparison answers: the scalar the
  // comparison came to, or whether it holds where it is never unknown (LRM
  // 11.4.5).
  enum class ComparisonAnswered : std::uint8_t {
    kAsItsScalar,
    kAsWhetherItHolds,
  };
  // The body of a witness whose method answers a comparison; of the one whose
  // method answers the value's stream of bits; of the one whose method takes a
  // stream; and of the one whose method takes a count's control bits.
  auto ScalarAnswerBody(
      lir::TypeId type, std::string_view witness,
      const WitnessSignature& stands_in_for, lir::FunctionId method,
      ComparisonAnswered answered) -> llvm::Constant*;
  auto StreamWriteBody(
      lir::TypeId type, std::string_view witness,
      const WitnessSignature& stands_in_for, lir::FunctionId method)
      -> llvm::Constant*;
  auto StreamReadBody(
      lir::TypeId type, std::string_view witness,
      const WitnessSignature& stands_in_for, lir::FunctionId method)
      -> llvm::Constant*;
  auto ControlReadBody(
      lir::TypeId type, std::string_view witness,
      const WitnessSignature& stands_in_for, lir::FunctionId method)
      -> llvm::Constant*;
  // At `b`, the component lying at `value` built as its own type builds a
  // value before anything writes it, through that type's table.
  void CallBuildDefault(
      llvm::IRBuilderBase& b, lir::TypeId component, llvm::Value* value);
  // The body building a tuple before anything writes it, each component as
  // its own type builds one.
  auto ComponentWiseDefaultBody(
      lir::TypeId tuple, std::string_view witness,
      const WitnessSignature& stands_in_for) -> llvm::Constant*;
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
  std::map<std::pair<std::string, LifecycleStep>, llvm::Function*> functions_;
  std::map<std::string, llvm::Function*> clear_functions_;
  std::map<std::string, llvm::GlobalVariable*> types_of_;
  std::map<std::string, llvm::GlobalVariable*> integral_types_;
};

}  // namespace lyra::backend::llvm_backend

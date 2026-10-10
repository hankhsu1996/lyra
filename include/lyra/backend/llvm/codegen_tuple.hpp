#pragma once

#include <cstdint>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

#include "lyra/backend/llvm/codegen_types.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"

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

// What a tuple value's storage asks of the code that holds one, whatever its
// type: copying it, moving it, ending it, and writing into one already there.
// A tuple is a value made of several components at once -- an unpacked
// structure (LRM 7.2), or a call's answer, its result together with its
// `output` and `inout` arguments (LRM 13.5) -- so each applies the same step to
// every component, the way clang emits a class's implicit special members.
enum class TupleLifecycle : std::uint8_t {
  kCopy,
  kMove,
  kDestroy,
  kAssign,
};

// The operation of the library's that carries out `step` on an object of its
// own.
auto LifecycleOp(TupleLifecycle step) -> RuntimeOp;

// The machine type one parameter or the answer of a virtual function of the
// library's has where a body of this module stands in for that function.
enum class MachineKind : std::uint8_t {
  kVoid,
  kAddress,
  kTruth,
  kInt8,
  kInt32,
  kInt64,
};

// What a virtual function of the library's type class takes after the type it
// is entered on, and what it answers, read off the function's own declaration.
struct VirtualSignature {
  MachineKind returns;
  std::vector<MachineKind> takes;
};

// The lifecycle of each tuple type a module uses, and the type a tuple of it
// opens with the address of. The type is how the runtime, compiled before any
// tuple type existed, reaches what a value it holds can do: the lifecycle
// compiled here, and every operation the language defines on the whole value,
// which are the methods the type's declaration states. It is an object of a
// class the runtime declares, emitted as clang emits one of a class deriving
// from it: the object holds the address of a table of bodies, one per virtual
// function.
//
// The type of a struct is its declaring unit's, one in the program, which
// every other module refers to. A tuple has no operations, so a type of one
// does nothing but its lifecycle and each module keeps its own.
class CodeGenTuples {
 public:
  CodeGenTuples(
      CodeGenModule& module, CodeGenTypes& types,
      const lir::CompilationUnit& unit);

  auto Function(lir::TypeId tuple, TupleLifecycle step) -> llvm::Function*;

  // The type a tuple of `tuple`'s type opens with the address of.
  auto TypeOf(lir::TypeId tuple) -> llvm::GlobalVariable*;

  // The type of every struct this unit declares, which another unit may refer
  // to whether or not this one builds a value of it.
  auto EmitDeclared() -> void;

 private:
  // The declaration of the struct `tuple` is, or nothing for a tuple.
  [[nodiscard]] auto DeclarationOf(lir::TypeId tuple) const
      -> std::optional<lir::TypeDeclarationRef>;
  // A name the type's components settle, which tells one type's code from
  // another's when reading the module.
  auto KeyOf(lir::TypeId tuple) -> const std::string&;
  auto Emit(lir::TypeId tuple, TupleLifecycle step, llvm::Function* fn) -> void;
  [[nodiscard]] auto MachineTypeOf(MachineKind kind) const -> llvm::Type*;
  // A body standing in for a virtual function of signature `stands_in_for`,
  // declared and left for its slot to fill: it takes the type it is entered
  // on and then what that function takes, and answers what it answers.
  auto DeclareBody(
      lir::TypeId tuple, std::string_view slot,
      const VirtualSignature& stands_in_for) -> llvm::Function*;
  // The body one slot of the type's table holds, which does what `filling`
  // does, or the one no slot is ever entered through where nothing fills it.
  auto Body(
      lir::TypeId tuple, std::string_view slot,
      const VirtualSignature& stands_in_for, llvm::Function* filling)
      -> llvm::Constant*;
  // A body doing what a struct method does where the two differ in how one
  // integral value crosses, opened and left for its slot to fill: it holds
  // storage for a value of the integral type `stated`, which the method takes
  // or answers.
  struct ThunkBody;
  auto OpenThunkBody(
      lir::TypeId tuple, std::string_view slot,
      const VirtualSignature& stands_in_for, lir::TypeId stated) -> ThunkBody;
  // What a slot whose method answers a comparison answers: the scalar the
  // comparison came to, or whether it holds where it is never unknown (LRM
  // 11.4.5).
  enum class ComparisonAnswered : std::uint8_t {
    kAsItsScalar,
    kAsWhetherItHolds,
  };
  // The body of a slot whose method answers a comparison; of the one whose
  // method answers the value's stream of bits; of the one whose method takes a
  // stream; and of the one whose method takes a count's control bits.
  auto ScalarAnswerBody(
      lir::TypeId tuple, std::string_view slot,
      const VirtualSignature& stands_in_for, lir::FunctionId method,
      ComparisonAnswered answered) -> llvm::Constant*;
  auto StreamWriteBody(
      lir::TypeId tuple, std::string_view slot,
      const VirtualSignature& stands_in_for, lir::FunctionId method)
      -> llvm::Constant*;
  auto StreamReadBody(
      lir::TypeId tuple, std::string_view slot,
      const VirtualSignature& stands_in_for, lir::FunctionId method)
      -> llvm::Constant*;
  auto ControlReadBody(
      lir::TypeId tuple, std::string_view slot,
      const VirtualSignature& stands_in_for, lir::FunctionId method)
      -> llvm::Constant*;
  // The body of a slot a structure answers with nothing.
  auto NullBody(
      lir::TypeId tuple, std::string_view slot,
      const VirtualSignature& stands_in_for) -> llvm::Constant*;
  // The integral value of type `read` that `planes`, `planes_width` bits
  // wide, hold below their `taken` most significant positions, laid out at
  // `laid_out`; and the bits of the value of type `written` at `value`,
  // written into the stream `stream`, `stream_width` bits wide, below its
  // `filled` most significant positions. Each answers how many positions are
  // taken, or filled, after it.
  auto ReadOutOfPlanes(
      llvm::IRBuilderBase& b, llvm::Value* planes, llvm::Value* planes_width,
      llvm::Value* taken, llvm::Value* laid_out, lir::TypeId read)
      -> llvm::Value*;
  auto WriteIntoStream(
      llvm::IRBuilderBase& b, llvm::Value* value, lir::TypeId written,
      llvm::Value* stream, llvm::Value* stream_width, llvm::Value* filled)
      -> llvm::Value*;

  CodeGenModule* owner_;
  CodeGenTypes* types_;
  const lir::CompilationUnit* unit_;
  std::unordered_map<lir::TypeId, std::string> keys_;
  std::map<std::pair<std::string, TupleLifecycle>, llvm::Function*> functions_;
  std::map<std::string, llvm::GlobalVariable*> types_of_;
};

}  // namespace lyra::backend::llvm_backend

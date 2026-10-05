#pragma once

#include <cstdint>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>

#include "lyra/backend/llvm/codegen_types.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"

namespace llvm {
class Constant;
class Function;
class GlobalVariable;
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
  // The body one slot of the type's table holds, which does what `filling`
  // does, or the one no slot is ever entered through where nothing fills it.
  auto Body(
      lir::TypeId tuple, std::string_view slot, llvm::Function* filling,
      bool answers_truth) -> llvm::Constant*;

  CodeGenModule* owner_;
  CodeGenTypes* types_;
  const lir::CompilationUnit* unit_;
  std::unordered_map<lir::TypeId, std::string> keys_;
  std::map<std::pair<std::string, TupleLifecycle>, llvm::Function*> functions_;
  std::map<std::string, llvm::GlobalVariable*> types_of_;
};

}  // namespace lyra::backend::llvm_backend

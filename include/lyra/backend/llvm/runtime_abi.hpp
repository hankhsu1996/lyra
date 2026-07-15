#pragma once

#include <cstdint>
#include <string>
#include <string_view>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Module.h>

#include "lyra/lir/operator.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lir {
struct CompilationUnit;
}  // namespace lyra::lir

namespace lyra::backend::llvm_backend {

class CodeGenTypes;

// The runtime value type a library entry operates on, and that a storage cell
// is realized as. Chosen from the LIR type at the site that needs it, never
// carried as a tag the runtime inspects: the generated module names the entry
// it means.
//
// This enumerates runtime realizations, not source types. Several source types
// share one domain -- an enumeration and an integral both become a packed value
// -- and a source type with no runtime realization has no domain at all. It
// grows only when the runtime library gains a value type, never to mirror the
// source language's type kinds.
enum class ValueDomain : std::uint8_t { kPacked, kString };

auto ValueDomainName(ValueDomain domain) -> std::string_view;

// The domain a LIR type is realized in. The one place a LIR type is classified,
// so the entry a call names and the storage a cell owns cannot disagree.
auto ValueDomainOf(const lir::CompilationUnit& unit, lir::TypeId type)
    -> ValueDomain;

// The runtime ABI the generated module calls: each runtime entry point declared
// once with its canonical signature. The ABI is execution-strategy-neutral --
// the same entry points serve a module that is JIT-compiled, AOT-linked, or
// interpreted; only how they resolve differs. Single source of truth for the
// contract: it declares callees and never builds instructions. The runtime owns
// the matching definitions.
class RuntimeAbi {
 public:
  RuntimeAbi(llvm::Module& module, llvm::LLVMContext& ctx, CodeGenTypes& types);

  auto Services() -> llvm::FunctionCallee;
  auto Files() -> llvm::FunctionCallee;
  auto TimeFormat() -> llvm::FunctionCallee;
  auto Format() -> llvm::FunctionCallee;
  auto Writeln() -> llvm::FunctionCallee;
  auto Write() -> llvm::FunctionCallee;

  // Binds a coroutine to an instance's startup or shutdown lifecycle. The
  // coroutine crosses as an opaque handle; the runtime owns the coroutine, so
  // no C++ coroutine frame is built on the generated side.
  auto RegisterInitial() -> llvm::FunctionCallee;
  auto RegisterFinal() -> llvm::FunctionCallee;

  // Registers the running process to wake after a delay, the runtime call a
  // delay's suspend edge is preceded by. The wakeup source is the running
  // process, read from the runtime; no token crosses the boundary.
  auto Delay() -> llvm::FunctionCallee;

  // Registers the running process to wake on any leaf of a trigger set, the
  // runtime call a value-change wait's suspend edge is preceded by. Like a
  // delay, the wakeup source is the running process, read from the runtime.
  auto WaitAny() -> llvm::FunctionCallee;

  // Builds one leaf of a wait: the observable cell it watches, the bit
  // projection of that cell it watches, and the edge polarity it watches for.
  auto MakeTrigger() -> llvm::FunctionCallee;

  // Builds a coroutine from an entry code reference and its environment; the
  // runtime owns the resulting coroutine and returns an opaque handle.
  auto MakeCoroutine() -> llvm::FunctionCallee;
  auto MakeString() -> llvm::FunctionCallee;
  auto MakePrintLiteralItem() -> llvm::FunctionCallee;
  auto PackedConst() -> llvm::FunctionCallee;

  // Builds a scope's structural identity from its parent-side label and its
  // per-dimension indices; the runtime owns the resulting segment handle.
  auto MakeSegment() -> llvm::FunctionCallee;

  // Allocates a generic instance of the unit named by `definition`, runs its
  // construct entry to build its subtree, and returns the owning handle. The
  // definition is an opaque cross-unit reference the generated code never
  // inspects.
  auto MakeUnit() -> llvm::FunctionCallee;

  // Attaches a freshly built child to its parent's containment edge, returning
  // the child as a borrowed scope handle.
  auto AddOwnedChild() -> llvm::FunctionCallee;

  // The address of an instance's member storage, by class-local member index.
  // A member is a logical place; the runtime owns the storage it resolves to.
  auto MemberAddress() -> llvm::FunctionCallee;

  // Operations on an observable storage cell, reached through its address.
  // `Initialize` installs the cell's declared representation once; `Set`
  // threads services so a change wakes the cell's subscribers.
  auto CellGet(ValueDomain domain) -> llvm::FunctionCallee;
  auto CellInitialize(ValueDomain domain) -> llvm::FunctionCallee;
  auto CellSet(ValueDomain domain) -> llvm::FunctionCallee;

  // Publishes a member cell under its source-level name so the scope can be
  // navigated by name.
  auto RegisterSignal() -> llvm::FunctionCallee;

  // The library realization of an operator over a value domain. The entry's
  // name is the domain and the operator's own spelling, so a new operator or a
  // new domain cannot silently resolve to the wrong entry.
  auto Binary(ValueDomain domain, lir::BinaryOp op) -> llvm::FunctionCallee;
  auto Unary(ValueDomain domain, lir::UnaryOp op) -> llvm::FunctionCallee;

  // The library realization of a value builtin -- an operation the source
  // language spells as a call rather than an operator (a shift, a reduction, a
  // conversion). Named the same way an operator is, from the domain and the
  // builtin's own spelling. Its signature is the call site's own: the value
  // model gives each runtime value one representation, so the operand and
  // result types at the call are the entry's parameter and result types.
  auto ValueBuiltin(
      ValueDomain domain, lyra::support::BuiltinFn fn, llvm::Type* result,
      llvm::ArrayRef<llvm::Type*> params) -> llvm::FunctionCallee;

  // Reduces a value to the machine boolean a conditional branch tests.
  auto ToBool(ValueDomain domain) -> llvm::FunctionCallee;

  // Builds the format specification of one conversion, and the print item that
  // pairs a value with it. A specification is written either as a bare
  // conversion kind, leaving every field at its default, or with every field
  // spelled out; `field_count` selects which, as an overload set would.
  auto MakeFormatSpec(std::size_t field_count) -> llvm::FunctionCallee;
  auto MakePrintValueItem(ValueDomain domain) -> llvm::FunctionCallee;

 private:
  auto Get(
      const char* name, llvm::Type* result, llvm::ArrayRef<llvm::Type*> params)
      -> llvm::FunctionCallee;
  auto Get(
      const std::string& name, llvm::Type* result,
      llvm::ArrayRef<llvm::Type*> params) -> llvm::FunctionCallee;

  llvm::Module* module_;
  llvm::LLVMContext* ctx_;
  CodeGenTypes* types_;
};

}  // namespace lyra::backend::llvm_backend

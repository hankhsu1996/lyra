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
enum class ValueDomain : std::uint8_t {
  kPacked,
  kString,
  kReal,
  kShortReal,
  kChandle,
  kTuple,
  kDynArray,
};

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

  auto CurrentServices() -> llvm::FunctionCallee;
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

  // Builds a real-family constant from its host-precision immediate: `double`
  // for `kReal`, `float` for `kShortReal`. The runtime owns the resulting value
  // and returns an opaque handle.
  auto RealConst(ValueDomain domain) -> llvm::FunctionCallee;

  // Builds a real-family value from a machine `int64` -- the outer step of the
  // integral-to-real conversion, whose inner step already read the operand out
  // as a host integer.
  auto RealFromInt(ValueDomain domain) -> llvm::FunctionCallee;

  // Reshapes one real-family precision into another (`shortreal` <-> `real`):
  // `dst` names the result precision, `src` the operand's.
  auto RealReshape(ValueDomain dst, ValueDomain src) -> llvm::FunctionCallee;

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
  // threads runtime so a change wakes the cell's subscribers.
  auto CellGet(ValueDomain domain) -> llvm::FunctionCallee;
  auto CellInitialize(ValueDomain domain) -> llvm::FunctionCallee;
  auto CellSet(ValueDomain domain) -> llvm::FunctionCallee;

  // A procedural local whose value crosses a suspension: its storage is a cell
  // in the running activation's frame, reached by a handle the generated frame
  // holds. `ActivationFrameAlloc` allocates the cell (uninitialized -- the
  // first store installs its representation); `ActivationFrameStore` overwrites
  // it; `ActivationFrameLoad` copies its value into the current stretch. No
  // runtime thread through: a procedural local is not observable.
  auto ActivationFrameAlloc(ValueDomain domain) -> llvm::FunctionCallee;
  auto ActivationFrameStore(ValueDomain domain) -> llvm::FunctionCallee;
  auto ActivationFrameLoad(ValueDomain domain) -> llvm::FunctionCallee;

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

  // Boxes a value-domain handle into a type-erased aggregate element. The
  // domain rides in the symbol name, as every domain-parametric entry does.
  // Shared by every aggregate family (a struct component, a dynamic-array
  // element).
  auto ValueBox(ValueDomain domain) -> llvm::FunctionCallee;

  // The product-value entries. `Make` collects the boxed components; `Extract`
  // and `Update` are value operations: update yields a new product with one
  // component replaced, never an in-place write, so value semantics hold even
  // when the product is shared.
  auto TupleMake() -> llvm::FunctionCallee;
  auto TupleExtract() -> llvm::FunctionCallee;
  auto TupleUpdate() -> llvm::FunctionCallee;

  // The dynamic-array constructors (LRM 7.5.1 / 10.9.1): the empty array, the
  // sized array, the sized-from-source array, and the assignment-pattern array.
  // Each takes the element default as a boxed prototype; `New` / `NewCopy` lead
  // with the size, `FromLiteral` follows the prototype with the boxed element
  // span. Element read / functional update / delete / size resolve through the
  // generic value-builtin path, not a dedicated entry here.
  auto MakeDynamicArrayDefault() -> llvm::FunctionCallee;
  auto MakeDynamicArrayNew() -> llvm::FunctionCallee;
  auto MakeDynamicArrayNewCopy() -> llvm::FunctionCallee;
  auto MakeDynamicArrayFromLiteral() -> llvm::FunctionCallee;

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

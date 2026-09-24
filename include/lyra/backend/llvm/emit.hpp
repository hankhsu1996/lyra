#pragma once

#include <memory>
#include <string>
#include <string_view>

#include "lyra/base/time.hpp"
#include "lyra/diag/diagnostic.hpp"

namespace llvm {
class LLVMContext;
class Module;
}  // namespace llvm

namespace lyra::lir {
struct CompilationUnit;
}  // namespace lyra::lir

namespace lyra::backend::llvm_backend {

// The backend's product: a self-contained LLVM module and the context it lives
// in, kept together because the module references the context. Textual IR is a
// projection (`Print`) for inspection, not the product; the module itself is
// what downstream executes.
class EmittedModule {
 public:
  EmittedModule(
      std::unique_ptr<llvm::LLVMContext> context,
      std::unique_ptr<llvm::Module> module);
  EmittedModule(EmittedModule&&) noexcept;
  auto operator=(EmittedModule&&) noexcept -> EmittedModule&;
  EmittedModule(const EmittedModule&) = delete;
  auto operator=(const EmittedModule&) -> EmittedModule& = delete;
  ~EmittedModule();

  // The context and module together, ownership transferred out. A consumer that
  // hands the module to an execution engine takes both, since the module
  // references the context and the two must share a lifetime.
  struct Owned {
    std::unique_ptr<llvm::LLVMContext> context;
    std::unique_ptr<llvm::Module> module;
  };

  [[nodiscard]] auto Print() const -> std::string;
  // What a composer checks the module against before taking it: the symbols
  // it leaves for something else to define, and the shapes it calls them at.
  [[nodiscard]] auto Module() const -> const llvm::Module&;
  [[nodiscard]] auto Release() && -> Owned;

 private:
  std::unique_ptr<llvm::LLVMContext> context_;
  std::unique_ptr<llvm::Module> module_;
};

// Lowers one compiled unit to an LLVM module. Every value type is an opaque
// runtime handle reached through the runtime facade; a callable whose result is
// a coroutine is a process step body, and a coroutine value is an opaque handle
// the runtime builds from a step entry and its environment. A construct this
// backend has no entry for is refused, naming what was met.
//
// A unit is its executable body and the facts that body carries none of, so the
// artifact takes both: what every scope of the unit runs at (LRM Table 20-2) is
// a constant the runtime is told rather than something the code computes, and
// the body it belongs beside states no source-language concept.
auto EmitModule(const lir::CompilationUnit& unit, TimeResolution time)
    -> diag::Result<EmittedModule>;

// The symbol a program starts at, which the host platform names rather than
// this backend.
inline constexpr std::string_view kProgramEntrySymbol = "main";

// The module holding where the program starts. A design has one, derived from
// its root unit, and it is the one thing composing a program adds to the units'
// own modules: it hands the arguments the program was started with to the
// runtime, together with the design root's definition as the root's own unit
// declares it. A linker starts the program there and a session calls it, so the
// two bring a design up the same way.
auto EmitProgramEntry(const lir::CompilationUnit& design_root) -> EmittedModule;

}  // namespace lyra::backend::llvm_backend

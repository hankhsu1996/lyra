#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/class_decl.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Lowers one HIR class declaration to its MIR object. A class is the same
// generic nominal object as a module or generate scope, differing in that it
// extends no runtime base (it is not a tree node), is reached through a managed
// reference, and is built by `new`. Its properties become plain value-typed
// members -- not observable cells -- and their construction-time defaults are
// the body of its constructor. `object_type` is the interned object type that
// names the class, minted with its registry identity before type translation.
class ClassDeclLowerer {
 public:
  ClassDeclLowerer(
      ModuleLowerer& module, mir::TypeId object_type,
      const hir::ClassDecl& hir_class)
      : module_(&module), object_type_(object_type), hir_class_(&hir_class) {
  }

  // Builds and returns the class object. The caller, which owns the class
  // registry and holds the pre-declared identity, defines the returned object
  // into the unit.
  auto Run() -> diag::Result<mir::Class>;

 private:
  ModuleLowerer* module_;
  mir::TypeId object_type_;
  const hir::ClassDecl* hir_class_;
};

}  // namespace lyra::lowering::hir_to_mir

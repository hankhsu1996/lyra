#pragma once

#include "lyra/base/translation.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/class_decl.hpp"
#include "lyra/lowering/hir_to_mir/declared_callable.hpp"
#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Lowers one HIR class declaration to its MIR object. A class is the same
// generic nominal object as a module or generate scope, differing in that it
// does not occupy the runtime object tree, is reached through a managed
// reference, and is built by `new`. Its properties become plain value-typed
// members -- not observable cells -- and their construction-time defaults are
// the body of its constructor. Its instance methods (LRM 8.6) are lowered as
// callables receiving the object handle as `self`. `object_type` is the
// interned object type that names the class, minted with its registry identity
// before type translation.
//
// A class method body names only its receiver and its own locals, so it is
// lowered inside no structural scope -- its body lowerer carries a null
// enclosing scope, never an owner that stands in for one.
//
// The lowering runs in two stages. `DeclareShape` publishes the class's
// structural facts -- fields, method signatures, canonical dispatch role --
// so peers can query them by id while their own bodies lower. `PopulateBodies`
// then composes the executable `mir::Class`. The stages run independently
// per class; every class's shape is published before any body lowers, so a
// body's cross-class reference always resolves against a settled shape.
class ClassDeclLowerer {
 public:
  ClassDeclLowerer(
      UnitLowerer& unit_lowerer, hir::ClassId hir_class_id,
      mir::ClassId class_id, mir::TypeId object_type,
      const hir::ClassDecl& hir_class)
      : owner_(&unit_lowerer),
        hir_class_id_(hir_class_id),
        class_id_(class_id),
        object_type_(object_type),
        hir_class_(&hir_class) {
  }

  // Settles this class's structural declaration so peer body lowering can read
  // every fact it might need -- the base reference, the field arena, each
  // method's dispatch role -- without waiting for any sibling class's body to
  // lower.
  auto DeclareShape() -> diag::Result<void>;

  // Composes the class from the already-published shape plus every body,
  // and commits it to the compilation unit. Any cross-class query the
  // bodies make resolves against the unit's declarations, never against
  // another class's still-in-progress state.
  auto PopulateBodies() -> diag::Result<void>;

 private:
  UnitLowerer* owner_;
  hir::ClassId hir_class_id_;
  mir::ClassId class_id_;
  mir::TypeId object_type_;
  const hir::ClassDecl* hir_class_;

  base::Translation<hir::MethodId, DeclaredCallable> declared_methods_;
  StaticVarBindings ctor_static_bindings_;
};

}  // namespace lyra::lowering::hir_to_mir

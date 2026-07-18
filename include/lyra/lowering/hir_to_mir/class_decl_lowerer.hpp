#pragma once

#include <optional>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/class_decl.hpp"
#include "lyra/lowering/hir_to_mir/callable_storage_plan.hpp"
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
      UnitLowerer& unit_lowerer, mir::ClassId class_id, mir::TypeId object_type,
      const hir::ClassDecl& hir_class)
      : owner_(&unit_lowerer),
        class_id_(class_id),
        object_type_(object_type),
        hir_class_(&hir_class) {
  }

  // Publishes this class's `ClassShape` to the module's shape store so peer
  // body lowering can read every fact it might need -- the base reference,
  // the field arena, each method's dispatch role -- without waiting for any
  // sibling class's body to lower.
  auto DeclareShape() -> diag::Result<void>;

  // Composes the class from the already-published shape plus every body,
  // and commits it to the compilation unit. Any cross-class query the
  // bodies make resolves against the shape store, never against another
  // class's still-in-progress state.
  auto PopulateBodies() -> diag::Result<void>;

 private:
  UnitLowerer* owner_;
  mir::ClassId class_id_;
  mir::TypeId object_type_;
  const hir::ClassDecl* hir_class_;

  // Storage layout the shape stage settles and the body stage reuses: the
  // per-callable static-storage plans whose placements name the exact field
  // ids the published shape declares. Property field ids are not stored --
  // they are the same identity as the HIR field ids the class carries, so
  // any consumer reaches a mir field id from a hir field id through
  // `UnitLowerer::TranslateField`.
  ProceduralScopeMaterializationTable class_scopes_;
  std::vector<CallableStoragePlan> method_plans_;
  std::optional<CallableStoragePlan> ctor_plan_;
};

}  // namespace lyra::lowering::hir_to_mir

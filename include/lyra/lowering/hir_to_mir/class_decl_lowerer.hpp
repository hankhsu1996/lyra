#pragma once

#include <optional>

#include "lyra/base/translation.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/class_decl.hpp"
#include "lyra/lowering/hir_to_mir/declared_callable.hpp"
#include "lyra/lowering/hir_to_mir/declared_scope.hpp"
#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

class StructuralScopeLowerer;

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
// A class declared in a structural scope is a type of that scope's instance
// (LRM 6.22), so the object records which instance it belongs to and its bodies
// name that scope's declarations the way a process of the scope does: the
// lowering runs against `declaring_scope`, and construction supplies the
// instance. A class a namespace unit declares has no such scope, because
// nothing replicates it.
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
      const hir::ClassDecl& hir_class,
      const StructuralScopeLowerer* declaring_scope)
      : owner_(&unit_lowerer),
        hir_class_id_(hir_class_id),
        class_id_(class_id),
        object_type_(object_type),
        hir_class_(&hir_class),
        declaring_scope_(declaring_scope) {
  }

  // Settles this class's structural declaration so peer body lowering can read
  // every fact it might need -- the base reference, the field arena, each
  // method's dispatch role -- without waiting for any sibling class's body to
  // lower.
  // `declaring_shape` is the not-yet-published shape of the scope that declares
  // the class, which takes the instance-side cells this class places -- the
  // instance it belongs to, and everything the class keeps for itself once that
  // scope replicates it. Null where no scope declares the class, which is a
  // namespace unit's, since nothing replicates it and it keeps its own cells.
  auto DeclareShape(ClassShape* declaring_shape) -> diag::Result<void>;

  // Composes the class from the already-published shape plus every body,
  // and commits it to the compilation unit. Any cross-class query the
  // bodies make resolves against the unit's declarations, never against
  // another class's still-in-progress state.
  // `declaring_init_frame` is where the scope that declares the class brings up
  // its instance's own storage: a class it replicates keeps its cells there, so
  // that is where their initializers run -- once per instance, and before any
  // process (LRM 10.5).
  auto PopulateBodies(WalkFrame declaring_frame, WalkFrame declaring_init_frame)
      -> diag::Result<void>;

 private:
  // The frame a body of this class lowers under: the class as the write
  // target, standing where the scope that declares it stands, and the member
  // holding the instance its outward references count from.
  [[nodiscard]] auto BodyFrame(
      const WalkFrame& declaring_frame, mir::Class& mir_class,
      ScopeChainNode& link) const -> WalkFrame;

  UnitLowerer* owner_;
  hir::ClassId hir_class_id_;
  mir::ClassId class_id_;
  mir::TypeId object_type_;
  const hir::ClassDecl* hir_class_;
  const StructuralScopeLowerer* declaring_scope_;
  // The member holding the instance this class's objects belong to, present
  // exactly where a structural scope declares the class.
  std::optional<mir::FieldId> declaring_scope_field_;

  base::Translation<hir::MethodId, DeclaredCallable> declared_methods_;
  StaticVarBindings ctor_static_bindings_;
  DeclaredScopes scopes_;
};

}  // namespace lyra::lowering::hir_to_mir

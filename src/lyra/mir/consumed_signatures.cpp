#include "lyra/mir/consumed_signatures.hpp"

#include <algorithm>
#include <optional>
#include <set>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/value_build.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir {

namespace {

class Reads {
 public:
  explicit Reads(const CompilationUnit& unit) : unit_(&unit) {
  }

  // A class read whole, with its lineage. A class this unit defines also
  // reads what its storage holds, which it builds and ends here.
  void ReadClass(ClassId id) {
    if (!read_classes_.insert(id).second) {
      return;
    }
    const mir::Class& cls = unit_->GetClass(id);
    if (cls.defining_unit.has_value()) {
      Add(ConsumedClass{
          .unit_name = *cls.defining_unit, .class_path = *cls.path});
    }
    for (const ClassId rested_on : RestsOnDeclaredClasses(cls)) {
      ReadClass(rested_on);
    }
    if (cls.defining_unit.has_value()) {
      return;
    }
    for (const FieldDecl& field : cls.fields) {
      ReadHeld(field.type);
    }
  }

  // A value of `type`, which reads the declaration a record or an object is.
  void ReadValue(TypeId type) {
    const Type& t = unit_->types.Get(type);
    if (const RecordId* record = RecordOf(t)) {
      ReadRecord(*record);
    } else if (const auto* object = t.As<ObjectType>()) {
      ReadClass(object->of);
    }
  }

  void ReadCode(const CallableCode& code) {
    if (code.body.has_value()) {
      ReadBlock(*code.body);
    }
  }

  void ReadBuild(const ValueBuild& build) {
    ReadBlock(build.body);
  }

  [[nodiscard]] auto Take() && -> std::vector<ConsumedSignature> {
    return std::move(consumed_);
  }

 private:
  // A value held: a value of a record, or an object owned, which has to be
  // complete to move or end; a handle or a borrowed pointer reads nothing.
  void ReadHeld(TypeId type) {
    const Type& t = unit_->types.Get(type);
    if (const auto* pointer = t.As<PointerType>();
        pointer != nullptr && pointer->ownership == PointerOwnership::kUnique) {
      ReadHeld(pointer->pointee);
    } else if (const auto* vector = t.As<VectorType>()) {
      ReadHeld(vector->element);
    } else {
      ReadValue(type);
    }
  }

  void ReadRecord(RecordId id) {
    if (const std::optional<std::string>& defining =
            unit_->GetRecord(id).defining_unit) {
      Add(ConsumedNamespace{.unit_name = *defining});
    }
  }

  void ReadBlock(const mir::Block& block) {
    for (const Expr& expr : block.exprs) {
      ReadHeld(expr.type);
      ReadExpression(block, expr);
    }
    for (const mir::Block& child : block.child_scopes) {
      ReadBlock(child);
    }
  }

  void ReadExpression(const mir::Block& block, const Expr& expr) {
    if (const auto* access = std::get_if<FieldAccessExpr>(&expr.data)) {
      ReadField(access->field);
    } else if (const auto* call = std::get_if<CallExpr>(&expr.data)) {
      ReadCall(block, *call);
    } else if (const auto* reference = std::get_if<ReferenceExpr>(&expr.data)) {
      ReadReference(reference->target);
    }
  }

  void ReadField(const FieldRef& field) {
    std::visit(
        Overloaded{
            [&](const ClassFieldTarget& target) { ReadClass(target.owner); },
            [](const ClosureFieldTarget&) {},
            [](const ComponentFieldTarget&) {}},
        field);
  }

  void ReadCall(const mir::Block& block, const CallExpr& call) {
    std::visit(
        Overloaded{
            [&](const Direct& direct) {
              ReadTarget(block, call, direct);
              if (direct.part.has_value()) {
                std::visit(
                    Overloaded{
                        [](const base::ComponentIndex&) {},
                        [&](const ClassFieldTarget& property) {
                          ReadClass(property.owner);
                        }},
                    *direct.part);
              }
            },
            [](const Indirect&) {}, [](const Construct&) {},
            [&](const Virtual& dispatched) {
              ReadClass(dispatched.slot.owner_class);
            }},
        call.callee);
  }

  void ReadTarget(
      const mir::Block& block, const CallExpr& call, const Direct& direct) {
    std::visit(
        Overloaded{
            [&](const CallableTarget& target) { ReadClass(target.owner); },
            [](const ClosureTarget&) {}, [](const BoundCallTarget&) {},
            [](const BoundCommitTarget&) {},
            [&](const UnitCallableTarget& target) {
              if (const std::optional<std::string>& defining =
                      unit_->callables.Get(target.slot).defining_unit) {
                Add(ConsumedNamespace{.unit_name = *defining});
              }
            },
            // A conversion between two classes reads the lineage of both
            // (LRM 8.15, 8.16).
            [&](const support::BuiltinFn& entry) {
              if (entry != support::BuiltinFn::kDerivedToBase &&
                  entry != support::BuiltinFn::kDynamicCast) {
                return;
              }
              for (const TypeId type_argument : direct.type_arguments) {
                ReadHandled(type_argument);
              }
              for (const ExprId argument : call.arguments) {
                ReadHandled(block.exprs.Get(argument).type);
              }
            },
            [&](const ConstructorTarget& target) { ReadClass(target.cls); },
            [&](const RecordMethodTarget& target) {
              ReadRecord(target.record);
            },
            [](const TypeOperationTarget&) {},
            // The design root brings up every namespace of the design by
            // name, its own set aside.
            [&](const ExternalBringUpTarget& target) {
              if (target.unit_name != unit_->name) {
                Add(ConsumedNamespace{.unit_name = target.unit_name});
              }
            },
            [](const ForeignSymbolTarget&) {}},
        direct.target);
  }

  // The class a handle of `type` reaches, read whole.
  void ReadHandled(TypeId type) {
    if (const auto* handle = unit_->types.Get(type).As<ManagedRefType>()) {
      ReadValue(handle->pointee);
    }
  }

  void ReadReference(const ReferenceTarget& target) {
    std::visit(
        Overloaded{
            [](const LocalRef&) {},
            [&](const DefinitionRef& definition) { ReadClass(definition.of); },
            [](const EnumTableRef&) {}, [](const IntegralConstantRef&) {},
            [&](const StaticPropertyRef& property) {
              ReadClass(property.owner);
            },
            [&](const StaticVariableRef& variable) {
              if (const std::optional<std::string>& defining =
                      unit_->static_variables.Get(variable.variable)
                          .defining_unit) {
                Add(ConsumedNamespace{.unit_name = *defining});
              }
            },
            [](const ClassConstantRef&) {},
            [&](const FunctionRef& function) {
              ReadClass(function.body.owner);
            },
            [&](const ExternalObjectEntryRef& entry) {
              Add(ConsumedObjectEntry{
                  .unit_name = entry.unit_name, .body_name = entry.body_name});
            }},
        target);
  }

  void Add(ConsumedSignature consumed) {
    if (std::ranges::find(consumed_, consumed) == consumed_.end()) {
      consumed_.push_back(std::move(consumed));
    }
  }

  const CompilationUnit* unit_;
  std::set<ClassId> read_classes_;
  std::vector<ConsumedSignature> consumed_;
};

}  // namespace

auto ConsumedSignaturesOf(const CompilationUnit& unit)
    -> std::vector<ConsumedSignature> {
  Reads reads(unit);
  // A class this unit defines is its own declaration, which reads what it
  // extends and what its storage holds.
  for (const ClassId id : unit.classes.Ids()) {
    const Class& cls = unit.GetClass(id);
    if (cls.defining_unit.has_value()) continue;
    reads.ReadClass(id);
    for (const ClassConstantDecl& constant : cls.constants) {
      reads.ReadBuild(constant.initializer);
    }
    reads.ReadBuild(cls.object_definition_initializer);
  }
  ForEachCode(unit, [&](const CodeSource&, const CallableCode& code) {
    reads.ReadCode(code);
  });
  return std::move(reads).Take();
}

}  // namespace lyra::mir

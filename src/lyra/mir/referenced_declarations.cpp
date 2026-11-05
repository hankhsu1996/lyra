#include "lyra/mir/referenced_declarations.hpp"

#include <algorithm>
#include <optional>
#include <set>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/constant_id.hpp"
#include "lyra/mir/constant_value.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/library_decl.hpp"

namespace lyra::mir {

namespace {

class Names {
 public:
  explicit Names(const CompilationUnit& unit) : unit_(&unit) {
  }

  // A class named whole, with its lineage. A class this unit defines also
  // names what its storage holds, which it builds and ends here.
  void NameClass(ClassId id) {
    if (!named_classes_.insert(id).second) {
      return;
    }
    const mir::Class& cls = unit_->GetClass(id);
    if (cls.defining_unit.has_value()) {
      Add(ReferencedClass{
          .unit_name = *cls.defining_unit,
          .class_path = *support::PathOf(cls.identity)});
    }
    for (const ClassId rested_on : RestsOnDeclaredClasses(cls)) {
      NameClass(rested_on);
    }
    if (cls.defining_unit.has_value()) {
      return;
    }
    for (const FieldDecl& field : cls.fields) {
      NameHeld(field.type);
    }
  }

  // A value of `type`, which names the declaration a record or an object is.
  void NameValue(TypeId type) {
    const Type& t = unit_->types.Get(type);
    if (const RecordId* record = RecordOf(t)) {
      NameRecord(*record);
    } else if (const auto* object = t.As<ObjectType>()) {
      NameClass(object->of);
    }
  }

  void NameCode(const CallableCode& code) {
    if (code.body.has_value()) {
      NameBlock(*code.body);
    }
  }

  void NameConstant(TypeId type, const ConstantValue& value) {
    NameHeld(type);
    NameConstantValue(value);
  }

  [[nodiscard]] auto Take() && -> std::vector<ReferencedDeclaration> {
    return std::move(referenced_);
  }

 private:
  // A value held: a value of a record, or an object owned, which has to be
  // complete to move or end; a handle or a borrowed pointer names nothing.
  void NameHeld(TypeId type) {
    const Type& t = unit_->types.Get(type);
    if (const auto* pointer = t.As<PointerType>();
        pointer != nullptr && pointer->ownership == PointerOwnership::kUnique) {
      NameHeld(pointer->pointee);
    } else if (const auto* vector = t.As<VectorType>()) {
      NameHeld(vector->element);
    } else {
      NameValue(type);
    }
  }

  void NameRecord(RecordId id) {
    if (const std::optional<std::string>& defining =
            unit_->GetRecord(id).defining_unit) {
      Add(ReferencedNamespace{.unit_name = *defining});
    }
  }

  void NameBlock(const mir::Block& block) {
    for (const Expr& expr : block.exprs) {
      NameHeld(expr.type);
      NameExpression(expr);
    }
    for (const mir::Block& child : block.child_scopes) {
      NameBlock(child);
    }
  }

  void NameConstantValue(const ConstantValue& value) {
    std::visit(
        Overloaded{
            [&](const PointerValue& pointer) {
              NameHeld(pointer.type);
              NameReference(pointer.to);
            },
            [&](const RecordValue& record) {
              for (const ConstantValue& part : record.parts) {
                NameConstantValue(part);
              }
            },
            [&](const ArrayValue& array) {
              for (const ConstantValue& element : array.elements) {
                NameConstantValue(element);
              }
            },
            [](const IntegralValue&) {}, [](const MachineIntValue&) {},
            [](const NullValue&) {}, [](const CStringValue&) {},
            [](const SpanValue&) {}},
        value.value);
  }

  void NameExpression(const Expr& expr) {
    if (const auto* access = std::get_if<FieldAccessExpr>(&expr.data)) {
      NameField(access->field);
    } else if (const auto* call = std::get_if<CallExpr>(&expr.data)) {
      NameCall(*call);
    } else if (const auto* reference = std::get_if<ReferenceExpr>(&expr.data)) {
      NameReference(reference->target);
    }
  }

  void NameField(const FieldRef& field) {
    std::visit(
        Overloaded{
            [&](const ClassFieldTarget& target) { NameClass(target.owner); },
            [](const ClosureFieldTarget&) {},
            [](const ComponentFieldTarget&) {}},
        field);
  }

  void NameCall(const CallExpr& call) {
    std::visit(
        Overloaded{
            [&](const Direct& direct) {
              NameTarget(direct);
              if (direct.part.has_value()) {
                std::visit(
                    Overloaded{
                        [](const base::ComponentIndex&) {},
                        [&](const ClassFieldTarget& property) {
                          NameClass(property.owner);
                        }},
                    *direct.part);
              }
            },
            [](const Indirect&) {}, [](const Construct&) {},
            [&](const Virtual& dispatched) {
              NameClass(dispatched.slot.owner_class);
            }},
        call.callee);
  }

  void NameTarget(const Direct& direct) {
    std::visit(
        Overloaded{
            [&](const CallableTarget& target) { NameClass(target.owner); },
            [](const ClosureTarget&) {},
            [&](const UnitCallableTarget& target) {
              if (const std::optional<std::string>& defining =
                      unit_->callables.Get(target.slot).defining_unit) {
                Add(ReferencedNamespace{.unit_name = *defining});
              }
            },
            [](const support::BuiltinFn&) {},
            [&](const ConstructorTarget& target) { NameClass(target.cls); },
            [&](const RecordMethodTarget& target) {
              NameRecord(target.record);
            },
            [](const TypeOperationTarget&) {},
            // The design root brings up every namespace of the design by
            // name, its own set aside.
            [&](const ExternalBringUpTarget& target) {
              if (target.unit_name != unit_->name) {
                Add(ReferencedNamespace{.unit_name = target.unit_name});
              }
            },
            [](const ForeignSymbolTarget&) {}},
        direct.target);
  }

  void NameReference(const ReferenceTarget& target) {
    std::visit(
        Overloaded{
            [](const LocalRef&) {},
            // A definition's address is a symbol its class's identity names,
            // as a handle's type is; it names nothing the class declares.
            [](const DefinitionRef&) {}, [](const ConstantRef&) {},
            [&](const StaticPropertyRef& property) {
              NameClass(property.owner);
            },
            [&](const StaticVariableRef& variable) {
              if (const std::optional<std::string>& defining =
                      unit_->static_variables.Get(variable.variable)
                          .defining_unit) {
                Add(ReferencedNamespace{.unit_name = *defining});
              }
            },
            [&](const FunctionRef& function) {
              NameClass(function.body.owner);
            },
            [&](const DispatchEntryRef& entry) { NameClass(entry.body.owner); },
            [&](const DestructorRef& end) { NameClass(end.of); },
            [&](const DispatchTableRef& table) { NameClass(table.of); },
            [&](const InterfaceTableRef& table) {
              NameClass(table.of);
              NameClass(table.iface);
            },
            [&](const ExternalObjectEntryRef& entry) {
              Add(ReferencedObjectEntry{
                  .unit_name = entry.unit_name, .body_name = entry.body_name});
            }},
        target);
  }

  void Add(ReferencedDeclaration referenced) {
    if (std::ranges::find(referenced_, referenced) == referenced_.end()) {
      referenced_.push_back(std::move(referenced));
    }
  }

  const CompilationUnit* unit_;
  std::set<ClassId> named_classes_;
  std::vector<ReferencedDeclaration> referenced_;
};

}  // namespace

auto ReferencedDeclarationsOf(const CompilationUnit& unit)
    -> std::vector<ReferencedDeclaration> {
  Names names(unit);
  // A class this unit defines is its own declaration, which names what it
  // extends and what its storage holds.
  for (const ClassId id : unit.classes.Ids()) {
    const Class& cls = unit.GetClass(id);
    if (cls.defining_unit.has_value()) continue;
    names.NameClass(id);
    if (cls.definition.has_value()) {
      names.NameConstant(
          LibraryTypeOf(unit.types, support::LibraryDecl::kObjectDefinition),
          *cls.definition);
    }
  }
  // Every constant of the unit is walked, since whichever body or definition
  // names it, the artifact holding it names what it points at.
  for (const ConstantId id : unit.constants.Ids()) {
    const ConstantDecl& constant = unit.constants.Get(id);
    names.NameConstant(constant.type, constant.value);
  }
  ForEachCode(unit, [&](const CodeSource&, const CallableCode& code) {
    names.NameCode(code);
  });
  return std::move(names).Take();
}

}  // namespace lyra::mir

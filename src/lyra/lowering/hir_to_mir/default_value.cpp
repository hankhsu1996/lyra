#include "lyra/lowering/hir_to_mir/default_value.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/real_bits.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/expression/references.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/real_literal.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::lowering::hir_to_mir {

// LRM Table 6-7: a 4-state integral type defaults to all `x`, a 2-state one to
// all zero.
auto DefaultIntegralConstant(const mir::IntegralType& integral)
    -> mir::IntegralConstant {
  mir::IntegralConstant held = mir::BlankIntegralConstant(integral);
  value::FillDefault(
      value::IntegralView{
          .value = held.value_words,
          .unknown = held.state_words,
          .width = integral.bit_width});
  return held;
}

namespace {

// The elements a construction is handed, as one value: the plain-data array of
// the container's element type holding them in order. The container is what
// the construction makes of the list, never what the list itself is.
auto BuildElementList(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId array_type,
    std::vector<mir::ExprId> elements) -> mir::ExprId {
  const mir::TypeId list_type = mir::MachineArrayOf(
      unit.types, RequiredContainerElementType(unit, array_type),
      elements.size());
  return block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(elements)},
          .type = list_type});
}

// Builds the queue or the dynamic array an element list feeds. Neither has a
// size of its own, so its construction states the list and how many times it
// repeats. What a position no element fills holds, and how many a queue may
// hold (LRM 7.4.5, 7.10.5), are the type's to say, so nothing here states
// either.
auto BuildContainerFromElements(
    const mir::CompilationUnit& unit, mir::TypeId array_type,
    mir::ExprId elements, mir::ExprId count) -> mir::Expr {
  if (unit.types.Get(array_type).Is<mir::UnpackedArrayType>()) {
    throw InternalError(
        "BuildContainerFromElements: a fixed-size unpacked array's element "
        "count is its type's, so its construction states no repeat count -- "
        "please report this as a bug");
  }
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Construct{}, .arguments = {elements, count}},
      .type = array_type};
}

// Materialize a folded member-default constant (LRM 7.2.2) as a MIR value of
// its type. A scalar leaf becomes a literal (a string constructs the runtime
// String); an unpacked struct is its recursively materialized components, and
// an unpacked array is the construction that takes them, with the type
// deciding which of the two a component list is.
auto MaterializeConstant(
    const UnitLowerer& unit_lowerer, mir::Block& block, hir::TypeId hir_type,
    const hir::ConstantValue& value) -> mir::Expr {
  const mir::TypeId mir_type = unit_lowerer.TranslateType(hir_type);
  return std::visit(
      Overloaded{
          [&](const hir::IntegralConstant& c) -> mir::Expr {
            return block.exprs.Get(BuildIntegralLiteral(
                unit_lowerer.Unit(), block, mir_type,
                LowerHirIntegralConstant(c)));
          },
          [&](hir::RealBits real) -> mir::Expr {
            return block.exprs.Get(BuildRealLiteral(
                unit_lowerer.Unit(), block, mir_type, real.Value()));
          },
          [&](const std::string& text) -> mir::Expr {
            const mir::ExprId literal = block.exprs.Add(
                mir::Expr{
                    .data = mir::StringLiteral{.value = text},
                    .type = mir_type});
            return mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee = mir::Construct{}, .arguments = {literal}},
                .type = mir_type};
          },
          [&](const std::vector<hir::ConstantValue>& components) -> mir::Expr {
            const auto& hir_ty = unit_lowerer.Hir().types.Get(hir_type);
            if (const auto* st = hir_ty.As<hir::UnpackedStructType>()) {
              std::vector<mir::ExprId> component_ids;
              component_ids.reserve(components.size());
              for (std::size_t i = 0; i < components.size(); ++i) {
                component_ids.push_back(block.exprs.Add(MaterializeConstant(
                    unit_lowerer, block, st->fields[i].type, components[i])));
              }
              return mir::Expr{
                  .data = mir::CompositeExpr{.parts = std::move(component_ids)},
                  .type = mir_type};
            }
            if (const auto* ua = hir_ty.As<hir::UnpackedArrayType>()) {
              std::vector<mir::ExprId> element_ids;
              element_ids.reserve(components.size());
              for (const auto& component : components) {
                element_ids.push_back(block.exprs.Add(MaterializeConstant(
                    unit_lowerer, block, ua->element_type, component)));
              }
              return BuildArrayConstructionCall(
                  unit_lowerer.Unit(), block, mir_type, std::move(element_ids));
            }
            throw InternalError(
                "MaterializeConstant: aggregate value for a non-aggregate "
                "type");
          },
      },
      value.data);
}

}  // namespace

auto BuildDefaultValueExpr(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId type)
    -> mir::Expr {
  const auto& ty = unit.types.Get(type);
  // The type an arm receives is a view into the pool, and building a component
  // default can intern a type and relocate what the pool holds. An arm that
  // recurses therefore reads what it needs out of its type first.
  //
  // LRM Table 7-1: an unpacked union defaults to its first member's default.
  // LRM 11.9 leaves an uninitialized tagged union undefined, and Lyra's
  // deterministic fallback is that same first member, so one build answers
  // both. Synthesized at each use rather than stored on the interned type, so
  // two source declarations with the same component types share one type.
  const auto first_member_default = [&](mir::TypeId first) -> mir::Expr {
    constexpr base::ComponentIndex kFirstMember{0};
    return mir::MakeActiveMemberExpr(
        block.exprs.Add(BuildDefaultValueExpr(unit, block, first)),
        kFirstMember, type);
  };
  const auto component_wise =
      [&](std::span<const mir::TypeId> view) -> mir::Expr {
    const std::vector<mir::TypeId> components(view.begin(), view.end());
    std::vector<mir::ExprId> parts;
    parts.reserve(components.size());
    for (const mir::TypeId component : components) {
      parts.push_back(
          block.exprs.Add(BuildDefaultValueExpr(unit, block, component)));
    }
    return mir::Expr{
        .data = mir::CompositeExpr{.parts = std::move(parts)}, .type = type};
  };
  // LRM Table 6-7 and Table 7-1 answer for the types a variable, a member, or
  // an element is declared to hold, and that is the whole of what is ever asked
  // here. The rest of the vocabulary is what a lowering builds to carry one of
  // those or to reach it, so a value of it starts out wherever the step that
  // built it put it, and being asked what one starts out as means a producer
  // handed over a type no declaration names.
  const auto holds_no_declared_value = [](std::string_view what) -> mir::Expr {
    throw InternalError(
        std::format(
            "BuildDefaultValueExpr: {} is not what a declaration holds, so "
            "there is no initial value to state for it",
            what));
  };
  return ty.Visit(
      Overloaded{
          [&](const mir::IntegralType& integral) -> mir::Expr {
            return block.exprs.Get(BuildIntegralLiteral(
                unit, block, type, DefaultIntegralConstant(integral)));
          },
          // An enumeration defaults as the vector it is: LRM Table 6-7 reads
          // the default off the state domain, which the base carries and the
          // names do not affect.
          [&](const mir::EnumType& e) -> mir::Expr {
            return block.exprs.Get(BuildIntegralLiteral(
                unit, block, type, DefaultIntegralConstant(e.base)));
          },
          [&](const mir::StringType&) -> mir::Expr {
            // The literal is machine text; the empty string value is what the
            // string's constructor builds from it.
            const mir::ExprId lit = block.exprs.Add(
                mir::Expr{
                    .data = mir::StringLiteral{.value = std::string{}},
                    .type = type});
            return mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee = mir::Construct{}, .arguments = {lit}},
                .type = type};
          },
          [&](const mir::RealType&) -> mir::Expr {
            return block.exprs.Get(BuildRealLiteral(unit, block, type, 0.0));
          },
          [&](const mir::ShortRealType&) -> mir::Expr {
            return block.exprs.Get(BuildRealLiteral(unit, block, type, 0.0));
          },
          // LRM Table 7-1: a fixed unpacked array defaults to every element at
          // the element type's default. One element default fills the array,
          // so stating it stays O(1) in the array's element count.
          [&](const mir::UnpackedArrayType& ua) -> mir::Expr {
            const mir::TypeId element_type = ua.element_type;
            return BuildFixedArrayConstructionCall(
                unit, block, type,
                {block.exprs.Add(
                    BuildDefaultValueExpr(unit, block, element_type))});
          },
          // LRM Table 7-1: a product no declaration names defaults
          // component-wise -- each takes its own type's default, recursively.
          [&](const mir::TupleType& t) -> mir::Expr {
            return component_wise(t.elements);
          },
          // A structure states its own default, where its members' initial
          // values are written (LRM 7.2.2).
          [&](const mir::StructType& s) -> mir::Expr {
            return mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee =
                            mir::Direct{
                                .target =
                                    mir::StructMethodTarget{
                                        .declaration =
                                            mir::StructDeclarationOf(unit, s),
                                        .answers = support::BuiltinFn::
                                            kDeclaredDefault}},
                        .arguments = {}},
                .type = type};
          },
          [&](const mir::UnionType& u) -> mir::Expr {
            return first_member_default(u.members.front());
          },
          [&](const mir::TaggedUnionType& u) -> mir::Expr {
            return first_member_default(u.members.front());
          },
          // LRM Table 7-1: a dynamic array's default and a queue's are the
          // empty array, which its own construction builds from a list of no
          // elements.
          [&](const mir::DynamicArrayType&) -> mir::Expr {
            return BuildArrayConstructionCall(unit, block, type, {});
          },
          [&](const mir::QueueType&) -> mir::Expr {
            return BuildArrayConstructionCall(unit, block, type, {});
          },
          // LRM Table 7-1: an associative array's default is empty, which is
          // its literal over no entries and no `default:` clause.
          [&](const mir::AssociativeArrayType&) -> mir::Expr {
            return BuildAssociativeConstructionCall(
                unit, block, type, {}, std::nullopt);
          },
          // Types whose default is what their own constructor makes of no
          // arguments: a named event, which SV gives no initializer grammar at
          // all, and an object, whose members the constructor scope is what
          // populates.
          [&](const mir::EventType&) -> mir::Expr {
            return mir::Expr{
                .data =
                    mir::CallExpr{.callee = mir::Construct{}, .arguments = {}},
                .type = type};
          },
          // What a declaration holds for an object whose class this unit does
          // not declare is a handle to it, and a handle starts out null; the
          // object type itself is what that handle points at, which a
          // construction brings into existence.
          [&](const mir::ObjectType& o) -> mir::Expr {
            if (std::holds_alternative<mir::CrossUnitClassRef>(o.of)) {
              return holds_no_declared_value(
                  "an object of another unit's class");
            }
            return mir::Expr{
                .data =
                    mir::CallExpr{.callee = mir::Construct{}, .arguments = {}},
                .type = type};
          },
          [&](const mir::PointerType&) -> mir::Expr {
            return mir::Expr{.data = mir::NullLiteral{}, .type = type};
          },
          // A reference is bound where it is declared and is never null.
          [&](const mir::ReferenceType&) -> mir::Expr {
            return holds_no_declared_value("a reference");
          },
          // LRM 8.4 / Table 7-1: an uninitialized class handle defaults to
          // null.
          [&](const mir::ManagedRefType&) -> mir::Expr {
            return mir::Expr{.data = mir::NullLiteral{}, .type = type};
          },
          // LRM 6.14: a chandle is always initialized to null.
          [&](const mir::ChandleType&) -> mir::Expr {
            return mir::Expr{.data = mir::NullLiteral{}, .type = type};
          },
          // A declaration standing for no object holds a sequence of nothing,
          // which its own constructor builds from a list of no elements.
          [&](const mir::VectorType&) -> mir::Expr {
            return BuildSequenceConstructionCall(unit, block, type, {});
          },
          // A type carrying no information has exactly one value, so that
          // value is its default.
          [&](const mir::EmptyType&) -> mir::Expr {
            return mir::Expr{
                .data =
                    mir::CallExpr{.callee = mir::Construct{}, .arguments = {}},
                .type = type};
          },

          // LRM 7.8 forbids an index type from declaring a type, so nothing is
          // ever declared to be one and no declaration's default can be asked
          // for. An index still reaches the entries of such an array, carrying
          // whatever the expression that named it carried; what it never does
          // is give a variable its type.
          [&](const mir::WildcardIndexType&) -> mir::Expr {
            return holds_no_declared_value("an associative array's index");
          },

          // The machine vocabulary a lowering builds to carry a value: a count,
          // a flag, a host string, a sequence of operands, the address of a
          // body. Each is built where it is used, out of what it is built from.
          [&](const mir::MachineIntType&) -> mir::Expr {
            return holds_no_declared_value("a machine integer");
          },
          [&](const mir::MachineFloatType&) -> mir::Expr {
            return holds_no_declared_value("a machine floating-point number");
          },
          [&](const mir::MachineBoolType&) -> mir::Expr {
            return holds_no_declared_value("a machine boolean");
          },
          [&](const mir::MachineCStringType&) -> mir::Expr {
            return holds_no_declared_value("a machine string");
          },
          [&](const mir::MachineArrayType&) -> mir::Expr {
            return holds_no_declared_value("a machine array of operands");
          },
          [&](const mir::MachineFunctionType&) -> mir::Expr {
            return holds_no_declared_value("a machine function address");
          },

          [&](const mir::RuntimeClassType&) -> mir::Expr {
            return holds_no_declared_value("an object of a runtime class");
          },

          // Storage the runtime installs and a service it provides: a cell that
          // is observed, a net's resolution, a driver's contribution, a write
          // open on one of them, the history a sampled value reads, what an
          // assertion has in flight, the effects a body runs under, the open
          // files, the diagnostic stream, and the library values a call names.
          // What a declaration holds is the value inside one of these, which
          // answers above.
          [&](const mir::ObservableType&) -> mir::Expr {
            return holds_no_declared_value("an observable cell");
          },
          [&](const mir::ResolvedType&) -> mir::Expr {
            return holds_no_declared_value("a net's resolution");
          },
          [&](const mir::DriverType&) -> mir::Expr {
            return holds_no_declared_value("a driver's contribution");
          },
          [&](const mir::OpenWriteType&) -> mir::Expr {
            return holds_no_declared_value("a write in progress");
          },
          [&](const mir::DesignationType&) -> mir::Expr {
            return holds_no_declared_value("a part a write designates");
          },
          [&](const mir::ObjectWriteType&) -> mir::Expr {
            return holds_no_declared_value("a write into an object");
          },
          [&](const mir::SampledHistoryType&) -> mir::Expr {
            return holds_no_declared_value("a sampled value's history");
          },
          [&](const mir::EvaluationAttemptsType&) -> mir::Expr {
            return holds_no_declared_value("an assertion's attempts");
          },
          [&](const mir::LoopBlocksType&) -> mir::Expr {
            return holds_no_declared_value("the blocks a loop built");
          },
          [&](const mir::RuntimeEffectsType&) -> mir::Expr {
            return holds_no_declared_value("the effects a body runs under");
          },
          [&](const mir::FilesType&) -> mir::Expr {
            return holds_no_declared_value("the open files");
          },
          [&](const mir::DiagnosticType&) -> mir::Expr {
            return holds_no_declared_value("the diagnostic stream");
          },
          [&](const mir::RuntimeLibraryType&) -> mir::Expr {
            return holds_no_declared_value("a runtime library value");
          },

          // Code, and the storage a body reaches through rather than holds: a
          // suspending body's frame, a closure's, and a reference bound to a
          // cell that already exists.
          [&](const mir::CoroutineType&) -> mir::Expr {
            return holds_no_declared_value("a suspending body");
          },
          [&](const mir::ClosureType&) -> mir::Expr {
            return holds_no_declared_value("a closure");
          },
          [&](const mir::VariableRefType&) -> mir::Expr {
            return holds_no_declared_value("a reference to a cell");
          },
          [&](const mir::ReferenceWrapperType&) -> mir::Expr {
            return holds_no_declared_value("a reference to a cell");
          },

          // LRM 7.3.2's `void` member occupies a value slot and is carried as
          // the type holding no information, which answers above. Everywhere
          // else `void` is the absence of a result, and an absence is not
          // declared.
          [&](const mir::VoidType&) -> mir::Expr {
            return holds_no_declared_value("the absence of a result");
          },
      });
}

auto BuildDeclaredStructValue(
    const UnitLowerer& unit_lowerer, mir::Block& block,
    const hir::UnpackedStructType& source, mir::TypeId structure) -> mir::Expr {
  std::vector<mir::ExprId> components;
  components.reserve(source.fields.size());
  for (const auto& field : source.fields) {
    components.push_back(
        field.default_init.has_value()
            ? block.exprs.Add(MaterializeConstant(
                  unit_lowerer, block, field.type, *field.default_init))
            : block.exprs.Add(
                  BuildDefaultValueFromHir(unit_lowerer, block, field.type)));
  }
  return mir::Expr{
      .data = mir::CompositeExpr{.parts = std::move(components)},
      .type = structure};
}

auto BuildDefaultValueFromHir(
    const UnitLowerer& unit_lowerer, mir::Block& block, hir::TypeId hir_type)
    -> mir::Expr {
  return BuildDefaultValueExpr(
      unit_lowerer.Unit(), block, unit_lowerer.TranslateType(hir_type));
}

namespace {

// The three containers that hold their elements as an ordered run, which is
// what makes a value of one of them cross into another as it stands.
auto IsArrayContainerType(const mir::Type& type) -> bool {
  return type.Is<mir::UnpackedArrayType>() ||
         type.Is<mir::DynamicArrayType>() || type.Is<mir::QueueType>();
}

}  // namespace

auto CrossesArrayContainerKinds(
    const mir::Type& source, const mir::Type& destination) -> bool {
  const bool same_kind =
      (source.Is<mir::UnpackedArrayType>() &&
       destination.Is<mir::UnpackedArrayType>()) ||
      (source.Is<mir::DynamicArrayType>() &&
       destination.Is<mir::DynamicArrayType>()) ||
      (source.Is<mir::QueueType>() && destination.Is<mir::QueueType>());
  return IsArrayContainerType(source) && IsArrayContainerType(destination) &&
         !same_kind;
}

auto RequiredContainerElementType(
    const mir::CompilationUnit& unit, mir::TypeId container) -> mir::TypeId {
  const std::optional<mir::TypeId> element =
      unit.types.Get(container).ContainerElementType();
  if (!element.has_value()) {
    throw InternalError(
        "RequiredContainerElementType: the type holds no elements, and the "
        "caller reached it only because its own construction said it would -- "
        "please report this as a bug");
  }
  return *element;
}

auto BuildFixedArrayConstructionCall(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId array_type,
    std::vector<mir::ExprId> repeat_unit) -> mir::Expr {
  const std::uint64_t element_count =
      unit.types.Get(array_type).Get<mir::UnpackedArrayType>().Size();
  if (repeat_unit.empty() || element_count % repeat_unit.size() != 0) {
    throw InternalError(
        std::format(
            "BuildFixedArrayConstructionCall: {} elements laid down again and "
            "again do not fill an array of {} -- please report this as a bug",
            repeat_unit.size(), element_count));
  }
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Construct{},
              .arguments = {BuildElementList(
                  unit, block, array_type, std::move(repeat_unit))}},
      .type = array_type};
}

auto BuildArrayConstructionCall(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId array_type,
    std::vector<mir::ExprId> elements) -> mir::Expr {
  if (unit.types.Get(array_type).Is<mir::UnpackedArrayType>()) {
    return BuildFixedArrayConstructionCall(
        unit, block, array_type, std::move(elements));
  }
  const mir::ExprId list_id =
      BuildElementList(unit, block, array_type, std::move(elements));
  return BuildContainerFromElements(
      unit, array_type, list_id, BuildMachineIntLiteral(unit, block, 1));
}

auto BuildArrayRepeatCall(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId array_type,
    std::vector<mir::ExprId> repeat_unit, mir::ExprId count_id) -> mir::Expr {
  const mir::ExprId repeat_unit_id =
      BuildElementList(unit, block, array_type, std::move(repeat_unit));
  return BuildContainerFromElements(unit, array_type, repeat_unit_id, count_id);
}

auto BuildSequenceConstructionCall(
    const mir::CompilationUnit& unit, mir::Block& block,
    mir::TypeId sequence_type, std::vector<mir::ExprId> elements) -> mir::Expr {
  const mir::TypePool& types = unit.types;
  const mir::TypeId list_type = mir::MachineArrayOf(
      types, types.Get(sequence_type).Get<mir::VectorType>().element,
      elements.size());
  const mir::ExprId list_id = block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(elements)},
          .type = list_type});
  return mir::Expr{
      .data = mir::CallExpr{.callee = mir::Construct{}, .arguments = {list_id}},
      .type = sequence_type};
}

auto BuildAssociativeConstructionCall(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId assoc_type,
    std::vector<std::pair<mir::ExprId, mir::ExprId>> entries,
    std::optional<mir::ExprId> stated_default) -> mir::Expr {
  const auto* assoc =
      unit.types.Get(assoc_type).As<mir::AssociativeArrayType>();
  if (assoc == nullptr) {
    throw InternalError(
        "BuildAssociativeConstructionCall: result type is not "
        "AssociativeArrayType");
  }
  const mir::TypeId key_type = assoc->key_type;
  const mir::TypeId element_type = assoc->element_type;

  const mir::TypeId tuple_type = unit.types.Intern(
      mir::Type{mir::TupleType{.elements = {key_type, element_type}}});
  std::vector<mir::ExprId> tuple_ids;
  tuple_ids.reserve(entries.size());
  for (const auto& [key_id, value_id] : entries) {
    tuple_ids.push_back(block.exprs.Add(
        mir::Expr{
            .data = mir::CompositeExpr{.parts = {key_id, value_id}},
            .type = tuple_type}));
  }
  const mir::TypeId entries_type =
      mir::MachineArrayOf(unit.types, tuple_type, tuple_ids.size());
  const mir::ExprId entries_id = block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(tuple_ids)},
          .type = entries_type});

  mir::Expr built{
      .data =
          mir::CallExpr{.callee = mir::Construct{}, .arguments = {entries_id}},
      .type = assoc_type};
  if (!stated_default.has_value()) {
    return built;
  }
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kAssocWithDefault,
                      .receiver = block.exprs.Add(std::move(built))},
              .arguments = {*stated_default}},
      .type = assoc_type};
}

}  // namespace lyra::lowering::hir_to_mir

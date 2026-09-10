#include "lyra/lowering/hir_to_mir/default_value.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
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

namespace lyra::lowering::hir_to_mir {

// LRM Table 6-7: 4-state integral types default to all-`x`; 2-state default
// to all-zero. The `x` encoding is (value=1, state=1) per bit, so the bit
// planes are 1s up to `width`, with the unused high bits of the top word
// zeroed per IntegralConstant's word-layout invariant.
auto DefaultIntegralConstant(const mir::PackedArrayType& pa)
    -> mir::IntegralConstant {
  const auto width = static_cast<std::uint32_t>(pa.BitWidth());
  const bool four_state = pa.state_kind == mir::IntegralStateKind::kFourState;
  const std::size_t word_count = (width + 63U) / 64U;
  mir::IntegralConstant c{
      .value_words = std::vector<std::uint64_t>(word_count, 0U),
      .state_words = {}};
  if (!four_state) {
    return c;
  }
  std::ranges::fill(c.value_words, ~std::uint64_t{0});
  c.state_words = std::vector<std::uint64_t>(word_count, ~std::uint64_t{0});
  const std::uint32_t top_bits = width % 64U;
  if (top_bits != 0U && !c.value_words.empty()) {
    const std::uint64_t mask = (std::uint64_t{1} << top_bits) - 1U;
    c.value_words.back() &= mask;
    c.state_words.back() &= mask;
  }
  return c;
}

namespace {

// The LRM 7.10.5 maximum index a bounded queue enforces, appended as the value
// the construction is handed so the runtime trims an over-long initializer. A
// container whose type declares no bound -- which is every container but a
// bounded queue -- appends nothing.
void AppendBoundedQueueMax(
    const mir::CompilationUnit& unit, mir::Block& block,
    std::vector<mir::ExprId>& args, mir::TypeId array_type) {
  const auto* queue = unit.types.Get(array_type).As<mir::QueueType>();
  if (queue == nullptr || !queue->max_bound.has_value()) {
    return;
  }
  args.push_back(BuildIntLiteral(
      unit, block, static_cast<std::int64_t>(*queue->max_bound)));
}

// Builds the container an element list feeds: the element type's default, the
// list, how many times the list repeats, and a bounded queue's declared bound.
// The list itself is the value literal; what it feeds is the container's own
// constructor, which is a call like any other.
auto BuildContainerFromElements(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId array_type,
    mir::ExprId element_default, mir::ExprId elements, mir::ExprId count)
    -> mir::Expr {
  std::vector<mir::ExprId> args = {element_default, elements, count};
  AppendBoundedQueueMax(unit, block, args, array_type);
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Construct{}, .arguments = std::move(args)},
      .type = array_type};
}

// A folded-constant array (LRM 7.2.2), whose elements are materialized
// individually because a constant fixes each element's own value. The element
// type's default carries its own member inits, and the list stands for itself,
// so it repeats once.
auto BuildUnpackedArrayValue(
    const UnitLowerer& unit_lowerer, mir::Block& block, mir::TypeId array_type,
    hir::TypeId element_type, std::vector<mir::ExprId> element_ids)
    -> mir::Expr {
  const mir::ExprId element_default = block.exprs.Add(
      BuildDefaultValueFromHir(unit_lowerer, block, element_type));
  const mir::TypeId list_type = mir::MachineArrayOf(
      unit_lowerer.Unit().types, unit_lowerer.TranslateType(element_type),
      element_ids.size());
  const mir::ExprId list_id = block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(element_ids)},
          .type = list_type});
  const mir::ExprId count_id =
      BuildMachineIntLiteral(unit_lowerer.Unit(), block, 1);
  return BuildContainerFromElements(
      unit_lowerer.Unit(), block, array_type, element_default, list_id,
      count_id);
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
          [&](double real) -> mir::Expr {
            return block.exprs.Get(
                BuildRealLiteral(unit_lowerer.Unit(), block, mir_type, real));
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
              return BuildUnpackedArrayValue(
                  unit_lowerer, block, mir_type, ua->element_type,
                  std::move(element_ids));
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
  const auto first_member_default =
      [&](std::span<const mir::TypeId> members) -> mir::Expr {
    constexpr base::ComponentIndex kFirstMember{0};
    return mir::MakeActiveMemberExpr(
        block.exprs.Add(
            BuildDefaultValueExpr(unit, block, members[kFirstMember.value])),
        kFirstMember, type);
  };
  return ty.Visit(
      Overloaded{
          [&](const mir::PackedArrayType& pa) -> mir::Expr {
            return block.exprs.Get(BuildIntegralLiteral(
                unit, block, type, DefaultIntegralConstant(pa)));
          },
          [&](const mir::EnumType& e) -> mir::Expr {
            return block.exprs.Get(BuildIntegralLiteral(
                unit, block, type, DefaultIntegralConstant(e.base)));
          },
          [&](const mir::StringType&) -> mir::Expr {
            // Software string literal -> `value::String("")` via the
            // constructor.
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
          [&](const mir::RealTimeType&) -> mir::Expr {
            return block.exprs.Get(BuildRealLiteral(unit, block, type, 0.0));
          },
          // LRM Table 7-1: a fixed unpacked array defaults to every element at
          // the element type's default. That uniform value is the element
          // default replicated across the array's size, so it builds through
          // the repeat call and stays O(1) in the array's element count. The
          // shield seed and the repeat unit are the same element default.
          [&](const mir::UnpackedArrayType& ua) -> mir::Expr {
            const auto size = static_cast<std::int64_t>(ua.Size());
            const mir::ExprId element_default = block.exprs.Add(
                BuildDefaultValueExpr(unit, block, ua.element_type));
            const mir::ExprId size_id =
                BuildMachineIntLiteral(unit, block, size);
            return BuildArrayRepeatCall(
                unit, block, type, element_default, {element_default}, size_id);
          },
          // LRM Table 7-1: an unpacked struct defaults member-wise -- each
          // component takes its own type's default, recursively. Synthesized at
          // each use rather than stored on the interned type, so structs with
          // the same component types but different member initializers share
          // one type.
          [&](const mir::TupleType& t) -> mir::Expr {
            const std::vector<mir::TypeId> element_types = t.elements;
            std::vector<mir::ExprId> components;
            components.reserve(element_types.size());
            for (const mir::TypeId elem : element_types) {
              components.push_back(
                  block.exprs.Add(BuildDefaultValueExpr(unit, block, elem)));
            }
            return mir::Expr{
                .data = mir::CompositeExpr{.parts = std::move(components)},
                .type = type};
          },
          [&](const mir::UnionType& u) -> mir::Expr {
            return first_member_default(u.elements);
          },
          [&](const mir::TaggedUnionType& u) -> mir::Expr {
            return first_member_default(u.elements);
          },
          // LRM Table 6-7: a dynamic array's default is the empty array.
          // The wrapper still needs the element type's default supplied at
          // construction so OOB reads and resize-fills have a shape source,
          // which is the one operand the empty form takes.
          [&](const mir::DynamicArrayType& da) -> mir::Expr {
            const mir::ExprId element_default = block.exprs.Add(
                BuildDefaultValueExpr(unit, block, da.element_type));
            return mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee =
                            mir::Direct{
                                .target = support::BuiltinFn::
                                    kMakeDynamicArrayDefault},
                        .arguments = {element_default}},
                .type = type};
          },
          // LRM Table 6-7: a queue's default is the empty queue, which its own
          // constructor builds from a list of no elements.
          [&](const mir::QueueType&) -> mir::Expr {
            return BuildArrayConstructionCall(unit, block, type, {});
          },
          // LRM Table 6-7: an associative array's default is empty, which is
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
          [&](const mir::ObjectType&) -> mir::Expr {
            return mir::Expr{
                .data =
                    mir::CallExpr{.callee = mir::Construct{}, .arguments = {}},
                .type = type};
          },
          [&](const mir::PointerType&) -> mir::Expr {
            return mir::Expr{.data = mir::NullLiteral{}, .type = type};
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
          [&](const auto&) -> mir::Expr {
            throw InternalError(
                "BuildDefaultValueExpr: type kind has no default-value "
                "representation");
          },
      });
}

// LRM 7.2.2 / Table 7-1: default-construct a value from its source (HIR) type
// so member declaration initializers are honored. An unpacked struct takes each
// member's own default (its declaration initializer, else the member type's
// recursive default); a fixed unpacked array fills every element with the
// element type's source default. Every other type carries no source-level
// initializer, so its default is the canonical type default.
auto BuildDefaultValueFromHir(
    const UnitLowerer& unit_lowerer, mir::Block& block, hir::TypeId hir_type)
    -> mir::Expr {
  const auto& hir_ty = unit_lowerer.Hir().types.Get(hir_type);
  const mir::TypeId mir_type = unit_lowerer.TranslateType(hir_type);

  if (const auto* st = hir_ty.As<hir::UnpackedStructType>()) {
    std::vector<mir::ExprId> components;
    components.reserve(st->fields.size());
    for (const auto& field : st->fields) {
      const mir::ExprId component =
          field.default_init.has_value()
              ? block.exprs.Add(MaterializeConstant(
                    unit_lowerer, block, field.type, *field.default_init))
              : block.exprs.Add(
                    BuildDefaultValueFromHir(unit_lowerer, block, field.type));
      components.push_back(component);
    }
    return mir::Expr{
        .data = mir::CompositeExpr{.parts = std::move(components)},
        .type = mir_type};
  }

  if (const auto* ua = hir_ty.As<hir::UnpackedArrayType>()) {
    const std::int64_t span = (ua->dim.left >= ua->dim.right)
                                  ? (ua->dim.left - ua->dim.right)
                                  : (ua->dim.right - ua->dim.left);
    const auto size = static_cast<std::uint64_t>(span) + 1U;
    const mir::ExprId element_default = block.exprs.Add(
        BuildDefaultValueFromHir(unit_lowerer, block, ua->element_type));
    const mir::ExprId size_id = BuildMachineIntLiteral(
        unit_lowerer.Unit(), block, static_cast<std::int64_t>(size));
    return BuildArrayRepeatCall(
        unit_lowerer.Unit(), block, mir_type, element_default,
        {element_default}, size_id);
  }

  return BuildDefaultValueExpr(unit_lowerer.Unit(), block, mir_type);
}

auto IsArrayContainerType(const mir::Type& type) -> bool {
  return type.Is<mir::UnpackedArrayType>() ||
         type.Is<mir::DynamicArrayType>() || type.Is<mir::QueueType>();
}

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

auto ContainerElementType(const mir::CompilationUnit& unit, mir::TypeId type)
    -> std::optional<mir::TypeId> {
  using Element = std::optional<mir::TypeId>;
  return unit.types.Get(type).Visit(
      Overloaded{
          [](const mir::UnpackedArrayType& t) -> Element {
            return t.element_type;
          },
          [](const mir::DynamicArrayType& t) -> Element {
            return t.element_type;
          },
          [](const mir::QueueType& t) -> Element { return t.element_type; },
          [](const mir::AssociativeArrayType& t) -> Element {
            return t.element_type;
          },
          [](const auto&) -> Element { return std::nullopt; }});
}

auto RequiredContainerElementType(
    const mir::CompilationUnit& unit, mir::TypeId container) -> mir::TypeId {
  const std::optional<mir::TypeId> element =
      ContainerElementType(unit, container);
  if (!element.has_value()) {
    throw InternalError(
        "RequiredContainerElementType: the type holds no elements, and the "
        "caller reached it only because its own construction said it would -- "
        "please report this as a bug");
  }
  return *element;
}

auto BuildArrayConstructionCall(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId array_type,
    std::vector<mir::ExprId> elements) -> mir::Expr {
  const mir::TypeId element_type =
      RequiredContainerElementType(unit, array_type);
  const mir::ExprId element_default =
      block.exprs.Add(BuildDefaultValueExpr(unit, block, element_type));
  const mir::TypeId list_type =
      mir::MachineArrayOf(unit.types, element_type, elements.size());
  const mir::ExprId list_id = block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(elements)},
          .type = list_type});
  const mir::ExprId count_id = BuildMachineIntLiteral(unit, block, 1);
  return BuildContainerFromElements(
      unit, block, array_type, element_default, list_id, count_id);
}

auto BuildArrayRepeatCall(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId array_type,
    mir::ExprId element_default, std::vector<mir::ExprId> repeat_unit,
    mir::ExprId count_id) -> mir::Expr {
  const mir::TypeId repeat_unit_type = mir::MachineArrayOf(
      unit.types, RequiredContainerElementType(unit, array_type),
      repeat_unit.size());
  const mir::ExprId repeat_unit_id = block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(repeat_unit)},
          .type = repeat_unit_type});
  return BuildContainerFromElements(
      unit, block, array_type, element_default, repeat_unit_id, count_id);
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
    std::optional<mir::ExprId> user_default) -> mir::Expr {
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

  const mir::ExprId element_default =
      block.exprs.Add(BuildDefaultValueExpr(unit, block, element_type));
  // Every associative array answers a read of an absent key with something
  // (LRM 7.8.6), so that answer is always an operand: a `default:` clause names
  // it, and a literal without one names the element type's own default, which
  // is what such a read returns.
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Construct{},
              .arguments =
                  {element_default, entries_id,
                   user_default.value_or(element_default)}},
      .type = assoc_type};
}

}  // namespace lyra::lowering::hir_to_mir

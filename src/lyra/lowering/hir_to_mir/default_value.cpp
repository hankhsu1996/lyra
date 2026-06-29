#include "lyra/lowering/hir_to_mir/default_value.hpp"

#include <algorithm>
#include <cstdint>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/expression/references.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

// LRM Table 6-7: 4-state integral types default to all-`x`; 2-state default
// to all-zero. The `x` encoding is (value=1, state=1) per bit, so the bit
// planes are 1s up to `width`, with the unused high bits of the top word
// zeroed per IntegralConstant's word-layout invariant.
auto DefaultIntegralConstant(const mir::PackedArrayType& pa)
    -> mir::IntegralConstant {
  const auto width = static_cast<std::uint32_t>(pa.BitWidth());
  const bool four_state = pa.IsFourState();
  const std::size_t word_count = (width + 63U) / 64U;
  mir::IntegralConstant c{
      .value_words = std::vector<std::uint64_t>(word_count, 0U),
      .state_words = {},
      .width = width,
      .signedness = pa.signedness,
      .state_kind = four_state ? mir::IntegralStateKind::kFourState
                               : mir::IntegralStateKind::kTwoState};
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

// Wrap element exprs into an unpacked-array construction: the element type's
// default prototype (honoring its own member inits) plus the element list.
// Shared by the type-default array path and the explicit-constant array path.
auto BuildUnpackedArrayValue(
    const ModuleLowerer& module, WalkFrame frame, mir::TypeId array_type,
    hir::TypeId element_type, std::vector<mir::ExprId> element_ids)
    -> mir::Expr {
  auto& block = *frame.current_block;
  const mir::ExprId element_default =
      block.exprs.Add(BuildDefaultValueFromHir(module, frame, element_type));
  const mir::ExprId list_id = block.exprs.Add(
      mir::Expr{
          .data = mir::ArrayLiteralExpr{.elements = std::move(element_ids)},
          .type = array_type});
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Construct{},
              .arguments = {element_default, list_id}},
      .type = array_type};
}

// Materialize a folded member-default constant (LRM 7.2.2) as a MIR value of
// its type. A scalar leaf becomes a literal (a string constructs the runtime
// String); an unpacked aggregate becomes a TupleExpr (struct) or an array
// construction (array) over the recursively materialized components, with the
// type disambiguating a component list as struct members or array elements.
auto MaterializeConstant(
    const ModuleLowerer& module, WalkFrame frame, hir::TypeId hir_type,
    const hir::ConstantValue& value) -> mir::Expr {
  auto& block = *frame.current_block;
  const mir::TypeId mir_type = module.TranslateType(hir_type);
  return std::visit(
      Overloaded{
          [&](const hir::IntegralConstant& c) -> mir::Expr {
            return mir::Expr{
                .data =
                    mir::IntegerLiteral{.value = LowerHirIntegralConstant(c)},
                .type = mir_type};
          },
          [&](double real) -> mir::Expr {
            return mir::Expr{
                .data = mir::RealLiteral{.value = real}, .type = mir_type};
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
            const auto& hir_ty = module.Hir().types.Get(hir_type);
            if (const auto* st =
                    std::get_if<hir::UnpackedStructType>(&hir_ty.data)) {
              std::vector<mir::ExprId> component_ids;
              component_ids.reserve(components.size());
              for (std::size_t i = 0; i < components.size(); ++i) {
                component_ids.push_back(block.exprs.Add(MaterializeConstant(
                    module, frame, st->fields[i].type, components[i])));
              }
              return mir::Expr{
                  .data =
                      mir::TupleExpr{.components = std::move(component_ids)},
                  .type = mir_type};
            }
            if (const auto* ua =
                    std::get_if<hir::UnpackedArrayType>(&hir_ty.data)) {
              std::vector<mir::ExprId> element_ids;
              element_ids.reserve(components.size());
              for (const auto& component : components) {
                element_ids.push_back(block.exprs.Add(MaterializeConstant(
                    module, frame, ua->element_type, component)));
              }
              return BuildUnpackedArrayValue(
                  module, frame, mir_type, ua->element_type,
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
    const ModuleLowerer& module, WalkFrame frame, mir::TypeId type)
    -> mir::Expr {
  auto& block = *frame.current_block;
  const auto& ty = module.Unit().types.Get(type);
  return std::visit(
      Overloaded{
          [&](const mir::PackedArrayType& pa) -> mir::Expr {
            return mir::Expr{
                .data =
                    mir::IntegerLiteral{.value = DefaultIntegralConstant(pa)},
                .type = type};
          },
          [&](const mir::EnumType& e) -> mir::Expr {
            return mir::Expr{
                .data =
                    mir::IntegerLiteral{
                        .value = DefaultIntegralConstant(e.base)},
                .type = type};
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
            return mir::Expr{
                .data = mir::RealLiteral{.value = 0.0}, .type = type};
          },
          [&](const mir::ShortRealType&) -> mir::Expr {
            return mir::Expr{
                .data = mir::RealLiteral{.value = 0.0}, .type = type};
          },
          [&](const mir::RealTimeType&) -> mir::Expr {
            return mir::Expr{
                .data = mir::RealLiteral{.value = 0.0}, .type = type};
          },
          [&](const mir::UnpackedArrayType& ua) -> mir::Expr {
            std::vector<mir::ExprId> elements;
            elements.reserve(ua.size);
            for (std::uint64_t i = 0; i < ua.size; ++i) {
              elements.push_back(block.exprs.Add(
                  BuildDefaultValueExpr(module, frame, ua.element_type)));
            }
            return BuildArrayConstructionCall(
                module, frame, type, std::move(elements));
          },
          // LRM Table 7-1: an unpacked struct defaults member-wise -- each
          // component takes its own type's default, recursively. Synthesized as
          // a TupleExpr at each use, never stored on the interned TupleType, so
          // structs with the same component types but different member
          // initializers share one type.
          [&](const mir::TupleType& t) -> mir::Expr {
            std::vector<mir::ExprId> components;
            components.reserve(t.elements.size());
            for (const mir::TypeId elem : t.elements) {
              components.push_back(
                  block.exprs.Add(BuildDefaultValueExpr(module, frame, elem)));
            }
            return mir::Expr{
                .data = mir::TupleExpr{.components = std::move(components)},
                .type = type};
          },
          // LRM Table 7-1: an unpacked union defaults to its first member's
          // default. Synthesized as a UnionExpr over component 0 at each use,
          // never stored on the interned UnionType (same rationale as the
          // struct's member-wise default).
          [&](const mir::UnionType& u) -> mir::Expr {
            const mir::ExprId member_default = block.exprs.Add(
                BuildDefaultValueExpr(module, frame, u.elements.front()));
            return mir::Expr{
                .data = mir::UnionExpr{.index = 0, .value = member_default},
                .type = type};
          },
          // LRM Table 6-7: a dynamic array's default is the empty array.
          // The wrapper still needs the element type's default supplied at
          // construction so OOB reads and resize-fills have a shape source.
          // Emit chain: a construction call on `element_default` -> the
          // `DynamicArray<T>(...)` ctor that stores the value and leaves
          // `data_` empty.
          [&](const mir::DynamicArrayType& da) -> mir::Expr {
            const mir::ExprId element_default = block.exprs.Add(
                BuildDefaultValueExpr(module, frame, da.element_type));
            return mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee = mir::Construct{},
                        .arguments = {element_default}},
                .type = type};
          },
          // LRM Table 6-7: a queue's default is the empty queue. Same emit
          // chain as the dynamic array -- the element default seeds the
          // wrapper's shield slot while storage starts empty.
          [&](const mir::QueueType& q) -> mir::Expr {
            const mir::ExprId element_default = block.exprs.Add(
                BuildDefaultValueExpr(module, frame, q.element_type));
            std::vector<mir::ExprId> args = {element_default};
            // LRM 7.10.5: a bounded queue `int q[$:N]` carries its max index N
            // as a second construction argument so the runtime can enforce it.
            if (q.max_bound.has_value()) {
              args.push_back(block.exprs.Add(
                  mir::MakeInt32Literal(
                      module.Unit().builtins.int32,
                      static_cast<std::int64_t>(*q.max_bound))));
            }
            return mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee = mir::Construct{},
                        .arguments = std::move(args)},
                .type = type};
          },
          // LRM Table 6-7: an associative array's default is empty. The element
          // default seeds the wrapper's shield slot (the source of nonexistent-
          // entry reads, LRM 7.8.6) while storage starts empty.
          [&](const mir::AssociativeArrayType& a) -> mir::Expr {
            const mir::ExprId element_default = block.exprs.Add(
                BuildDefaultValueExpr(module, frame, a.element_type));
            return mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee = mir::Construct{},
                        .arguments = {element_default}},
                .type = type};
          },
          // Types whose runtime default is the C++ language-level default
          // (named-event handle, child module instance, `unique_ptr<Child>`,
          // `vector<Child>`). The constructor scope is the real populator
          // for the object family; named-events have no SV initializer
          // grammar at all. An empty-argument construction call renders as
          // `T()` and invokes the type's default ctor.
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
          [&](const mir::VectorType&) -> mir::Expr {
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
      },
      ty.data);
}

// LRM 7.2.2 / Table 7-1: default-construct a value from its source (HIR) type
// so member declaration initializers are honored. An unpacked struct takes each
// member's own default (its declaration initializer, else the member type's
// recursive default); a fixed unpacked array fills every element with the
// element type's source default. Every other type carries no source-level
// initializer, so its default is the canonical type default.
auto BuildDefaultValueFromHir(
    const ModuleLowerer& module, WalkFrame frame, hir::TypeId hir_type)
    -> mir::Expr {
  auto& block = *frame.current_block;
  const auto& hir_ty = module.Hir().types.Get(hir_type);
  const mir::TypeId mir_type = module.TranslateType(hir_type);

  if (const auto* st = std::get_if<hir::UnpackedStructType>(&hir_ty.data)) {
    std::vector<mir::ExprId> components;
    components.reserve(st->fields.size());
    for (const auto& field : st->fields) {
      const mir::ExprId component =
          field.default_init.has_value()
              ? block.exprs.Add(MaterializeConstant(
                    module, frame, field.type, *field.default_init))
              : block.exprs.Add(
                    BuildDefaultValueFromHir(module, frame, field.type));
      components.push_back(component);
    }
    return mir::Expr{
        .data = mir::TupleExpr{.components = std::move(components)},
        .type = mir_type};
  }

  if (const auto* ua = std::get_if<hir::UnpackedArrayType>(&hir_ty.data)) {
    const std::int64_t span = (ua->dim.left >= ua->dim.right)
                                  ? (ua->dim.left - ua->dim.right)
                                  : (ua->dim.right - ua->dim.left);
    const auto size = static_cast<std::uint64_t>(span) + 1U;
    std::vector<mir::ExprId> elements;
    elements.reserve(size);
    for (std::uint64_t i = 0; i < size; ++i) {
      elements.push_back(block.exprs.Add(
          BuildDefaultValueFromHir(module, frame, ua->element_type)));
    }
    return BuildUnpackedArrayValue(
        module, frame, mir_type, ua->element_type, std::move(elements));
  }

  return BuildDefaultValueExpr(module, frame, mir_type);
}

auto BuildArrayConstructionCall(
    const ModuleLowerer& module, WalkFrame frame, mir::TypeId array_type,
    std::vector<mir::ExprId> elements) -> mir::Expr {
  auto& block = *frame.current_block;
  const auto& ty = module.Unit().types.Get(array_type);
  const mir::TypeId element_type = std::visit(
      [](const auto& t) -> mir::TypeId {
        using TyT = std::decay_t<decltype(t)>;
        if constexpr (
            std::same_as<TyT, mir::UnpackedArrayType> ||
            std::same_as<TyT, mir::DynamicArrayType> ||
            std::same_as<TyT, mir::QueueType>) {
          return t.element_type;
        } else {
          throw InternalError(
              "BuildArrayConstructionCall: type is not UnpackedArrayType, "
              "DynamicArrayType, or QueueType");
        }
      },
      ty.data);
  const mir::ExprId element_default =
      block.exprs.Add(BuildDefaultValueExpr(module, frame, element_type));
  const mir::ExprId list_id = block.exprs.Add(
      mir::Expr{
          .data = mir::ArrayLiteralExpr{.elements = std::move(elements)},
          .type = array_type});
  std::vector<mir::ExprId> args = {element_default, list_id};
  // LRM 7.10.5: a bounded queue initialized by an assignment pattern still
  // carries its max index, so the bound rides as a third construction argument
  // and the runtime trims an over-long initializer.
  if (const auto* q = std::get_if<mir::QueueType>(&ty.data);
      q != nullptr && q->max_bound.has_value()) {
    args.push_back(block.exprs.Add(
        mir::MakeInt32Literal(
            module.Unit().builtins.int32,
            static_cast<std::int64_t>(*q->max_bound))));
  }
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Construct{}, .arguments = std::move(args)},
      .type = array_type};
}

auto BuildAssociativeConstructionCall(
    ModuleLowerer& module, WalkFrame frame, mir::TypeId assoc_type,
    std::vector<std::pair<mir::ExprId, mir::ExprId>> entries,
    std::optional<mir::ExprId> user_default) -> mir::Expr {
  auto& block = *frame.current_block;
  const auto* assoc = std::get_if<mir::AssociativeArrayType>(
      &module.Unit().types.Get(assoc_type).data);
  if (assoc == nullptr) {
    throw InternalError(
        "BuildAssociativeConstructionCall: result type is not "
        "AssociativeArrayType");
  }
  const mir::TypeId key_type = assoc->key_type;
  const mir::TypeId element_type = assoc->element_type;

  const mir::TypeId tuple_type = module.Unit().types.Intern(
      mir::TupleType{.elements = {key_type, element_type}});
  std::vector<mir::ExprId> tuple_ids;
  tuple_ids.reserve(entries.size());
  for (const auto& [key_id, value_id] : entries) {
    tuple_ids.push_back(block.exprs.Add(
        mir::Expr{
            .data = mir::TupleExpr{.components = {key_id, value_id}},
            .type = tuple_type}));
  }
  const mir::TypeId entries_type = module.Unit().types.Intern(
      mir::UnpackedArrayType{
          .element_type = tuple_type,
          .size = static_cast<std::uint64_t>(tuple_ids.size())});
  const mir::ExprId entries_id = block.exprs.Add(
      mir::Expr{
          .data = mir::ArrayLiteralExpr{.elements = std::move(tuple_ids)},
          .type = entries_type});

  const mir::ExprId element_default =
      block.exprs.Add(BuildDefaultValueExpr(module, frame, element_type));
  std::vector<mir::ExprId> args;
  args.reserve(user_default.has_value() ? 3U : 2U);
  args.push_back(element_default);
  args.push_back(entries_id);
  if (user_default.has_value()) {
    args.push_back(*user_default);
  }
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Construct{}, .arguments = std::move(args)},
      .type = assoc_type};
}

}  // namespace lyra::lowering::hir_to_mir

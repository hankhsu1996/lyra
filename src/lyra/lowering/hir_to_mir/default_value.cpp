#include "lyra/lowering/hir_to_mir/default_value.hpp"

#include <algorithm>
#include <cstdint>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

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

}  // namespace

auto SynthesizeDefaultValueExpr(
    const UnitLoweringState& unit_state,
    ProceduralScopeLoweringState& scope_state, mir::TypeId type_id)
    -> mir::ExprId {
  const auto& ty = unit_state.GetType(type_id);
  return std::visit(
      Overloaded{
          [&](const mir::PackedArrayType& pa) -> mir::ExprId {
            return scope_state.AddExpr(
                mir::Expr{
                    .data =
                        mir::IntegerLiteral{
                            .value = DefaultIntegralConstant(pa)},
                    .type = type_id});
          },
          [&](const mir::EnumType& e) -> mir::ExprId {
            return scope_state.AddExpr(
                mir::Expr{
                    .data =
                        mir::IntegerLiteral{
                            .value = DefaultIntegralConstant(e.base)},
                    .type = type_id});
          },
          [&](const mir::StringType&) -> mir::ExprId {
            return scope_state.AddExpr(
                mir::Expr{
                    .data = mir::StringLiteral{.value = std::string{}},
                    .type = type_id});
          },
          [&](const mir::RealType&) -> mir::ExprId {
            return scope_state.AddExpr(
                mir::Expr{
                    .data = mir::RealLiteral{.value = 0.0}, .type = type_id});
          },
          [&](const mir::ShortRealType&) -> mir::ExprId {
            return scope_state.AddExpr(
                mir::Expr{
                    .data = mir::RealLiteral{.value = 0.0}, .type = type_id});
          },
          [&](const mir::RealTimeType&) -> mir::ExprId {
            return scope_state.AddExpr(
                mir::Expr{
                    .data = mir::RealLiteral{.value = 0.0}, .type = type_id});
          },
          [&](const mir::UnpackedArrayType& ua) -> mir::ExprId {
            std::vector<mir::ExprId> elements;
            elements.reserve(ua.size);
            for (std::uint64_t i = 0; i < ua.size; ++i) {
              elements.push_back(SynthesizeDefaultValueExpr(
                  unit_state, scope_state, ua.element_type));
            }
            return scope_state.AddExpr(BuildArrayConstructExpr(
                unit_state, scope_state, type_id, std::move(elements)));
          },
          // LRM Table 6-7: a dynamic array's default is the empty array.
          // The wrapper still needs the element type's default supplied at
          // construction so OOB reads and resize-fills have a shape source --
          // see `docs/decisions/runtime-shape-and-default-value.md`. Emit
          // chain: ConstructExpr{element_default} -> `DynamicArray<T>(...)`
          // ctor that stores the value and leaves `data_` empty.
          [&](const mir::DynamicArrayType& da) -> mir::ExprId {
            const mir::ExprId element_default = SynthesizeDefaultValueExpr(
                unit_state, scope_state, da.element_type);
            return scope_state.AddExpr(
                mir::Expr{
                    .data = mir::ConstructExpr{.args = {element_default}},
                    .type = type_id});
          },
          // Types whose runtime default is the C++ language-level default
          // (named-event handle, child module instance, `unique_ptr<Child>`,
          // `vector<Child>`). The constructor scope is the real populator
          // for the object family; named-events have no SV initializer
          // grammar at all. Empty `ConstructExpr` renders as `T()` and
          // invokes the type's default ctor.
          [&](const mir::EventType&) -> mir::ExprId {
            return scope_state.AddExpr(
                mir::Expr{
                    .data = mir::ConstructExpr{.args = {}}, .type = type_id});
          },
          [&](const mir::ObjectType&) -> mir::ExprId {
            return scope_state.AddExpr(
                mir::Expr{
                    .data = mir::ConstructExpr{.args = {}}, .type = type_id});
          },
          [&](const mir::OwningPtrType&) -> mir::ExprId {
            return scope_state.AddExpr(
                mir::Expr{
                    .data = mir::ConstructExpr{.args = {}}, .type = type_id});
          },
          [&](const mir::VectorType&) -> mir::ExprId {
            return scope_state.AddExpr(
                mir::Expr{
                    .data = mir::ConstructExpr{.args = {}}, .type = type_id});
          },
          [&](const auto&) -> mir::ExprId {
            throw InternalError(
                "SynthesizeDefaultValueExpr: type kind has no default-value "
                "representation");
          },
      },
      ty.data);
}

auto BuildArrayConstructExpr(
    const UnitLoweringState& unit_state,
    ProceduralScopeLoweringState& scope_state, mir::TypeId array_type_id,
    std::vector<mir::ExprId> elements) -> mir::Expr {
  const auto& ty = unit_state.GetType(array_type_id);
  const mir::TypeId element_type_id = std::visit(
      [](const auto& t) -> mir::TypeId {
        using TyT = std::decay_t<decltype(t)>;
        if constexpr (
            std::same_as<TyT, mir::UnpackedArrayType> ||
            std::same_as<TyT, mir::DynamicArrayType>) {
          return t.element_type;
        } else {
          throw InternalError(
              "BuildArrayConstructExpr: type_id is not UnpackedArrayType or "
              "DynamicArrayType");
        }
      },
      ty.data);
  const mir::ExprId element_default =
      SynthesizeDefaultValueExpr(unit_state, scope_state, element_type_id);
  const mir::ExprId list_id = scope_state.AddExpr(
      mir::Expr{
          .data = mir::ArrayLiteralExpr{.elements = std::move(elements)},
          .type = array_type_id});
  return mir::Expr{
      .data = mir::ConstructExpr{.args = {element_default, list_id}},
      .type = array_type_id};
}

}  // namespace lyra::lowering::hir_to_mir

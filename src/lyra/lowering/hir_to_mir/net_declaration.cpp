#include "lyra/lowering/hir_to_mir/net_declaration.hpp"

#include <cstdint>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/strength_level.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The scalar a net shows at a position nothing drives (LRM 6.7.1), which the
// install carries as a one-bit value the runtime fills the declared type with.
enum class UndrivenScalar : std::uint8_t {
  kZero,
  kOne,
  kUnknown,
  kHighImpedance,
};

// What the net type states, before either half is written as an operand.
struct NetTypeResolution {
  support::BuiltinFn entry;
  UndrivenScalar undriven;
  support::StrengthLevel strength;
};

auto ResolutionOf(const hir::StructuralNetDecl& net) -> NetTypeResolution {
  using support::BuiltinFn;
  using support::StrengthLevel;
  switch (net.net_type) {
    // LRM 6.6: a net shows high impedance where nothing drives it, which is a
    // contribution that determines no position and therefore takes no part in
    // any resolution. A `uwire` is one driver's worth of the same resolution;
    // what makes it single-driver is decided before this layer (LRM 6.6.2).
    case hir::NetType::kWire:
    case hir::NetType::kTri:
    case hir::NetType::kUwire:
      return {
          .entry = BuiltinFn::kNetInitializeTriState,
          .undriven = UndrivenScalar::kHighImpedance,
          .strength = StrengthLevel::kHighImpedance};
    case hir::NetType::kWand:
    case hir::NetType::kTriand:
      return {
          .entry = BuiltinFn::kNetInitializeWiredAnd,
          .undriven = UndrivenScalar::kHighImpedance,
          .strength = StrengthLevel::kHighImpedance};
    case hir::NetType::kWor:
    case hir::NetType::kTrior:
      return {
          .entry = BuiltinFn::kNetInitializeWiredOr,
          .undriven = UndrivenScalar::kHighImpedance,
          .strength = StrengthLevel::kHighImpedance};
    // LRM 6.6.5: a resistive pulldown or pullup, which every ordinary driver
    // outranks, so it shows only where nothing else drives.
    case hir::NetType::kTri0:
      return {
          .entry = BuiltinFn::kNetInitializeTriState,
          .undriven = UndrivenScalar::kZero,
          .strength = StrengthLevel::kPull};
    case hir::NetType::kTri1:
      return {
          .entry = BuiltinFn::kNetInitializeTriState,
          .undriven = UndrivenScalar::kOne,
          .strength = StrengthLevel::kPull};
    // LRM 6.6.6: a power supply, which outranks every ordinary driver instead.
    case hir::NetType::kSupply0:
      return {
          .entry = BuiltinFn::kNetInitializeTriState,
          .undriven = UndrivenScalar::kZero,
          .strength = StrengthLevel::kSupply};
    case hir::NetType::kSupply1:
      return {
          .entry = BuiltinFn::kNetInitializeTriState,
          .undriven = UndrivenScalar::kOne,
          .strength = StrengthLevel::kSupply};
    // LRM 6.6.4, 6.7.1: a stored value, unknown until something drives it, and
    // held at the charge the declaration names once something has -- medium
    // where the declaration names none (LRM 28.15.2).
    case hir::NetType::kTrireg:
      return {
          .entry = BuiltinFn::kNetInitializeRetaining,
          .undriven = UndrivenScalar::kUnknown,
          .strength =
              net.charge_strength.value_or(support::StrengthLevel::kMedium)};
  }
  throw InternalError("ResolutionOf: unknown net type");
}

// The two planes spell the four scalars: 0 and 1 carry no unknown bit, x
// carries both, and z carries the unknown bit alone.
auto BuildUndrivenScalar(
    const mir::CompilationUnit& unit, mir::Block& block, UndrivenScalar scalar)
    -> mir::ExprId {
  const auto constant = [scalar]() -> mir::IntegralConstant {
    switch (scalar) {
      case UndrivenScalar::kZero:
        return {.value_words = {0}, .state_words = {0}};
      case UndrivenScalar::kOne:
        return {.value_words = {1}, .state_words = {0}};
      case UndrivenScalar::kUnknown:
        return {.value_words = {1}, .state_words = {1}};
      case UndrivenScalar::kHighImpedance:
        return {.value_words = {0}, .state_words = {1}};
    }
    throw InternalError("BuildUndrivenScalar: unknown scalar");
  }();
  return BuildIntegralLiteral(
      unit, block,
      mir::PackedVectorOf(unit.types, 1, mir::IntegralStateKind::kFourState),
      constant);
}

}  // namespace

auto BuildNetInstall(
    const mir::CompilationUnit& unit, mir::Block& block,
    const hir::StructuralNetDecl& net) -> NetInstall {
  const NetTypeResolution resolution = ResolutionOf(net);
  return NetInstall{
      .entry = resolution.entry,
      .fill = BuildUndrivenScalar(unit, block, resolution.undriven),
      .strength = BuildStrengthOperand(unit, block, resolution.strength)};
}

auto BuildStrengthOperand(
    const mir::CompilationUnit& unit, mir::Block& block,
    support::StrengthLevel level) -> mir::ExprId {
  return BuildIntLiteral(unit, block, static_cast<std::int64_t>(level));
}

}  // namespace lyra::lowering::hir_to_mir

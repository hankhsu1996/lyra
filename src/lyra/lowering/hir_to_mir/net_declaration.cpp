#include "lyra/lowering/hir_to_mir/net_declaration.hpp"

#include <cstdint>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/strength_level.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// How contributions of equal strength resolve, and whether the net type's own
// contribution takes what the drivers last decided (LRM 6.6.1, 6.6.3, 6.6.4).
enum class NetFold : std::uint8_t {
  kTriState,
  kWiredAnd,
  kWiredOr,
  kRetaining,
};

// What the net type states, before any of it is written as an operand.
struct NetTypeResolution {
  NetFold fold;
  value::FourStateBit undriven;
  support::StrengthLevel strength;
};

auto ResolutionOf(const hir::StructuralNetDecl& net) -> NetTypeResolution {
  using support::StrengthLevel;
  using value::FourStateBit;
  switch (net.net_type) {
    // LRM 6.6: a net shows high impedance where nothing drives it, which is a
    // contribution that determines no position and therefore takes no part in
    // any resolution. A `uwire` is one driver's worth of the same resolution;
    // what makes it single-driver is decided before this layer (LRM 6.6.2).
    case hir::NetType::kWire:
    case hir::NetType::kTri:
    case hir::NetType::kUwire:
      return {
          .fold = NetFold::kTriState,
          .undriven = FourStateBit::kHighImpedance,
          .strength = StrengthLevel::kHighImpedance};
    case hir::NetType::kWand:
    case hir::NetType::kTriand:
      return {
          .fold = NetFold::kWiredAnd,
          .undriven = FourStateBit::kHighImpedance,
          .strength = StrengthLevel::kHighImpedance};
    case hir::NetType::kWor:
    case hir::NetType::kTrior:
      return {
          .fold = NetFold::kWiredOr,
          .undriven = FourStateBit::kHighImpedance,
          .strength = StrengthLevel::kHighImpedance};
    // LRM 6.6.5: a resistive pulldown or pullup, which every ordinary driver
    // outranks, so it shows only where nothing else drives.
    case hir::NetType::kTri0:
      return {
          .fold = NetFold::kTriState,
          .undriven = FourStateBit::kZero,
          .strength = StrengthLevel::kPull};
    case hir::NetType::kTri1:
      return {
          .fold = NetFold::kTriState,
          .undriven = FourStateBit::kOne,
          .strength = StrengthLevel::kPull};
    // LRM 6.6.6: a power supply, which outranks every ordinary driver instead.
    case hir::NetType::kSupply0:
      return {
          .fold = NetFold::kTriState,
          .undriven = FourStateBit::kZero,
          .strength = StrengthLevel::kSupply};
    case hir::NetType::kSupply1:
      return {
          .fold = NetFold::kTriState,
          .undriven = FourStateBit::kOne,
          .strength = StrengthLevel::kSupply};
    // LRM 6.6.4, 6.7.1: a stored value, unknown until something drives it, and
    // held at the charge the declaration names once something has -- medium
    // where the declaration names none (LRM 28.15.2).
    case hir::NetType::kTrireg:
      return {
          .fold = NetFold::kRetaining,
          .undriven = FourStateBit::kUnknown,
          .strength =
              net.charge_strength.value_or(support::StrengthLevel::kMedium)};
  }
  throw InternalError("ResolutionOf: unknown net type");
}

auto IntegralNetEntry(NetFold fold) -> support::BuiltinFn {
  using support::BuiltinFn;
  switch (fold) {
    case NetFold::kTriState:
      return BuiltinFn::kNetInitializeTriState;
    case NetFold::kWiredAnd:
      return BuiltinFn::kNetInitializeWiredAnd;
    case NetFold::kWiredOr:
      return BuiltinFn::kNetInitializeWiredOr;
    case NetFold::kRetaining:
      return BuiltinFn::kNetInitializeRetaining;
  }
  throw InternalError("IntegralNetEntry: unknown net fold");
}

auto AggregateNetEntry(NetFold fold) -> support::BuiltinFn {
  using support::BuiltinFn;
  switch (fold) {
    case NetFold::kTriState:
      return BuiltinFn::kAggregateNetInitializeTriState;
    case NetFold::kWiredAnd:
      return BuiltinFn::kAggregateNetInitializeWiredAnd;
    case NetFold::kWiredOr:
      return BuiltinFn::kAggregateNetInitializeWiredOr;
    case NetFold::kRetaining:
      return BuiltinFn::kAggregateNetInitializeRetaining;
  }
  throw InternalError("AggregateNetEntry: unknown net fold");
}

}  // namespace

auto BuildNetInstall(
    const mir::CompilationUnit& unit, mir::Block& block,
    const hir::StructuralNetDecl& net, mir::ExprId target, const NetData& data,
    mir::TypeId void_type) -> mir::Expr {
  const NetTypeResolution resolution = ResolutionOf(net);
  // The scalar crosses as the number it is, the way a strength does.
  const mir::ExprId fill = BuildMachineIntLiteral(
      unit, block, std::int64_t{std::to_underlying(resolution.undriven)});
  const mir::ExprId strength =
      BuildStrengthOperand(unit, block, resolution.strength);
  return std::visit(
      Overloaded{
          [&](const IntegralNetData& integral) {
            return mir::MakeNetInstallCallExpr(
                target, integral.position_count, fill, strength,
                IntegralNetEntry(resolution.fold), void_type);
          },
          [&](const AggregateNetData& aggregate) {
            return mir::MakeNetInstallCallExpr(
                target, aggregate.prototype, fill, strength,
                AggregateNetEntry(resolution.fold), void_type);
          }},
      data);
}

auto BuildNetPositionCount(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId data_type)
    -> mir::ExprId {
  return BuildMachineIntLiteral(
      unit, block,
      static_cast<std::int64_t>(
          unit.types.Get(data_type).Integral().bit_width));
}

auto BuildStrengthOperand(
    const mir::CompilationUnit& unit, mir::Block& block,
    support::StrengthLevel level) -> mir::ExprId {
  return BuildMachineIntLiteral(unit, block, static_cast<std::int64_t>(level));
}

}  // namespace lyra::lowering::hir_to_mir

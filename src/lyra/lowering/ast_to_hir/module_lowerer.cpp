#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"

#include <limits>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/timescale_format.hpp"
#include "lyra/lowering/ast_to_hir/timescale.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto ComputeFrame(Context& ctx, const slang::ast::Scope& scope)
    -> LoweringFrame {
  LoweringFrame frame{};

  // Compute scope's timeunit
  auto ts = scope.getTimeScale();
  if (ts) {
    frame.unit_power = TimeScaleValueToPower(ts->base);
  } else {
    frame.unit_power = kDefaultTimeScalePower;
  }

  // Get global precision (computed once per compilation, cached in Context)
  if (!ctx.cached_global_precision) {
    ctx.cached_global_precision =
        ComputeGlobalPrecision(scope.getCompilation());
  }
  frame.global_precision_power = *ctx.cached_global_precision;

  return frame;
}

}  // namespace

ScopeLowerer::ScopeLowerer(
    Context& ctx, SymbolRegistrar& registrar,
    const slang::ast::InstanceSymbol& instance)
    : ScopeLowerer(ctx, registrar, ComputeFrame(ctx, instance.body)) {
}

ScopeLowerer::ScopeLowerer(
    Context& ctx, SymbolRegistrar& registrar,
    const slang::ast::PackageSymbol& package)
    : ScopeLowerer(ctx, registrar, ComputeFrame(ctx, package)) {
}

ScopeLowerer::ScopeLowerer(
    Context& ctx, SymbolRegistrar& registrar, LoweringFrame frame)
    : ctx_(ctx), registrar_(registrar), frame_(frame) {
}

auto ScopeLowerer::ScaleDelayTicks(uint64_t literal_ticks) const -> uint64_t {
  int exponent = frame_.unit_power - frame_.global_precision_power;

  if (exponent < 0) {
    throw common::InternalError(
        "delay scaling",
        "negative exponent - global precision coarser than timeunit");
  }

  if (exponent == 0) {
    return literal_ticks;
  }

  auto multiplier = IntegerPow10(exponent);
  if (!multiplier) {
    throw common::InternalError("delay scaling", "multiplier overflow");
  }

  if (literal_ticks > std::numeric_limits<uint64_t>::max() / *multiplier) {
    throw common::InternalError("delay scaling", "ticks overflow");
  }

  return literal_ticks * *multiplier;
}

}  // namespace lyra::lowering::ast_to_hir

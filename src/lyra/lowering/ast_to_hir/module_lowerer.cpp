#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"

#include <cstdint>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/timescale_format.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/delay_scaler.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
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
  DelayScaler scaler(frame_.unit_power, frame_.global_precision_power);
  auto result = scaler.ScaleInteger(literal_ticks);
  if (!result) {
    throw common::InternalError("delay scaling", result.error());
  }
  return *result;
}

auto ScopeLowerer::ScaleDelayReal(double value) const -> uint64_t {
  DelayScaler scaler(frame_.unit_power, frame_.global_precision_power);
  auto result = scaler.ScaleReal(value);
  if (!result) {
    throw common::InternalError("delay scaling", result.error());
  }
  return *result;
}

}  // namespace lyra::lowering::ast_to_hir

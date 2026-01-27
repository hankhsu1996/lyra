#include "lyra/lowering/hir_to_mir/statement.hpp"

#include <cstdint>
#include <expected>
#include <functional>
#include <optional>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/constant.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/edge_kind.hpp"
#include "lyra/common/format.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/hir/system_call.hpp"
#include "lyra/lowering/hir_to_mir/builder.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/expression.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

class LoopGuard {
 public:
  LoopGuard(MirBuilder& builder, LoopContext ctx) : builder_(builder) {
    builder_.PushLoop(ctx);
  }
  ~LoopGuard() {
    builder_.PopLoop();
  }
  LoopGuard(const LoopGuard&) = delete;
  LoopGuard(LoopGuard&&) = delete;
  auto operator=(const LoopGuard&) -> LoopGuard& = delete;
  auto operator=(LoopGuard&&) -> LoopGuard& = delete;

 private:
  MirBuilder& builder_;
};

auto LowerBlock(const hir::BlockStatementData& data, MirBuilder& builder)
    -> Result<void> {
  for (hir::StatementId stmt_id : data.statements) {
    Result<void> result = LowerStatement(stmt_id, builder);
    if (!result) return std::unexpected(result.error());
  }
  return {};
}

auto LowerVariableDeclaration(
    const hir::VariableDeclarationStatementData& data, MirBuilder& builder)
    -> Result<void> {
  Context& ctx = builder.GetContext();
  const Symbol& sym = (*ctx.symbol_table)[data.symbol];
  auto alloc = ctx.AllocLocal(data.symbol, sym.type);

  if (data.init != hir::kInvalidExpressionId) {
    Result<mir::Operand> value_result = LowerExpression(data.init, builder);
    if (!value_result) return std::unexpected(value_result.error());
    builder.EmitAssign(alloc.place, std::move(*value_result));
  }
  return {};
}

auto LowerAssignment(
    const hir::AssignmentStatementData& data, MirBuilder& builder)
    -> Result<void> {
  Result<LvalueResult> target_result = LowerLvalue(data.target, builder);
  if (!target_result) return std::unexpected(target_result.error());
  LvalueResult target = *target_result;

  Result<mir::Operand> value_result = LowerExpression(data.value, builder);
  if (!value_result) return std::unexpected(value_result.error());
  mir::Operand value = *value_result;

  if (target.IsAlwaysValid()) {
    builder.EmitAssign(target.place, std::move(value));
    return {};
  }

  // Guarded store: only write if validity is true (OOB/X/Z = no-op)
  Context& ctx = builder.GetContext();
  mir::Operand cond = target.validity;

  // EmitBranch requires Use operand - materialize constant if needed
  if (cond.kind == mir::Operand::Kind::kConst) {
    mir::PlaceId temp = ctx.AllocTemp(ctx.GetBitType());
    builder.EmitAssign(temp, std::move(cond));
    cond = mir::Operand::Use(temp);
  }

  BlockIndex store_bb = builder.CreateBlock();
  BlockIndex merge_bb = builder.CreateBlock();
  builder.EmitBranch(cond, store_bb, merge_bb);

  builder.SetCurrentBlock(store_bb);
  builder.EmitAssign(target.place, std::move(value));
  builder.EmitJump(merge_bb);

  builder.SetCurrentBlock(merge_bb);
  return {};
}

// Lower format ops for display/strobe (shared logic).
// Returns the lowered MIR format ops.
auto LowerFormatOps(
    const std::vector<hir::FormatOp>& hir_ops,
    std::optional<hir::ExpressionId> descriptor, MirBuilder& builder)
    -> Result<
        std::pair<std::vector<mir::FormatOp>, std::optional<mir::Operand>>> {
  Context& ctx = builder.GetContext();

  // Lower descriptor first (single eval, before format args)
  std::optional<mir::Operand> mir_descriptor;
  if (descriptor) {
    Result<mir::Operand> desc_result = LowerExpression(*descriptor, builder);
    if (!desc_result) return std::unexpected(desc_result.error());
    mir::Operand desc_val = *desc_result;
    // Materialize to a temp so it's evaluated once
    const hir::Expression& desc_expr = (*ctx.hir_arena)[*descriptor];
    mir::PlaceId temp = ctx.AllocTemp(desc_expr.type);
    builder.EmitAssign(temp, std::move(desc_val));
    mir_descriptor = mir::Operand::Use(temp);
  }

  std::vector<mir::FormatOp> mir_ops;
  mir_ops.reserve(hir_ops.size());

  for (const auto& hir_op : hir_ops) {
    if (hir_op.kind == FormatKind::kLiteral) {
      mir_ops.push_back(
          mir::FormatOp{
              .kind = FormatKind::kLiteral,
              .value = std::nullopt,
              .literal = hir_op.literal,
              .type = TypeId{},
              .mods = {},
              .module_timeunit_power = hir_op.module_timeunit_power});
    } else {
      // Lower the value expression and get its type
      Result<mir::Operand> operand_result =
          LowerExpression(*hir_op.value, builder);
      if (!operand_result) return std::unexpected(operand_result.error());
      mir::Operand operand = *operand_result;
      const hir::Expression& expr = (*ctx.hir_arena)[*hir_op.value];
      mir_ops.push_back(
          mir::FormatOp{
              .kind = hir_op.kind,
              .value = std::move(operand),
              .literal = {},
              .type = expr.type,
              .mods = hir_op.mods,
              .module_timeunit_power = hir_op.module_timeunit_power});
    }
  }

  return std::make_pair(std::move(mir_ops), std::move(mir_descriptor));
}

// Generate a synthetic thunk function for $strobe.
// The thunk re-evaluates expressions and prints at Postponed time.
auto LowerStrobeEffect(
    const hir::DisplaySystemCallData& data, MirBuilder& builder)
    -> Result<void> {
  Context& original_ctx = builder.GetContext();

  // Create a new context for the thunk function (shares design state mappings).
  Context thunk_ctx{
      .mir_arena = original_ctx.mir_arena,
      .hir_arena = original_ctx.hir_arena,
      .type_arena = original_ctx.type_arena,
      .constant_arena = original_ctx.constant_arena,
      .symbol_table = original_ctx.symbol_table,
      .module_places = original_ctx.module_places,
      .local_places = {},  // Fresh local mapping (thunk has no SV locals)
      .next_local_id = 0,
      .next_temp_id = 0,
      .local_types = {},
      .temp_types = {},
      .builtin_types = original_ctx.builtin_types,
      .symbol_to_mir_function = original_ctx.symbol_to_mir_function,
  };

  // Create builder for the thunk function
  MirBuilder thunk_builder(original_ctx.mir_arena, &thunk_ctx);

  // Create entry block for thunk
  BlockIndex entry = thunk_builder.CreateBlock();
  thunk_builder.SetCurrentBlock(entry);

  // Lower format ops into thunk (re-evaluates expressions at Postponed time)
  auto ops_result = LowerFormatOps(data.ops, data.descriptor, thunk_builder);
  if (!ops_result) return std::unexpected(ops_result.error());
  auto [mir_ops, mir_descriptor] = std::move(*ops_result);

  // Emit DisplayEffect in thunk
  mir::DisplayEffect display{
      .print_kind = data.print_kind,
      .ops = std::move(mir_ops),
      .descriptor = std::move(mir_descriptor),
  };
  thunk_builder.EmitEffect(std::move(display));

  // Emit void return (thunk is always void)
  thunk_builder.EmitTerminate(std::nullopt);

  // Build thunk function
  auto blocks = thunk_builder.Finish();
  mir::Function thunk{
      .signature =
          {
              .return_type = original_ctx.builtin_types.void_type,
              .params = {},  // Called via runtime ABI, not MIR call
          },
      .thunk_kind = mir::ThunkKind::kStrobe,
      .entry = mir::BasicBlockId{0},
      .blocks = std::move(blocks),
      .local_types = std::move(thunk_ctx.local_types),
      .temp_types = std::move(thunk_ctx.temp_types),
      .param_local_slots = {},
      .param_origins = {},
  };
  mir::FunctionId thunk_id =
      original_ctx.mir_arena->AddFunction(std::move(thunk));

  // Track the thunk so it gets declared in LLVM lowering
  if (original_ctx.generated_functions != nullptr) {
    original_ctx.generated_functions->push_back(thunk_id);
  }

  // Emit StrobeEffect in original builder
  builder.EmitEffect(mir::StrobeEffect{.thunk = thunk_id});
  return {};
}

auto LowerDisplayEffect(
    const hir::DisplaySystemCallData& data, MirBuilder& builder)
    -> Result<void> {
  // $strobe: create a thunk function and schedule for Postponed region
  if (data.is_strobe) {
    return LowerStrobeEffect(data, builder);
  }

  // $display/$write: immediate output
  auto ops_result = LowerFormatOps(data.ops, data.descriptor, builder);
  if (!ops_result) return std::unexpected(ops_result.error());
  auto [mir_ops, mir_descriptor] = std::move(*ops_result);

  mir::DisplayEffect display{
      .print_kind = data.print_kind,
      .ops = std::move(mir_ops),
      .descriptor = std::move(mir_descriptor),
  };
  builder.EmitEffect(std::move(display));
  return {};
}

auto LowerSeverityEffect(
    const hir::SeveritySystemCallData& data, MirBuilder& builder)
    -> Result<void> {
  Context& ctx = builder.GetContext();

  std::vector<mir::FormatOp> mir_ops;
  mir_ops.reserve(data.ops.size());

  for (const auto& hir_op : data.ops) {
    if (hir_op.kind == FormatKind::kLiteral) {
      mir_ops.push_back(
          mir::FormatOp{
              .kind = FormatKind::kLiteral,
              .value = std::nullopt,
              .literal = hir_op.literal,
              .type = TypeId{},
              .mods = {},
              .module_timeunit_power = hir_op.module_timeunit_power});
    } else {
      // Lower the value expression and get its type
      Result<mir::Operand> operand_result =
          LowerExpression(*hir_op.value, builder);
      if (!operand_result) return std::unexpected(operand_result.error());
      mir::Operand operand = *operand_result;
      const hir::Expression& expr = (*ctx.hir_arena)[*hir_op.value];
      mir_ops.push_back(
          mir::FormatOp{
              .kind = hir_op.kind,
              .value = std::move(operand),
              .literal = {},
              .type = expr.type,
              .mods = hir_op.mods,
              .module_timeunit_power = hir_op.module_timeunit_power});
    }
  }

  mir::SeverityEffect severity{
      .level = data.level,
      .ops = std::move(mir_ops),
  };
  builder.EmitEffect(std::move(severity));
  return {};
}

auto BuildSFormatRvalue(
    const hir::SFormatSystemCallData& data, MirBuilder& builder)
    -> Result<mir::Rvalue> {
  Context& ctx = builder.GetContext();
  mir::SFormatRvalueInfo info;
  info.default_format = data.default_format;

  if (!data.ops.empty()) {
    // Compile-time path: convert HIR FormatOps to MIR FormatOps
    info.ops.reserve(data.ops.size());
    for (const auto& hir_op : data.ops) {
      if (hir_op.kind == FormatKind::kLiteral) {
        info.ops.push_back(
            mir::FormatOp{
                .kind = FormatKind::kLiteral,
                .value = std::nullopt,
                .literal = hir_op.literal,
                .type = TypeId{},
                .mods = {},
                .module_timeunit_power = hir_op.module_timeunit_power});
      } else {
        Result<mir::Operand> operand_result =
            LowerExpression(*hir_op.value, builder);
        if (!operand_result) return std::unexpected(operand_result.error());
        mir::Operand operand = *operand_result;
        const hir::Expression& expr = (*ctx.hir_arena)[*hir_op.value];
        info.ops.push_back(
            mir::FormatOp{
                .kind = hir_op.kind,
                .value = std::move(operand),
                .literal = {},
                .type = expr.type,
                .mods = hir_op.mods,
                .module_timeunit_power = hir_op.module_timeunit_power});
      }
    }
    return mir::Rvalue{.operands = {}, .info = std::move(info)};
  }

  // Runtime path: lower args to operands.
  // In AST->HIR lowering, runtime format path puts format+value args in
  // data.args (args[0] is format string). For $swrite* (no format string),
  // all args go into data.ops (not data.args). So if data.args is non-empty,
  // it's always the runtime format path.
  info.has_runtime_format = !data.args.empty();

  std::vector<mir::Operand> operands;
  operands.reserve(data.args.size());
  for (hir::ExpressionId arg_id : data.args) {
    Result<mir::Operand> arg_result = LowerExpression(arg_id, builder);
    if (!arg_result) return std::unexpected(arg_result.error());
    operands.push_back(*arg_result);
  }
  return mir::Rvalue{.operands = std::move(operands), .info = std::move(info)};
}

auto LowerSFormatEffect(
    const hir::SFormatSystemCallData& data, MirBuilder& builder)
    -> Result<void> {
  Context& ctx = builder.GetContext();

  // Build the SFormat rvalue
  Result<mir::Rvalue> rvalue_result = BuildSFormatRvalue(data, builder);
  if (!rvalue_result) return std::unexpected(rvalue_result.error());
  mir::Rvalue rvalue = std::move(*rvalue_result);

  // Get string type from the output expression (output is always a string)
  const hir::Expression& out_expr = (*ctx.hir_arena)[*data.output];
  mir::PlaceId tmp = builder.EmitTemp(out_expr.type, std::move(rvalue));

  // Assign to output
  Result<LvalueResult> target_result = LowerLvalue(*data.output, builder);
  if (!target_result) return std::unexpected(target_result.error());
  LvalueResult target = *target_result;
  builder.EmitAssign(target.place, mir::Operand::Use(tmp));
  return {};
}

// Compute the snapshot buffer layout for monitor prev_values.
// Returns total buffer size and per-operand offsets.
struct SnapshotLayout {
  uint32_t total_size = 0;
  std::vector<uint32_t> offsets;
  std::vector<uint32_t> byte_sizes;
};

auto ComputeSnapshotLayout(
    const std::vector<mir::FormatOp>& ops, const TypeArena& types)
    -> SnapshotLayout {
  SnapshotLayout layout;
  layout.offsets.reserve(ops.size());
  layout.byte_sizes.reserve(ops.size());

  uint32_t offset = 0;
  for (const auto& op : ops) {
    if (op.kind == FormatKind::kLiteral) {
      // Literals don't contribute to snapshot
      layout.offsets.push_back(0);
      layout.byte_sizes.push_back(0);
      continue;
    }

    const Type& ty = types[op.type];
    uint32_t byte_size = 0;
    uint32_t align = 1;

    if (ty.Kind() == TypeKind::kReal) {
      // double: 8 bytes, 8-byte aligned
      byte_size = 8;
      align = 8;
    } else if (ty.Kind() == TypeKind::kShortReal) {
      // float: 4 bytes, 4-byte aligned
      byte_size = 4;
      align = 4;
    } else if (IsPacked(ty)) {
      // Integral/PackedArray/PackedStruct/Enum: ceil(bits / 8), 8-byte aligned
      auto bit_width = PackedBitWidth(ty, types);
      byte_size = (bit_width + 7) / 8;
      align = 8;
    } else {
      // Unsupported type: should be rejected by eligibility gate in AST→HIR.
      throw common::InternalError(
          "ComputeSnapshotLayout", "unsupported type for $monitor");
    }

    // Align offset
    offset = (offset + align - 1) & ~(align - 1);
    layout.offsets.push_back(offset);
    layout.byte_sizes.push_back(byte_size);
    offset += byte_size;
  }

  // Total size aligned to 8 bytes
  layout.total_size = (offset + 7) & ~7;
  return layout;
}

// Generate a synthetic check thunk function for $monitor.
// The check thunk re-evaluates expressions and compares with prev_values.
// If any changed, it prints and updates prev_values.
auto LowerMonitorCheckThunk(
    const hir::MonitorSystemCallData& data, MirBuilder& original_builder,
    [[maybe_unused]] const SnapshotLayout& layout) -> Result<mir::FunctionId> {
  Context& original_ctx = original_builder.GetContext();

  // Create a new context for the thunk function
  Context thunk_ctx{
      .mir_arena = original_ctx.mir_arena,
      .hir_arena = original_ctx.hir_arena,
      .type_arena = original_ctx.type_arena,
      .constant_arena = original_ctx.constant_arena,
      .symbol_table = original_ctx.symbol_table,
      .module_places = original_ctx.module_places,
      .local_places = {},
      .next_local_id = 0,
      .next_temp_id = 0,
      .local_types = {},
      .temp_types = {},
      .builtin_types = original_ctx.builtin_types,
      .symbol_to_mir_function = original_ctx.symbol_to_mir_function,
  };

  MirBuilder thunk_builder(original_ctx.mir_arena, &thunk_ctx);

  BlockIndex entry = thunk_builder.CreateBlock();
  thunk_builder.SetCurrentBlock(entry);

  // Lower format ops into thunk (re-evaluates at Postponed time)
  auto ops_result = LowerFormatOps(data.ops, data.descriptor, thunk_builder);
  if (!ops_result) return std::unexpected(ops_result.error());
  auto [mir_ops, mir_descriptor] = std::move(*ops_result);

  // Emit DisplayEffect for printing
  mir::DisplayEffect display{
      .print_kind = data.print_kind,
      .ops = std::move(mir_ops),
      .descriptor = std::move(mir_descriptor),
  };
  thunk_builder.EmitEffect(std::move(display));

  // Emit void return
  thunk_builder.EmitTerminate(std::nullopt);

  // Build thunk function
  // Signature: void check_thunk(DesignState*, Engine*, prev_buffer*)
  auto blocks = thunk_builder.Finish();
  mir::Function thunk{
      .signature =
          {
              .return_type = original_ctx.builtin_types.void_type,
              .params = {},
          },
      .thunk_kind = mir::ThunkKind::kMonitorCheck,
      .entry = mir::BasicBlockId{0},
      .blocks = std::move(blocks),
      .local_types = std::move(thunk_ctx.local_types),
      .temp_types = std::move(thunk_ctx.temp_types),
      .param_local_slots = {},
      .param_origins = {},
  };
  mir::FunctionId thunk_id =
      original_ctx.mir_arena->AddFunction(std::move(thunk));

  if (original_ctx.generated_functions != nullptr) {
    original_ctx.generated_functions->push_back(thunk_id);
  }

  return thunk_id;
}

// Generate a synthetic setup thunk function for $monitor.
// The setup thunk performs initial print and registers the check thunk.
auto LowerMonitorSetupThunk(
    const hir::MonitorSystemCallData& data, MirBuilder& original_builder,
    [[maybe_unused]] mir::FunctionId check_thunk_id,
    [[maybe_unused]] const SnapshotLayout& layout) -> Result<mir::FunctionId> {
  Context& original_ctx = original_builder.GetContext();

  Context thunk_ctx{
      .mir_arena = original_ctx.mir_arena,
      .hir_arena = original_ctx.hir_arena,
      .type_arena = original_ctx.type_arena,
      .constant_arena = original_ctx.constant_arena,
      .symbol_table = original_ctx.symbol_table,
      .module_places = original_ctx.module_places,
      .local_places = {},
      .next_local_id = 0,
      .next_temp_id = 0,
      .local_types = {},
      .temp_types = {},
      .builtin_types = original_ctx.builtin_types,
      .symbol_to_mir_function = original_ctx.symbol_to_mir_function,
  };

  MirBuilder thunk_builder(original_ctx.mir_arena, &thunk_ctx);

  BlockIndex entry = thunk_builder.CreateBlock();
  thunk_builder.SetCurrentBlock(entry);

  // Lower format ops for initial print
  auto ops_result = LowerFormatOps(data.ops, data.descriptor, thunk_builder);
  if (!ops_result) return std::unexpected(ops_result.error());
  auto [mir_ops, mir_descriptor] = std::move(*ops_result);

  // Emit DisplayEffect for initial print
  mir::DisplayEffect display{
      .print_kind = data.print_kind,
      .ops = std::move(mir_ops),
      .descriptor = std::move(mir_descriptor),
  };
  thunk_builder.EmitEffect(std::move(display));

  // The setup thunk is called with (DesignState*, Engine*).
  // LLVM lowering appends serialization + LyraMonitorRegister() in the
  // epilogue.

  // Emit void return
  thunk_builder.EmitTerminate(std::nullopt);

  auto blocks = thunk_builder.Finish();
  mir::Function thunk{
      .signature =
          {
              .return_type = original_ctx.builtin_types.void_type,
              .params = {},
          },
      .thunk_kind = mir::ThunkKind::kMonitorSetup,
      .entry = mir::BasicBlockId{0},
      .blocks = std::move(blocks),
      .local_types = std::move(thunk_ctx.local_types),
      .temp_types = std::move(thunk_ctx.temp_types),
      .param_local_slots = {},
      .param_origins = {},
  };
  mir::FunctionId thunk_id =
      original_ctx.mir_arena->AddFunction(std::move(thunk));

  if (original_ctx.generated_functions != nullptr) {
    original_ctx.generated_functions->push_back(thunk_id);
  }

  return thunk_id;
}

auto LowerMonitorEffect(
    const hir::MonitorSystemCallData& data, MirBuilder& builder)
    -> Result<void> {
  Context& ctx = builder.GetContext();

  // First, lower format ops to get MIR types for layout computation
  auto ops_result = LowerFormatOps(data.ops, data.descriptor, builder);
  if (!ops_result) return std::unexpected(ops_result.error());
  auto [mir_ops, mir_descriptor] = std::move(*ops_result);

  // Compute snapshot layout
  SnapshotLayout layout = ComputeSnapshotLayout(mir_ops, *ctx.type_arena);

  // Create check thunk
  auto check_result = LowerMonitorCheckThunk(data, builder, layout);
  if (!check_result) return std::unexpected(check_result.error());
  mir::FunctionId check_thunk_id = *check_result;

  // Create setup thunk
  auto setup_result =
      LowerMonitorSetupThunk(data, builder, check_thunk_id, layout);
  if (!setup_result) return std::unexpected(setup_result.error());
  mir::FunctionId setup_thunk_id = *setup_result;

  // Emit MonitorEffect with format ops for LLVM comparison code generation
  builder.EmitEffect(
      mir::MonitorEffect{
          .setup_thunk = setup_thunk_id,
          .check_thunk = check_thunk_id,
          .print_kind = data.print_kind,
          .format_ops = std::move(mir_ops),
          .offsets = layout.offsets,
          .byte_sizes = layout.byte_sizes,
          .prev_buffer_size = layout.total_size});
  return {};
}

auto LowerMonitorControlEffect(
    const hir::MonitorControlData& data, MirBuilder& builder) -> Result<void> {
  builder.EmitEffect(mir::MonitorControlEffect{.enable = data.enable});
  return {};
}

auto LowerMemIOEffect(const hir::MemIOData& data, MirBuilder& builder)
    -> Result<void> {
  Context& ctx = builder.GetContext();

  Result<mir::Operand> filename_result =
      LowerExpression(data.filename, builder);
  if (!filename_result) return std::unexpected(filename_result.error());
  mir::Operand filename = *filename_result;

  Result<LvalueResult> target_result = LowerLvalue(data.target, builder);
  if (!target_result) return std::unexpected(target_result.error());
  LvalueResult target = *target_result;
  const hir::Expression& target_expr = (*ctx.hir_arena)[data.target];

  std::optional<mir::Operand> start_addr;
  std::optional<mir::Operand> end_addr;
  if (data.start_addr) {
    Result<mir::Operand> addr_result =
        LowerExpression(*data.start_addr, builder);
    if (!addr_result) return std::unexpected(addr_result.error());
    start_addr = *addr_result;
  }
  if (data.end_addr) {
    Result<mir::Operand> addr_result = LowerExpression(*data.end_addr, builder);
    if (!addr_result) return std::unexpected(addr_result.error());
    end_addr = *addr_result;
  }

  mir::MemIOEffect effect{
      .is_read = data.is_read,
      .is_hex = data.is_hex,
      .target = target.place,
      .target_type = target_expr.type,
      .filename = std::move(filename),
      .start_addr = std::move(start_addr),
      .end_addr = std::move(end_addr),
  };
  builder.EmitEffect(std::move(effect));
  return {};
}

auto LowerExpressionStatement(
    const hir::ExpressionStatementData& data, MirBuilder& builder)
    -> Result<void> {
  Context& ctx = builder.GetContext();
  const hir::Expression& expr = (*ctx.hir_arena)[data.expression];

  // Check if this is a system call that should be an Effect instruction
  if (const auto* syscall =
          std::get_if<hir::SystemCallExpressionData>(&expr.data)) {
    Result<void> result;
    std::visit(
        [&](const auto& call_data) {
          using T = std::decay_t<decltype(call_data)>;
          if constexpr (std::is_same_v<T, hir::DisplaySystemCallData>) {
            result = LowerDisplayEffect(call_data, builder);
          } else if constexpr (std::is_same_v<T, hir::SeveritySystemCallData>) {
            result = LowerSeverityEffect(call_data, builder);
          } else if constexpr (std::is_same_v<T, hir::SFormatSystemCallData>) {
            result = LowerSFormatEffect(call_data, builder);
          } else if constexpr (
              std::is_same_v<T, hir::TestPlusargsData> ||
              std::is_same_v<T, hir::ValuePlusargsData>) {
            // Plusargs always produce a value; evaluate and discard
            Result<mir::Operand> expr_result =
                LowerExpression(data.expression, builder);
            if (!expr_result) {
              result = std::unexpected(expr_result.error());
            } else {
              result = {};
            }
          } else if constexpr (std::is_same_v<T, hir::MemIOData>) {
            result = LowerMemIOEffect(call_data, builder);
          } else if constexpr (std::is_same_v<T, hir::FopenData>) {
            // $fopen as statement - evaluate for side effect, discard result
            Result<mir::Operand> expr_result =
                LowerExpression(data.expression, builder);
            if (!expr_result) {
              result = std::unexpected(expr_result.error());
            } else {
              result = {};
            }
          } else if constexpr (std::is_same_v<T, hir::FcloseData>) {
            Result<mir::Operand> desc_result =
                LowerExpression(call_data.descriptor, builder);
            if (!desc_result) {
              result = std::unexpected(desc_result.error());
            } else {
              builder.EmitEffect(
                  mir::SystemTfEffect{
                      .opcode = SystemTfOpcode::kFclose,
                      .args = {*desc_result}});
              result = {};
            }
          } else if constexpr (std::is_same_v<T, hir::TimeFormatData>) {
            builder.EmitEffect(
                mir::TimeFormatEffect{
                    .units = call_data.units,
                    .precision = call_data.precision,
                    .suffix = call_data.suffix,
                    .min_width = call_data.min_width});
            result = {};
          } else if constexpr (std::is_same_v<T, hir::RuntimeQueryData>) {
            // Runtime queries produce a value; evaluate and discard
            Result<mir::Operand> expr_result =
                LowerExpression(data.expression, builder);
            if (!expr_result) {
              result = std::unexpected(expr_result.error());
            } else {
              result = {};
            }
          } else if constexpr (std::is_same_v<T, hir::MonitorSystemCallData>) {
            result = LowerMonitorEffect(call_data, builder);
          } else if constexpr (std::is_same_v<T, hir::MonitorControlData>) {
            result = LowerMonitorControlEffect(call_data, builder);
          } else {
            throw common::InternalError(
                "LowerExpressionStatement", "unhandled system call kind");
          }
        },
        *syscall);
    return result;
  }

  // Regular expression statement - evaluate for side effects, discard result
  // All builtin methods (including void methods like push_back, delete)
  // are handled uniformly through LowerExpression.
  Result<mir::Operand> expr_result = LowerExpression(data.expression, builder);
  if (!expr_result) return std::unexpected(expr_result.error());
  return {};
}

// Represents a flattened condition-body pair from an if-else chain.
struct ConditionBodyPair {
  hir::ExpressionId condition;
  hir::StatementId body;
};

// Result of flattening an if-else chain.
struct FlattenResult {
  std::vector<ConditionBodyPair> pairs;
  std::optional<hir::StatementId> else_body;  // Final else statement, if any
};

// Flatten an if-else chain into a list of (condition, body) pairs.
// Chains else-if's that have no qualifier (kNone) or the same qualifier.
// If an else-if has a DIFFERENT qualifier, it's treated as the final else body
// (preserving its own semantics).
auto FlattenIfElseChain(
    const hir::ConditionalStatementData& data, const hir::Arena& hir_arena,
    hir::UniquePriorityCheck expected_check) -> FlattenResult {
  FlattenResult result;
  result.pairs.push_back({data.condition, data.then_branch});

  if (!data.else_branch.has_value()) {
    return result;  // No else clause
  }

  // Check if else branch is another conditional (else-if chain)
  const hir::Statement& else_stmt = hir_arena[*data.else_branch];
  if (const auto* else_cond =
          std::get_if<hir::ConditionalStatementData>(&else_stmt.data)) {
    // Chain if the else-if has the same qualifier OR no qualifier (kNone).
    // A plain "else if" is part of the outer qualified construct.
    // Only "else <different-qualifier> if" starts a new construct.
    if (else_cond->check == expected_check ||
        else_cond->check == hir::UniquePriorityCheck::kNone) {
      FlattenResult rest =
          FlattenIfElseChain(*else_cond, hir_arena, expected_check);
      result.pairs.insert(
          result.pairs.end(), rest.pairs.begin(), rest.pairs.end());
      result.else_body = rest.else_body;
      return result;
    }
    // Different qualifier - treat the else-if as the final else body
    // It will be lowered with its own qualifier semantics
  }

  // Final else clause (non-conditional, or conditional with different
  // qualifier)
  result.else_body = *data.else_branch;
  return result;
}

// Materialize condition to a PlaceId (for EmitBranch/QualifiedDispatch).
auto MaterializeCondition(hir::ExpressionId cond_expr_id, MirBuilder& builder)
    -> Result<mir::PlaceId> {
  Context& ctx = builder.GetContext();
  Result<mir::Operand> cond_result = LowerExpression(cond_expr_id, builder);
  if (!cond_result) return std::unexpected(cond_result.error());
  mir::Operand cond = *cond_result;

  if (cond.kind == mir::Operand::Kind::kUse) {
    return std::get<mir::PlaceId>(cond.payload);
  }
  const hir::Expression& cond_expr = (*ctx.hir_arena)[cond_expr_id];
  mir::PlaceId temp = ctx.AllocTemp(cond_expr.type);
  builder.EmitAssign(temp, std::move(cond));
  return temp;
}

// Standard if-else lowering without qualifiers.
auto LowerConditionalNone(
    const hir::ConditionalStatementData& data, MirBuilder& builder)
    -> Result<void> {
  Result<mir::PlaceId> cond_result =
      MaterializeCondition(data.condition, builder);
  if (!cond_result) return std::unexpected(cond_result.error());
  mir::PlaceId cond_place = *cond_result;
  mir::Operand cond = mir::Operand::Use(cond_place);

  // Use a result holder to capture errors from callbacks
  Result<void> callback_result;
  if (data.else_branch.has_value()) {
    builder.EmitIfElse(
        cond,
        [&] { callback_result = LowerStatement(data.then_branch, builder); },
        [&] {
          if (callback_result) {
            callback_result = LowerStatement(*data.else_branch, builder);
          }
        });
  } else {
    builder.EmitIf(cond, [&] {
      callback_result = LowerStatement(data.then_branch, builder);
    });
  }
  return callback_result;
}

// Priority if: branch cascade + warning at end if no else.
auto LowerConditionalPriority(
    const hir::ConditionalStatementData& data, MirBuilder& builder)
    -> Result<void> {
  Context& ctx = builder.GetContext();

  // Flatten the if-else chain (only chain same-qualifier else-if's)
  FlattenResult flat = FlattenIfElseChain(
      data, *ctx.hir_arena, hir::UniquePriorityCheck::kPriority);
  const auto& pairs = flat.pairs;

  // Use a result holder to capture errors from callbacks
  Result<void> callback_result;

  // Build condition callbacks (each evaluates and materializes a condition)
  std::vector<std::function<mir::Operand()>> conditions;
  for (const auto& pair : pairs) {
    conditions.emplace_back([&builder, &callback_result,
                             cond_id = pair.condition]() {
      Result<mir::PlaceId> cond_result = MaterializeCondition(cond_id, builder);
      if (!cond_result) {
        callback_result = std::unexpected(cond_result.error());
        return mir::Operand::Use(mir::kInvalidPlaceId);  // Will be ignored
      }
      return mir::Operand::Use(*cond_result);
    });
  }

  // Build body callbacks
  std::vector<std::function<void()>> bodies;
  for (const auto& pair : pairs) {
    bodies.emplace_back([&builder, &callback_result, body_id = pair.body]() {
      if (callback_result) {
        callback_result = LowerStatement(body_id, builder);
      }
    });
  }

  // Build else callback
  auto else_callback = [&]() {
    if (!callback_result) {
      return;
    }
    if (flat.else_body.has_value()) {
      callback_result = LowerStatement(*flat.else_body, builder);
    } else {
      // No else clause: emit warning
      mir::DisplayEffect display{
          .print_kind = PrintKind::kDisplay,
          .ops = {mir::FormatOp{
              .kind = FormatKind::kLiteral,
              .value = std::nullopt,
              .literal = "warning: no condition matched in priority if",
              .type = TypeId{},
              .mods = {}}},
          .descriptor = std::nullopt,
      };
      builder.EmitEffect(std::move(display));
    }
  };

  builder.EmitPriorityChain(conditions, bodies, else_callback);
  return callback_result;
}

// Unique/Unique0 if: eval all conditions, emit QualifiedDispatch.
auto LowerConditionalUnique(
    const hir::ConditionalStatementData& data, MirBuilder& builder,
    hir::UniquePriorityCheck check) -> Result<void> {
  Context& ctx = builder.GetContext();

  // Flatten the if-else chain (only chain same-qualifier else-if's)
  FlattenResult flat = FlattenIfElseChain(data, *ctx.hir_arena, check);
  const auto& pairs = flat.pairs;
  bool has_else = flat.else_body.has_value();

  // Evaluate all conditions upfront
  std::vector<mir::PlaceId> condition_places;
  for (const auto& pair : pairs) {
    Result<mir::PlaceId> cond_result =
        MaterializeCondition(pair.condition, builder);
    if (!cond_result) return std::unexpected(cond_result.error());
    condition_places.push_back(*cond_result);
  }

  // Use a result holder to capture errors from callbacks
  Result<void> callback_result;

  // Build body callbacks
  std::vector<std::function<void()>> bodies;
  for (const auto& pair : pairs) {
    bodies.emplace_back([&builder, &callback_result, body_id = pair.body]() {
      if (callback_result) {
        callback_result = LowerStatement(body_id, builder);
      }
    });
  }

  // Determine qualifier
  mir::DispatchQualifier qualifier =
      (check == hir::UniquePriorityCheck::kUnique)
          ? mir::DispatchQualifier::kUnique
          : mir::DispatchQualifier::kUnique0;

  builder.EmitUniqueDispatch(
      qualifier, mir::DispatchStatementKind::kIf, condition_places, bodies,
      [&]() {
        if (callback_result && flat.else_body.has_value()) {
          callback_result = LowerStatement(*flat.else_body, builder);
        }
      },
      has_else);
  return callback_result;
}

auto LowerConditional(
    const hir::ConditionalStatementData& data, MirBuilder& builder)
    -> Result<void> {
  switch (data.check) {
    case hir::UniquePriorityCheck::kNone:
      return LowerConditionalNone(data, builder);
    case hir::UniquePriorityCheck::kPriority:
      return LowerConditionalPriority(data, builder);
    case hir::UniquePriorityCheck::kUnique:
    case hir::UniquePriorityCheck::kUnique0:
      return LowerConditionalUnique(data, builder, data.check);
  }
  return {};
}

// Helper: get comparison operator based on case condition type.
auto GetCaseComparisonOp(hir::CaseCondition condition) -> mir::BinaryOp {
  switch (condition) {
    case hir::CaseCondition::kNormal:
      return mir::BinaryOp::kEqual;
    case hir::CaseCondition::kCaseZ:
      return mir::BinaryOp::kCaseZMatch;
    case hir::CaseCondition::kCaseX:
      return mir::BinaryOp::kCaseXMatch;
  }
  return mir::BinaryOp::kEqual;
}

// Helper: materialize selector to a place.
auto MaterializeSelector(hir::ExpressionId selector_id, MirBuilder& builder)
    -> Result<mir::PlaceId> {
  Context& ctx = builder.GetContext();
  Result<mir::Operand> sel_result = LowerExpression(selector_id, builder);
  if (!sel_result) return std::unexpected(sel_result.error());
  mir::Operand sel = *sel_result;
  if (sel.kind == mir::Operand::Kind::kUse) {
    return std::get<mir::PlaceId>(sel.payload);
  }
  const hir::Expression& sel_expr = (*ctx.hir_arena)[selector_id];
  mir::PlaceId sel_place = ctx.AllocTemp(sel_expr.type);
  builder.EmitAssign(sel_place, std::move(sel));
  return sel_place;
}

// Standard case lowering without qualifiers.
auto LowerCaseNone(const hir::CaseStatementData& data, MirBuilder& builder)
    -> Result<void> {
  mir::BinaryOp cmp_op = GetCaseComparisonOp(data.condition);
  Result<mir::PlaceId> sel_result = MaterializeSelector(data.selector, builder);
  if (!sel_result) return std::unexpected(sel_result.error());
  mir::PlaceId sel_place = *sel_result;

  // Use a result holder to capture errors from callbacks
  Result<void> callback_result;

  // Build case items
  std::vector<MirBuilder::CaseItem> items;
  for (const auto& item : data.items) {
    MirBuilder::CaseItem case_item;
    for (auto expr_id : item.expressions) {
      case_item.expressions.emplace_back([&builder, &callback_result,
                                          expr_id]() {
        Result<mir::Operand> expr_result = LowerExpression(expr_id, builder);
        if (!expr_result) {
          callback_result = std::unexpected(expr_result.error());
          return mir::Operand::Const(Constant{});  // Will be ignored
        }
        return *expr_result;
      });
    }
    case_item.body = [&builder, &callback_result, stmt_id = item.statement]() {
      if (callback_result && stmt_id.has_value()) {
        callback_result = LowerStatement(*stmt_id, builder);
      }
    };
    items.push_back(std::move(case_item));
  }

  builder.EmitCaseCascade(sel_place, cmp_op, items, [&]() {
    if (callback_result && data.default_statement.has_value()) {
      callback_result = LowerStatement(*data.default_statement, builder);
    }
  });
  return callback_result;
}

// Priority case: comparison cascade + warning at end if no default.
auto LowerCasePriority(const hir::CaseStatementData& data, MirBuilder& builder)
    -> Result<void> {
  mir::BinaryOp cmp_op = GetCaseComparisonOp(data.condition);
  Result<mir::PlaceId> sel_result = MaterializeSelector(data.selector, builder);
  if (!sel_result) return std::unexpected(sel_result.error());
  mir::PlaceId sel_place = *sel_result;
  bool has_default = data.default_statement.has_value();

  // Use a result holder to capture errors from callbacks
  Result<void> callback_result;

  // Build case items
  std::vector<MirBuilder::CaseItem> items;
  for (const auto& item : data.items) {
    MirBuilder::CaseItem case_item;
    for (auto expr_id : item.expressions) {
      case_item.expressions.emplace_back([&builder, &callback_result,
                                          expr_id]() {
        Result<mir::Operand> expr_result = LowerExpression(expr_id, builder);
        if (!expr_result) {
          callback_result = std::unexpected(expr_result.error());
          return mir::Operand::Const(Constant{});  // Will be ignored
        }
        return *expr_result;
      });
    }
    case_item.body = [&builder, &callback_result, stmt_id = item.statement]() {
      if (callback_result && stmt_id.has_value()) {
        callback_result = LowerStatement(*stmt_id, builder);
      }
    };
    items.push_back(std::move(case_item));
  }

  builder.EmitCaseCascade(sel_place, cmp_op, items, [&]() {
    if (!callback_result) {
      return;
    }
    if (has_default) {
      callback_result = LowerStatement(*data.default_statement, builder);
    } else {
      mir::DisplayEffect display{
          .print_kind = PrintKind::kDisplay,
          .ops = {mir::FormatOp{
              .kind = FormatKind::kLiteral,
              .value = std::nullopt,
              .literal = "warning: no matching case item in priority case",
              .type = TypeId{},
              .mods = {}}},
          .descriptor = std::nullopt,
      };
      builder.EmitEffect(std::move(display));
    }
  });
  return callback_result;
}

// Unique/Unique0 case: eval all matches, emit QualifiedDispatch.
auto LowerCaseUnique(
    const hir::CaseStatementData& data, MirBuilder& builder,
    hir::UniquePriorityCheck check) -> Result<void> {
  Context& ctx = builder.GetContext();
  mir::BinaryOp cmp_op = GetCaseComparisonOp(data.condition);
  Result<mir::PlaceId> sel_result = MaterializeSelector(data.selector, builder);
  if (!sel_result) return std::unexpected(sel_result.error());
  mir::PlaceId sel_place = *sel_result;
  bool has_default = data.default_statement.has_value();

  // Evaluate all item matches upfront.
  // For each item, OR together all its expressions to get a single condition.
  std::vector<mir::PlaceId> item_conditions;
  for (const auto& item : data.items) {
    mir::PlaceId item_match = mir::kInvalidPlaceId;

    for (const auto& expr : item.expressions) {
      auto val_result = LowerExpression(expr, builder);
      if (!val_result) return std::unexpected(val_result.error());
      mir::Operand val = std::move(*val_result);
      TypeId cmp_type = ctx.GetBitType();
      mir::Rvalue cmp_rvalue{
          .operands = {mir::Operand::Use(sel_place), std::move(val)},
          .info = mir::BinaryRvalueInfo{.op = cmp_op},
      };
      mir::PlaceId cmp_result =
          builder.EmitTemp(cmp_type, std::move(cmp_rvalue));

      if (item_match == mir::kInvalidPlaceId) {
        item_match = cmp_result;
      } else {
        // OR with previous matches (any expression matching means item matches)
        mir::Rvalue or_rvalue{
            .operands =
                {mir::Operand::Use(item_match), mir::Operand::Use(cmp_result)},
            .info = mir::BinaryRvalueInfo{.op = mir::BinaryOp::kBitwiseOr},
        };
        item_match = builder.EmitTemp(cmp_type, std::move(or_rvalue));
      }
    }
    item_conditions.push_back(item_match);
  }

  // Build body callbacks with error capture
  Result<void> callback_result;
  std::vector<std::function<void()>> bodies;
  for (const auto& item : data.items) {
    bodies.emplace_back(
        [&builder, &callback_result, stmt_id = item.statement]() {
          if (callback_result && stmt_id.has_value()) {
            callback_result = LowerStatement(*stmt_id, builder);
          }
        });
  }

  mir::DispatchQualifier qualifier =
      (check == hir::UniquePriorityCheck::kUnique)
          ? mir::DispatchQualifier::kUnique
          : mir::DispatchQualifier::kUnique0;

  builder.EmitUniqueDispatch(
      qualifier, mir::DispatchStatementKind::kCase, item_conditions, bodies,
      [&]() {
        if (!callback_result) {
          return;
        }
        if (has_default) {
          callback_result = LowerStatement(*data.default_statement, builder);
        }
      },
      has_default);
  return callback_result;
}

auto LowerCase(const hir::CaseStatementData& data, MirBuilder& builder)
    -> Result<void> {
  // Slang invariant: each case item has at least one expression.
  for (const auto& item : data.items) {
    if (item.expressions.empty()) {
      throw common::InternalError(
          "LowerCase",
          "case item has no expressions (Slang invariant violated)");
    }
  }

  switch (data.check) {
    case hir::UniquePriorityCheck::kNone:
      return LowerCaseNone(data, builder);
    case hir::UniquePriorityCheck::kPriority:
      return LowerCasePriority(data, builder);
    case hir::UniquePriorityCheck::kUnique:
    case hir::UniquePriorityCheck::kUnique0:
      return LowerCaseUnique(data, builder, data.check);
  }
  return {};
}

auto LowerForLoop(const hir::ForLoopStatementData& data, MirBuilder& builder)
    -> Result<void> {
  Context& ctx = builder.GetContext();

  // 1. Execute variable declarations in current block
  for (hir::StatementId var_decl : data.var_decls) {
    auto result = LowerStatement(var_decl, builder);
    if (!result) return std::unexpected(result.error());
  }

  // 2. Evaluate init expressions (for side effects, result discarded)
  for (hir::ExpressionId init_expr : data.init_exprs) {
    auto result = LowerExpression(init_expr, builder);
    if (!result) return std::unexpected(result.error());
  }

  // 3. Create blocks
  BlockIndex cond_bb = builder.CreateBlock();
  BlockIndex body_bb = builder.CreateBlock();
  BlockIndex step_bb = builder.CreateBlock();
  BlockIndex exit_bb = builder.CreateBlock();

  // 4. Jump to condition block
  builder.EmitJump(cond_bb);

  // 5. Condition block
  builder.SetCurrentBlock(cond_bb);
  if (data.condition.has_value()) {
    auto cond_result = LowerExpression(*data.condition, builder);
    if (!cond_result) return std::unexpected(cond_result.error());
    mir::Operand cond = std::move(*cond_result);

    // Materialize constant to temp if needed (EmitBranch requires Use operand)
    if (cond.kind == mir::Operand::Kind::kConst) {
      const hir::Expression& cond_expr = (*ctx.hir_arena)[*data.condition];
      mir::PlaceId temp = ctx.AllocTemp(cond_expr.type);
      builder.EmitAssign(temp, std::move(cond));
      cond = mir::Operand::Use(temp);
    }

    builder.EmitBranch(cond, body_bb, exit_bb);
  } else {
    // No condition = infinite loop
    builder.EmitJump(body_bb);
  }

  // 6. Body block
  builder.SetCurrentBlock(body_bb);
  {
    LoopGuard guard(
        builder, {.exit_block = exit_bb, .continue_block = step_bb});
    auto result = LowerStatement(data.body, builder);
    if (!result) return std::unexpected(result.error());
  }
  builder.EmitJump(step_bb);

  // 7. Step block - evaluate step expressions (for side effects)
  builder.SetCurrentBlock(step_bb);
  for (hir::ExpressionId step_expr : data.steps) {
    auto result = LowerExpression(step_expr, builder);
    if (!result) return std::unexpected(result.error());
  }
  builder.EmitJump(cond_bb);

  // 8. Continue in exit block
  builder.SetCurrentBlock(exit_bb);
  return {};
}

auto LowerWhileLoop(
    const hir::WhileLoopStatementData& data, MirBuilder& builder)
    -> Result<void> {
  Context& ctx = builder.GetContext();

  BlockIndex cond_bb = builder.CreateBlock();
  BlockIndex body_bb = builder.CreateBlock();
  BlockIndex exit_bb = builder.CreateBlock();

  builder.EmitJump(cond_bb);

  // Condition block
  builder.SetCurrentBlock(cond_bb);
  auto cond_result = LowerExpression(data.condition, builder);
  if (!cond_result) return std::unexpected(cond_result.error());
  mir::Operand cond = std::move(*cond_result);
  if (cond.kind == mir::Operand::Kind::kConst) {
    const hir::Expression& cond_expr = (*ctx.hir_arena)[data.condition];
    mir::PlaceId temp = ctx.AllocTemp(cond_expr.type);
    builder.EmitAssign(temp, std::move(cond));
    cond = mir::Operand::Use(temp);
  }
  builder.EmitBranch(cond, body_bb, exit_bb);

  // Body block
  builder.SetCurrentBlock(body_bb);
  {
    LoopGuard guard(
        builder, {.exit_block = exit_bb, .continue_block = cond_bb});
    auto result = LowerStatement(data.body, builder);
    if (!result) return std::unexpected(result.error());
  }
  builder.EmitJump(cond_bb);

  builder.SetCurrentBlock(exit_bb);
  return {};
}

auto LowerDoWhileLoop(
    const hir::DoWhileLoopStatementData& data, MirBuilder& builder)
    -> Result<void> {
  Context& ctx = builder.GetContext();

  BlockIndex body_bb = builder.CreateBlock();
  BlockIndex cond_bb = builder.CreateBlock();
  BlockIndex exit_bb = builder.CreateBlock();

  builder.EmitJump(body_bb);

  // Body block (executes first)
  builder.SetCurrentBlock(body_bb);
  {
    LoopGuard guard(
        builder, {.exit_block = exit_bb, .continue_block = cond_bb});
    auto result = LowerStatement(data.body, builder);
    if (!result) return std::unexpected(result.error());
  }
  builder.EmitJump(cond_bb);

  // Condition block
  builder.SetCurrentBlock(cond_bb);
  auto cond_result = LowerExpression(data.condition, builder);
  if (!cond_result) return std::unexpected(cond_result.error());
  mir::Operand cond = std::move(*cond_result);
  if (cond.kind == mir::Operand::Kind::kConst) {
    const hir::Expression& cond_expr = (*ctx.hir_arena)[data.condition];
    mir::PlaceId temp = ctx.AllocTemp(cond_expr.type);
    builder.EmitAssign(temp, std::move(cond));
    cond = mir::Operand::Use(temp);
  }
  builder.EmitBranch(cond, body_bb, exit_bb);

  builder.SetCurrentBlock(exit_bb);
  return {};
}

auto MakeIntConstant(uint64_t value, TypeId type) -> Constant {
  IntegralConstant ic;
  ic.value.push_back(value);
  ic.unknown.push_back(0);
  return Constant{.type = type, .value = std::move(ic)};
}

auto LowerEventWait(
    const hir::EventWaitStatementData& data, MirBuilder& builder)
    -> Result<void> {
  Context& ctx = builder.GetContext();
  BlockIndex resume_bb = builder.CreateBlock();

  std::vector<mir::WaitTrigger> triggers;
  for (const auto& hir_trigger : data.triggers) {
    auto signal_result = LowerExpression(hir_trigger.signal, builder);
    if (!signal_result) return std::unexpected(signal_result.error());
    mir::Operand signal_op = std::move(*signal_result);
    if (signal_op.kind != mir::Operand::Kind::kUse) {
      throw common::InternalError(
          "LowerEventWait", "event trigger signal must be a design variable");
    }
    auto place_id = std::get<mir::PlaceId>(signal_op.payload);
    const auto& place = (*ctx.mir_arena)[place_id];
    if (place.root.kind != mir::PlaceRoot::Kind::kDesign) {
      throw common::InternalError(
          "LowerEventWait", "event trigger must reference a design variable");
    }
    auto slot_id = mir::SlotId{static_cast<uint32_t>(place.root.id)};

    if (hir_trigger.edge == hir::EventEdgeKind::kBothEdges) {
      triggers.push_back(
          {.signal = slot_id, .edge = common::EdgeKind::kPosedge});
      triggers.push_back(
          {.signal = slot_id, .edge = common::EdgeKind::kNegedge});
    } else {
      common::EdgeKind edge = common::EdgeKind::kAnyChange;
      switch (hir_trigger.edge) {
        case hir::EventEdgeKind::kNone:
          edge = common::EdgeKind::kAnyChange;
          break;
        case hir::EventEdgeKind::kPosedge:
          edge = common::EdgeKind::kPosedge;
          break;
        case hir::EventEdgeKind::kNegedge:
          edge = common::EdgeKind::kNegedge;
          break;
        case hir::EventEdgeKind::kBothEdges:
          break;  // Already handled above
      }
      triggers.push_back({.signal = slot_id, .edge = edge});
    }
  }

  builder.EmitWait(std::move(triggers), resume_bb);
  builder.SetCurrentBlock(resume_bb);
  return {};
}

auto LowerRepeatLoop(
    const hir::RepeatLoopStatementData& data, MirBuilder& builder)
    -> Result<void> {
  Context& ctx = builder.GetContext();

  // 1. Evaluate count once and store in a temp
  auto count_result = LowerExpression(data.count, builder);
  if (!count_result) return std::unexpected(count_result.error());
  mir::Operand count_val = std::move(*count_result);
  const hir::Expression& count_expr = (*ctx.hir_arena)[data.count];
  mir::PlaceId counter = ctx.AllocTemp(count_expr.type);
  builder.EmitAssign(counter, std::move(count_val));

  // 2. Create blocks
  BlockIndex cond_bb = builder.CreateBlock();
  BlockIndex body_bb = builder.CreateBlock();
  BlockIndex step_bb = builder.CreateBlock();
  BlockIndex exit_bb = builder.CreateBlock();

  builder.EmitJump(cond_bb);

  // 3. Condition block: counter > 0
  builder.SetCurrentBlock(cond_bb);
  TypeId cond_type = ctx.GetBitType();
  Constant zero = MakeIntConstant(0, count_expr.type);

  // Choose signed or unsigned comparison based on count expression type
  const Type& count_type = (*ctx.type_arena)[count_expr.type];
  mir::BinaryOp cmp_op = mir::BinaryOp::kGreaterThan;
  if (IsPacked(count_type) && IsPackedSigned(count_type, *ctx.type_arena)) {
    cmp_op = mir::BinaryOp::kGreaterThanSigned;
  }

  mir::Rvalue cmp_rvalue{
      .operands = {mir::Operand::Use(counter), mir::Operand::Const(zero)},
      .info = mir::BinaryRvalueInfo{.op = cmp_op},
  };
  mir::PlaceId cmp_result = builder.EmitTemp(cond_type, std::move(cmp_rvalue));
  builder.EmitBranch(mir::Operand::Use(cmp_result), body_bb, exit_bb);

  // 4. Body block
  builder.SetCurrentBlock(body_bb);
  {
    LoopGuard guard(
        builder, {.exit_block = exit_bb, .continue_block = step_bb});
    auto result = LowerStatement(data.body, builder);
    if (!result) return std::unexpected(result.error());
  }
  builder.EmitJump(step_bb);

  // 5. Step block: decrement counter
  builder.SetCurrentBlock(step_bb);
  Constant one = MakeIntConstant(1, count_expr.type);
  mir::Rvalue dec_rvalue{
      .operands = {mir::Operand::Use(counter), mir::Operand::Const(one)},
      .info = mir::BinaryRvalueInfo{.op = mir::BinaryOp::kSubtract},
  };
  mir::PlaceId new_count =
      builder.EmitTemp(count_expr.type, std::move(dec_rvalue));
  builder.EmitAssign(counter, mir::Operand::Use(new_count));
  builder.EmitJump(cond_bb);

  // 6. Exit
  builder.SetCurrentBlock(exit_bb);
  return {};
}

}  // namespace

auto LowerStatement(hir::StatementId stmt_id, MirBuilder& builder)
    -> Result<void> {
  // Set HIR source for origin tracking (recorded at emit time)
  builder.SetCurrentHirSource(stmt_id);

  const hir::Statement& stmt = (*builder.GetContext().hir_arena)[stmt_id];

  Result<void> result;
  std::visit(
      [&](const auto& data) {
        using T = std::decay_t<decltype(data)>;
        if constexpr (std::is_same_v<T, hir::BlockStatementData>) {
          result = LowerBlock(data, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::VariableDeclarationStatementData>) {
          result = LowerVariableDeclaration(data, builder);
        } else if constexpr (std::is_same_v<T, hir::AssignmentStatementData>) {
          result = LowerAssignment(data, builder);
        } else if constexpr (std::is_same_v<T, hir::ExpressionStatementData>) {
          result = LowerExpressionStatement(data, builder);
        } else if constexpr (std::is_same_v<T, hir::ConditionalStatementData>) {
          result = LowerConditional(data, builder);
        } else if constexpr (std::is_same_v<T, hir::CaseStatementData>) {
          result = LowerCase(data, builder);
        } else if constexpr (std::is_same_v<T, hir::ForLoopStatementData>) {
          result = LowerForLoop(data, builder);
        } else if constexpr (std::is_same_v<T, hir::WhileLoopStatementData>) {
          result = LowerWhileLoop(data, builder);
        } else if constexpr (std::is_same_v<T, hir::DoWhileLoopStatementData>) {
          result = LowerDoWhileLoop(data, builder);
        } else if constexpr (std::is_same_v<T, hir::RepeatLoopStatementData>) {
          result = LowerRepeatLoop(data, builder);
        } else if constexpr (std::is_same_v<T, hir::BreakStatementData>) {
          const auto* loop = builder.CurrentLoop();
          if (loop == nullptr) {
            throw common::InternalError(
                "LowerStatement", "break statement outside loop");
          }
          builder.EmitJump(loop->exit_block);
          // After emitting a terminator, subsequent code is unreachable (no-op)
        } else if constexpr (std::is_same_v<T, hir::ContinueStatementData>) {
          const auto* loop = builder.CurrentLoop();
          if (loop == nullptr) {
            throw common::InternalError(
                "LowerStatement", "continue statement outside loop");
          }
          builder.EmitJump(loop->continue_block);
          // After emitting a terminator, subsequent code is unreachable (no-op)
        } else if constexpr (std::is_same_v<T, hir::TerminateStatementData>) {
          // Map HIR TerminationKind to MIR TerminationKind
          mir::TerminationKind mir_kind = mir::TerminationKind::kFinish;
          switch (data.kind) {
            case hir::TerminationKind::kFinish:
              mir_kind = mir::TerminationKind::kFinish;
              break;
            case hir::TerminationKind::kStop:
              mir_kind = mir::TerminationKind::kStop;
              break;
            case hir::TerminationKind::kExit:
              mir_kind = mir::TerminationKind::kExit;
              break;
            case hir::TerminationKind::kFatal:
              mir_kind = mir::TerminationKind::kFatal;
              break;
          }

          // Format message for $fatal using SFormat (reuse $sformatf infra)
          std::optional<mir::Operand> message = std::nullopt;
          if (mir_kind == mir::TerminationKind::kFatal &&
              !data.message_args.empty()) {
            Context& ctx = builder.GetContext();

            // Build SFormat rvalue from message args
            std::vector<mir::Operand> format_operands;
            format_operands.reserve(data.message_args.size());
            for (hir::ExpressionId arg_id : data.message_args) {
              auto arg_result = LowerExpression(arg_id, builder);
              if (!arg_result) {
                result = std::unexpected(arg_result.error());
                return;
              }
              format_operands.push_back(std::move(*arg_result));
            }

            // SFormat with runtime format (args[0] is format string or first
            // value)
            mir::SFormatRvalueInfo info{
                .ops = {},
                .default_format = FormatKind::kDecimal,
                .has_runtime_format = true,
            };
            mir::Rvalue rvalue{
                .operands = std::move(format_operands),
                .info = std::move(info)};
            mir::PlaceId msg_place =
                builder.EmitTemp(ctx.GetStringType(), std::move(rvalue));
            message = mir::Operand::Use(msg_place);
          }

          builder.EmitTerminate(
              mir::Finish{
                  .kind = mir_kind, .level = data.level, .message = message});
          // After emitting a terminator, subsequent code is unreachable (no-op)
        } else if constexpr (std::is_same_v<T, hir::ReturnStatementData>) {
          // Purely structural return lowering:
          // - `return expr;` → Return(value)
          // - `return;` → Return(nullopt)
          // Invariants (void vs non-void) are enforced by MIR verifier.
          // After emitting a terminator, IsReachable() returns false and
          // subsequent Emit* calls are no-ops.
          if (data.value != hir::kInvalidExpressionId) {
            auto value_result = LowerExpression(data.value, builder);
            if (!value_result) {
              result = std::unexpected(value_result.error());
              return;
            }
            builder.EmitReturn(std::move(*value_result));
          } else {
            builder.EmitTerminate(std::nullopt);
          }
        } else if constexpr (std::is_same_v<T, hir::DelayStatementData>) {
          // Create resume block for code after delay
          BlockIndex resume_bb = builder.CreateBlock();

          // Emit delay terminator with resume block
          builder.EmitDelay(data.ticks, resume_bb);

          // Continue lowering in the resume block
          builder.SetCurrentBlock(resume_bb);
        } else if constexpr (std::is_same_v<T, hir::EventWaitStatementData>) {
          result = LowerEventWait(data, builder);
        } else {
          throw common::InternalError(
              "LowerStatement", "unhandled statement kind");
        }
      },
      stmt.data);
  return result;
}

}  // namespace lyra::lowering::hir_to_mir

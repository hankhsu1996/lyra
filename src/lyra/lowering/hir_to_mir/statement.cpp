#include "lyra/lowering/hir_to_mir/statement.hpp"

#include <algorithm>
#include <cstdint>
#include <expected>
#include <format>
#include <functional>
#include <optional>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/bit_target_mapping.hpp"
#include "lyra/common/constant.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/edge_kind.hpp"
#include "lyra/common/format.hpp"
#include "lyra/common/index_plan.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/system_tf.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/operator.hpp"
#include "lyra/hir/rvalue.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/hir/system_call.hpp"
#include "lyra/lowering/hir_to_mir/bound_hierarchy.hpp"
#include "lyra/lowering/hir_to_mir/builder.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/expression.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/lowering/hir_to_mir/packed_alignment.hpp"
#include "lyra/lowering/hir_to_mir/pattern.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"
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

// Canonical event resolver: extract EventId from an HIR event expression.
// Single entry point for all event operations (wait, trigger).
//
// L8a scope: only simple named references (`event e; @e; -> e;`) are
// supported. Hierarchical event references, .triggered, and other
// event expression forms will be added in L8b/L8c and must extend this
// resolver rather than creating parallel resolution paths.
auto ResolveEventRef(hir::ExpressionId expr_id, MirBuilder& builder)
    -> mir::EventId {
  const auto& expr = (*builder.GetContext().hir_arena)[expr_id];
  const auto* name_ref = std::get_if<hir::NameRefExpressionData>(&expr.data);
  if (name_ref == nullptr) {
    throw common::InternalError(
        "ResolveEventRef",
        "unsupported event expression form (L8a supports named refs only)");
  }
  return builder.GetContext().LookupEvent(name_ref->symbol);
}

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

  if (data.initializer.has_value()) {
    const hir::RValue& rvalue = *data.initializer;
    if (rvalue.IsExpression()) {
      // Expression RValue: compute value then store
      Result<mir::Operand> value_result =
          LowerExpression(rvalue.AsExpression(), builder);
      if (!value_result) return std::unexpected(value_result.error());
      builder.EmitTypedAssign(alloc.place, std::move(*value_result), sym.type);
    } else {
      // Pattern RValue: emit fill/override effects
      Result<void> pattern_result =
          LowerPattern(rvalue.AsPattern(), alloc.place, builder);
      if (!pattern_result) return std::unexpected(pattern_result.error());
    }
  }
  return {};
}

auto LowerAssignment(
    const hir::AssignmentStatementData& data, MirBuilder& builder)
    -> Result<void> {
  Context& ctx = builder.GetContext();
  const hir::Expression& target_expr = (*ctx.hir_arena)[data.target];
  TypeId dest_type = target_expr.type;

  Result<LvalueResult> target_result = LowerLvalue(data.target, builder);
  if (!target_result) return std::unexpected(target_result.error());
  LvalueResult target = std::move(*target_result);

  // Fast path for object-creation assignments: `this.child = new_object(...)`.
  // Evaluate the NewObject rvalue straight into the child-handle slot so
  // MIR carries a single PlainAssign{dest: slot, rhs: Rvalue{NewObject}}
  // instead of materializing a temp and copying it. This keeps the MIR
  // constructor-body shape minimal and lets the backend drive its
  // three-step allocate/call/store lowering directly off the slot store.
  const hir::Expression& value_expr = (*ctx.hir_arena)[data.value];
  if (value_expr.kind == hir::ExpressionKind::kNewObject &&
      target.IsAlwaysValid() &&
      std::holds_alternative<mir::PlaceId>(target.dest)) {
    const auto& new_obj =
        std::get<hir::NewObjectExpressionData>(value_expr.data);
    std::vector<mir::Operand> operands;
    operands.reserve(new_obj.constructor_arguments.size());
    for (hir::ExpressionId arg_id : new_obj.constructor_arguments) {
      BlockIndex before = builder.CurrentBlock();
      Result<mir::Operand> arg_result = LowerExpression(arg_id, builder);
      if (!arg_result) return std::unexpected(arg_result.error());
      if (builder.CurrentBlock() != before) {
        for (auto& op : operands) {
          op = builder.ThreadValueToCurrentBlock(op);
        }
      }
      operands.push_back(*arg_result);
    }
    mir::Rvalue rvalue{
        .operands = std::move(operands),
        .info =
            mir::NewObjectRvalueInfo{
                .target_instance_sym = new_obj.target_instance_sym},
    };
    builder.EmitTypedAssign(target.dest, std::move(rvalue), dest_type);
    return {};
  }

  Result<mir::Operand> value_result = LowerExpression(data.value, builder);
  if (!value_result) return std::unexpected(value_result.error());
  mir::Operand value = *value_result;

  // B2: Use WriteTarget for assignment destination.
  mir::WriteTarget dest = target.dest;

  if (target.IsAlwaysValid()) {
    builder.EmitTypedAssign(dest, std::move(value), dest_type);
    return {};
  }

  // Guarded store: only write if validity is true (OOB/X/Z = no-op)
  mir::Operand cond = target.validity;

  // EmitBranch requires Use operand - materialize constant if needed
  if (cond.kind == mir::Operand::Kind::kConst) {
    mir::PlaceId temp = ctx.AllocTemp(ctx.GetBitType());
    builder.EmitPlainAssign(temp, std::move(cond));
    cond = mir::Operand::Use(temp);
  }

  BlockIndex store_bb = builder.CreateBlock();
  BlockIndex merge_bb = builder.CreateBlock();
  builder.EmitBranch(cond, store_bb, merge_bb);

  builder.SetCurrentBlock(store_bb);
  // Thread value through: it may be a UseTemp from before the guard blocks
  // (e.g., if LowerExpression created blocks for a ternary expression).
  value = builder.ThreadValueToCurrentBlock(std::move(value));
  builder.EmitTypedAssign(dest, std::move(value), dest_type);
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
    builder.EmitPlainAssign(temp, std::move(desc_val));
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
    } else if (hir_op.kind == FormatKind::kModulePath) {
      // %m: no value operand, uses current instance path at runtime
      mir_ops.push_back(
          mir::FormatOp{
              .kind = FormatKind::kModulePath,
              .value = std::nullopt,
              .literal = {},
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

// Generate a synthetic observer program for $strobe.
// The program re-evaluates expressions and prints at Postponed time.
auto LowerStrobeEffect(
    const hir::DisplaySystemCallData& data, MirBuilder& builder)
    -> Result<void> {
  Context& original_ctx = builder.GetContext();

  // Create a new context for the program function (shares design state
  // mappings).
  Context program_ctx{
      .mir_arena = original_ctx.mir_arena,
      .hir_arena = original_ctx.hir_arena,
      .type_arena = original_ctx.type_arena,
      .active_constant_arena = original_ctx.active_constant_arena,
      .symbol_table = original_ctx.symbol_table,
      .body_places = original_ctx.body_places,
      .design_places = original_ctx.design_places,
      .local_places = {},
      .design_place_cache = {},
      .body_events = original_ctx.body_events,
      .next_local_id = 0,
      .next_temp_id = 0,
      .local_types = {},
      .local_place_ids = {},
      .temp_types = {},
      .temp_metadata = {},
      .builtin_types = original_ctx.builtin_types,
      .symbol_to_mir_function = original_ctx.symbol_to_mir_function,
      .return_slot = std::nullopt,
      .return_type = original_ctx.builtin_types.void_type,
      .external_ref_cache = {},
  };

  // Create builder for the program function
  MirBuilder program_builder(
      original_ctx.mir_arena, &program_ctx, builder.GetOriginMap());

  // Create entry block
  BlockIndex entry = program_builder.CreateBlock();
  program_builder.SetCurrentBlock(entry);

  // Lower format ops (re-evaluates expressions at Postponed time)
  auto ops_result = LowerFormatOps(data.ops, data.descriptor, program_builder);
  if (!ops_result) return std::unexpected(ops_result.error());
  auto [mir_ops, mir_descriptor] = std::move(*ops_result);

  // Emit DisplayEffect
  mir::DisplayEffect display{
      .print_kind = data.print_kind,
      .ops = std::move(mir_ops),
      .descriptor = std::move(mir_descriptor),
  };
  program_builder.EmitEffect(std::move(display));

  // Emit void return
  program_builder.EmitTerminate(std::nullopt);

  // Build program function
  auto blocks = program_builder.Finish();
  mir::Function program{
      .signature =
          {
              .return_type = original_ctx.builtin_types.void_type,
              .params = {},  // Called via runtime ABI, not MIR call
          },
      .runtime_meta = mir::StrobeProgramMeta{},
      .canonical_symbol = kInvalidSymbolId,
      .entry = mir::BasicBlockId{0},
      .blocks = std::move(blocks),
      .local_types = std::move(program_ctx.local_types),
      .temp_types = std::move(program_ctx.temp_types),
      .temp_metadata = std::move(program_ctx.temp_metadata),
      .param_local_slots = {},
      .param_origins = {},
      .decision_sites = {},
      .abi_contract = {},
  };
  mir::FunctionId program_id =
      original_ctx.mir_arena->AddFunction(std::move(program));

  // Track the program so it gets declared in LLVM lowering
  if (original_ctx.generated_functions != nullptr) {
    original_ctx.generated_functions->push_back(program_id);
  }

  // Emit StrobeEffect in original builder
  builder.EmitEffect(mir::StrobeEffect{.program = program_id});
  return {};
}

auto LowerDisplayEffect(
    const hir::DisplaySystemCallData& data, MirBuilder& builder)
    -> Result<void> {
  // $strobe: create an observer program and schedule for Postponed region
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

// Emit a report effect. Canonical way to produce a semantic report in MIR.
auto EmitReportEffect(
    MirBuilder& builder, mir::ReportIntent intent, Severity severity,
    std::optional<common::OriginId> origin = std::nullopt,
    std::vector<mir::FormatOp> ops = {},
    mir::ReportContinuation continuation = mir::ReportContinuation::kContinue)
    -> Result<void> {
  builder.EmitEffect(
      mir::ReportEffect{
          .intent = intent,
          .severity = severity,
          .origin = origin,
          .ops = std::move(ops),
          .continuation = continuation,
      });
  return {};
}

auto LowerSeverityToReport(
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

  return EmitReportEffect(
      builder, mir::ReportIntent::kUserSeverity, data.level, std::nullopt,
      std::move(mir_ops));
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
  mir::PlaceId tmp = builder.EmitPlaceTemp(out_expr.type, std::move(rvalue));

  // Assign to output
  Result<LvalueResult> target_result = LowerLvalue(*data.output, builder);
  if (!target_result) return std::unexpected(target_result.error());
  LvalueResult target = std::move(*target_result);
  builder.EmitTypedAssign(target.dest, mir::Operand::Use(tmp), out_expr.type);
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
      // Unsupported type: should be rejected by eligibility gate in AST->HIR.
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

// Generate a synthetic check program for $monitor.
// The check program re-evaluates expressions and compares with prev_values.
// If any changed, it prints and updates prev_values.
auto LowerMonitorCheckProgram(
    const hir::MonitorSystemCallData& data, MirBuilder& original_builder,
    [[maybe_unused]] const SnapshotLayout& layout) -> Result<mir::FunctionId> {
  Context& original_ctx = original_builder.GetContext();

  Context program_ctx{
      .mir_arena = original_ctx.mir_arena,
      .hir_arena = original_ctx.hir_arena,
      .type_arena = original_ctx.type_arena,
      .active_constant_arena = original_ctx.active_constant_arena,
      .symbol_table = original_ctx.symbol_table,
      .body_places = original_ctx.body_places,
      .design_places = original_ctx.design_places,
      .local_places = {},
      .design_place_cache = {},
      .body_events = original_ctx.body_events,
      .next_local_id = 0,
      .next_temp_id = 0,
      .local_types = {},
      .local_place_ids = {},
      .temp_types = {},
      .temp_metadata = {},
      .builtin_types = original_ctx.builtin_types,
      .symbol_to_mir_function = original_ctx.symbol_to_mir_function,
      .return_slot = std::nullopt,
      .return_type = original_ctx.builtin_types.void_type,
      .external_ref_cache = {},
  };

  MirBuilder program_builder(
      original_ctx.mir_arena, &program_ctx, original_builder.GetOriginMap());

  BlockIndex entry = program_builder.CreateBlock();
  program_builder.SetCurrentBlock(entry);

  // Lower format ops (re-evaluates at Postponed time)
  auto ops_result = LowerFormatOps(data.ops, data.descriptor, program_builder);
  if (!ops_result) return std::unexpected(ops_result.error());
  auto [mir_ops, mir_descriptor] = std::move(*ops_result);

  // Emit DisplayEffect for printing
  mir::DisplayEffect display{
      .print_kind = data.print_kind,
      .ops = std::move(mir_ops),
      .descriptor = std::move(mir_descriptor),
  };
  program_builder.EmitEffect(std::move(display));

  // Emit void return
  program_builder.EmitTerminate(std::nullopt);

  // Build check program
  // Signature: void check_program(DesignState*, Engine*, ObserverContext*,
  // prev_buffer*)
  auto blocks = program_builder.Finish();
  mir::Function program{
      .signature =
          {
              .return_type = original_ctx.builtin_types.void_type,
              .params = {},
          },
      .runtime_meta =
          mir::MonitorCheckProgramMeta{
              .meta =
                  {
                      .offsets = layout.offsets,
                      .byte_sizes = layout.byte_sizes,
                      .total_size = layout.total_size,
                  },
          },
      .canonical_symbol = kInvalidSymbolId,
      .entry = mir::BasicBlockId{0},
      .blocks = std::move(blocks),
      .local_types = std::move(program_ctx.local_types),
      .temp_types = std::move(program_ctx.temp_types),
      .temp_metadata = std::move(program_ctx.temp_metadata),
      .param_local_slots = {},
      .param_origins = {},
      .decision_sites = {},
      .abi_contract = {},
  };
  mir::FunctionId program_id =
      original_ctx.mir_arena->AddFunction(std::move(program));

  if (original_ctx.generated_functions != nullptr) {
    original_ctx.generated_functions->push_back(program_id);
  }

  return program_id;
}

// Generate a synthetic setup program for $monitor.
// The setup program performs initial print and registers the check program.
auto LowerMonitorSetupProgram(
    const hir::MonitorSystemCallData& data, MirBuilder& original_builder,
    [[maybe_unused]] mir::FunctionId check_program_id,
    [[maybe_unused]] const SnapshotLayout& layout) -> Result<mir::FunctionId> {
  Context& original_ctx = original_builder.GetContext();

  Context program_ctx{
      .mir_arena = original_ctx.mir_arena,
      .hir_arena = original_ctx.hir_arena,
      .type_arena = original_ctx.type_arena,
      .active_constant_arena = original_ctx.active_constant_arena,
      .symbol_table = original_ctx.symbol_table,
      .body_places = original_ctx.body_places,
      .design_places = original_ctx.design_places,
      .local_places = {},
      .design_place_cache = {},
      .body_events = original_ctx.body_events,
      .next_local_id = 0,
      .next_temp_id = 0,
      .local_types = {},
      .local_place_ids = {},
      .temp_types = {},
      .temp_metadata = {},
      .builtin_types = original_ctx.builtin_types,
      .symbol_to_mir_function = original_ctx.symbol_to_mir_function,
      .return_slot = std::nullopt,
      .return_type = original_ctx.builtin_types.void_type,
      .external_ref_cache = {},
  };

  MirBuilder program_builder(
      original_ctx.mir_arena, &program_ctx, original_builder.GetOriginMap());

  BlockIndex entry = program_builder.CreateBlock();
  program_builder.SetCurrentBlock(entry);

  // Lower format ops for initial print
  auto ops_result = LowerFormatOps(data.ops, data.descriptor, program_builder);
  if (!ops_result) return std::unexpected(ops_result.error());
  auto [mir_ops, mir_descriptor] = std::move(*ops_result);

  // Emit DisplayEffect for initial print
  mir::DisplayEffect display{
      .print_kind = data.print_kind,
      .ops = std::move(mir_ops),
      .descriptor = std::move(mir_descriptor),
  };
  program_builder.EmitEffect(std::move(display));

  // The setup program is called with (DesignState*, Engine*, ObserverContext*).
  // LLVM lowering appends serialization + LyraMonitorRegister() in the
  // epilogue.

  // Emit void return
  program_builder.EmitTerminate(std::nullopt);

  auto blocks = program_builder.Finish();
  mir::Function program{
      .signature =
          {
              .return_type = original_ctx.builtin_types.void_type,
              .params = {},
          },
      .runtime_meta =
          mir::MonitorSetupProgramMeta{
              .meta = {.check_program = check_program_id},
          },
      .canonical_symbol = kInvalidSymbolId,
      .entry = mir::BasicBlockId{0},
      .blocks = std::move(blocks),
      .local_types = std::move(program_ctx.local_types),
      .temp_types = std::move(program_ctx.temp_types),
      .temp_metadata = std::move(program_ctx.temp_metadata),
      .param_local_slots = {},
      .param_origins = {},
      .decision_sites = {},
      .abi_contract = {},
  };
  mir::FunctionId program_id =
      original_ctx.mir_arena->AddFunction(std::move(program));

  if (original_ctx.generated_functions != nullptr) {
    original_ctx.generated_functions->push_back(program_id);
  }

  return program_id;
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

  // Create check program
  auto check_result = LowerMonitorCheckProgram(data, builder, layout);
  if (!check_result) return std::unexpected(check_result.error());
  mir::FunctionId check_program_id = *check_result;

  // Create setup program
  auto setup_result =
      LowerMonitorSetupProgram(data, builder, check_program_id, layout);
  if (!setup_result) return std::unexpected(setup_result.error());
  mir::FunctionId setup_program_id = *setup_result;

  // Emit MonitorEffect with format ops for LLVM comparison code generation
  builder.EmitEffect(
      mir::MonitorEffect{
          .setup_program = setup_program_id,
          .check_program = check_program_id,
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
  const hir::Expression& filename_expr = (*ctx.hir_arena)[data.filename];

  Result<LvalueResult> target_result = LowerLvalue(data.target, builder);
  if (!target_result) return std::unexpected(target_result.error());
  LvalueResult target = std::move(*target_result);
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
      .target = target.GetLocalPlace(),
      .target_type = target_expr.type,
      .filename =
          mir::TypedOperand{
              .operand = std::move(filename), .type = filename_expr.type},
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
            result = LowerSeverityToReport(call_data, builder);
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
          } else if constexpr (std::is_same_v<T, hir::FflushData>) {
            std::vector<mir::Operand> args;
            if (call_data.descriptor) {
              Result<mir::Operand> desc_result =
                  LowerExpression(*call_data.descriptor, builder);
              if (!desc_result) {
                result = std::unexpected(desc_result.error());
                return;
              }
              args.push_back(*desc_result);
            }
            builder.EmitEffect(
                mir::SystemTfEffect{
                    .opcode = SystemTfOpcode::kFflush,
                    .args = std::move(args)});
            result = {};
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

// Materialize condition to an Operand (for EmitBranch/DecisionDispatch).
// Returns Use or UseTemp operands directly; materializes constants to temps.
auto MaterializeCondition(hir::ExpressionId cond_expr_id, MirBuilder& builder)
    -> Result<mir::Operand> {
  Context& ctx = builder.GetContext();
  Result<mir::Operand> cond_result = LowerExpression(cond_expr_id, builder);
  if (!cond_result) return std::unexpected(cond_result.error());
  mir::Operand cond = *cond_result;

  // Use and UseTemp operands can be used directly as branch conditions
  if (cond.kind == mir::Operand::Kind::kUse ||
      cond.kind == mir::Operand::Kind::kUseTemp) {
    return cond;
  }
  // Other operands (constants) need to be materialized to a temp
  const hir::Expression& cond_expr = (*ctx.hir_arena)[cond_expr_id];
  mir::PlaceId temp = ctx.AllocTemp(cond_expr.type);
  builder.EmitPlainAssign(temp, std::move(cond));
  return mir::Operand::Use(temp);
}

// Map HIR qualifier to semantic DecisionQualifier.
auto MapQualifier(hir::UniquePriorityCheck check)
    -> semantic::DecisionQualifier {
  switch (check) {
    case hir::UniquePriorityCheck::kNone:
      return semantic::DecisionQualifier::kNone;
    case hir::UniquePriorityCheck::kUnique:
      return semantic::DecisionQualifier::kUnique;
    case hir::UniquePriorityCheck::kUnique0:
      return semantic::DecisionQualifier::kUnique0;
    case hir::UniquePriorityCheck::kPriority:
      return semantic::DecisionQualifier::kPriority;
  }
  return semantic::DecisionQualifier::kNone;
}

// Unified conditional (if) lowering for all qualifiers.
auto LowerConditional(
    const hir::ConditionalStatementData& data, MirBuilder& builder)
    -> Result<void> {
  Context& ctx = builder.GetContext();
  auto qualifier = MapQualifier(data.check);

  // Flatten the if-else chain
  FlattenResult flat = FlattenIfElseChain(data, *ctx.hir_arena, data.check);
  bool has_fallback = flat.else_body.has_value();

  // Capture decision origin before any nested lowering mutates it.
  const common::OriginId decision_origin = builder.GetCurrentOrigin();

  // Allocate decision site for qualified decisions
  std::optional<semantic::DecisionId> decision_id;
  if (qualifier != semantic::DecisionQualifier::kNone) {
    decision_id = builder.AllocateDecisionSite(
        qualifier, semantic::DecisionKind::kIf, has_fallback,
        semantic::DecisionArmCount{static_cast<uint16_t>(flat.pairs.size())},
        decision_origin);
  }

  // Build arm builders
  Result<void> callback_result;
  std::vector<MirBuilder::DecisionArmBuilder> arms;
  for (const auto& pair : flat.pairs) {
    arms.push_back(
        MirBuilder::DecisionArmBuilder{
            .build_condition = [&callback_result, cond_id = pair.condition](
                                   MirBuilder& b) -> mir::Operand {
              Result<mir::Operand> cond_result =
                  MaterializeCondition(cond_id, b);
              if (!cond_result) {
                callback_result = std::unexpected(cond_result.error());
                return mir::Operand::Use(mir::kInvalidPlaceId);
              }
              return *cond_result;
            },
            .build_body =
                [&callback_result, body_id = pair.body](MirBuilder& b) {
                  if (callback_result) {
                    callback_result = LowerStatement(body_id, b);
                  }
                },
        });
  }

  MirBuilder::DecisionBuildSpec spec{
      .qualifier = qualifier,
      .id = decision_id,
      .arms = std::move(arms),
      .build_fallback_body =
          has_fallback ? std::optional<std::function<void(MirBuilder&)>>(
                             [&callback_result,
                              else_id = *flat.else_body](MirBuilder& b) {
                               if (callback_result) {
                                 callback_result = LowerStatement(else_id, b);
                               }
                             })
                       : std::nullopt,
  };
  builder.EmitDecision(spec);
  return callback_result;
}

// Resolve the semantic type of a lowered MIR operand using the lowering
// context. Handles constants (inline type), places (root type for
// unprojected, InternalError for projected), and temps (temp_metadata).
auto ResolveLoweredOperandType(const mir::Operand& op, const Context& ctx)
    -> TypeId {
  switch (op.kind) {
    case mir::Operand::Kind::kConst:
      return std::get<Constant>(op.payload).type;
    case mir::Operand::Kind::kUse: {
      auto place_id = std::get<mir::PlaceId>(op.payload);
      const auto& place = ctx.ResolvePlace(place_id);
      if (!place.projections.empty()) {
        throw common::InternalError(
            "ResolveLoweredOperandType",
            "projected place in deferred capture (not supported)");
      }
      return place.root.type;
    }
    case mir::Operand::Kind::kUseTemp: {
      auto temp_id = std::get<mir::TempId>(op.payload);
      if (static_cast<size_t>(temp_id.value) >= ctx.temp_metadata.size()) {
        throw common::InternalError(
            "ResolveLoweredOperandType",
            std::format("temp_id {} out of range", temp_id.value));
      }
      return ctx.temp_metadata[temp_id.value].type;
    }
    case mir::Operand::Kind::kPoison:
      throw common::InternalError(
          "ResolveLoweredOperandType", "poison operand in deferred capture");
  }
  throw common::InternalError(
      "ResolveLoweredOperandType", "unknown operand kind");
}

// Lower a deferred captured value with type verification. Evaluates the
// HIR expression at encounter time and verifies the lowered MIR operand
// type matches the formal's semantic type (captured_type). This enforces
// the payload contract at the lowering boundary.
auto LowerDeferredCapturedValue(
    hir::ExpressionId expr_id, TypeId captured_type, MirBuilder& b)
    -> Result<mir::Operand> {
  auto value = LowerExpression(expr_id, b);
  if (!value) return std::unexpected(value.error());

  TypeId lowered_type = ResolveLoweredOperandType(*value, b.GetContext());
  if (lowered_type != captured_type) {
    throw common::InternalError(
        "LowerDeferredCapturedValue",
        std::format(
            "captured value type mismatch: lowered type {} != formal type {}",
            lowered_type.value, captured_type.value));
  }
  return *value;
}

// Look up a symbol as a required place. Returns a diagnostic on failure.
// Shared by NameRef and HierarchicalRef branches of deferred ref resolution.
auto LookupRequiredPlace(SymbolId sym, SourceSpan span, Context& ctx)
    -> Result<mir::PlaceId> {
  mir::PlaceId place = ctx.LookupPlace(sym);
  if (place == mir::kInvalidPlaceId) {
    return std::unexpected(
        Diagnostic::Unsupported(
            span, "deferred ref actual could not be resolved to a place",
            UnsupportedCategory::kFeature));
  }
  return place;
}

// Extract a compile-time constant index from an HIR expression.
// Returns the constant as a MIR operand, or an error if the expression
// is not a compile-time constant. This is a pure operation -- it does
// not emit any MIR instructions.
struct DeferredCapturedValuePlan {
  hir::ExpressionId expr_id;
  TypeId captured_type;
};

// Named build result from BuildDeferredAssertionAction.
// snapshot_captures is the filtered projection of action.actuals for
// kSnapshotValue entries, in exactly the same order. This invariant is
// validated before building EnqueueDeferredAssertionEffect.
struct DeferredAssertionActionBuildResult {
  mir::DeferredUserCallAction action;
  std::vector<DeferredCapturedValuePlan> snapshot_captures;
};

auto EmitDefaultImmediateAssertionFail(
    MirBuilder& builder, hir::ImmediateAssertionKind kind,
    common::OriginId origin) -> Result<void> {
  const char* default_msg = (kind == hir::ImmediateAssertionKind::kAssume)
                                ? "immediate assumption failed"
                                : "immediate assertion failed";
  return EmitReportEffect(
      builder, mir::ReportIntent::kAssertionFailure, Severity::kError, origin,
      {mir::FormatOp{
          .kind = FormatKind::kLiteral,
          .value = std::nullopt,
          .literal = default_msg,
          .type = {},
          .mods = {}}});
}

// Assert-like immediate assertion lowering (assert and assume).
// Mirrors the plain non-qualified conditional lowering path: condition is
// materialized inside the arm callback, matching LowerConditional() exactly.
// assume is semantically identical to assert in simulation (LRM 16.3).
auto LowerImmediateAssertLike(
    const hir::ImmediateAssertionStatementData& data, common::OriginId origin,
    MirBuilder& builder) -> Result<void> {
  Result<void> callback_result;

  std::vector<MirBuilder::DecisionArmBuilder> arms;
  arms.push_back(
      MirBuilder::DecisionArmBuilder{
          .build_condition = [&callback_result, cond_id = data.condition](
                                 MirBuilder& b) -> mir::Operand {
            if (!callback_result) {
              return mir::Operand::Use(mir::kInvalidPlaceId);
            }
            auto cond = MaterializeCondition(cond_id, b);
            if (!cond) {
              callback_result = std::unexpected(cond.error());
              return mir::Operand::Use(mir::kInvalidPlaceId);
            }
            return *cond;
          },
          .build_body =
              [&callback_result, &data](MirBuilder& b) {
                if (callback_result && data.pass_action.has_value()) {
                  callback_result = LowerStatement(*data.pass_action, b);
                }
              },
      });

  MirBuilder::DecisionBuildSpec spec{
      .qualifier = semantic::DecisionQualifier::kNone,
      .id = std::nullopt,
      .arms = std::move(arms),
      .build_fallback_body = std::optional<std::function<void(MirBuilder&)>>(
          [&callback_result, &data, origin](MirBuilder& b) {
            if (!callback_result) return;
            if (data.fail_action.has_value()) {
              callback_result = LowerStatement(*data.fail_action, b);
            } else {
              callback_result =
                  EmitDefaultImmediateAssertionFail(b, data.kind, origin);
            }
          }),
  };

  builder.EmitDecision(spec);
  return callback_result;
}

// Immediate cover lowering (LRM 16.3).
// cover(expr) records a hit when true, then executes optional pass action.
// No fail path, no default error synthesis.
auto LowerImmediateCover(
    const hir::ImmediateAssertionStatementData& data, SourceSpan span,
    MirBuilder& builder) -> Result<void> {
  if (data.fail_action.has_value()) {
    throw common::InternalError(
        "LowerImmediateCover", "immediate cover must not carry fail_action");
  }

  auto site_id = builder.GetContext().AllocateCoverSite(span);

  Result<void> callback_result;

  std::vector<MirBuilder::DecisionArmBuilder> arms;
  arms.push_back(
      MirBuilder::DecisionArmBuilder{
          .build_condition = [&callback_result, cond_id = data.condition](
                                 MirBuilder& b) -> mir::Operand {
            if (!callback_result) {
              return mir::Operand::Use(mir::kInvalidPlaceId);
            }
            auto cond = MaterializeCondition(cond_id, b);
            if (!cond) {
              callback_result = std::unexpected(cond.error());
              return mir::Operand::Use(mir::kInvalidPlaceId);
            }
            return *cond;
          },
          .build_body =
              [&callback_result, &data, site_id](MirBuilder& b) {
                if (!callback_result) return;
                b.EmitEffect(mir::CoverHitEffect{.site_id = site_id});
                if (data.pass_action.has_value()) {
                  callback_result = LowerStatement(*data.pass_action, b);
                }
              },
      });

  MirBuilder::DecisionBuildSpec spec{
      .qualifier = semantic::DecisionQualifier::kNone,
      .id = std::nullopt,
      .arms = std::move(arms),
      .build_fallback_body = std::nullopt,
  };

  builder.EmitDecision(spec);
  return callback_result;
}

// Map HIR assertion kind to MIR deferred assertion kind.
auto MapDeferredAssertionKind(hir::ImmediateAssertionKind kind)
    -> mir::DeferredAssertionKind {
  switch (kind) {
    case hir::ImmediateAssertionKind::kAssert:
      return mir::DeferredAssertionKind::kAssert;
    case hir::ImmediateAssertionKind::kAssume:
      return mir::DeferredAssertionKind::kAssume;
    case hir::ImmediateAssertionKind::kCover:
      return mir::DeferredAssertionKind::kCover;
  }
}

// Lower an HIR CaptureTarget to a MIR PlaceId.
// Resolves the root symbol to a place, then applies each projection in the
// capture path. Uses the shared Projection vocabulary from assign_target.hpp.
// All legality was validated at HIR time by ExtractHirCaptureTarget();
// this is purely translational. Only MemberProjection and IndexProjection
// are expected (other projection kinds are rejected at HIR time).
//
// Projection origin: all projection steps use a single origin derived from
// the capture target's source expression. This is an intentional
// normalized-origin policy for deferred capture targets -- the HIR legality
// gate produces one normalized path per capture, and the origin identifies
// the outermost expression that produced that path.
auto LowerHirCaptureTargetToPlace(
    const hir::HirCaptureTarget& target, Context& ctx, MirBuilder& builder)
    -> Result<mir::PlaceId> {
  auto root = LookupRequiredPlace(target.root_symbol, target.span, ctx);
  if (!root) return std::unexpected(root.error());

  common::OriginId origin = builder.RecordProjectionOrigin(target.source_expr);
  const auto& hir_arena = *ctx.hir_arena;

  mir::PlaceId place = *root;
  for (const auto& proj : target.path) {
    mir::Projection mir_proj = std::visit(
        [&](const auto& p) -> mir::Projection {
          using T = std::decay_t<decltype(p)>;
          if constexpr (std::is_same_v<T, hir::MemberProjection>) {
            return mir::Projection{
                .info =
                    mir::FieldProjection{
                        .field_index = static_cast<int>(p.field.value)},
                .origin = origin,
            };
          } else if constexpr (std::is_same_v<T, hir::IndexProjection>) {
            // The index was validated as a compile-time constant at HIR
            // time by ExtractHirCaptureTarget. Read the constant directly
            // from the HIR arena -- do not re-enter general expression
            // lowering for a validated constant.
            const auto& index_expr = hir_arena[p.index];
            const auto* const_data =
                std::get_if<hir::ConstantExpressionData>(&index_expr.data);
            if (const_data == nullptr) {
              throw common::InternalError(
                  "LowerHirCaptureTargetToPlace",
                  "capture target index is not a constant expression "
                  "(HIR legality gate should have rejected this)");
            }
            const Constant& constant =
                (*ctx.active_constant_arena)[const_data->constant];
            return mir::Projection{
                .info =
                    mir::IndexProjection{
                        .index = mir::Operand::Const(constant)},
                .origin = origin,
            };
          } else {
            throw common::InternalError(
                "LowerHirCaptureTargetToPlace",
                "unexpected projection kind in deferred capture target");
          }
        },
        proj);
    place = ctx.DerivePlace(place, std::move(mir_proj));
  }
  return place;
}

// Lower one HIR capture to a MIR actual + optional snapshot plan.
auto LowerDeferredCaptureToMirActual(
    const hir::DeferredCapture& capture, Context& ctx, MirBuilder& builder,
    std::vector<mir::DeferredAssertionActual>& actuals,
    std::vector<DeferredCapturedValuePlan>& snapshot_captures) -> Result<void> {
  return std::visit(
      [&](const auto& cap) -> Result<void> {
        using T = std::decay_t<decltype(cap)>;
        if constexpr (std::is_same_v<T, hir::ValueCapture>) {
          actuals.push_back(
              mir::DeferredAssertionActual{
                  .kind = mir::DeferredActualKind::kSnapshotValue,
                  .type = cap.type,
              });
          snapshot_captures.push_back(
              DeferredCapturedValuePlan{
                  .expr_id = cap.expr,
                  .captured_type = cap.type,
              });
          return {};
        } else if constexpr (std::is_same_v<T, hir::RefCapture>) {
          auto place = LowerHirCaptureTargetToPlace(cap.target, ctx, builder);
          if (!place) return std::unexpected(place.error());
          actuals.push_back(
              mir::DeferredAssertionActual{
                  .kind = mir::DeferredActualKind::kLiveRef,
                  .type = cap.type,
                  .ref_place = *place,
              });
          return {};
        } else if constexpr (std::is_same_v<T, hir::ConstRefCapture>) {
          auto place = LowerHirCaptureTargetToPlace(cap.target, ctx, builder);
          if (!place) return std::unexpected(place.error());
          actuals.push_back(
              mir::DeferredAssertionActual{
                  .kind = mir::DeferredActualKind::kConstLiveRef,
                  .type = cap.type,
                  .ref_place = *place,
              });
          return {};
        }
      },
      capture);
}

// Lower an HIR DeferredAction to MIR DeferredUserCallAction.
// Resolves callee SymbolId -> FunctionId, converts captures to MIR actuals,
// and resolves ref capture targets to PlaceIds via
// LowerHirCaptureTargetToPlace.
auto LowerDeferredActionToMir(
    const hir::DeferredAction& action, SourceSpan span, MirBuilder& builder)
    -> Result<DeferredAssertionActionBuildResult> {
  Context& ctx = builder.GetContext();

  mir::FunctionId callee = ctx.ResolveCallee(action.callee_symbol);
  if (callee == mir::kInvalidFunctionId) {
    return std::unexpected(
        Diagnostic::Unsupported(
            span, "deferred assertion callee could not be resolved",
            UnsupportedCategory::kFeature));
  }

  std::vector<mir::DeferredAssertionActual> actuals;
  std::vector<DeferredCapturedValuePlan> snapshot_captures;

  for (const auto& capture : action.captures) {
    auto result = LowerDeferredCaptureToMirActual(
        capture, ctx, builder, actuals, snapshot_captures);
    if (!result) return std::unexpected(result.error());
  }

  // Capture callee ABI metadata now while the body arena is still active.
  // Thunk compilation runs after the body arena scope has ended, so it
  // cannot look up the MIR function from the ambient context arena.
  const auto& callee_func = (*ctx.mir_arena)[callee];
  bool accepts_decision_owner = callee_func.abi_contract.accepts_decision_owner;

  return DeferredAssertionActionBuildResult{
      .action =
          mir::DeferredUserCallAction{
              .callee = callee,
              .actuals = std::move(actuals),
              .accepts_decision_owner = accepts_decision_owner,
          },
      .snapshot_captures = std::move(snapshot_captures),
  };
}

// Lower a DeferredAssertionStatementData (observed deferred assertion).
// Consumes the inline-owned DeferredAction directly from the HIR node.
// Does not reconstruct semantics from pass_action/fail_action child
// statements -- those are source-shaped children kept for fidelity only.
auto LowerDeferredAssertion(
    const hir::DeferredAssertionStatementData& data, SourceSpan span,
    MirBuilder& builder) -> Result<void> {
  auto origin = builder.GetCurrentOrigin();
  bool is_cover = data.kind == hir::ImmediateAssertionKind::kCover;

  std::optional<DeferredAssertionActionBuildResult> pass_result;
  std::optional<DeferredAssertionActionBuildResult> fail_result;

  if (data.deferred_pass_action.has_value()) {
    auto result =
        LowerDeferredActionToMir(*data.deferred_pass_action, span, builder);
    if (!result) return std::unexpected(result.error());
    pass_result = std::move(*result);
  }

  if (data.deferred_fail_action.has_value()) {
    auto result =
        LowerDeferredActionToMir(*data.deferred_fail_action, span, builder);
    if (!result) return std::unexpected(result.error());
    fail_result = std::move(*result);
  }

  mir::DeferredAssertionSiteInfo site_info{
      .span = span,
      .origin = origin,
      .kind = MapDeferredAssertionKind(data.kind),
      .has_default_fail_report = data.has_default_fail_report,
      .fail_action = std::nullopt,
      .pass_action = std::nullopt,
  };

  if (is_cover) {
    if (pass_result.has_value()) {
      site_info.pass_action = std::move(pass_result->action);
    } else {
      auto cover_site_id = builder.GetContext().AllocateCoverSite(span);
      site_info.pass_action =
          mir::DeferredCoverHitAction{.cover_site_id = cover_site_id};
    }
  } else {
    if (fail_result.has_value()) {
      site_info.fail_action = std::move(fail_result->action);
    }
    if (pass_result.has_value()) {
      site_info.pass_action = std::move(pass_result->action);
    }
  }

  // Site allocation stores only static deferred-action metadata (callee,
  // ref-place bindings, assertion kind, origin). Snapshot values are
  // encounter-time only: they are evaluated in the decision-arm callbacks
  // below and carried exclusively by EnqueueDeferredAssertionEffect,
  // never pre-stored in the site.
  auto site_id =
      builder.GetContext().AllocateDeferredAssertionSite(std::move(site_info));

  auto emit_enqueue =
      [](Result<void>& cb_result, MirBuilder& b,
         mir::DeferredAssertionSiteId sid,
         mir::DeferredAssertionDisposition disposition,
         const std::vector<DeferredCapturedValuePlan>* captures) {
        std::vector<mir::Operand> snapshot_values;
        if (captures != nullptr) {
          for (const auto& cap : *captures) {
            auto val =
                LowerDeferredCapturedValue(cap.expr_id, cap.captured_type, b);
            if (!val) {
              cb_result = std::unexpected(val.error());
              return;
            }
            snapshot_values.push_back(*val);
          }
        }
        b.EmitEffect(
            mir::EnqueueDeferredAssertionEffect{
                .site_id = sid,
                .disposition = disposition,
                .snapshot_values = std::move(snapshot_values),
            });
      };

  Result<void> callback_result;

  std::vector<MirBuilder::DecisionArmBuilder> arms;
  arms.push_back(
      MirBuilder::DecisionArmBuilder{
          .build_condition = [&callback_result, cond_id = data.condition](
                                 MirBuilder& b) -> mir::Operand {
            if (!callback_result) {
              return mir::Operand::Use(mir::kInvalidPlaceId);
            }
            auto cond = MaterializeCondition(cond_id, b);
            if (!cond) {
              callback_result = std::unexpected(cond.error());
              return mir::Operand::Use(mir::kInvalidPlaceId);
            }
            return *cond;
          },
          .build_body =
              [&callback_result, &emit_enqueue, is_cover, site_id,
               &pass_result](MirBuilder& b) {
                if (!callback_result) return;
                if (is_cover) {
                  if (pass_result.has_value()) {
                    emit_enqueue(
                        callback_result, b, site_id,
                        mir::DeferredAssertionDisposition::kPassAction,
                        &pass_result->snapshot_captures);
                  } else {
                    emit_enqueue(
                        callback_result, b, site_id,
                        mir::DeferredAssertionDisposition::kCoverHit, nullptr);
                  }
                } else if (pass_result.has_value()) {
                  emit_enqueue(
                      callback_result, b, site_id,
                      mir::DeferredAssertionDisposition::kPassAction,
                      &pass_result->snapshot_captures);
                }
              },
      });

  auto fallback_body =
      is_cover
          ? std::nullopt
          : std::optional<std::function<void(MirBuilder&)>>(
                [&callback_result, &emit_enqueue, site_id, &fail_result,
                 has_default = data.has_default_fail_report](MirBuilder& b) {
                  if (!callback_result) return;
                  if (fail_result.has_value()) {
                    emit_enqueue(
                        callback_result, b, site_id,
                        mir::DeferredAssertionDisposition::kFailAction,
                        &fail_result->snapshot_captures);
                  } else if (has_default) {
                    emit_enqueue(
                        callback_result, b, site_id,
                        mir::DeferredAssertionDisposition::kDefaultFailReport,
                        nullptr);
                  }
                });

  MirBuilder::DecisionBuildSpec spec{
      .qualifier = semantic::DecisionQualifier::kNone,
      .id = std::nullopt,
      .arms = std::move(arms),
      .build_fallback_body = std::move(fallback_body),
  };

  builder.EmitDecision(spec);
  return callback_result;
}

// Dispatch immediate assertion lowering by kind.
// Only handles simple (non-deferred) assertions. Deferred assertions
// are routed through the DeferredAssertionStatementData visitor arm.
auto LowerImmediateAssertion(
    const hir::ImmediateAssertionStatementData& data, SourceSpan span,
    MirBuilder& builder) -> Result<void> {
  switch (data.kind) {
    case hir::ImmediateAssertionKind::kAssert:
    case hir::ImmediateAssertionKind::kAssume:
      return LowerImmediateAssertLike(
          data, builder.GetCurrentOrigin(), builder);
    case hir::ImmediateAssertionKind::kCover:
      return LowerImmediateCover(data, span, builder);
  }
}

// Unified case lowering for all qualifiers.
auto LowerCase(const hir::CaseStatementData& data, MirBuilder& builder)
    -> Result<void> {
  auto qualifier = MapQualifier(data.check);
  bool has_fallback = data.default_statement.has_value();

  // Capture decision origin before any nested lowering mutates it.
  const common::OriginId decision_origin = builder.GetCurrentOrigin();

  // Allocate decision site for qualified decisions
  std::optional<semantic::DecisionId> decision_id;
  if (qualifier != semantic::DecisionQualifier::kNone) {
    decision_id = builder.AllocateDecisionSite(
        qualifier, semantic::DecisionKind::kCase, has_fallback,
        semantic::DecisionArmCount{static_cast<uint16_t>(data.items.size())},
        decision_origin);
  }

  // Build arm builders
  Result<void> callback_result;
  std::vector<MirBuilder::DecisionArmBuilder> arms;
  for (const auto& item : data.items) {
    arms.push_back(
        MirBuilder::DecisionArmBuilder{
            .build_condition = [&callback_result, pred_id = item.predicate](
                                   MirBuilder& b) -> mir::Operand {
              Result<mir::Operand> pred_result = LowerExpression(pred_id, b);
              if (!pred_result) {
                callback_result = std::unexpected(pred_result.error());
                return mir::Operand::Use(mir::kInvalidPlaceId);
              }
              return *pred_result;
            },
            .build_body =
                [&callback_result, stmt_id = item.statement](MirBuilder& b) {
                  if (callback_result && stmt_id.has_value()) {
                    callback_result = LowerStatement(*stmt_id, b);
                  }
                },
        });
  }

  MirBuilder::DecisionBuildSpec spec{
      .qualifier = qualifier,
      .id = decision_id,
      .arms = std::move(arms),
      .build_fallback_body =
          has_fallback
              ? std::optional<std::function<void(MirBuilder&)>>(
                    [&callback_result,
                     default_id = *data.default_statement](MirBuilder& b) {
                      if (callback_result) {
                        callback_result = LowerStatement(default_id, b);
                      }
                    })
              : std::nullopt,
  };
  builder.EmitDecision(spec);
  return callback_result;
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
      builder.EmitPlainAssign(temp, std::move(cond));
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
    builder.EmitPlainAssign(temp, std::move(cond));
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
    builder.EmitPlainAssign(temp, std::move(cond));
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

// Recursively walk an HIR index expression and build IndexPlanOp bytecode.
// Returns true if all leaves are constants or design-state slots.
// Returns false if a local variable is encountered in a compound expression.
// Helper to convert PlaceRoot::Kind to ScopedSlotRef::Scope.
auto PlaceRootScope(mir::PlaceRoot::Kind kind) -> mir::ScopedSlotRef::Scope {
  return (kind == mir::PlaceRoot::Kind::kModuleSlot)
             ? mir::ScopedSlotRef::Scope::kModuleLocal
             : mir::ScopedSlotRef::Scope::kDesignGlobal;
}

auto BuildIndexPlan(
    hir::ExpressionId expr_id, Context& ctx,
    std::vector<mir::ScopedPlanOp>& plan, std::vector<mir::ScopedSlotRef>& deps,
    std::vector<mir::PendingIndexPlanExternalRef>& pending_ext_refs) -> bool {
  const auto& expr = (*ctx.hir_arena)[expr_id];

  switch (expr.kind) {
    case hir::ExpressionKind::kConstant: {
      const auto& cdata = std::get<hir::ConstantExpressionData>(expr.data);
      const Constant& constant = (*ctx.active_constant_arena)[cdata.constant];
      const auto& ic = std::get<IntegralConstant>(constant.value);
      int64_t val = 0;
      if (!ic.value.empty()) {
        val = static_cast<int64_t>(ic.value[0]);
      }
      plan.push_back({.op = runtime::IndexPlanOp::MakeConst(val)});
      return true;
    }
    case hir::ExpressionKind::kNameRef: {
      const auto& name_data = std::get<hir::NameRefExpressionData>(expr.data);
      auto place_id = ctx.LookupPlace(name_data.symbol);
      const auto& place = (*ctx.mir_arena)[place_id];
      if (place.root.kind == mir::PlaceRoot::Kind::kModuleSlot ||
          place.root.kind == mir::PlaceRoot::Kind::kDesignGlobal) {
        auto slot_id = static_cast<uint32_t>(place.root.id);
        auto scope = PlaceRootScope(place.root.kind);
        const Type& idx_type = (*ctx.type_arena)[expr.type];
        uint32_t bit_width = PackedBitWidth(idx_type, *ctx.type_arena);
        bool is_signed = IsPackedSigned(idx_type, *ctx.type_arena);
        uint32_t byte_size = (bit_width + 7) / 8;
        plan.push_back(
            {.op = runtime::IndexPlanOp::MakeReadSlot(
                 slot_id, 0, static_cast<uint8_t>(byte_size),
                 static_cast<uint8_t>(bit_width), is_signed),
             .slot_scope = scope});
        deps.push_back({.scope = scope, .id = slot_id});
        return true;
      }
      // Local variable -- not supported in compound expressions.
      return false;
    }
    case hir::ExpressionKind::kHierarchicalRef: {
      const auto& href_data =
          std::get<hir::HierarchicalRefExpressionData>(expr.data);
      if (ctx.external_refs == nullptr) {
        throw common::InternalError(
            "BuildIndexPlan/HierarchicalRef",
            "hierarchical index-plan requires external-ref context");
      }
      auto ref = ctx.LowerHierarchicalRefToExternalRef(
          href_data, expr.type, mir::ExternalAccessKind::kRead);
      const Type& idx_type = (*ctx.type_arena)[expr.type];
      uint32_t bit_width = PackedBitWidth(idx_type, *ctx.type_arena);
      bool is_signed = IsPackedSigned(idx_type, *ctx.type_arena);
      uint32_t byte_size = (bit_width + 7) / 8;
      auto op_idx = static_cast<uint32_t>(plan.size());
      plan.push_back(
          {.op = runtime::IndexPlanOp::MakeReadSlot(
               0, 0, static_cast<uint8_t>(byte_size),
               static_cast<uint8_t>(bit_width), is_signed),
           .slot_scope = mir::ScopedSlotRef::Scope::kDesignGlobal});
      auto dep_idx = static_cast<uint32_t>(deps.size());
      deps.push_back(
          {.scope = mir::ScopedSlotRef::Scope::kDesignGlobal, .id = 0});
      pending_ext_refs.push_back(
          {.ref_id = ref.ref_id, .op_index = op_idx, .dep_index = dep_idx});
      return true;
    }
    case hir::ExpressionKind::kBinaryOp: {
      const auto& bin_data = std::get<hir::BinaryExpressionData>(expr.data);
      if (!BuildIndexPlan(bin_data.lhs, ctx, plan, deps, pending_ext_refs))
        return false;
      if (!BuildIndexPlan(bin_data.rhs, ctx, plan, deps, pending_ext_refs))
        return false;
      auto op_kind = runtime::IndexPlanOp::Kind::kConst;
      switch (bin_data.op) {
        case hir::BinaryOp::kAdd:
          op_kind = runtime::IndexPlanOp::Kind::kAdd;
          break;
        case hir::BinaryOp::kSubtract:
          op_kind = runtime::IndexPlanOp::Kind::kSub;
          break;
        case hir::BinaryOp::kMultiply:
          op_kind = runtime::IndexPlanOp::Kind::kMul;
          break;
        case hir::BinaryOp::kBitwiseAnd:
          op_kind = runtime::IndexPlanOp::Kind::kAnd;
          break;
        case hir::BinaryOp::kBitwiseOr:
          op_kind = runtime::IndexPlanOp::Kind::kOr;
          break;
        case hir::BinaryOp::kBitwiseXor:
          op_kind = runtime::IndexPlanOp::Kind::kXor;
          break;
        case hir::BinaryOp::kLogicalShiftLeft:
        case hir::BinaryOp::kArithmeticShiftLeft:
          op_kind = runtime::IndexPlanOp::Kind::kShl;
          break;
        case hir::BinaryOp::kLogicalShiftRight:
          op_kind = runtime::IndexPlanOp::Kind::kLShr;
          break;
        case hir::BinaryOp::kArithmeticShiftRight:
          op_kind = runtime::IndexPlanOp::Kind::kAShr;
          break;
        case hir::BinaryOp::kDivide: {
          const Type& result_type = (*ctx.type_arena)[expr.type];
          bool is_signed = IsPackedSigned(result_type, *ctx.type_arena);
          op_kind = is_signed ? runtime::IndexPlanOp::Kind::kDivS
                              : runtime::IndexPlanOp::Kind::kDivU;
          break;
        }
        case hir::BinaryOp::kMod: {
          const Type& result_type = (*ctx.type_arena)[expr.type];
          bool is_signed = IsPackedSigned(result_type, *ctx.type_arena);
          op_kind = is_signed ? runtime::IndexPlanOp::Kind::kModS
                              : runtime::IndexPlanOp::Kind::kModU;
          break;
        }
        default:
          return false;
      }
      plan.push_back({.op = runtime::IndexPlanOp::MakeBinaryOp(op_kind)});
      return true;
    }
    case hir::ExpressionKind::kUnaryOp: {
      const auto& unary_data = std::get<hir::UnaryExpressionData>(expr.data);
      if (unary_data.op == hir::UnaryOp::kMinus) {
        plan.push_back({.op = runtime::IndexPlanOp::MakeConst(0)});
        if (!BuildIndexPlan(
                unary_data.operand, ctx, plan, deps, pending_ext_refs))
          return false;
        plan.push_back(
            {.op = runtime::IndexPlanOp::MakeBinaryOp(
                 runtime::IndexPlanOp::Kind::kSub)});
        return true;
      }
      if (unary_data.op == hir::UnaryOp::kBitwiseNot) {
        if (!BuildIndexPlan(
                unary_data.operand, ctx, plan, deps, pending_ext_refs))
          return false;
        plan.push_back({.op = runtime::IndexPlanOp::MakeConst(-1)});
        plan.push_back(
            {.op = runtime::IndexPlanOp::MakeBinaryOp(
                 runtime::IndexPlanOp::Kind::kXor)});
        return true;
      }
      return false;
    }
    case hir::ExpressionKind::kCast: {
      const auto& cast_data = std::get<hir::CastExpressionData>(expr.data);
      return BuildIndexPlan(
          cast_data.operand, ctx, plan, deps, pending_ext_refs);
    }
    default:
      return false;
  }
}

// Typed trigger-source result: either a local design place or an unresolved
// external ref handle. HIR syntax discrimination is isolated here so that
// LowerEventWait consumes a typed result rather than branching on expression
// kind inline.
using TriggerSource = std::variant<mir::PlaceId, mir::ExternalRefId>;

auto LowerEventTriggerSource(hir::ExpressionId expr_id, MirBuilder& builder)
    -> Result<TriggerSource> {
  auto& ctx = builder.GetContext();
  const auto& expr = (*ctx.hir_arena)[expr_id];
  if (expr.kind == hir::ExpressionKind::kHierarchicalRef &&
      ctx.external_refs != nullptr) {
    const auto& href_data =
        std::get<hir::HierarchicalRefExpressionData>(expr.data);
    auto ref = ctx.LowerHierarchicalRefToExternalRef(
        href_data, expr.type, mir::ExternalAccessKind::kRead);
    return TriggerSource{ref.ref_id};
  }
  auto op_result = LowerExpression(expr_id, builder);
  if (!op_result) return std::unexpected(op_result.error());
  if (op_result->kind != mir::Operand::Kind::kUse) {
    throw common::InternalError(
        "LowerEventTriggerSource",
        "event trigger signal must be a design variable");
  }
  return TriggerSource{std::get<mir::PlaceId>(op_result->payload)};
}

auto LowerEventWait(
    const hir::EventWaitStatementData& data, MirBuilder& builder)
    -> Result<void> {
  Context& ctx = builder.GetContext();
  BlockIndex resume_bb = builder.CreateBlock();

  std::vector<mir::WaitTrigger> triggers;
  for (const auto& hir_trigger : data.triggers) {
    const auto& hir_expr = (*ctx.hir_arena)[hir_trigger.signal];
    mir::PlaceId place_id;
    std::optional<mir::LateBoundIndex> late_bound_info;

    if (hir_expr.kind == hir::ExpressionKind::kPackedElementSelect ||
        hir_expr.kind == hir::ExpressionKind::kBitSelect) {
      // Packed element/bit-select: lower base to get design place,
      // then add BitRangeProjection for the storage bit offset.
      // For multi-bit elements, edge triggers observe the LSB (width=1);
      // level-sensitive triggers observe the full element.
      hir::ExpressionId base_id;
      hir::ExpressionId index_id;
      if (hir_expr.kind == hir::ExpressionKind::kPackedElementSelect) {
        const auto& sel_data =
            std::get<hir::PackedElementSelectExpressionData>(hir_expr.data);
        base_id = sel_data.base;
        index_id = sel_data.index;
      } else {
        const auto& sel_data =
            std::get<hir::BitSelectExpressionData>(hir_expr.data);
        base_id = sel_data.base;
        index_id = sel_data.index;
      }

      auto base_result = LowerExpression(base_id, builder);
      if (!base_result) return std::unexpected(base_result.error());
      mir::Operand base_op = std::move(*base_result);
      if (base_op.kind != mir::Operand::Kind::kUse) {
        throw common::InternalError(
            "LowerEventWait", "bit-select base must be a design variable");
      }
      auto base_place_id = std::get<mir::PlaceId>(base_op.payload);

      auto index_result = LowerExpression(index_id, builder);
      if (!index_result) return std::unexpected(index_result.error());
      mir::Operand index_op = std::move(*index_result);

      // Normalize source-level index to storage element offset.
      // Packed arrays have declared ranges (e.g. [15:8]); the storage
      // offset is relative to lower bound for descending ranges.
      // Integrals/packed structs use implicit [width-1:0], so offset=index.
      const auto& base_expr = (*ctx.hir_arena)[base_id];
      const Type& base_type = (*ctx.type_arena)[base_expr.type];
      mir::Operand storage_offset = index_op;
      if (base_type.Kind() == TypeKind::kPackedArray) {
        const auto& range = base_type.AsPackedArray().range;
        TypeId offset_type = ctx.GetOffsetType();
        if (range.IsDescending()) {
          if (range.Lower() != 0) {
            auto lower_const = mir::Operand::Const(MakeIntConstant(
                static_cast<uint64_t>(range.Lower()), offset_type));
            storage_offset = builder.EmitBinary(
                mir::BinaryOp::kSubtract, index_op, lower_const, offset_type);
          }
        } else {
          auto upper_const = mir::Operand::Const(MakeIntConstant(
              static_cast<uint64_t>(range.Upper()), offset_type));
          storage_offset = builder.EmitBinary(
              mir::BinaryOp::kSubtract, upper_const, index_op, offset_type);
        }
      }

      // For multi-bit elements, convert element index to bit offset.
      uint32_t element_bit_width = 1;
      if (hir_expr.kind == hir::ExpressionKind::kPackedElementSelect) {
        element_bit_width =
            PackedBitWidth((*ctx.type_arena)[hir_expr.type], *ctx.type_arena);
      }
      if (element_bit_width > 1) {
        TypeId offset_type = ctx.GetOffsetType();
        auto width_const = mir::Operand::Const(MakeIntConstant(
            static_cast<uint64_t>(element_bit_width), offset_type));
        storage_offset = builder.EmitBinary(
            mir::BinaryOp::kMultiply, storage_offset, width_const, offset_type);
      }

      uint32_t proj_width = (hir_trigger.edge != hir::EventEdgeKind::kNone)
                                ? 1
                                : element_bit_width;

      // Check if index is dynamic (not a constant) for late-bound edge
      // triggers. The HIR index expression determines whether we need
      // rebinding.
      const auto& index_expr = (*ctx.hir_arena)[index_id];
      bool is_dynamic_index =
          (index_expr.kind != hir::ExpressionKind::kConstant);

      if (is_dynamic_index && hir_trigger.edge != hir::EventEdgeKind::kNone) {
        // Compute BitTargetMapping from the base array's declared range.
        runtime::BitTargetMapping mapping;
        uint32_t total_bits = PackedBitWidth(base_type, *ctx.type_arena);

        if (base_type.Kind() == TypeKind::kPackedArray) {
          const auto& range = base_type.AsPackedArray().range;
          if (range.IsDescending()) {
            mapping.index_base = range.Lower();
            mapping.index_step = 1;
          } else {
            mapping.index_base = range.Upper();
            mapping.index_step = -1;
          }
        } else {
          mapping.index_base = 0;
          mapping.index_step = 1;
        }
        mapping.total_bits = total_bits;

        // Build index plan from the HIR index expression.
        std::vector<mir::ScopedPlanOp> plan;
        std::vector<mir::ScopedSlotRef> deps;
        std::vector<mir::PendingIndexPlanExternalRef> pending_ext;
        bool plan_ok = BuildIndexPlan(index_id, ctx, plan, deps, pending_ext);

        if (!plan_ok && !deps.empty()) {
          throw common::InternalError(
              "LowerEventWait",
              "compound index mixes local and design-state variables");
        }

        if (plan.size() > runtime::kMaxPlanOps) {
          throw common::InternalError(
              "LowerEventWait",
              std::format(
                  "index expression too complex for edge trigger "
                  "({} ops, max {})",
                  plan.size(), runtime::kMaxPlanOps));
        }

        if (plan_ok && !deps.empty()) {
          // Deduplicate dep_slots.
          std::ranges::sort(deps);
          auto [first, last] = std::ranges::unique(deps);
          deps.erase(first, last);

          late_bound_info = mir::LateBoundIndex{
              .plan = std::move(plan),
              .dep_slots = std::move(deps),
              .mapping = mapping,
              .element_type = {},
              .pending_external_refs = std::move(pending_ext)};
        } else {
          // Local-only index: no rebinding needed.
          // dep_slots empty signals codegen to use dynamic path but skip
          // runtime rebind subscriptions.
          late_bound_info = mir::LateBoundIndex{
              .plan = {},
              .dep_slots = {},
              .mapping = mapping,
              .element_type = {},
              .pending_external_refs = {}};
        }
      }

      uint32_t alignment =
          (hir_expr.kind == hir::ExpressionKind::kPackedElementSelect &&
           element_bit_width > 1)
              ? PowerOfTwoAlignment(element_bit_width)
              : 1;
      place_id = ctx.DerivePlace(
          base_place_id, mir::Projection{
                             .info = mir::BitRangeProjection{
                                 .bit_offset = storage_offset,
                                 .width = proj_width,
                                 .element_type = hir_expr.type,
                                 .guaranteed_alignment_bits = alignment}});
    } else if (hir_expr.kind == hir::ExpressionKind::kPackedFieldAccess) {
      // Packed struct field: constant bit offset from field.
      // For multi-bit fields, edge triggers observe the LSB (width=1);
      // level-sensitive triggers observe the full field.
      const auto& data =
          std::get<hir::PackedFieldAccessExpressionData>(hir_expr.data);

      auto base_result = LowerExpression(data.base, builder);
      if (!base_result) return std::unexpected(base_result.error());
      mir::Operand base_op = std::move(*base_result);
      if (base_op.kind != mir::Operand::Kind::kUse) {
        throw common::InternalError(
            "LowerEventWait",
            "packed field access base must be a design variable");
      }
      auto base_place_id = std::get<mir::PlaceId>(base_op.payload);

      TypeId offset_type = ctx.GetOffsetType();
      mir::Operand storage_offset = mir::Operand::Const(
          MakeIntConstant(static_cast<uint64_t>(data.bit_offset), offset_type));

      uint32_t field_bit_width =
          PackedBitWidth((*ctx.type_arena)[hir_expr.type], *ctx.type_arena);
      uint32_t proj_width =
          (hir_trigger.edge != hir::EventEdgeKind::kNone) ? 1 : field_bit_width;

      place_id = ctx.DerivePlace(
          base_place_id, mir::Projection{
                             .info = mir::BitRangeProjection{
                                 .bit_offset = storage_offset,
                                 .width = proj_width,
                                 .element_type = hir_expr.type}});
    } else if (hir_expr.kind == hir::ExpressionKind::kRangeSelect) {
      // Constant range select: compute LSB storage offset.
      // For multi-bit ranges, edge triggers observe the LSB (width=1);
      // level-sensitive triggers observe the full range.
      const auto& data =
          std::get<hir::RangeSelectExpressionData>(hir_expr.data);

      auto base_result = LowerExpression(data.base, builder);
      if (!base_result) return std::unexpected(base_result.error());
      mir::Operand base_op = std::move(*base_result);
      if (base_op.kind != mir::Operand::Kind::kUse) {
        throw common::InternalError(
            "LowerEventWait", "range select base must be a design variable");
      }
      auto base_place_id = std::get<mir::PlaceId>(base_op.payload);

      int32_t select_lower = std::min(data.left, data.right);
      int32_t select_upper = std::max(data.left, data.right);
      auto select_width =
          static_cast<uint32_t>(select_upper - select_lower + 1);

      const auto& base_expr = (*ctx.hir_arena)[data.base];
      const Type& base_type = (*ctx.type_arena)[base_expr.type];
      int32_t bit_offset = 0;
      switch (base_type.Kind()) {
        case TypeKind::kIntegral:
        case TypeKind::kPackedStruct:
          bit_offset = select_lower;
          break;
        case TypeKind::kPackedArray: {
          const auto& packed = base_type.AsPackedArray();
          if (packed.range.IsDescending()) {
            bit_offset = select_lower - packed.range.Lower();
          } else {
            bit_offset = packed.range.Upper() - select_upper;
          }
          break;
        }
        default:
          throw common::InternalError(
              "LowerEventWait", "range select base must be packed type");
      }

      TypeId offset_type = ctx.GetOffsetType();
      mir::Operand storage_offset = mir::Operand::Const(
          MakeIntConstant(static_cast<uint64_t>(bit_offset), offset_type));

      uint32_t proj_width =
          (hir_trigger.edge != hir::EventEdgeKind::kNone) ? 1 : select_width;

      place_id = ctx.DerivePlace(
          base_place_id, mir::Projection{
                             .info = mir::BitRangeProjection{
                                 .bit_offset = storage_offset,
                                 .width = proj_width,
                                 .element_type = hir_expr.type}});
    } else if (hir_expr.kind == hir::ExpressionKind::kIndexedPartSelect) {
      // Indexed part-select: compute LSB storage offset.
      // For +: the index IS the LSB; for -: the LSB is index-(width-1).
      // Edge triggers observe the LSB (width=1); level-sensitive observes
      // the full part-select range.
      const auto& ips_data =
          std::get<hir::IndexedPartSelectExpressionData>(hir_expr.data);

      auto base_result = LowerExpression(ips_data.base, builder);
      if (!base_result) return std::unexpected(base_result.error());
      mir::Operand base_op = std::move(*base_result);
      if (base_op.kind != mir::Operand::Kind::kUse) {
        throw common::InternalError(
            "LowerEventWait",
            "indexed part-select base must be a design variable");
      }
      auto base_place_id = std::get<mir::PlaceId>(base_op.payload);

      auto index_result = LowerExpression(ips_data.index, builder);
      if (!index_result) return std::unexpected(index_result.error());
      mir::Operand index_op = std::move(*index_result);

      // For descending part-select (-:), compute LSB = index - (width - 1).
      mir::Operand lsb_index = index_op;
      if (!ips_data.ascending && ips_data.width > 1) {
        TypeId offset_type = ctx.GetOffsetType();
        auto width_minus_one = mir::Operand::Const(MakeIntConstant(
            static_cast<uint64_t>(ips_data.width - 1), offset_type));
        lsb_index = builder.EmitBinary(
            mir::BinaryOp::kSubtract, index_op, width_minus_one, offset_type);
      }

      // Normalize LSB index to storage offset.
      const auto& base_expr = (*ctx.hir_arena)[ips_data.base];
      const Type& base_type = (*ctx.type_arena)[base_expr.type];
      mir::Operand storage_offset = lsb_index;
      if (base_type.Kind() == TypeKind::kPackedArray) {
        const auto& range = base_type.AsPackedArray().range;
        TypeId offset_type = ctx.GetOffsetType();
        if (range.IsDescending()) {
          if (range.Lower() != 0) {
            auto lower_const = mir::Operand::Const(MakeIntConstant(
                static_cast<uint64_t>(range.Lower()), offset_type));
            storage_offset = builder.EmitBinary(
                mir::BinaryOp::kSubtract, lsb_index, lower_const, offset_type);
          }
        } else {
          auto upper_const = mir::Operand::Const(MakeIntConstant(
              static_cast<uint64_t>(range.Upper()), offset_type));
          storage_offset = builder.EmitBinary(
              mir::BinaryOp::kSubtract, upper_const, lsb_index, offset_type);
        }
      }

      // Check if index is dynamic for late-bound edge triggers.
      const auto& ips_index_expr = (*ctx.hir_arena)[ips_data.index];
      bool ips_is_dynamic =
          (ips_index_expr.kind != hir::ExpressionKind::kConstant);

      if (ips_is_dynamic && hir_trigger.edge != hir::EventEdgeKind::kNone) {
        runtime::BitTargetMapping mapping;
        uint32_t total_bits = PackedBitWidth(base_type, *ctx.type_arena);

        if (base_type.Kind() == TypeKind::kPackedArray) {
          const auto& range = base_type.AsPackedArray().range;
          if (range.IsDescending()) {
            mapping.index_base = range.Lower();
            mapping.index_step = 1;
          } else {
            mapping.index_base = range.Upper();
            mapping.index_step = -1;
          }
        } else {
          mapping.index_base = 0;
          mapping.index_step = 1;
        }
        mapping.total_bits = total_bits;

        // Build index plan. For ascending (+:), plan evaluates to the SV index
        // (same as bus[i]). For descending (-:), plan evaluates to i-(w-1).
        std::vector<mir::ScopedPlanOp> plan;
        std::vector<mir::ScopedSlotRef> deps;
        std::vector<mir::PendingIndexPlanExternalRef> pending_ext;
        bool plan_ok =
            BuildIndexPlan(ips_data.index, ctx, plan, deps, pending_ext);

        if (plan_ok && !ips_data.ascending && ips_data.width > 1) {
          // Append: MakeConst(width-1), MakeBinaryOp(kSub)
          plan.push_back(
              {.op = runtime::IndexPlanOp::MakeConst(
                   static_cast<int64_t>(ips_data.width - 1))});
          plan.push_back(
              {.op = runtime::IndexPlanOp::MakeBinaryOp(
                   runtime::IndexPlanOp::Kind::kSub)});
        }

        if (!plan_ok && !deps.empty()) {
          throw common::InternalError(
              "LowerEventWait",
              "compound index mixes local and design-state variables");
        }

        if (plan.size() > runtime::kMaxPlanOps) {
          throw common::InternalError(
              "LowerEventWait",
              std::format(
                  "index expression too complex for edge trigger "
                  "({} ops, max {})",
                  plan.size(), runtime::kMaxPlanOps));
        }

        if (plan_ok && !deps.empty()) {
          std::ranges::sort(deps);
          auto [first, last] = std::ranges::unique(deps);
          deps.erase(first, last);

          late_bound_info = mir::LateBoundIndex{
              .plan = std::move(plan),
              .dep_slots = std::move(deps),
              .mapping = mapping,
              .element_type = {},
              .pending_external_refs = std::move(pending_ext)};
        } else {
          late_bound_info = mir::LateBoundIndex{
              .plan = {},
              .dep_slots = {},
              .mapping = mapping,
              .element_type = {},
              .pending_external_refs = {}};
        }
      }

      uint32_t proj_width =
          (hir_trigger.edge != hir::EventEdgeKind::kNone) ? 1 : ips_data.width;

      place_id = ctx.DerivePlace(
          base_place_id, mir::Projection{
                             .info = mir::BitRangeProjection{
                                 .bit_offset = storage_offset,
                                 .width = proj_width,
                                 .element_type = hir_expr.type}});
    } else if (
        hir_trigger.edge != hir::EventEdgeKind::kNone &&
        hir_expr.kind == hir::ExpressionKind::kElementAccess) {
      // Unpacked array element edge trigger.
      // For dynamic index: build late-bound index plan for runtime rebinding.
      // For constant index: lower normally + BitRangeProjection for LSB.
      const auto& elem_data =
          std::get<hir::ElementAccessExpressionData>(hir_expr.data);
      const auto& index_expr = (*ctx.hir_arena)[elem_data.index];
      const auto& base_expr = (*ctx.hir_arena)[elem_data.base];
      const Type& base_type = (*ctx.type_arena)[base_expr.type];

      bool is_container = base_type.Kind() == TypeKind::kDynamicArray ||
                          base_type.Kind() == TypeKind::kQueue;
      bool is_dynamic = !is_container &&
                        (index_expr.kind != hir::ExpressionKind::kConstant) &&
                        base_type.Kind() == TypeKind::kUnpackedArray;

      if (is_container) {
        // Dynamic array/queue element edge trigger.
        // Lower base to get container slot, lower index for suspend-time
        // evaluation, build LateBoundIndex with is_container=true.
        auto base_result = LowerExpression(elem_data.base, builder);
        if (!base_result) return std::unexpected(base_result.error());
        mir::Operand base_op = std::move(*base_result);
        if (base_op.kind != mir::Operand::Kind::kUse) {
          throw common::InternalError(
              "LowerEventWait", "container base must be a design variable");
        }
        auto base_place_id = std::get<mir::PlaceId>(base_op.payload);

        auto index_result = LowerExpression(elem_data.index, builder);
        if (!index_result) return std::unexpected(index_result.error());
        mir::Operand index_op = std::move(*index_result);

        // Store index in BitRangeProjection for codegen to read.
        place_id = ctx.DerivePlace(
            base_place_id, mir::Projection{
                               .info = mir::BitRangeProjection{
                                   .bit_offset = index_op,
                                   .width = 1,
                                   .element_type = hir_expr.type}});

        TypeId elem_type_id = (base_type.Kind() == TypeKind::kDynamicArray)
                                  ? base_type.AsDynamicArray().element_type
                                  : base_type.AsQueue().element_type;

        runtime::BitTargetMapping mapping;
        mapping.index_base = 0;
        mapping.index_step = 1;
        mapping.total_bits = 0;  // Unused for containers.

        std::vector<mir::ScopedPlanOp> plan;
        std::vector<mir::ScopedSlotRef> deps;
        std::vector<mir::PendingIndexPlanExternalRef> pending_ext;
        bool plan_ok =
            BuildIndexPlan(elem_data.index, ctx, plan, deps, pending_ext);

        if (!plan_ok && !deps.empty()) {
          throw common::InternalError(
              "LowerEventWait",
              "compound index mixes local and design-state variables");
        }

        if (plan.size() > runtime::kMaxPlanOps) {
          throw common::InternalError(
              "LowerEventWait",
              std::format(
                  "index expression too complex for edge trigger "
                  "({} ops, max {})",
                  plan.size(), runtime::kMaxPlanOps));
        }

        if (plan_ok && !deps.empty()) {
          std::ranges::sort(deps);
          auto [first, last] = std::ranges::unique(deps);
          deps.erase(first, last);

          late_bound_info = mir::LateBoundIndex{
              .plan = std::move(plan),
              .dep_slots = std::move(deps),
              .mapping = mapping,
              .element_type = elem_type_id,
              .num_elements = 0,
              .is_container = true,
              .pending_external_refs = std::move(pending_ext)};
        } else {
          late_bound_info = mir::LateBoundIndex{
              .plan = {},
              .dep_slots = {},
              .mapping = mapping,
              .element_type = elem_type_id,
              .num_elements = 0,
              .is_container = true,
              .pending_external_refs = {}};
        }
      } else if (is_dynamic) {
        // Dynamic unpacked: lower base array to get root slot, lower
        // index expression for suspend-time target computation, and
        // build index plan for runtime rebinding (design-state indices).
        auto base_result = LowerExpression(elem_data.base, builder);
        if (!base_result) return std::unexpected(base_result.error());
        mir::Operand base_op = std::move(*base_result);
        if (base_op.kind != mir::Operand::Kind::kUse) {
          throw common::InternalError(
              "LowerEventWait",
              "unpacked array base must be a design variable");
        }
        auto base_place_id = std::get<mir::PlaceId>(base_op.payload);

        // Lower the index expression so codegen can compute the initial
        // target at suspend time (needed for local-variable indices
        // where no rebind subscription exists).
        auto index_result = LowerExpression(elem_data.index, builder);
        if (!index_result) return std::unexpected(index_result.error());
        mir::Operand index_op = std::move(*index_result);

        // Store the SV index in a BitRangeProjection. Codegen reads
        // this value and multiplies by element_bit_stride to compute
        // the actual byte_offset.
        place_id = ctx.DerivePlace(
            base_place_id, mir::Projection{
                               .info = mir::BitRangeProjection{
                                   .bit_offset = index_op,
                                   .width = 1,
                                   .element_type = hir_expr.type}});

        const auto& arr_info = base_type.AsUnpackedArray();
        runtime::BitTargetMapping mapping;
        mapping.index_base = arr_info.range.left;
        mapping.index_step = arr_info.range.IsDescending() ? -1 : 1;
        mapping.total_bits = 0;  // Codegen fills from DataLayout.

        std::vector<mir::ScopedPlanOp> plan;
        std::vector<mir::ScopedSlotRef> deps;
        std::vector<mir::PendingIndexPlanExternalRef> pending_ext;
        bool plan_ok =
            BuildIndexPlan(elem_data.index, ctx, plan, deps, pending_ext);

        if (!plan_ok && !deps.empty()) {
          throw common::InternalError(
              "LowerEventWait",
              "compound index mixes local and design-state variables");
        }

        if (plan.size() > runtime::kMaxPlanOps) {
          throw common::InternalError(
              "LowerEventWait",
              std::format(
                  "index expression too complex for edge trigger "
                  "({} ops, max {})",
                  plan.size(), runtime::kMaxPlanOps));
        }

        if (plan_ok && !deps.empty()) {
          std::ranges::sort(deps);
          auto [first, last] = std::ranges::unique(deps);
          deps.erase(first, last);

          late_bound_info = mir::LateBoundIndex{
              .plan = std::move(plan),
              .dep_slots = std::move(deps),
              .mapping = mapping,
              .element_type = arr_info.element_type,
              .num_elements = arr_info.range.Size(),
              .pending_external_refs = std::move(pending_ext)};
        } else {
          late_bound_info = mir::LateBoundIndex{
              .plan = {},
              .dep_slots = {},
              .mapping = mapping,
              .element_type = arr_info.element_type,
              .num_elements = arr_info.range.Size(),
              .pending_external_refs = {}};
        }
      } else {
        // Constant index or non-unpacked: lower whole expression,
        // append BitRangeProjection for LSB sampling.
        auto signal_result = LowerExpression(hir_trigger.signal, builder);
        if (!signal_result) return std::unexpected(signal_result.error());
        mir::Operand signal_op = std::move(*signal_result);
        if (signal_op.kind != mir::Operand::Kind::kUse) {
          throw common::InternalError(
              "LowerEventWait",
              "unpacked sub-expression base must be a design variable");
        }
        auto base_place_id = std::get<mir::PlaceId>(signal_op.payload);

        TypeId offset_type = ctx.GetOffsetType();
        mir::Operand storage_offset =
            mir::Operand::Const(MakeIntConstant(0, offset_type));

        place_id = ctx.DerivePlace(
            base_place_id, mir::Projection{
                               .info = mir::BitRangeProjection{
                                   .bit_offset = storage_offset,
                                   .width = 1,
                                   .element_type = hir_expr.type}});
      }
    } else if (
        hir_trigger.edge != hir::EventEdgeKind::kNone &&
        hir_expr.kind == hir::ExpressionKind::kMemberAccess) {
      // Unpacked struct field edge trigger.
      // LowerExpression emits FieldProjection to select the unpacked leaf.
      // We append BitRangeProjection{bit_offset=0, width=1} to sample
      // bit 0 (LSB) for edge detection.
      auto signal_result = LowerExpression(hir_trigger.signal, builder);
      if (!signal_result) return std::unexpected(signal_result.error());
      mir::Operand signal_op = std::move(*signal_result);
      if (signal_op.kind != mir::Operand::Kind::kUse) {
        throw common::InternalError(
            "LowerEventWait",
            "unpacked sub-expression base must be a design variable");
      }
      auto base_place_id = std::get<mir::PlaceId>(signal_op.payload);

      TypeId offset_type = ctx.GetOffsetType();
      mir::Operand storage_offset =
          mir::Operand::Const(MakeIntConstant(0, offset_type));

      place_id = ctx.DerivePlace(
          base_place_id, mir::Projection{
                             .info = mir::BitRangeProjection{
                                 .bit_offset = storage_offset,
                                 .width = 1,
                                 .element_type = hir_expr.type}});
    } else {
      auto source = LowerEventTriggerSource(hir_trigger.signal, builder);
      if (!source) return std::unexpected(source.error());
      if (const auto* ref_id = std::get_if<mir::ExternalRefId>(&*source)) {
        // External ref trigger: carry unresolved handle for backend
        // normalization (FillTriggerArray).
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
            triggers.push_back(
                {.signal = {},
                 .edge = common::EdgeKind::kPosedge,
                 .observed_place = std::nullopt,
                 .late_bound = std::nullopt,
                 .unresolved_external_ref = *ref_id});
            triggers.push_back(
                {.signal = {},
                 .edge = common::EdgeKind::kNegedge,
                 .observed_place = std::nullopt,
                 .late_bound = std::nullopt,
                 .unresolved_external_ref = *ref_id});
            continue;
        }
        triggers.push_back(
            {.signal = {},
             .edge = edge,
             .observed_place = std::nullopt,
             .late_bound = std::nullopt,
             .unresolved_external_ref = *ref_id});
        continue;
      }
      place_id = std::get<mir::PlaceId>(*source);
    }

    const auto& place = (*ctx.mir_arena)[place_id];
    mir::SignalRef signal_ref{};
    if (place.root.kind == mir::PlaceRoot::Kind::kModuleSlot) {
      signal_ref = {
          .scope = mir::SignalRef::Scope::kModuleLocal,
          .id = static_cast<uint32_t>(place.root.id)};
    } else if (place.root.kind == mir::PlaceRoot::Kind::kDesignGlobal) {
      signal_ref = {
          .scope = mir::SignalRef::Scope::kDesignGlobal,
          .id = static_cast<uint32_t>(place.root.id)};
    } else {
      throw common::InternalError(
          "LowerEventWait", "event trigger must reference a design variable");
    }

    std::optional<mir::PlaceId> obs_place;
    if (!place.projections.empty()) {
      obs_place = place_id;
    }

    if (hir_trigger.edge == hir::EventEdgeKind::kBothEdges) {
      triggers.push_back(
          {.signal = signal_ref,
           .edge = common::EdgeKind::kPosedge,
           .observed_place = obs_place,
           .late_bound = late_bound_info,
           .unresolved_external_ref = std::nullopt});
      triggers.push_back(
          {.signal = signal_ref,
           .edge = common::EdgeKind::kNegedge,
           .observed_place = obs_place,
           .late_bound = late_bound_info,
           .unresolved_external_ref = std::nullopt});
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
      triggers.push_back(
          {.signal = signal_ref,
           .edge = edge,
           .observed_place = obs_place,
           .late_bound = late_bound_info,
           .unresolved_external_ref = std::nullopt});
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
  builder.EmitPlainAssign(counter, std::move(count_val));

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
  mir::PlaceId cmp_result =
      builder.EmitPlaceTemp(cond_type, std::move(cmp_rvalue));
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
      builder.EmitPlaceTemp(count_expr.type, std::move(dec_rvalue));
  builder.EmitPlainAssign(counter, mir::Operand::Use(new_count));
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
                builder.EmitPlaceTemp(ctx.GetStringType(), std::move(rvalue));
            message = mir::Operand::Use(msg_place);
          }

          builder.EmitTerminate(
              mir::Finish{
                  .kind = mir_kind, .level = data.level, .message = message});
          // After emitting a terminator, subsequent code is unreachable (no-op)
        } else if constexpr (std::is_same_v<T, hir::ReturnStatementData>) {
          // Single-exit form: all returns branch to exit block.
          // For non-void: store value to return_slot, then jump.
          // For void: just jump (no return_slot).
          Context& ctx = builder.GetContext();
          if (data.value != hir::kInvalidExpressionId) {
            auto value_result = LowerExpression(data.value, builder);
            if (!value_result) {
              result = std::unexpected(value_result.error());
              return;
            }
            if (ctx.return_slot.has_value()) {
              builder.EmitTypedAssign(
                  *ctx.return_slot, std::move(*value_result), ctx.return_type);
            }
          }
          // Jump to exit block (processes don't use single-exit form)
          BlockIndex exit_block = builder.GetExitBlock();
          if (exit_block != kInvalidBlockIndex) {
            builder.EmitJump(exit_block);
          } else {
            // Legacy path for processes without single-exit form
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
        } else if constexpr (std::is_same_v<
                                 T, hir::NamedEventWaitStatementData>) {
          auto event_id = ResolveEventRef(data.event_expr, builder);
          auto resume_bb = builder.CreateBlock();
          builder.EmitWaitEvent(event_id, resume_bb);
          builder.SetCurrentBlock(resume_bb);
        } else if constexpr (std::is_same_v<
                                 T, hir::EventTriggerStatementData>) {
          auto event_id = ResolveEventRef(data.target, builder);
          builder.EmitTriggerEvent(event_id);
        } else if constexpr (std::is_same_v<
                                 T, hir::ImmediateAssertionStatementData>) {
          result = LowerImmediateAssertion(data, stmt.span, builder);
        } else if constexpr (std::is_same_v<
                                 T, hir::DeferredAssertionStatementData>) {
          // Temporary: delegate to existing deferred lowering via compat
          // shim. This will be replaced when HIR-to-MIR lowering is
          // switched to consume inline-owned DeferredAction directly.
          result = LowerDeferredAssertion(data, stmt.span, builder);
        } else {
          throw common::InternalError(
              "LowerStatement", "unhandled statement kind");
        }
      },
      stmt.data);
  return result;
}

}  // namespace lyra::lowering::hir_to_mir

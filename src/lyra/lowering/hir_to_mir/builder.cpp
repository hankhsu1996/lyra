#include "lyra/lowering/hir_to_mir/builder.hpp"

#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/system_tf.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/materialize_cache.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Verifier for Rvalue invariants. Always-on (fail-loud in all builds).
// Checks that operand storage follows the MIR design contract:
// - FopenRvalueInfo: uses info.filename/mode, Rvalue::operands must be empty
// - TestPlusargs: uses info.query, Rvalue::operands must be empty
void VerifyRvalueInvariants(const mir::Rvalue& rv) {
  std::visit(
      common::Overloaded{
          [&](const mir::TestPlusargsRvalueInfo& /*info*/) {
            if (!rv.operands.empty()) {
              throw common::InternalError(
                  "VerifyRvalueInvariants",
                  "TestPlusargs: operands must be empty (query stored in "
                  "info.query)");
            }
          },
          [&](const mir::FopenRvalueInfo& /*info*/) {
            if (!rv.operands.empty()) {
              throw common::InternalError(
                  "VerifyRvalueInvariants",
                  "FopenRvalueInfo: operands must be empty (use "
                  "filename/mode)");
            }
          },
          [](const auto& /*info*/) {
            // Other rvalue kinds: no special constraints
          }},
      rv.info);
}

}  // namespace

MirBuilder::MirBuilder(mir::Arena* arena, Context* ctx, OriginMap* origin_map)
    : arena_(arena),
      ctx_(ctx),
      origin_map_(origin_map),
      current_block_(kInvalidBlockIndex),
      blocks_{} {
  if (arena_ == nullptr || ctx_ == nullptr) {
    throw common::InternalError("MirBuilder", "arena and ctx must not be null");
  }
}

void MirBuilder::SetCurrentHirSource(hir::StatementId stmt_id) {
  current_hir_source_.emplace(stmt_id);
}

void MirBuilder::SetCurrentHirSource(hir::ExpressionId expr_id) {
  current_hir_source_.emplace(expr_id);
}

void MirBuilder::ClearCurrentHirSource() {
  current_hir_source_.reset();
}

auto MirBuilder::RecordProjectionOrigin(hir::ExpressionId expr_id)
    -> common::OriginId {
  if (origin_map_ == nullptr) {
    return common::OriginId::Invalid();
  }
  return origin_map_->Record(std::monostate{}, expr_id);
}

auto MirBuilder::IsReachable() const -> bool {
  if (current_block_ == kInvalidBlockIndex) {
    return false;
  }
  return !blocks_[current_block_.value].terminator.has_value();
}

void MirBuilder::ClearInsertionPoint() {
  current_block_ = kInvalidBlockIndex;
}

// Instruction emission: permissive for dead code.
// - No insertion point -> no-op (after ClearInsertionPoint from control flow)
// - Block sealed -> no-op (dead code after terminator, frontend should catch)
template <class InstT>
void MirBuilder::EmitInst(InstT inst) {
  if (finished_) {
    throw common::InternalError("MirBuilder", "Emit after Finish()");
  }
  if (current_block_ == kInvalidBlockIndex) {
    return;
  }
  auto& block = blocks_[current_block_.value];
  if (block.terminator.has_value()) {
    return;
  }

  // Record origin at emit time (ONLY place for instructions).
  // Uses current_hir_source_ (deferred recording) if set.
  common::OriginId origin = current_origin_;
  if (origin_map_ != nullptr && current_hir_source_.has_value()) {
    auto stmt_index = static_cast<uint32_t>(block.statements.size());
    StatementRef ref{
        .block = mir::BasicBlockId{current_block_.value},
        .statement_index = stmt_index,
    };
    origin = std::visit(
        [&](auto id) { return origin_map_->Record(ref, id); },
        *current_hir_source_);
  }

  block.statements.push_back(
      mir::Statement{.data = std::move(inst), .origin = origin});
}

// Terminator emission: strict for CFG correctness.
// - No insertion point -> no-op (after ClearInsertionPoint from control flow)
// - Block already sealed -> THROW (double-terminator is a CFG bug)
// Use EmitProcessEpilogue/EmitImplicitReturn for conditional termination.
template <class TermT>
void MirBuilder::EmitTerm(TermT term) {
  if (finished_) {
    throw common::InternalError("MirBuilder", "EmitTerm after Finish()");
  }
  if (current_block_ == kInvalidBlockIndex) {
    return;
  }
  auto& block = blocks_[current_block_.value];
  if (block.terminator.has_value()) {
    throw common::InternalError("MirBuilder", "block already has terminator");
  }

  // Record origin at emit time (ONLY place for terminators).
  // Uses current_hir_source_ (deferred recording) if set.
  common::OriginId origin = current_origin_;
  if (origin_map_ != nullptr && current_hir_source_.has_value()) {
    TerminatorRef ref{.block = mir::BasicBlockId{current_block_.value}};
    origin = std::visit(
        [&](auto id) { return origin_map_->Record(ref, id); },
        *current_hir_source_);
  }

  block.terminator = mir::Terminator{.data = std::move(term), .origin = origin};
}

auto MirBuilder::CreateBlock() -> BlockIndex {
  if (finished_) {
    throw common::InternalError(
        "MirBuilder", "CreateBlock called after Finish()");
  }
  BlockIndex idx{static_cast<uint32_t>(blocks_.size())};
  blocks_.emplace_back();
  return idx;
}

auto MirBuilder::CreateBlockWithParams(std::vector<TypeId> param_types)
    -> std::pair<BlockIndex, std::vector<int>> {
  if (finished_) {
    throw common::InternalError(
        "MirBuilder", "CreateBlockWithParams called after Finish()");
  }
  BlockIndex idx{static_cast<uint32_t>(blocks_.size())};
  blocks_.emplace_back();

  std::vector<int> temp_ids;
  temp_ids.reserve(param_types.size());
  for (TypeId ty : param_types) {
    // Use AllocValueTemp to properly record in temp_metadata
    int temp_id = ctx_->AllocValueTemp(ty);
    blocks_[idx.value].params.push_back(
        mir::BlockParam{
            .temp_id = temp_id,
            .type = ty,
        });
    temp_ids.push_back(temp_id);
  }
  return {idx, temp_ids};
}

void MirBuilder::SetCurrentBlock(BlockIndex block) {
  if (finished_) {
    throw common::InternalError(
        "MirBuilder", "SetCurrentBlock called after Finish()");
  }
  if (block.value >= blocks_.size()) {
    throw common::InternalError(
        "MirBuilder", "SetCurrentBlock: invalid block index");
  }
  current_block_ = block;
  // Reachability is now derived on-demand via IsReachable()
}

auto MirBuilder::CurrentBlock() const -> BlockIndex {
  return current_block_;
}

void MirBuilder::EmitAssign(mir::PlaceId target, mir::Operand source) {
  EmitInst(mir::Assign{.dest = target, .rhs = std::move(source)});
}

void MirBuilder::EmitAssign(mir::PlaceId target, mir::Rvalue value) {
  VerifyRvalueInvariants(value);
  EmitInst(mir::Assign{.dest = target, .rhs = std::move(value)});
}

void MirBuilder::EmitEffect(mir::EffectOp op) {
  EmitInst(mir::Effect{.op = std::move(op)});
}

auto MirBuilder::EmitPlaceTemp(TypeId type, mir::Rvalue value) -> mir::PlaceId {
  mir::PlaceId temp = ctx_->AllocTemp(type);
  EmitAssign(temp, std::move(value));
  return temp;
}

auto MirBuilder::EmitValueTemp(TypeId type, mir::Rvalue value) -> mir::Operand {
  VerifyRvalueInvariants(value);
  int temp_id = ctx_->AllocValueTemp(type);
  EmitInst(
      mir::DefineTemp{
          .temp_id = temp_id, .type = type, .rhs = std::move(value)});
  return mir::Operand::UseTemp(temp_id);
}

auto MirBuilder::EmitValueTempAssign(TypeId type, mir::Operand source)
    -> mir::Operand {
  int temp_id = ctx_->AllocValueTemp(type);
  EmitInst(mir::DefineTemp{.temp_id = temp_id, .type = type, .rhs = source});
  return mir::Operand::UseTemp(temp_id);
}

auto MirBuilder::MaterializeOperandToPlace(TypeId type, mir::Operand value)
    -> mir::PlaceId {
  ++ctx_->materialize_count;
  mir::PlaceId place = ctx_->AllocTemp(type);
  EmitAssign(place, std::move(value));
  return place;
}

namespace {

// Check if a place has no projections (is "unprojected").
// Used for debug type checks: unprojected places have type == root.type.
auto IsUnprojectedPlace(const mir::Place& place) -> bool {
  return place.projections.empty();
}

}  // namespace

auto MirBuilder::MaterializeIfNeededToPlace(TypeId type, mir::Operand operand)
    -> mir::PlaceId {
  if (operand.kind == mir::Operand::Kind::kUse) {
    mir::PlaceId place = std::get<mir::PlaceId>(operand.payload);
    // Debug check: for unprojected temps, verify type matches.
    // Projected places have different effective type than root - skip check.
    const mir::Place& place_data = (*arena_)[place];
    if (IsUnprojectedPlace(place_data) && place_data.root.type != type) {
      throw common::InternalError(
          "MaterializeIfNeededToPlace", "type mismatch for unprojected place");
    }
    return place;
  }
  return MaterializeOperandToPlace(type, std::move(operand));
}

auto MirBuilder::EnsurePlaceCached(
    TypeId type, mir::Operand operand, PlaceMaterializationCache& cache)
    -> mir::PlaceId {
  // kUse: already a place, return directly
  if (operand.kind == mir::Operand::Kind::kUse) {
    return std::get<mir::PlaceId>(operand.payload);
  }

  // kUseTemp: memoize by (temp_id, type)
  if (operand.kind == mir::Operand::Kind::kUseTemp) {
    mir::TempId temp_id = std::get<mir::TempId>(operand.payload);
    PlaceMaterializationCache::Key key{.temp_id = temp_id.value, .type = type};
    auto it = cache.map.find(key);
    if (it != cache.map.end()) {
      return it->second;
    }
    mir::PlaceId place = MaterializeOperandToPlace(type, std::move(operand));
    cache.map[key] = place;
    return place;
  }

  // kConst: just materialize (no memoization)
  return MaterializeOperandToPlace(type, std::move(operand));
}

auto MirBuilder::EmitCall(
    mir::FunctionId callee, std::vector<mir::Operand> args, TypeId return_type)
    -> mir::Operand {
  return EmitCallWithWritebacks(callee, std::move(args), {}, return_type);
}

auto MirBuilder::EmitCallWithWritebacks(
    mir::FunctionId callee, std::vector<mir::Operand> in_args,
    std::vector<mir::CallWriteback> writebacks, TypeId return_type)
    -> mir::Operand {
  const auto& types = *ctx_->type_arena;
  bool is_void = types[return_type].Kind() == TypeKind::kVoid;

  std::optional<mir::CallReturn> ret;
  mir::PlaceId ret_tmp;

  if (!is_void) {
    ret_tmp = ctx_->AllocTemp(return_type);
    ret = mir::CallReturn{
        .tmp = ret_tmp,
        .dest = std::nullopt,  // Expression form: caller handles commit
        .type = return_type,
    };
  }

  EmitInst(
      mir::Call{
          .callee = callee,
          .in_args = std::move(in_args),
          .ret = ret,
          .writebacks = std::move(writebacks),
      });

  return is_void ? mir::Operand::Poison() : mir::Operand::Use(ret_tmp);
}

auto MirBuilder::EmitBuiltinCall(
    mir::BuiltinMethod method, mir::PlaceId receiver,
    std::vector<mir::Operand> args, TypeId return_type) -> mir::Operand {
  const auto& types = *ctx_->type_arena;
  bool is_void = types[return_type].Kind() == TypeKind::kVoid;

  if (is_void) {
    EmitInst(
        mir::BuiltinCall{
            .dest = std::nullopt,
            .method = method,
            .receiver = receiver,
            .args = std::move(args)});
    return mir::Operand::Poison();
  }

  mir::PlaceId temp = ctx_->AllocTemp(return_type);
  EmitInst(
      mir::BuiltinCall{
          .dest = temp,
          .method = method,
          .receiver = receiver,
          .args = std::move(args)});
  return mir::Operand::Use(temp);
}

auto MirBuilder::EmitSystemTfCallExpr(
    SystemTfOpcode opcode, std::vector<mir::Operand> in_args,
    TypeId return_type,
    std::vector<std::tuple<mir::PlaceId, TypeId, mir::PassMode>> writebacks)
    -> mir::Operand {
  // Delegate to 4-tuple overload with kStaged default
  std::vector<
      std::tuple<mir::PlaceId, TypeId, mir::PassMode, mir::WritebackKind>>
      full_writebacks;
  full_writebacks.reserve(writebacks.size());
  for (auto& [dest, type, mode] : writebacks) {
    full_writebacks.emplace_back(dest, type, mode, mir::WritebackKind::kStaged);
  }
  return EmitSystemTfCallExpr(
      opcode, std::move(in_args), return_type, std::move(full_writebacks));
}

auto MirBuilder::EmitSystemTfCallExpr(
    SystemTfOpcode opcode, std::vector<mir::Operand> in_args,
    TypeId return_type,
    std::vector<
        std::tuple<mir::PlaceId, TypeId, mir::PassMode, mir::WritebackKind>>
        writebacks) -> mir::Operand {
  // Validate no Ref mode
  for (const auto& [dest, type, mode, kind] : writebacks) {
    if (mode == mir::PassMode::kRef) {
      throw common::InternalError(
          "EmitSystemTfCallExpr", "Ref parameters not yet supported");
    }
  }

  // Build return output (expression form: no dest, caller assigns)
  mir::PlaceId ret_tmp = ctx_->AllocTemp(return_type);
  mir::CallReturn ret{
      .tmp = ret_tmp,
      .dest = std::nullopt,  // Expression form
      .type = return_type,
  };

  // Build writebacks: allocate staging temp for kStaged, nullopt for
  // kDirectToDest
  std::vector<mir::CallWriteback> wb_outputs;
  for (size_t i = 0; i < writebacks.size(); ++i) {
    auto [dest, type, mode, kind] = writebacks[i];
    std::optional<mir::PlaceId> tmp =
        (kind == mir::WritebackKind::kDirectToDest)
            ? std::nullopt
            : std::optional{ctx_->AllocTemp(type)};
    wb_outputs.push_back(
        mir::CallWriteback{
            .tmp = tmp,
            .dest = dest,
            .type = type,
            .mode = mode,
            .kind = kind,
            .arg_index = static_cast<int32_t>(i),
        });
  }

  EmitInst(
      mir::Call{
          .callee = opcode,
          .in_args = std::move(in_args),
          .ret = ret,
          .writebacks = std::move(wb_outputs),
      });

  return mir::Operand::Use(ret_tmp);
}

auto MirBuilder::EmitUnary(
    mir::UnaryOp op, mir::Operand operand, TypeId result_type) -> mir::Operand {
  mir::Rvalue rvalue{
      .operands = {std::move(operand)},
      .info = mir::UnaryRvalueInfo{.op = op},
  };
  return EmitValueTemp(result_type, std::move(rvalue));
}

auto MirBuilder::EmitBinary(
    mir::BinaryOp op, mir::Operand lhs, mir::Operand rhs, TypeId result_type)
    -> mir::Operand {
  mir::Rvalue rvalue{
      .operands = {std::move(lhs), std::move(rhs)},
      .info = mir::BinaryRvalueInfo{.op = op},
  };
  return EmitValueTemp(result_type, std::move(rvalue));
}

auto MirBuilder::EmitCast(
    mir::Operand source, TypeId source_type, TypeId target_type)
    -> mir::Operand {
  if (source_type == target_type) {
    return source;
  }
  mir::Rvalue rvalue{
      .operands = {std::move(source)},
      .info =
          mir::CastRvalueInfo{
              .source_type = source_type, .target_type = target_type},
  };
  return EmitValueTemp(target_type, std::move(rvalue));
}

auto MirBuilder::EmitIndexValidity(
    mir::Operand index, int64_t lower, int64_t upper, bool check_known)
    -> mir::Operand {
  mir::Rvalue rvalue{
      .operands = {std::move(index)},
      .info =
          mir::IndexValidityRvalueInfo{
              .lower_bound = lower,
              .upper_bound = upper,
              .check_known = check_known},
  };
  return EmitValueTemp(ctx_->GetBitType(), std::move(rvalue));
}

auto MirBuilder::EmitGuardedUse(
    mir::Operand validity, mir::PlaceId place, TypeId result_type)
    -> mir::Operand {
  mir::Rvalue rvalue{
      .operands = {std::move(validity)},
      .info =
          mir::GuardedUseRvalueInfo{.place = place, .result_type = result_type},
  };
  // Use PlaceTemp for guarded reads - may be used as bases for projections
  return mir::Operand::Use(EmitPlaceTemp(result_type, std::move(rvalue)));
}

void MirBuilder::EmitGuardedAssign(
    mir::PlaceId dest, mir::RightHandSide rhs, mir::Operand guard) {
  EmitInst(
      mir::GuardedAssign{
          .dest = dest, .rhs = std::move(rhs), .guard = std::move(guard)});
}

void MirBuilder::EmitDeferredAssign(mir::PlaceId dest, mir::RightHandSide rhs) {
  EmitInst(mir::DeferredAssign{.dest = dest, .rhs = std::move(rhs)});
}

void MirBuilder::EmitIf(
    mir::Operand condition, std::function<void()> then_body) {
  // For if-then (no else), merge is always reachable via the false branch.
  BlockIndex then_bb = CreateBlock();
  BlockIndex merge_bb = CreateBlock();

  EmitBranch(condition, then_bb, merge_bb);

  SetCurrentBlock(then_bb);
  then_body();
  if (IsReachable()) {
    EmitJump(merge_bb);
  }

  SetCurrentBlock(merge_bb);
  // Merge is always reachable because the else branch (false) goes here
}

void MirBuilder::EmitIfElse(
    mir::Operand condition, std::function<void()> then_body,
    std::function<void()> else_body) {
  BlockIndex then_bb = CreateBlock();
  BlockIndex else_bb = CreateBlock();

  EmitBranch(condition, then_bb, else_bb);

  // Execute then branch
  SetCurrentBlock(then_bb);
  then_body();
  bool then_falls_through = IsReachable();
  BlockIndex then_end_bb = CurrentBlock();

  // Execute else branch
  SetCurrentBlock(else_bb);
  else_body();
  bool else_falls_through = IsReachable();
  BlockIndex else_end_bb = CurrentBlock();

  // Structural join: only create merge block if needed
  if (then_falls_through || else_falls_through) {
    BlockIndex merge_bb = CreateBlock();
    if (then_falls_through) {
      SetCurrentBlock(then_end_bb);
      EmitJump(merge_bb);
    }
    if (else_falls_through) {
      SetCurrentBlock(else_end_bb);
      EmitJump(merge_bb);
    }
    SetCurrentBlock(merge_bb);
  } else {
    // Both branches terminate - no insertion point
    ClearInsertionPoint();
  }
}

void MirBuilder::EmitPriorityChain(
    const std::vector<std::function<mir::Operand()>>& conditions,
    const std::vector<std::function<void()>>& bodies,
    std::function<void()> else_body) {
  if (conditions.size() != bodies.size()) {
    throw common::InternalError(
        "EmitPriorityChain", "conditions.size() != bodies.size()");
  }

  // Create body blocks for each condition
  std::vector<BlockIndex> body_blocks;
  for (size_t i = 0; i < conditions.size(); ++i) {
    body_blocks.push_back(CreateBlock());
  }

  // Else block
  BlockIndex else_bb = CreateBlock();

  // Generate branch cascade: condition[i] ? body[i] : next_check
  for (size_t i = 0; i < conditions.size(); ++i) {
    mir::Operand cond = conditions[i]();
    BlockIndex next_check =
        (i + 1 < conditions.size()) ? CreateBlock() : else_bb;
    EmitBranch(cond, body_blocks[i], next_check);

    if (i + 1 < conditions.size()) {
      SetCurrentBlock(next_check);
    }
  }

  // Execute body blocks and track which fall through
  std::vector<BlockIndex> fallthrough_blocks;
  for (size_t i = 0; i < bodies.size(); ++i) {
    SetCurrentBlock(body_blocks[i]);
    bodies[i]();
    if (IsReachable()) {
      fallthrough_blocks.push_back(CurrentBlock());
    }
  }

  // Execute else block
  SetCurrentBlock(else_bb);
  else_body();
  if (IsReachable()) {
    fallthrough_blocks.push_back(CurrentBlock());
  }

  // Structural join: only create merge block if needed
  if (!fallthrough_blocks.empty()) {
    BlockIndex merge_bb = CreateBlock();
    for (BlockIndex fb : fallthrough_blocks) {
      SetCurrentBlock(fb);
      EmitJump(merge_bb);
    }
    SetCurrentBlock(merge_bb);
  } else {
    ClearInsertionPoint();
  }
}

void MirBuilder::EmitUniqueDispatch(
    mir::DispatchQualifier qualifier, mir::DispatchStatementKind statement_kind,
    const std::vector<mir::Operand>& conditions,
    const std::vector<std::function<void()>>& bodies,
    std::function<void()> else_body, bool has_else) {
  // Create body blocks + else block
  std::vector<BlockIndex> body_blocks;
  for (size_t i = 0; i < bodies.size(); ++i) {
    body_blocks.push_back(CreateBlock());
  }
  BlockIndex else_bb = CreateBlock();

  // Build targets: [body0, body1, ..., bodyN, else]
  std::vector<BlockIndex> targets = body_blocks;
  targets.push_back(else_bb);

  EmitQualifiedDispatch(
      qualifier, statement_kind, conditions, targets, has_else);

  // Execute body blocks and track which fall through
  std::vector<BlockIndex> fallthrough_blocks;
  for (size_t i = 0; i < bodies.size(); ++i) {
    SetCurrentBlock(body_blocks[i]);
    bodies[i]();
    if (IsReachable()) {
      fallthrough_blocks.push_back(CurrentBlock());
    }
  }

  // Execute else block
  SetCurrentBlock(else_bb);
  else_body();
  if (IsReachable()) {
    fallthrough_blocks.push_back(CurrentBlock());
  }

  // Structural join: only create merge block if needed
  if (!fallthrough_blocks.empty()) {
    BlockIndex merge_bb = CreateBlock();
    for (BlockIndex fb : fallthrough_blocks) {
      SetCurrentBlock(fb);
      EmitJump(merge_bb);
    }
    SetCurrentBlock(merge_bb);
  } else {
    ClearInsertionPoint();
  }
}

void MirBuilder::EmitCaseCascade(
    mir::PlaceId selector, mir::BinaryOp comparison_op,
    const std::vector<CaseItem>& items, std::function<void()> default_body) {
  BlockIndex default_bb = CreateBlock();

  // Create body blocks and first-check blocks for each item
  std::vector<BlockIndex> item_body_blocks;
  std::vector<BlockIndex> item_first_check_blocks;
  for (size_t i = 0; i < items.size(); ++i) {
    item_body_blocks.push_back(CreateBlock());
    item_first_check_blocks.push_back(CreateBlock());
  }

  // Jump to first check or default
  if (!items.empty()) {
    EmitJump(item_first_check_blocks[0]);
  } else {
    EmitJump(default_bb);
  }

  // Emit check cascade and body blocks, tracking fallthrough
  std::vector<BlockIndex> fallthrough_blocks;
  for (size_t i = 0; i < items.size(); ++i) {
    const CaseItem& item = items[i];
    BlockIndex body_bb = item_body_blocks[i];
    BlockIndex next_item_bb =
        (i + 1 < items.size()) ? item_first_check_blocks[i + 1] : default_bb;

    SetCurrentBlock(item_first_check_blocks[i]);

    // Emit OR'd expression checks: any match -> body
    for (size_t j = 0; j < item.expressions.size(); ++j) {
      mir::Operand val = item.expressions[j]();
      mir::Rvalue cmp_rvalue{
          .operands = {mir::Operand::Use(selector), std::move(val)},
          .info = mir::BinaryRvalueInfo{.op = comparison_op},
      };
      mir::PlaceId cmp_result =
          EmitPlaceTemp(ctx_->GetBitType(), std::move(cmp_rvalue));

      bool is_last_expr = (j + 1 == item.expressions.size());
      BlockIndex nomatch_bb = is_last_expr ? next_item_bb : CreateBlock();

      EmitBranch(mir::Operand::Use(cmp_result), body_bb, nomatch_bb);

      if (!is_last_expr) {
        SetCurrentBlock(nomatch_bb);
      }
    }

    // Execute body
    SetCurrentBlock(body_bb);
    item.body();
    if (IsReachable()) {
      fallthrough_blocks.push_back(CurrentBlock());
    }
  }

  // Execute default block
  SetCurrentBlock(default_bb);
  default_body();
  if (IsReachable()) {
    fallthrough_blocks.push_back(CurrentBlock());
  }

  // Structural join: only create merge block if needed
  if (!fallthrough_blocks.empty()) {
    BlockIndex merge_bb = CreateBlock();
    for (BlockIndex fb : fallthrough_blocks) {
      SetCurrentBlock(fb);
      EmitJump(merge_bb);
    }
    SetCurrentBlock(merge_bb);
  } else {
    ClearInsertionPoint();
  }
}

void MirBuilder::EmitJump(BlockIndex target, std::vector<mir::Operand> args) {
  EmitTerm(
      mir::Jump{
          .target = mir::BasicBlockId{target.value},
          .args = std::move(args),
      });
}

void MirBuilder::EmitBranch(
    mir::Operand cond, BlockIndex then_bb, std::vector<mir::Operand> then_args,
    BlockIndex else_bb, std::vector<mir::Operand> else_args) {
  // Condition can be Use (place) or UseTemp (value temp)
  if (cond.kind != mir::Operand::Kind::kUse &&
      cond.kind != mir::Operand::Kind::kUseTemp) {
    throw common::InternalError(
        "EmitBranch", "branch condition must be a Use or UseTemp operand");
  }
  EmitTerm(
      mir::Branch{
          .condition = std::move(cond),
          .then_target = mir::BasicBlockId{then_bb.value},
          .then_args = std::move(then_args),
          .else_target = mir::BasicBlockId{else_bb.value},
          .else_args = std::move(else_args),
      });
}

void MirBuilder::EmitBranch(
    mir::Operand cond, BlockIndex then_bb, BlockIndex else_bb) {
  EmitBranch(cond, then_bb, {}, else_bb, {});
}

void MirBuilder::EmitQualifiedDispatch(
    mir::DispatchQualifier qualifier, mir::DispatchStatementKind statement_kind,
    const std::vector<mir::Operand>& conditions,
    const std::vector<BlockIndex>& targets, bool has_else) {
  // Validate invariant: targets = one per condition + else fallthrough
  if (targets.size() != conditions.size() + 1) {
    throw common::InternalError(
        "EmitQualifiedDispatch",
        "targets.size() must equal conditions.size() + 1");
  }

  std::vector<mir::BasicBlockId> bb_targets;
  bb_targets.reserve(targets.size());
  for (const auto& target : targets) {
    bb_targets.push_back(mir::BasicBlockId{target.value});
  }

  EmitTerm(
      mir::QualifiedDispatch{
          .qualifier = qualifier,
          .statement_kind = statement_kind,
          .conditions = conditions,
          .targets = std::move(bb_targets),
          .has_else = has_else,
      });
}

void MirBuilder::EmitReturn(mir::Operand value) {
  EmitTerm(mir::Return{.value = std::make_optional(std::move(value))});
}

void MirBuilder::EmitRepeat() {
  EmitTerm(mir::Repeat{});
}

void MirBuilder::EmitDelay(uint64_t ticks, BlockIndex resume) {
  EmitTerm(
      mir::Delay{
          .ticks = ticks,
          .resume = mir::BasicBlockId{resume.value},
      });
}

void MirBuilder::EmitWait(
    std::vector<mir::WaitTrigger> triggers, BlockIndex resume) {
  EmitTerm(
      mir::Wait{
          .triggers = std::move(triggers),
          .resume = mir::BasicBlockId{resume.value},
      });
}

void MirBuilder::EmitTerminate(std::optional<mir::Finish> info) {
  if (info) {
    EmitTerm(*std::move(info));
  } else {
    // Implicit process termination: use Return without value.
    // Only explicit $finish/$stop/$fatal should use Finish terminator.
    EmitTerm(mir::Return{.value = std::nullopt});
  }
}

void MirBuilder::EmitImplicitReturn(TypeId return_type) {
  if (!IsReachable()) {
    return;  // All paths explicitly returned
  }

  const Type& ret_type = (*ctx_->type_arena)[return_type];
  if (ret_type.Kind() != TypeKind::kVoid) {
    throw common::InternalError(
        "MirBuilder::EmitImplicitReturn",
        "non-void function fell through without return");
  }
  EmitTerminate(std::nullopt);
}

void MirBuilder::EmitProcessEpilogue(mir::ProcessKind kind) {
  if (!IsReachable()) {
    return;  // Body explicitly terminated (e.g., $finish)
  }

  if (kind == mir::ProcessKind::kLooping) {
    EmitRepeat();
  } else {
    EmitTerminate(std::nullopt);
  }
}

namespace {

// Remap a single block target using the provided mapping.
// Throws if the target maps to a filtered-out block.
void RemapTarget(mir::BasicBlockId& target, const std::vector<int>& block_map) {
  int mapped = block_map[target.value];
  if (mapped < 0) {
    throw common::InternalError(
        "MirBuilder", "terminator targets filtered-out block");
  }
  target.value = static_cast<uint32_t>(mapped);
}

// Remap all block targets in a terminator after filtering blocks.
void RemapTerminatorTargets(
    mir::Terminator& term, const std::vector<int>& block_map) {
  std::visit(
      [&](auto& t) {
        using T = std::decay_t<decltype(t)>;
        if constexpr (std::is_same_v<T, mir::Jump>) {
          RemapTarget(t.target, block_map);
        } else if constexpr (std::is_same_v<T, mir::Branch>) {
          RemapTarget(t.then_target, block_map);
          RemapTarget(t.else_target, block_map);
        } else if constexpr (std::is_same_v<T, mir::Switch>) {
          for (auto& target : t.targets) {
            RemapTarget(target, block_map);
          }
        } else if constexpr (std::is_same_v<T, mir::QualifiedDispatch>) {
          for (auto& target : t.targets) {
            RemapTarget(target, block_map);
          }
        } else if constexpr (std::is_same_v<T, mir::Delay>) {
          RemapTarget(t.resume, block_map);
        } else if constexpr (std::is_same_v<T, mir::Wait>) {
          RemapTarget(t.resume, block_map);
        }
        // Return, Finish, Repeat have no targets
      },
      term.data);
}

}  // namespace

auto MirBuilder::Finish() -> std::vector<mir::BasicBlock> {
  if (finished_) {
    throw common::InternalError("MirBuilder", "Finish called twice");
  }
  finished_ = true;

  // Build block mapping: old index -> new index (or -1 if filtered out).
  // Blocks with no terminator AND no instructions are dead/unreachable.
  std::vector<int> block_map(blocks_.size(), -1);
  std::vector<mir::BasicBlock> result;
  result.reserve(blocks_.size());

  for (size_t i = 0; i < blocks_.size(); ++i) {
    auto& bb = blocks_[i];
    if (!bb.terminator.has_value()) {
      if (!bb.statements.empty()) {
        throw common::InternalError(
            "MirBuilder", "block has instructions but no terminator");
      }
      // Empty block with no terminator - dead/unreachable, skip
      continue;
    }
    block_map[i] = static_cast<int>(result.size());
    result.push_back(
        mir::BasicBlock{
            .params = std::move(bb.params),
            .statements = std::move(bb.statements),
            .terminator = std::move(*bb.terminator),
        });
  }

  // Remap terminator targets to new indices
  for (auto& bb : result) {
    RemapTerminatorTargets(bb.terminator, block_map);
  }

  return result;
}

void MirBuilder::PushLoop(LoopContext ctx) {
  loop_stack_.push_back(ctx);
}

void MirBuilder::PopLoop() {
  loop_stack_.pop_back();
}

auto MirBuilder::CurrentLoop() const -> const LoopContext* {
  return loop_stack_.empty() ? nullptr : &loop_stack_.back();
}

void MirBuilder::SetExitBlock(BlockIndex block) {
  exit_block_ = block;
}

auto MirBuilder::GetExitBlock() const -> BlockIndex {
  return exit_block_;
}

}  // namespace lyra::lowering::hir_to_mir

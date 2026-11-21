#include "lyra/mir_transform/loop_idiom_recognize.hpp"

#include <algorithm>
#include <array>
#include <cstdint>
#include <functional>
#include <map>
#include <optional>
#include <ranges>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/constant_value.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/operand_use.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir_transform/block_rebuild.hpp"
#include "lyra/mir_transform/loop_analysis.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir_transform {

namespace {

using Node = NodeRef;

auto ReceiverOf(const mir::CallExpr& call) -> mir::ExprId {
  return *mir::CalleeReceiver(call.callee);
}

// Whether `expr` calls an entry that computes a value and changes nothing:
// reaching what a cell holds, a value as a position, and a value at another
// integral type.
auto CallsOnlyToCompute(const mir::Expr& expr) -> bool {
  return BuiltinCallOf(expr, support::BuiltinFn::kGet) != nullptr ||
         BuiltinCallOf(expr, support::BuiltinFn::kToPosition) != nullptr ||
         BuiltinCallOf(expr, support::BuiltinFn::kIntegralConvert) != nullptr;
}

// The call `expr` makes to reach storage a write lands in: a write in
// progress opened on it, or the whole of what that write holds.
auto ReachesWritten(const mir::Expr& expr) -> const mir::CallExpr* {
  for (const support::BuiltinFn reaching :
       {support::BuiltinFn::kDesignateWhole,
        support::BuiltinFn::kOpenForWrite}) {
    if (const mir::CallExpr* call = BuiltinCallOf(expr, reaching)) {
      return call;
    }
  }
  return nullptr;
}

// A loop storing one bit a pass of what it loads, as LLVM's
// LoopIdiomRecognize finds a store of a load (`processLoopStoreOfLoopLoad`),
// each node named in the block of the loop holding it.
struct BitStoreOfLoad {
  InductionVariable induction;
  mir::TypeId variable_type;
  Node written_variable;
  Node bound;
  // The block naming the move's nodes, and the local the bit read is held
  // in, if one is.
  const mir::Block* moved_in = nullptr;
  std::optional<mir::LocalId> held;
  mir::TypeId bit_type;
  // What the bit is a step of where it is written and where it is read, and
  // where each is.
  mir::ExprId destination;
  mir::ExprId written_at;
  mir::ExprId source;
  mir::ExprId read_at;
  AddRecurrence written_recurrence;
  AddRecurrence read_recurrence;
  // Whether the bits read lie in the value written, numbered alike.
  bool same_object = false;
  // How many passes the loop makes, which is known before it runs.
  std::uint64_t trip_count = 0;
  // What every local the loop declares is initialized with, so what one of
  // them holds is stated again where the loop starts.
  LoopLocals locals;
};

// The node a chain of block expressions starting at `node` ends with, and the
// blocks of their steps, outermost first.
struct Chain {
  Node value;
  std::vector<const mir::Block*> steps;
};

auto ChainOf(Node node) -> Chain {
  Chain chain{.value = node, .steps = {}};
  while (const auto* block =
             std::get_if<mir::BlockExpr>(&chain.value.Get().data)) {
    const mir::Block& steps = chain.value.block->child_scopes.Get(block->scope);
    chain.steps.push_back(&steps);
    chain.value = Node{.block = &steps, .id = block->value};
  }
  return chain;
}

// Reads one loop as the moves it makes, or answers nothing where it is not a
// loop moving one bit a pass whose run can come out as each pass would leave
// it.
class LoopReading {
 public:
  LoopReading(
      const mir::CompilationUnit& unit, std::optional<mir::LocalId> receiver)
      : unit_(&unit), receiver_(receiver) {
  }

  auto Read(const mir::Block& holding, const mir::ForStmt& loop)
      -> std::optional<BitStoreOfLoad> {
    if (!loop.init.empty() || !loop.condition.has_value() ||
        loop.step.size() != 1 || loop.break_label.has_value()) {
      return std::nullopt;
    }
    const Node condition{.block = &holding, .id = *loop.condition};
    const Node step{.block = &holding, .id = loop.step.front()};
    if (!std::holds_alternative<mir::BlockExpr>(condition.Get().data) ||
        !std::holds_alternative<mir::BlockExpr>(step.Get().data)) {
      return std::nullopt;
    }
    if (!ReadCondition(condition) || !ReadStep(step)) {
      return std::nullopt;
    }
    const mir::Block& body = holding.child_scopes.Get(loop.scope);
    if (!WalkBody(body) || move_stmt_ == nullptr || !ReadMove()) {
      return std::nullopt;
    }
    for (const Chain& chain : {ChainOf(condition), ChainOf(step)}) {
      if (!Scan(*chain.steps.front())) {
        return std::nullopt;
      }
    }
    if (!Scan(body) || held_reads_ != (found_.held.has_value() ? 1 : 0)) {
      return std::nullopt;
    }
    entering_ = EnteringValue(holding, loop);
    return Finish();
  }

 private:
  // The value the loop variable enters the loop holding, where the statement
  // just before the loop states it -- the variable's declaration, a write of
  // it, or a set of the cell holding it -- as ScalarEvolution reads an add
  // recurrence's start off the value entering from ahead of the loop.
  [[nodiscard]] auto EnteringValue(
      const mir::Block& holding, const mir::ForStmt& loop) const
      -> std::optional<Node> {
    const InductionVariable& induction = found_.induction;
    const mir::Stmt* before = nullptr;
    for (const mir::StmtId id : holding.root_stmts) {
      const mir::Stmt& stmt = holding.stmts.Get(id);
      if (std::get_if<mir::ForStmt>(&stmt.data) == &loop) {
        break;
      }
      before = &stmt;
    }
    if (before == nullptr) {
      return std::nullopt;
    }
    const bool a_local = !induction.in_cell &&
                         !induction.object.through_pointer &&
                         induction.object.members.empty();
    if (const auto* decl = std::get_if<mir::LocalDeclStmt>(&before->data)) {
      if (!a_local || decl->target != induction.object.root) {
        return std::nullopt;
      }
      return Node{.block = &holding, .id = decl->init};
    }
    const auto* evaluated = std::get_if<mir::ExprStmt>(&before->data);
    if (evaluated == nullptr) {
      return std::nullopt;
    }
    const Node write{.block = &holding, .id = evaluated->expr};
    if (const auto* assign = std::get_if<mir::AssignExpr>(&write.Get().data)) {
      if (induction.in_cell ||
          UnderlyingObjectOf(*unit_, write.At(assign->target)) !=
              induction.object) {
        return std::nullopt;
      }
      return write.At(assign->value);
    }
    const mir::CallExpr* set =
        BuiltinCallOf(write.Get(), support::BuiltinFn::kSet);
    if (set == nullptr || !induction.in_cell || set->arguments.size() != 1 ||
        UnderlyingObjectOf(*unit_, write.At(ReceiverOf(*set))) !=
            induction.object) {
      return std::nullopt;
    }
    return write.At(set->arguments.front());
  }

  // How many passes the loop makes where the value it enters with and its
  // bound are both constants, as ScalarEvolution's trip count of an add
  // recurrence with a constant start and a constant bound is, which is no
  // more than zero where it makes none; nothing where either is not a
  // constant.
  [[nodiscard]] auto ConstantTripCount() const -> std::optional<std::int64_t> {
    if (!entering_.has_value()) {
      return std::nullopt;
    }
    const mir::IntegralType& type =
        unit_->types.Get(found_.variable_type).Integral();
    if (type.bit_width > 62) {
      return std::nullopt;
    }
    const auto number = [&](Node node) -> std::optional<std::int64_t> {
      const mir::IntegralConstant* constant =
          mir::IntegralPlanesNamedBy(unit_->constants, node.Get().data);
      if (constant == nullptr || node.Get().type != found_.variable_type ||
          constant->value_words.empty()) {
        return std::nullopt;
      }
      const std::uint64_t word = constant->value_words.front();
      const std::uint64_t sign = std::uint64_t{1} << (type.bit_width - 1);
      if (type.signedness == mir::Signedness::kSigned && (word & sign) != 0) {
        return static_cast<std::int64_t>(word) -
               static_cast<std::int64_t>(sign << 1U);
      }
      return static_cast<std::int64_t>(word);
    };
    const std::optional<std::int64_t> start = number(*entering_);
    const std::optional<std::int64_t> bound = number(found_.bound);
    if (!start.has_value() || !bound.has_value()) {
      return std::nullopt;
    }
    return *bound - *start;
  }
  // The loop runs while the variable is below a bound no pass changes.
  auto ReadCondition(Node condition) -> bool {
    const Chain chain = ChainOf(condition);
    for (const mir::Block* steps : chain.steps) {
      if (!Declares(*steps)) {
        return false;
      }
    }
    const mir::Expr& tested = chain.value.Get();
    const auto* truth = std::get_if<mir::CastExpr>(&tested.data);
    if (truth == nullptr || tested.type != unit_->builtins.machine_bool) {
      return false;
    }
    const auto* less = std::get_if<mir::BinaryExpr>(
        &chain.value.block->exprs.Get(truth->operand).data);
    if (less == nullptr || less->op != mir::BinaryOp::kLessThan) {
      return false;
    }
    const Node variable = Resolved(chain.value.At(less->lhs));
    std::optional<InductionVariable> read =
        InductionVariableReadBy(*unit_, variable);
    if (!read.has_value()) {
      return false;
    }
    found_.induction = *std::move(read);
    found_.variable_type = variable.Get().type;
    found_.bound = chain.value.At(less->rhs);
    const mir::Type& type = unit_->types.Get(found_.variable_type);
    return found_.bound.Get().type == found_.variable_type &&
           type.IsIntegral() &&
           type.Integral().state_kind == mir::IntegralStateKind::kTwoState;
  }

  // Each pass ends by adding one to the variable.
  auto ReadStep(Node step) -> bool {
    const Chain chain = ChainOf(step);
    for (const mir::Block* steps : chain.steps) {
      if (!steps->root_stmts.empty()) {
        return false;
      }
    }
    const mir::Expr& write = chain.value.Get();
    std::optional<Node> target;
    std::optional<Node> value;
    if (const auto* assign = std::get_if<mir::AssignExpr>(&write.data)) {
      if (found_.induction.in_cell) {
        return false;
      }
      target = chain.value.At(assign->target);
      value = chain.value.At(assign->value);
    } else if (
        const mir::CallExpr* set =
            BuiltinCallOf(write, support::BuiltinFn::kSet)) {
      if (!found_.induction.in_cell || set->arguments.size() != 1) {
        return false;
      }
      target = chain.value.At(ReceiverOf(*set));
      value = chain.value.At(set->arguments.front());
    } else {
      return false;
    }
    if (UnderlyingObjectOf(*unit_, *target) != found_.induction.object) {
      return false;
    }
    const auto* sum = std::get_if<mir::BinaryExpr>(&value->Get().data);
    if (sum == nullptr || sum->op != mir::BinaryOp::kAdd ||
        !IsVariableRead(Resolved(value->At(sum->lhs))) ||
        !IsOne(value->At(sum->rhs))) {
      return false;
    }
    found_.written_variable = *target;
    step_write_ = chain.value;
    return true;
  }

  // The body declares what it computes and makes one write: no other
  // statement, and nothing a `break` leaves.
  auto WalkBody(const mir::Block& block) -> bool {
    for (const mir::StmtId id : block.root_stmts) {
      const mir::Stmt& stmt = block.stmts.Get(id);
      if (const auto* decl = std::get_if<mir::LocalDeclStmt>(&stmt.data)) {
        found_.locals.insert_or_assign(
            decl->target, Node{.block = &block, .id = decl->init});
        continue;
      }
      if (const auto* nested = std::get_if<mir::BlockStmt>(&stmt.data)) {
        if (nested->break_label.has_value() ||
            !WalkBody(block.child_scopes.Get(nested->scope))) {
          return false;
        }
        continue;
      }
      if (const auto* evaluated = std::get_if<mir::ExprStmt>(&stmt.data)) {
        if (move_stmt_ != nullptr) {
          return false;
        }
        move_stmt_ = &stmt;
        move_expr_ = Node{.block = &block, .id = evaluated->expr};
        continue;
      }
      return false;
    }
    return true;
  }

  // The one write sets one bit of a place to one bit read: at the steps of
  // block expressions that only declare, the last of them holding the write.
  auto ReadMove() -> bool {
    const Chain chain = ChainOf(move_expr_);
    for (std::size_t i = 0; i < chain.steps.size(); ++i) {
      const bool last = i + 1 == chain.steps.size();
      if (last ? !Declares(*chain.steps[i])
               : !chain.steps[i]->root_stmts.empty()) {
        return false;
      }
    }
    const Node assign = chain.value;
    found_.moved_in = assign.block;
    const auto* write = std::get_if<mir::AssignExpr>(&assign.Get().data);
    if (write == nullptr) {
      return false;
    }
    const Node bit = assign.At(write->target);
    const auto* written = std::get_if<mir::SliceExpr>(&bit.Get().data);
    if (written == nullptr) {
      return false;
    }
    found_.bit_type = bit.Get().type;
    const mir::Type& type = unit_->types.Get(found_.bit_type);
    if (!type.IsIntegral() || type.Integral().bit_width != 1) {
      return false;
    }
    found_.destination = written->receiver;
    found_.written_at = written->start;
    Node value = assign.At(write->value);
    if (const std::optional<Node> init = LoopLocalRead(value, found_.locals)) {
      if (init->block != assign.block) {
        return false;
      }
      const auto& read = std::get<mir::CopyExpr>(value.Get().data);
      found_.held =
          mir::ReferencedLocal(value.block->exprs.Get(read.place).data);
      value = *init;
    }
    const auto* copy = std::get_if<mir::CopyExpr>(&value.Get().data);
    if (copy == nullptr || value.Get().type != found_.bit_type) {
      return false;
    }
    read_bit_ = value.At(copy->place);
    const auto* read = std::get_if<mir::SliceExpr>(&read_bit_.Get().data);
    if (read == nullptr || read_bit_.Get().type != found_.bit_type) {
      return false;
    }
    found_.source = read->receiver;
    found_.read_at = read->start;
    effects_ = {assign, bit};
    for (Node reached = bit.At(found_.destination);;) {
      if (const auto* outer =
              std::get_if<mir::SliceExpr>(&reached.Get().data)) {
        reached = reached.At(outer->receiver);
        continue;
      }
      if (const auto* deref =
              std::get_if<mir::DerefExpr>(&reached.Get().data)) {
        reached = reached.At(deref->pointer);
        continue;
      }
      const mir::CallExpr* call = ReachesWritten(reached.Get());
      if (call == nullptr) {
        break;
      }
      effects_.push_back(reached);
      reached = reached.At(ReceiverOf(*call));
    }
    return true;
  }

  // Records what `steps` declares, which is all it does.
  auto Declares(const mir::Block& steps) -> bool {
    const auto declares = [&](mir::StmtId id) {
      return std::holds_alternative<mir::LocalDeclStmt>(
          steps.stmts.Get(id).data);
    };
    if (!std::ranges::all_of(steps.root_stmts, declares)) {
      return false;
    }
    for (const mir::StmtId id : steps.root_stmts) {
      const auto& decl = std::get<mir::LocalDeclStmt>(steps.stmts.Get(id).data);
      found_.locals.insert_or_assign(
          decl.target, Node{.block = &steps, .id = decl.init});
    }
    return true;
  }

  // Whether every node of `block` and the blocks in it computes a value and
  // changes nothing, but the step's write and the move, and whether what it
  // reads is never what the move writes, but the bits the move reads.
  auto Scan(const mir::Block& block) -> bool {
    const auto admitted_stmt = [&](const mir::Stmt& stmt) {
      if (const auto* decl = std::get_if<mir::LocalDeclStmt>(&stmt.data)) {
        found_.locals.try_emplace(
            decl->target, Node{.block = &block, .id = decl->init});
        return true;
      }
      const auto* nested = std::get_if<mir::BlockStmt>(&stmt.data);
      return (nested != nullptr && !nested->break_label.has_value()) ||
             &stmt == move_stmt_;
    };
    const auto admitted_expr = [&](mir::ExprId id) {
      return Admits(Node{.block = &block, .id = id});
    };
    const auto scanned = [&](const mir::Block& nested) { return Scan(nested); };
    return std::ranges::all_of(block.stmts, admitted_stmt) &&
           std::ranges::all_of(block.exprs.Ids(), admitted_expr) &&
           std::ranges::all_of(block.child_scopes, scanned);
  }

  auto Admits(Node node) -> bool {
    const mir::Expr& expr = node.Get();
    const mir::ExprData& data = expr.data;
    if (node == step_write_ ||
        std::ranges::find(effects_, node) != effects_.end()) {
      return true;
    }
    if (const auto* read = std::get_if<mir::CopyExpr>(&data)) {
      const Node place = node.At(read->place);
      if (found_.held.has_value() &&
          mir::ReferencedLocal(place.Get().data) == found_.held) {
        ++held_reads_;
      }
      return ReadsApart(place);
    }
    if (std::holds_alternative<mir::CallExpr>(data)) {
      return CallsOnlyToCompute(expr);
    }
    return std::holds_alternative<mir::MachineBoolLiteral>(data) ||
           std::holds_alternative<mir::MachineIntLiteral>(data) ||
           std::holds_alternative<mir::ReferenceExpr>(data) ||
           std::holds_alternative<mir::UnaryExpr>(data) ||
           std::holds_alternative<mir::BinaryExpr>(data) ||
           std::holds_alternative<mir::CastExpr>(data) ||
           std::holds_alternative<mir::ConditionalExpr>(data) ||
           std::holds_alternative<mir::BlockExpr>(data) ||
           std::holds_alternative<mir::DerefExpr>(data) ||
           std::holds_alternative<mir::FieldAccessExpr>(data) ||
           std::holds_alternative<mir::SliceExpr>(data);
  }

  // Whether reading `place` leaves what the move writes alone, or is the read
  // the move takes its bit from.
  auto ReadsApart(Node place) -> bool {
    const std::optional<UnderlyingObject> read =
        UnderlyingObjectOf(*unit_, place);
    const std::optional<UnderlyingObject> written = WrittenObject();
    if (!read.has_value() || !written.has_value()) {
      return false;
    }
    if (place == read_bit_ ||
        (!read->through_pointer && found_.locals.contains(read->root))) {
      return true;
    }
    return !MayAlias(*read, *written, receiver_);
  }

  [[nodiscard]] auto WrittenObject() const -> std::optional<UnderlyingObject> {
    return UnderlyingObjectOf(
        *unit_, Node{.block = found_.moved_in, .id = found_.destination});
  }

  // The read `node` follows to what it is the value of: a local the loop
  // declares is its initializer.
  [[nodiscard]] auto Resolved(Node node) const -> Node {
    const std::optional<Node> init = LoopLocalRead(node, found_.locals);
    return init.has_value() ? Resolved(*init) : node;
  }

  [[nodiscard]] auto IsVariableRead(Node node) const -> bool {
    return InductionVariableReadBy(*unit_, node) == found_.induction;
  }

  // Whether `node` is the constant 1 at the loop variable's type.
  [[nodiscard]] auto IsOne(Node node) const -> bool {
    const mir::IntegralConstant* constant =
        mir::IntegralPlanesNamedBy(unit_->constants, node.Get().data);
    if (constant == nullptr || node.Get().type != found_.variable_type ||
        constant->value_words.empty() || constant->value_words.front() != 1) {
      return false;
    }
    const auto is_zero = [](std::uint64_t word) { return word == 0; };
    return std::ranges::all_of(
               constant->value_words | std::views::drop(1), is_zero) &&
           std::ranges::all_of(constant->state_words, is_zero);
  }

  auto Finish() -> std::optional<BitStoreOfLoad> {
    const LoopLocals& locals = found_.locals;
    const LoopEvolution evolution(*unit_, receiver_, locals, found_.induction);
    const Node written_at{.block = found_.moved_in, .id = found_.written_at};
    const Node read_at{.block = found_.moved_in, .id = found_.read_at};
    const Node destination{.block = found_.moved_in, .id = found_.destination};
    const Node source{.block = found_.moved_in, .id = found_.source};
    const std::optional<AddRecurrence> written =
        evolution.AddRecurrenceOf(written_at);
    const std::optional<AddRecurrence> read =
        evolution.AddRecurrenceOf(read_at);
    const std::optional<UnderlyingObject> written_object = WrittenObject();
    const auto expandable = [&](Node node) {
      return IsSafeToExpand(node, locals);
    };
    if (!written.has_value() || !read.has_value() ||
        !written_object.has_value() ||
        !mir::IsPositionType(unit_->types, written_at.Get().type) ||
        !mir::IsPositionType(unit_->types, read_at.Get().type) ||
        !CheckedForWrap(*written) || !CheckedForWrap(*read) ||
        !evolution.IsLoopInvariant(destination) ||
        !evolution.IsLoopInvariant(source) ||
        !evolution.IsLoopInvariant(found_.bound) ||
        !std::ranges::all_of(
            std::array{
                destination, source, written_at, read_at, found_.bound,
                found_.written_variable},
            expandable) ||
        locals.contains(written_object->root) ||
        MayAlias(*written_object, found_.induction.object, receiver_)) {
      return std::nullopt;
    }
    found_.written_recurrence = *written;
    found_.read_recurrence = *read;
    const std::optional<std::int64_t> trips = ConstantTripCount();
    if (!trips.has_value() || *trips <= 0) {
      return std::nullopt;
    }
    found_.trip_count = static_cast<std::uint64_t>(*trips);
    if (mir::IsPlace(source.Get().data)) {
      const std::optional<UnderlyingObject> read_object =
          UnderlyingObjectOf(*unit_, source);
      if (!read_object.has_value()) {
        return std::nullopt;
      }
      if (MayAlias(*read_object, *written_object, receiver_)) {
        // The bits read are compared with the bits written only where both
        // are steps of one place, numbered alike.
        if (*read_object != *written_object ||
            std::holds_alternative<mir::SliceExpr>(destination.Get().data) ||
            std::holds_alternative<mir::SliceExpr>(source.Get().data)) {
          return std::nullopt;
        }
        found_.same_object = true;
      }
    }
    return found_;
  }

  // Whether a sum that may wrap is narrow enough that whether it does is
  // asked of the position it becomes.
  static auto CheckedForWrap(const AddRecurrence& recurrence) -> bool {
    return !recurrence.wraps_at.has_value() ||
           recurrence.wraps_at->bit_width <= 32;
  }

  const mir::CompilationUnit* unit_;
  std::optional<mir::LocalId> receiver_;
  BitStoreOfLoad found_;
  const mir::Stmt* move_stmt_ = nullptr;
  Node move_expr_;
  Node step_write_;
  // The bit the move reads, a step of the place it reads.
  Node read_bit_;
  // The move's write and every call reaching what it writes.
  std::vector<Node> effects_;
  int held_reads_ = 0;
  std::optional<Node> entering_;
};

// Calls `visit` with each block of `block`, `block` first.
void ForEachBlock(
    const mir::Block& block,
    const std::function<void(const mir::Block&)>& visit) {
  visit(block);
  for (const mir::Block& nested : block.child_scopes) {
    ForEachBlock(nested, visit);
  }
}

// Builds, ahead of a loop moving one bit a pass, the move of the run it
// moves, taken where moving it gives every bit what the loop would and
// leaving the loop variable where the loop would end, so the loop as written
// then makes no pass; and where it would not, the loop runs as written.
class RunBuilder {
 public:
  // The locals the run is computed in are declared in `locals`.
  RunBuilder(
      const mir::CompilationUnit& unit, const BitStoreOfLoad& loop,
      base::Arena<mir::LocalDecl, mir::LocalId>& locals)
      : unit_(&unit),
        loop_(&loop),
        position_(mir::PositionType(unit.types)),
        truth_(
            mir::PackedVectorOf(
                unit.types, 1,
                unit.types.Get(position_).Integral().state_kind)),
        run_(
            mir::PackedVectorOf(
                unit.types, loop.trip_count,
                unit.types.Get(loop.bit_type).Integral().state_kind)),
        start_written_(locals.Add(mir::LocalDecl{.type = position_})),
        start_read_(locals.Add(mir::LocalDecl{.type = position_})),
        source_value_(SourceValueIn(loop, locals)) {
  }

  // The statements preceding the loop: where the run starts, as the loop's
  // first pass computes it, then the run moved where that is exact. Answers
  // the loop itself, carried over as it is.
  auto Build(
      BlockRebuild& rebuild, const mir::Stmt& loop, diag::SourceSpan span)
      -> mir::StmtData {
    const Into into = [&](mir::Expr expr) {
      return rebuild.Add(std::move(expr));
    };
    const auto declare = [&](mir::LocalId local, mir::ExprId init) {
      rebuild.Precede(
          rebuild.Add(mir::LocalDeclStmt{.target = local, .init = init}));
    };
    const auto expanded = [&](mir::ExprId id) {
      return ExpandCode(
          Node{.block = loop_->moved_in, .id = id}, into, loop_->locals);
    };
    declare(start_read_, expanded(loop_->read_at));
    declare(start_written_, expanded(loop_->written_at));
    rebuild.Precede(rebuild.Add(
        mir::IfStmt{
            .condition = RuntimeChecks(rebuild.To(), into),
            .then_scope = rebuild.Add(RunMoved(span)),
            .else_scope = std::nullopt}));
    return rebuild.WithOperandsCarried(loop.data);
  }

 private:
  // Where the bits are read from a value the loop computes rather than from
  // storage, the local that value is computed once into.
  static auto SourceValueIn(
      const BitStoreOfLoad& loop,
      base::Arena<mir::LocalDecl, mir::LocalId>& locals)
      -> std::optional<mir::LocalId> {
    const mir::Expr& source = loop.moved_in->exprs.Get(loop.source);
    if (mir::IsPlace(source.data)) {
      return std::nullopt;
    }
    return locals.Add(mir::LocalDecl{.type = source.type});
  }

  [[nodiscard]] auto Literal(std::uint64_t value, const Into& into) const
      -> mir::ExprId {
    return into(
        mir::MakeIntegralLiteral(
            *unit_, position_,
            mir::IntegralConstant{.value_words = {value}, .state_words = {0}}));
  }

  // How many bits the run is: the loop's trip count.
  [[nodiscard]] auto TripCount(const Into& into) const -> mir::ExprId {
    return Literal(loop_->trip_count, into);
  }

  [[nodiscard]] auto Local(mir::LocalId local, const Into& into) const
      -> mir::ExprId {
    return into(
        mir::MakeCopyExpr(
            into(mir::MakeLocalRefExpr(local, position_)), position_));
  }

  // Whether `op` holds of `lhs` and `rhs`, as the machine boolean a
  // condition tests.
  [[nodiscard]] auto Holds(
      const mir::Block& block, const Into& into, mir::BinaryOp op,
      mir::ExprId lhs, mir::ExprId rhs) const -> mir::ExprId {
    const mir::ExprId held =
        into(mir::MakeBinary(*unit_, block, op, lhs, rhs, truth_));
    return into(
        mir::Expr{
            .data = mir::CastExpr{.operand = held},
            .type = unit_->builtins.machine_bool});
  }

  [[nodiscard]] auto Truth(bool value, const Into& into) const -> mir::ExprId {
    return into(
        mir::Expr{
            .data = mir::MachineBoolLiteral{.value = value},
            .type = unit_->builtins.machine_bool});
  }

  // Whether both `first` and `second` hold, `second` asked only where `first`
  // does.
  [[nodiscard]] auto AllOf(
      const Into& into, mir::ExprId first, mir::ExprId second) const
      -> mir::ExprId {
    return into(
        mir::Expr{
            .data =
                mir::ConditionalExpr{
                    .condition = first,
                    .then_value = second,
                    .else_value = Truth(false, into)},
            .type = unit_->builtins.machine_bool});
  }

  [[nodiscard]] auto Sum(
      const mir::Block& block, const Into& into, mir::BinaryOp op,
      mir::ExprId lhs, mir::ExprId rhs) const -> mir::ExprId {
    return into(mir::MakeBinary(*unit_, block, op, lhs, rhs, position_));
  }

  // The runtime checks that writing the run at once leaves every bit as the
  // loop would, as LLVM's vectorizer states its own ahead of a loop. A position
  // summed where it can wrap must not wrap over the run, since past that the
  // loop's positions start again where the run's go on; and a run read from
  // the value it writes must read ahead of what it has written, or far enough
  // behind it that no part reads bits an earlier part wrote (LRM 12.7.1: each
  // pass reads what the earlier ones left).
  [[nodiscard]] auto RuntimeChecks(
      const mir::Block& block, const Into& into) const -> mir::ExprId {
    std::vector<mir::ExprId> conditions;
    const auto unwrapped = [&](const AddRecurrence& recurrence,
                               mir::LocalId start) {
      if (!recurrence.wraps_at.has_value()) {
        return;
      }
      const mir::IntegralType& at = *recurrence.wraps_at;
      const std::uint64_t first_wrapped =
          at.signedness == mir::Signedness::kSigned
              ? std::uint64_t{1} << (at.bit_width - 1)
              : std::uint64_t{1} << at.bit_width;
      conditions.push_back(Holds(
          block, into, mir::BinaryOp::kLessEqual,
          Sum(block, into, mir::BinaryOp::kAdd, Local(start, into),
              TripCount(into)),
          Literal(first_wrapped, into)));
    };
    unwrapped(loop_->written_recurrence, start_written_);
    unwrapped(loop_->read_recurrence, start_read_);
    if (loop_->same_object) {
      const mir::ExprId ahead = Holds(
          block, into, mir::BinaryOp::kGreaterEqual, Local(start_read_, into),
          Local(start_written_, into));
      const mir::ExprId behind_by_the_run = Holds(
          block, into, mir::BinaryOp::kGreaterEqual,
          Sum(block, into, mir::BinaryOp::kSub, Local(start_written_, into),
              Local(start_read_, into)),
          TripCount(into));
      conditions.push_back(into(
          mir::Expr{
              .data =
                  mir::ConditionalExpr{
                      .condition = ahead,
                      .then_value = Truth(true, into),
                      .else_value = behind_by_the_run},
              .type = unit_->builtins.machine_bool}));
    }
    mir::ExprId all = Truth(true, into);
    for (const mir::ExprId condition : conditions) {
      all = AllOf(into, condition, all);
    }
    return all;
  }

  // The run written as one assignment of a select as wide as the run, of the
  // select of the run read, and the variable left where the loop ends it, at
  // its bound. Each select names its bits as the one-bit selects of the passes
  // would (LRM 11.5.1), so the run reads x and writes nothing at the positions
  // the passes would.
  [[nodiscard]] auto RunMoved(diag::SourceSpan span) const -> mir::Block {
    mir::Block block;
    mir::Builder body(block, span);
    const Into into = [&](mir::Expr expr) { return body.Add(std::move(expr)); };
    if (source_value_.has_value()) {
      body.AppendStmt(
          mir::LocalDeclStmt{
              .target = *source_value_,
              .init = ExpandCode(
                  Node{.block = loop_->moved_in, .id = loop_->source}, into,
                  loop_->locals)});
    }
    const mir::ExprId read = into(
        mir::MakeCopyExpr(
            into(
                mir::MakeSliceExpr(
                    Source(into), Local(start_read_, into), run_)),
            run_));
    const mir::ExprId written = into(
        mir::MakeSliceExpr(
            ExpandCode(
                Node{.block = loop_->moved_in, .id = loop_->destination}, into,
                loop_->locals),
            Local(start_written_, into), run_));
    body.AppendStmt(
        mir::ExprStmt{
            .expr = into(mir::MakeAssignExpr(unit_->builtins, written, read))});
    body.AppendStmt(
        mir::ExprStmt{
            .expr = WriteVariable(
                body, ExpandCode(loop_->bound, into, loop_->locals))});
    return block;
  }

  // What the bits are read from: the storage the loop reads them from, or the
  // value it reads them from, computed once.
  [[nodiscard]] auto Source(const Into& into) const -> mir::ExprId {
    const Node source{.block = loop_->moved_in, .id = loop_->source};
    if (!source_value_.has_value()) {
      return ExpandCode(source, into, loop_->locals);
    }
    return into(mir::MakeLocalRefExpr(*source_value_, source.Get().type));
  }

  // The write of `value` to the loop variable, as the loop's step writes it.
  auto WriteVariable(mir::Builder& body, mir::ExprId value) const
      -> mir::ExprId {
    const Into into = [&](mir::Expr expr) { return body.Add(std::move(expr)); };
    const mir::ExprId target =
        ExpandCode(loop_->written_variable, into, loop_->locals);
    if (!loop_->induction.in_cell) {
      return into(mir::MakeAssignExpr(unit_->builtins, target, value));
    }
    return into(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kSet,
                            .receiver = target},
                    .arguments = {value}},
            .type = unit_->builtins.void_type});
  }

  const mir::CompilationUnit* unit_;
  const BitStoreOfLoad* loop_;
  mir::TypeId position_;
  mir::TypeId truth_;
  // The type of the run: as many bits as the loop makes passes.
  mir::TypeId run_;
  // Where the run starts in the value written and in the bits read, as the
  // loop's first pass computes them.
  mir::LocalId start_written_;
  mir::LocalId start_read_;
  std::optional<mir::LocalId> source_value_;
};

}  // namespace

auto LoopIdiomRecognizeOnBody(
    const PassContext& context, const mir::CallableCode& code)
    -> std::optional<mir::CallableCode> {
  if (!code.body.has_value()) {
    return std::nullopt;
  }
  const mir::CompilationUnit& unit = *context.unit;
  std::map<const mir::Stmt*, BitStoreOfLoad> loops;
  base::Arena<mir::LocalDecl, mir::LocalId> locals = code.locals;
  ForEachBlock(*code.body, [&](const mir::Block& block) {
    for (const mir::Stmt& stmt : block.stmts) {
      const auto* loop = std::get_if<mir::ForStmt>(&stmt.data);
      if (loop == nullptr) {
        continue;
      }
      LoopReading reading(unit, code.receiver);
      std::optional<BitStoreOfLoad> found = reading.Read(block, *loop);
      if (found.has_value()) {
        loops.emplace(&stmt, *std::move(found));
      }
    }
  });
  if (loops.empty()) {
    return std::nullopt;
  }
  const RebuildRules rules{
      .stmt = [&](BlockRebuild& rebuild,
                  const mir::Stmt& stmt) -> std::optional<mir::StmtData> {
        const auto found = loops.find(&stmt);
        if (found == loops.end()) {
          return std::nullopt;
        }
        return RunBuilder(unit, found->second, locals)
            .Build(rebuild, stmt, stmt.span);
      }};
  mir::CallableCode rewritten = RebuildCode(code, rules);
  rewritten.locals = std::move(locals);
  return rewritten;
}

}  // namespace lyra::mir_transform

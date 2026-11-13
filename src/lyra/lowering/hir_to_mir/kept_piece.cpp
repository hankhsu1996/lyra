#include "lyra/lowering/hir_to_mir/kept_piece.hpp"

#include <algorithm>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/constraint.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/hir/operands.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// What a part of the operand answers with as of an earlier tick.
enum class PastAnswer {
  // What it was at that tick, which a history has to have kept.
  kKept,
  // The same at every tick: it reads nothing that changes.
  kFixed,
  // What it is where the operand is read: it reads something whose past value
  // is its current one, or is called there.
  kNow,
};

// Whether a part answering `answer` is what a history keeps.
auto IsKept(PastAnswer answer) -> bool {
  switch (answer) {
    case PastAnswer::kKept:
      return true;
    case PastAnswer::kFixed:
    case PastAnswer::kNow:
      return false;
  }
  throw InternalError("IsKept: unknown PastAnswer");
}

// The arenas the expressions of one operand are in: a body's, or a scope's
// own, which holds no constraint and no property.
struct OperandArenas {
  const base::Arena<hir::Expr, hir::ExprId>* exprs;
  hir::TreeArenas trees;
};

auto PiecesOf(const OperandArenas& arenas, hir::ExprId call)
    -> std::vector<KeptPiece>;

struct PieceWalk {
  const OperandArenas* arenas;
  std::vector<KeptPiece>* kept;

  // The expressions `expr` is made of, each where `expr` evaluates it.
  [[nodiscard]] auto Parts(const hir::Expr& expr) const
      -> std::vector<hir::ExprId> {
    std::vector<hir::ExprId> parts;
    const auto collect = [&](hir::ExprId part) { parts.push_back(part); };
    hir::ForEachOperand(
        expr.data, hir::EvaluatedOperands<decltype(collect)>{
                       .trees = arenas->trees, .visit = &collect});
    return parts;
  }

  void Keep(KeptPiece piece) const {
    if (std::ranges::find(*kept, piece) == kept->end()) {
      kept->push_back(std::move(piece));
    }
  }

  // Whether `id` reads a queue's `$` that a select around it states the queue
  // of (LRM 7.10.1): within a select, its own base is the queue its bounds
  // count from, so only the base can read one from further out.
  [[nodiscard]] auto ReadsAnEnclosingQueuesLast(hir::ExprId id) const -> bool {
    const hir::Expr& expr = arenas->exprs->Get(id);
    if (const auto* primary = std::get_if<hir::PrimaryExpr>(&expr.data)) {
      return std::holds_alternative<hir::QueueLastIndex>(primary->data);
    }
    if (const auto* sel = std::get_if<hir::ElementSelectExpr>(&expr.data)) {
      return ReadsAnEnclosingQueuesLast(sel->base_value);
    }
    if (const auto* sel = std::get_if<hir::RangeSelectExpr>(&expr.data)) {
      return ReadsAnEnclosingQueuesLast(sel->base_value);
    }
    return std::ranges::any_of(Parts(expr), [&](hir::ExprId part) {
      return ReadsAnEnclosingQueuesLast(part);
    });
  }

  // Whether `id` is evaluated on its own, as a history's sampler evaluates
  // what it keeps and the read asks for it: a range of values is only a
  // bound of the membership or the case item it stands in, and a part reading
  // a `$` is evaluated within the select it counts from.
  [[nodiscard]] auto StandsAlone(hir::ExprId id) const -> bool {
    return !std::holds_alternative<hir::ValueRangeExpr>(
               arenas->exprs->Get(id).data) &&
           !ReadsAnEnclosingQueuesLast(id);
  }

  // Keeps a kept part of an expression read where the operand is: the part
  // itself where it stands alone, and otherwise each of its own kept parts,
  // which its parent evaluates it from.
  void KeepPart(hir::ExprId part) const {
    if (StandsAlone(part)) {
      Keep(KeptPiece{.piece = part, .through = {}});
      return;
    }
    for (const hir::ExprId inner : Parts(arenas->exprs->Get(part))) {
      if (IsKept(Answer(inner))) KeepPart(inner);
    }
  }

  // The answer of an expression that is no more than what it is made of: kept
  // where every part is kept or fixed and one at least is kept, fixed where
  // every part is fixed, and otherwise read where the operand is, with each
  // kept part a piece of its own.
  [[nodiscard]] auto AnswerOfParts(const hir::Expr& expr) const -> PastAnswer {
    std::vector<std::pair<hir::ExprId, PastAnswer>> parts;
    for (const hir::ExprId part : Parts(expr)) {
      parts.emplace_back(part, Answer(part));
    }
    bool any_now = false;
    bool any_kept = false;
    for (const auto& part : parts) {
      switch (part.second) {
        case PastAnswer::kKept:
          any_kept = true;
          break;
        case PastAnswer::kFixed:
          break;
        case PastAnswer::kNow:
          any_now = true;
          break;
      }
    }
    if (!any_now) return any_kept ? PastAnswer::kKept : PastAnswer::kFixed;
    for (const auto& [part, answer] : parts) {
      if (IsKept(answer)) KeepPart(part);
    }
    return PastAnswer::kNow;
  }

  // An expression evaluated where the operand is read whatever its parts
  // answer, whose kept parts are pieces of their own.
  [[nodiscard]] auto AnswerAtTheRead(const hir::Expr& expr) const
      -> PastAnswer {
    for (const hir::ExprId part : Parts(expr)) {
      if (IsKept(Answer(part))) KeepPart(part);
    }
    return PastAnswer::kNow;
  }

  // A sampled value function inside the operand reads what its own history
  // kept, so what this one keeps of each of those pieces is that history's
  // reading of it at each of this one's ticks; a value change function reads
  // its pieces as of the current step too, which this one keeps as they are.
  [[nodiscard]] auto AnswerThroughInnerHistory(
      hir::ExprId inner, bool now_too) const -> PastAnswer {
    for (KeptPiece piece : PiecesOf(*arenas, inner)) {
      if (now_too) Keep(piece);
      piece.through.insert(piece.through.begin(), inner);
      Keep(std::move(piece));
    }
    return PastAnswer::kNow;
  }

  [[nodiscard]] auto AnswerOfCall(
      hir::ExprId id, const hir::Expr& expr, const hir::CallExpr& call) const
      -> PastAnswer {
    return std::visit(
        Overloaded{
            [&](const hir::PastValueRef&) {
              return AnswerThroughInnerHistory(id, false);
            },
            [&](const hir::ValueChangeRef&) {
              return AnswerThroughInnerHistory(id, true);
            },
            // A call is made where the operand is read (LRM 16.5.1): what it
            // answers may follow the state of the run, an object's state or
            // what no argument names, so only its arguments' past values are
            // what a history keeps.
            [&](const hir::BuiltinMethodRef& method) {
              // A triggered state is sampled in the Postponed region of the
              // past tick (LRM 16.5.1), which no history here takes, so a
              // sampled value function reading one is refused before HIR.
              if (method.method == support::BuiltinFn::kTriggered) {
                throw InternalError(
                    "KeptPieces: a past value of a triggered state reached "
                    "lowering -- please report this as a bug");
              }
              return AnswerAtTheRead(expr);
            },
            [&](const hir::EnumMethodRef&) { return AnswerAtTheRead(expr); },
            [&](const hir::ArrayIteratorMethodRef&) {
              return AnswerAtTheRead(expr);
            },
            [&](const hir::SystemSubroutineRef&) {
              return AnswerAtTheRead(expr);
            },
            [&](const hir::StructuralSubroutineRef&) {
              return AnswerAtTheRead(expr);
            },
            [&](const hir::MethodCallRef&) { return AnswerAtTheRead(expr); },
            [&](const hir::StaticMethodCallRef&) {
              return AnswerAtTheRead(expr);
            },
            [&](const hir::ForeignImportRef&) { return AnswerAtTheRead(expr); },
            [&](const hir::NamespaceSubroutineRef&) {
              return AnswerAtTheRead(expr);
            },
            [&](const hir::PublishedScopeMethodRef&) {
              return AnswerAtTheRead(expr);
            }},
        call.callee);
  }

  [[nodiscard]] auto AnswerOfPrimary(const hir::Primary& p) const
      -> PastAnswer {
    return std::visit(
        Overloaded{
            [](const hir::IntegerLiteral&) { return PastAnswer::kFixed; },
            [](const hir::StringLiteral&) { return PastAnswer::kFixed; },
            [](const hir::RealLiteral&) { return PastAnswer::kFixed; },
            [](const hir::NullLiteral&) { return PastAnswer::kFixed; },
            // `$` reads nothing that changes of its own: the queue it counts
            // from is the select's around it, and a part reading it is kept
            // only within that select.
            [](const hir::QueueLastIndex&) { return PastAnswer::kFixed; },
            [](const hir::RoutedObjectRef&) { return PastAnswer::kFixed; },
            [](const hir::ScopeArgumentRef&) { return PastAnswer::kFixed; },
            // A variable a body declares is automatic, a static one being
            // the scope's or the class's (LRM 6.21).
            [](const hir::ProceduralVarRef&) { return PastAnswer::kNow; },
            // What an evaluation binds exists only within it.
            [](const hir::IterationBindingRef&) { return PastAnswer::kNow; },
            [](const hir::PatternVarRef&) { return PastAnswer::kNow; },
            [](const hir::ThisHandle&) { return PastAnswer::kNow; },
            [](const hir::RandomizedObject&) { return PastAnswer::kNow; },
            [](const hir::ClassPropertyRef&) { return PastAnswer::kKept; },
            [](const hir::StaticPropertyRef&) { return PastAnswer::kKept; },
            [](const hir::RoutedValueRef&) { return PastAnswer::kKept; },
            [](const hir::RoutedViewPlaceRef&) { return PastAnswer::kKept; },
            [](const hir::RoutedViewValueRef&) { return PastAnswer::kKept; },
            [](const hir::NamespaceValueRef&) { return PastAnswer::kKept; }},
        p);
  }

  [[nodiscard]] auto Answer(hir::ExprId id) const -> PastAnswer {
    const hir::Expr& expr = arenas->exprs->Get(id);
    return std::visit(
        Overloaded{
            [&](const hir::PrimaryExpr& p) { return AnswerOfPrimary(p.data); },
            [&](const hir::CallExpr& call) {
              return AnswerOfCall(id, expr, call);
            },
            // The sampled value of a constant cast, past ones included, is
            // its argument's current value (LRM 16.5.1).
            [&](const hir::ConversionExpr& conversion) {
              return IsConstCast(conversion.kind) ? PastAnswer::kNow
                                                  : AnswerOfParts(expr);
            },
            [](const hir::ConstantExpr&) { return PastAnswer::kFixed; },
            [&](const hir::UnaryExpr&) { return AnswerOfParts(expr); },
            [&](const hir::BinaryExpr&) { return AnswerOfParts(expr); },
            [&](const hir::ConditionalExpr&) { return AnswerOfParts(expr); },
            [&](const hir::ValueRangeExpr&) { return AnswerOfParts(expr); },
            [&](const hir::InsideExpr&) { return AnswerOfParts(expr); },
            [&](const hir::ElementSelectExpr&) { return AnswerOfParts(expr); },
            [&](const hir::RangeSelectExpr&) { return AnswerOfParts(expr); },
            [&](const hir::MemberAccessExpr&) { return AnswerOfParts(expr); },
            [&](const hir::ClassPropertyAccessExpr&) {
              return AnswerOfParts(expr);
            },
            [&](const hir::InterfaceMemberAccessExpr&) {
              return AnswerOfParts(expr);
            },
            [&](const hir::InterfaceInstanceAccessExpr&) {
              return AnswerOfParts(expr);
            },
            [&](const hir::ConcatExpr&) { return AnswerOfParts(expr); },
            [&](const hir::StreamingConcatExpr&) {
              return AnswerOfParts(expr);
            },
            [&](const hir::ReplicationExpr&) { return AnswerOfParts(expr); },
            [&](const hir::AssignmentPatternExpr&) {
              return AnswerOfParts(expr);
            },
            [&](const hir::AssignmentPatternReplicationExpr&) {
              return AnswerOfParts(expr);
            },
            [&](const hir::AssignmentPatternKeyedExpr&) {
              return AnswerOfParts(expr);
            },
            [&](const hir::AssociativeAssignmentPatternExpr&) {
              return AnswerOfParts(expr);
            },
            [&](const hir::TaggedUnionExpr&) { return AnswerOfParts(expr); },
            // What changes something, or makes something new, is done where
            // the operand is read.
            [&](const hir::AssignExpr&) { return AnswerAtTheRead(expr); },
            [&](const hir::IncDecExpr&) { return AnswerAtTheRead(expr); },
            [&](const hir::CalleeDefaultExpr&) {
              return AnswerAtTheRead(expr);
            },
            [&](const hir::DynamicArrayNewExpr&) {
              return AnswerAtTheRead(expr);
            },
            [&](const hir::ClassNewExpr&) { return AnswerAtTheRead(expr); },
            [&](const hir::DynamicCastExpr&) { return AnswerAtTheRead(expr); },
            [&](const hir::CovergroupNewExpr&) {
              return AnswerAtTheRead(expr);
            },
            [&](const hir::CovergroupMethodCallExpr&) {
              return AnswerAtTheRead(expr);
            },
            [&](const hir::CovergroupOptionExpr&) {
              return AnswerAtTheRead(expr);
            },
            [&](const hir::RandomizeExpr&) { return AnswerAtTheRead(expr); },
            [&](const hir::RandModeSetExpr&) { return AnswerAtTheRead(expr); },
            [&](const hir::RandModeGetExpr&) { return AnswerAtTheRead(expr); },
            [&](const hir::ConstraintModeSetExpr&) {
              return AnswerAtTheRead(expr);
            },
            [&](const hir::ConstraintModeGetExpr&) {
              return AnswerAtTheRead(expr);
            }},
        expr.data);
  }
};

auto PiecesOf(const OperandArenas& arenas, hir::ExprId call)
    -> std::vector<KeptPiece> {
  std::vector<KeptPiece> kept;
  const PieceWalk walk{.arenas = &arenas, .kept = &kept};
  const hir::ExprId subject = SampledOperandOf(arenas.exprs->Get(call));
  if (IsKept(walk.Answer(subject))) {
    walk.Keep(KeptPiece{.piece = subject, .through = {}});
  }
  return kept;
}

auto BodyArenas(const hir::ProceduralBody& body) -> OperandArenas {
  return OperandArenas{
      .exprs = &body.exprs,
      .trees = hir::TreeArenas{
          .patterns = &body.patterns,
          .properties = &body.property_exprs,
          .sequences = &body.sequence_exprs,
          .constraints = &body.constraints}};
}

auto ScopeArenas(const hir::StructuralScope& scope) -> OperandArenas {
  return OperandArenas{
      .exprs = &scope.exprs,
      .trees = hir::TreeArenas{
          .patterns = &scope.patterns,
          .properties = nullptr,
          .sequences = nullptr,
          .constraints = nullptr}};
}

}  // namespace

auto IsConstCast(hir::ConversionKind kind) -> bool {
  switch (kind) {
    case hir::ConversionKind::kConstCast:
      return true;
    case hir::ConversionKind::kImplicit:
    case hir::ConversionKind::kPropagated:
    case hir::ConversionKind::kStreamingConcat:
    case hir::ConversionKind::kExplicit:
    case hir::ConversionKind::kBitstreamCast:
      return false;
  }
  throw InternalError("IsConstCast: unknown hir::ConversionKind");
}

auto ProceduralVarRead(const hir::Expr& expr)
    -> std::optional<hir::ProceduralVarId> {
  const auto* primary = std::get_if<hir::PrimaryExpr>(&expr.data);
  const auto* var = primary == nullptr
                        ? nullptr
                        : std::get_if<hir::ProceduralVarRef>(&primary->data);
  if (var == nullptr) return std::nullopt;
  return var->var;
}

auto KeptPieces(const hir::ProceduralBody& body, hir::ExprId call)
    -> std::vector<KeptPiece> {
  return PiecesOf(BodyArenas(body), call);
}

auto KeptPieces(const hir::StructuralScope& scope, hir::ExprId call)
    -> std::vector<KeptPiece> {
  return PiecesOf(ScopeArenas(scope), call);
}

auto SampledClockOf(const hir::Expr& call) -> const hir::EventControl* {
  const auto* as_call = std::get_if<hir::CallExpr>(&call.data);
  if (as_call == nullptr) return nullptr;
  if (const auto* past = std::get_if<hir::PastValueRef>(&as_call->callee)) {
    return &past->clock;
  }
  if (const auto* change = std::get_if<hir::ValueChangeRef>(&as_call->callee)) {
    return &change->clock;
  }
  return nullptr;
}

auto SampledOperandOf(const hir::Expr& call) -> hir::ExprId {
  const auto* as_call = std::get_if<hir::CallExpr>(&call.data);
  if (as_call == nullptr || SampledClockOf(call) == nullptr ||
      as_call->arguments.empty() || !as_call->arguments.front().has_value()) {
    throw InternalError(
        "SampledOperandOf: a sampled value function states its operand first");
  }
  return *as_call->arguments.front();
}

auto SampledValueCalls(const base::Arena<hir::Expr, hir::ExprId>& exprs)
    -> std::vector<hir::ExprId> {
  std::vector<hir::ExprId> calls;
  for (const hir::ExprId id : exprs.Ids()) {
    if (SampledClockOf(exprs.Get(id)) != nullptr) calls.push_back(id);
  }
  return calls;
}

}  // namespace lyra::lowering::hir_to_mir

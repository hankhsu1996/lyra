#include "lyra/lowering/ast_to_hir/expression/calls.hpp"

#include <concepts>
#include <cstddef>
#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Scope.h>
#include <slang/ast/SystemSubroutine.h>
#include <slang/ast/TimingControl.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/numeric/SVInt.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/external_scope_class.hpp"
#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/sampled_history.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/expression/dynamic_cast.hpp"
#include "lyra/lowering/ast_to_hir/expression/expr_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/expression/query.hpp"
#include "lyra/lowering/ast_to_hir/expression/references.hpp"
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/expression/virtual_interface.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/statement/timing.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// True when the call reaches an instance method through its enclosing class
// -- either the source supplied a handle (`h.foo()`) or slang resolved an
// unqualified call to a class-scope subroutine, whose enclosing class is
// the receiver's own type (LRM 8.6 implicit `this`, including LRM 8.15
// `super.foo()`).
auto CallReachesInstanceMethod(
    const slang::ast::CallExpression& call,
    const slang::ast::SubroutineSymbol& sym) -> bool {
  if (call.thisClass() != nullptr) return true;
  const auto* parent = sym.getParentScope();
  return parent != nullptr &&
         parent->asSymbol().kind == slang::ast::SymbolKind::ClassType;
}

// Classifies which object an instance-method call runs against into a
// MethodReceiver arm. Super takes precedence over an accompanying `thisClass`,
// because `this.super.foo()` reaches the base through the `super` qualifier
// while still naming the outer `this` as the object. A receiver written as
// `this` (LRM 8.11) names the object the enclosing method was invoked on --
// the same object an unqualified call names -- so it takes the self arm and no
// handle is evaluated.
template <ExprLowerer Lowerer>
auto ClassifyMethodReceiver(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call)
    -> diag::Result<hir::MethodReceiver> {
  if (call.lookupInfo.viaSuper) return hir::SuperReceiver{};
  const auto* this_class = call.thisClass();
  if (this_class == nullptr || NamesCurrentInstance(*this_class)) {
    return hir::SelfReceiver{};
  }
  auto receiver_or = lowerer.LowerExpr(*this_class, frame);
  if (!receiver_or) return std::unexpected(std::move(receiver_or.error()));
  return hir::HandleReceiver{
      .expr = frame.Exprs().Add(*std::move(receiver_or))};
}

// Whether a variable answers for a sampled value with its current value, which
// an automatic variable does (LRM 16.5.1) -- and so does a `ref` formal that is
// not `ref static`, being usable only where an automatic variable is (LRM
// 13.5.2). A property of an object has a lifetime too, but it is the object's
// rather than a call's, and the clause does not reach it.
auto SampledAsCurrent(const slang::ast::ValueSymbol& symbol) -> bool {
  const auto* var = symbol.as_if<slang::ast::VariableSymbol>();
  return var != nullptr &&
         symbol.kind != slang::ast::SymbolKind::ClassProperty &&
         var->lifetime == slang::ast::VariableLifetime::Automatic &&
         !var->flags.has(slang::ast::VariableFlags::RefStatic);
}

// How the variables a sampled value function's operand reads answer for it:
// how many are cells this scope arms, and how many answer with what they hold
// now.
struct SampledOperandReads {
  std::size_t armed = 0;
  std::size_t current = 0;
};

// Records the cells a sampled value function's operand reads, so each is armed
// to answer for one (LRM 16.5.1). The sampled value of an expression is
// composed from the sampled values of the variables it reads, which makes those
// variables exactly what has to answer -- and they are the same set an event
// control watches, taken from the same analysis. A variable that answers with
// its current value needs nothing armed.
//
// The set only ever has to cover them: a cell armed that nothing samples holds
// a value nobody reads, while one left unarmed cannot answer at all. So a
// coarser read set is safe here in a way it is not for a wake-up.
template <typename Lowerer>
auto RecordSampledCells(
    Lowerer& lowerer, const WalkFrame& frame,
    const slang::ast::CallExpression& call, diag::SourceSpan span)
    -> diag::Result<SampledOperandReads> {
  if (call.arguments().empty()) {
    throw InternalError(
        "AST->HIR sampled value: no operand to take a sampled value of");
  }
  auto& unit_lowerer = lowerer.Owner();
  if (frame.reader_scope == nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a sampled value read outside a design element's scope is not yet "
        "supported");
  }
  const auto& reads = unit_lowerer.Sensitivity().AnalyzeReads(
      *call.arguments()[0], frame.reader_scope->asSymbol());
  std::vector<AccessedPart> armed;
  SampledOperandReads counted;
  for (const AccessedPart& read : reads) {
    const auto* var = read.symbol->as_if<slang::ast::VariableSymbol>();
    if (var != nullptr &&
        var->flags.has(slang::ast::VariableFlags::RefStatic)) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "a sampled value of a `ref static` argument is not yet supported");
    }
    if (SampledAsCurrent(*read.symbol)) {
      ++counted.current;
      continue;
    }
    armed.push_back(read);
  }
  counted.armed = armed.size();
  if (armed.empty()) return counted;
  if (frame.current_structural_scope == nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a sampled value read outside a design element's scope is not yet "
        "supported");
  }
  auto cells = unit_lowerer.CellsRead(armed, frame);
  if (!cells) return std::unexpected(std::move(cells.error()));
  // Every variable the operand reads has to be armed, so a read the translation
  // could not name as a cell of this design leaves one that can never answer.
  // The reachable case is a property of an object (LRM 8.4), which no scope
  // holds a cell of.
  if (cells->size() < armed.size()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a sampled value of storage this design element holds no cell of is "
        "not yet supported");
  }
  std::vector<hir::ValueTarget>& sampled =
      frame.current_structural_scope->sampled_cells;
  for (hir::ValueTarget& cell : *cells) {
    sampled.push_back(std::move(cell));
  }
  return counted;
}

// What a value change function answers of a value that is the same at every
// tick: that it is stable, and so that it neither rose, fell nor changed (LRM
// 16.9.3).
auto UnchangedAnswer(
    const UnitLowerer& unit_lowerer, support::ValueChangeReading reading,
    diag::SourceSpan span) -> hir::Expr {
  const bool holds = [&] {
    switch (reading) {
      case support::ValueChangeReading::kUnchanged:
        return true;
      case support::ValueChangeReading::kRoseToOne:
      case support::ValueChangeReading::kFellToZero:
      case support::ValueChangeReading::kChanged:
        return false;
    }
    throw InternalError("UnchangedAnswer: unknown ValueChangeReading");
  }();
  return hir::Expr{
      .type = unit_lowerer.Unit().builtins.scalar_bit,
      .data =
          hir::PrimaryExpr{
              .data =
                  hir::IntegerLiteral{
                      .value =
                          hir::IntegralConstant{
                              .value_words = {holds ? 1ULL : 0ULL},
                              .state_words = {},
                              .width = 1,
                              .signedness = hir::Signedness::kUnsigned,
                              .state_kind =
                                  hir::IntegralStateKind::kTwoState}}},
      .span = span};
}

// The argument the source wrote at `position`, or nothing where it wrote none.
// A sampled value function's optional arguments are positional, so one elided
// in the middle arrives as an empty argument rather than shortening the list,
// and one elided at the end shortens it -- both mean the source wrote nothing
// there.
auto WrittenArgument(
    const slang::ast::CallExpression& call, std::size_t position)
    -> const slang::ast::Expression* {
  const auto arguments = call.arguments();
  if (arguments.size() <= position ||
      arguments[position]->kind == slang::ast::ExpressionKind::EmptyArgument) {
    return nullptr;
  }
  return arguments[position];
}

// The clocking event a sampled value function counts ticks of. The source may
// write one at the call; where it does not, the front end has already applied
// the two of LRM 16.9.3's ordered rules that reach a call outside an assertion
// -- the clock the procedure settles (LRM 16.14.6), and then the enclosing
// scope's default clocking (LRM 14.12) -- so this reads that answer rather than
// working the ordering out a second time. A context settling neither is
// refused, which is the error the standard requires; sampling against a guess
// would answer wrongly rather than not at all.
template <typename Lowerer>
auto ResolveClockingEvent(
    Lowerer& lowerer, const slang::ast::CallExpression& call,
    std::size_t clock_arg, std::string_view name, diag::SourceSpan span)
    -> diag::Result<const slang::ast::TimingControl*> {
  const slang::ast::Expression* written = WrittenArgument(call, clock_arg);
  if (written != nullptr &&
      written->kind == slang::ast::ExpressionKind::ClockingEvent) {
    return &written->as<slang::ast::ClockingEventExpression>().timingControl;
  }
  if constexpr (std::same_as<Lowerer, ProcessLowerer>) {
    const auto* clock =
        lowerer.Owner().InferredProcedureClock(lowerer.ContainingSymbol());
    if (clock != nullptr) {
      return clock;
    }
  }
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedExpressionForm,
      std::string{"'"} + std::string{name} +
          "' names no clocking event and none is settled where it is written, "
          "so there is no event whose ticks it could count");
}

// How far back a read reaches: the expression the source wrote, or the 1 the
// standard defaults it to (LRM 16.9.3). Two readers need this same distance --
// the read itself, and the history whose entries have to go back that far --
// and they are built into different arenas, so each asks for its own and puts
// it where it belongs.
template <typename Lowerer>
auto LowerTicksBack(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::Expression* ticks_back,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  if (ticks_back == nullptr) {
    return hir::MakeIntLiteral(
        1, lowerer.Owner().Unit().builtins.int_type, span);
  }
  return lowerer.LowerExpr(*ticks_back, frame);
}

// What a sampled value function needs from the scope it is read in, once its
// expression's cells are armed: the scope has to keep a history of that
// expression under the event this call counts ticks of. The call then names
// that history, since which event it is was settled here.
//
// The subject and the event are lowered into the scope's own arena rather than
// the reader's: nothing the source wrote evaluates them, and what does is a
// process synthesized a layer down, the way a continuous assignment's
// expression is already carried.
template <typename Lowerer>
auto RecordSampledHistory(
    Lowerer& lowerer, const WalkFrame& frame,
    const slang::ast::CallExpression& call, std::size_t clock_arg,
    const slang::ast::Expression* ticks_back,
    const slang::ast::Expression* gate, std::string_view name,
    diag::SourceSpan span) -> diag::Result<hir::SampledHistoryId> {
  auto clock_or = ResolveClockingEvent(lowerer, call, clock_arg, name, span);
  if (!clock_or) return std::unexpected(std::move(clock_or.error()));

  WalkFrame scope_frame = frame;
  scope_frame.current_exprs = &frame.current_structural_scope->exprs;

  auto subject_or = lowerer.LowerExpr(*call.arguments()[0], scope_frame);
  if (!subject_or) return std::unexpected(std::move(subject_or.error()));

  if constexpr (!std::same_as<Lowerer, ProcessLowerer>) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
        std::string{"'"} + std::string{name} +
            "' is not yet supported outside a procedure");
  } else {
    auto event_or = LowerEventControl(lowerer, scope_frame, **clock_or, span);
    if (!event_or) return std::unexpected(std::move(event_or.error()));
    auto* value_change = std::get_if<hir::EventControl>(&*event_or);
    if (value_change == nullptr) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          std::string{"'"} + std::string{name} +
              "' counting ticks of a named event is not yet supported");
    }
    // The gate belongs to the event, and the event already carries a qualifier
    // of exactly that shape -- the LRM 9.4.2.3 `iff`, read when what is watched
    // moves -- so it composes with whatever the event stated rather than
    // needing a place of its own. A trigger the source already qualified admits
    // a tick only where both hold, and each trigger reads the gate for itself.
    if (gate != nullptr) {
      for (hir::EventTrigger& trigger : value_change->triggers) {
        auto gate_or = lowerer.LowerExpr(*gate, scope_frame);
        if (!gate_or) return std::unexpected(std::move(gate_or.error()));
        const hir::ExprId gate_id =
            scope_frame.Exprs().Add(*std::move(gate_or));
        if (!trigger.condition.has_value()) {
          trigger.condition = gate_id;
          continue;
        }
        trigger.condition = scope_frame.Exprs().Add(
            hir::Expr{
                .type = lowerer.Owner().Unit().builtins.scalar_bit,
                .data =
                    hir::BinaryExpr{
                        .op = hir::BinaryOp::kLogicalAnd,
                        .lhs = *trigger.condition,
                        .rhs = gate_id},
                .span = span,
            });
      }
    }
    auto depth_or = LowerTicksBack(lowerer, scope_frame, ticks_back, span);
    if (!depth_or) return std::unexpected(std::move(depth_or.error()));
    return frame.current_structural_scope->sampled_histories.Add(
        hir::SampledHistoryDecl{
            .subject = scope_frame.Exprs().Add(*std::move(subject_or)),
            .clock = *value_change,
            .depth = scope_frame.Exprs().Add(*std::move(depth_or)),
        });
  }
}

// A sampled value function that reaches across the ticks of a clocking event
// (LRM 16.9.3), where the call is one. Absent for everything else.
//
// It answers from a history the scope keeps rather than from the cell, so the
// call names that history, and which of the two shapes it is decides what else
// it carries. That also decides which of the source's arguments are values at
// all: the tick count, the gate and the event are read where the history is
// declared rather than evaluated where the call stands, and `$past` evaluates
// nothing there at all. So this resolves before the generic argument loop and
// lowers the one operand a value change function still reads, for the same
// reason the LRM 20.6 / 20.7 queries do.
template <ExprLowerer Lowerer>
auto LowerSampledHistoryExpr(
    Lowerer& lowerer, const WalkFrame& frame,
    const slang::ast::CallExpression& call, diag::SourceSpan span)
    -> diag::Result<std::optional<hir::Expr>> {
  if (!call.isSystemCall()) {
    return std::optional<hir::Expr>{std::nullopt};
  }
  const std::string_view name = call.getSubroutineName();
  const auto* desc = support::FindSystemSubroutine(name);
  if (desc == nullptr) {
    return std::optional<hir::Expr>{std::nullopt};
  }
  const bool past =
      std::holds_alternative<support::PastValueSystemSubroutineInfo>(
          desc->semantic);
  const auto* change =
      std::get_if<support::ValueChangeSystemSubroutineInfo>(&desc->semantic);
  if (!past && change == nullptr) {
    return std::optional<hir::Expr>{std::nullopt};
  }

  // Arming is the same requirement `$sampled` has, and the same check reports
  // an expression reading storage this scope cannot name.
  auto operand_reads = RecordSampledCells(lowerer, frame, call, span);
  if (!operand_reads) {
    return std::unexpected(std::move(operand_reads.error()));
  }
  // An operand reading only variables that answer with their current value has
  // that value at every tick, the past ones included (LRM 16.5.1), so no
  // history has anything to keep: `$past` is the operand as it stands, and it
  // never changed.
  if (operand_reads->armed == 0) {
    if (past) {
      auto operand_or = lowerer.LowerExpr(*call.arguments()[0], frame);
      if (!operand_or) return std::unexpected(std::move(operand_or.error()));
      return std::optional<hir::Expr>{*std::move(operand_or)};
    }
    return std::optional<hir::Expr>{
        UnchangedAnswer(lowerer.Owner(), change->reading, span)};
  }
  if (operand_reads->current != 0) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        std::string{"'"} + std::string{name} +
            "' of an expression reading both an automatic variable and one "
            "the design element holds is not yet supported");
  }

  if (past) {
    // `$past` states its operand first, then how far back, then the gate, then
    // the event (LRM 16.9.3).
    const slang::ast::Expression* ticks_back = WrittenArgument(call, 1);
    auto history_or = RecordSampledHistory(
        lowerer, frame, call, 3, ticks_back, WrittenArgument(call, 2), name,
        span);
    if (!history_or) return std::unexpected(std::move(history_or.error()));
    auto read_depth_or = LowerTicksBack(lowerer, frame, ticks_back, span);
    if (!read_depth_or) {
      return std::unexpected(std::move(read_depth_or.error()));
    }
    // The result follows the operand's type, and the operand that survives is
    // the history's subject -- the same expression, lowered where the process
    // that settles it will read it.
    const hir::StructuralScope& scope = *frame.current_structural_scope;
    return std::optional<hir::Expr>{hir::Expr{
        .type =
            scope.exprs.Get(scope.sampled_histories.Get(*history_or).subject)
                .type,
        .data =
            hir::CallExpr{
                .callee = hir::PastValueRef{.history = *history_or},
                .arguments = {frame.Exprs().Add(*std::move(read_depth_or))}},
        .span = span,
    }};
  }

  // A value change function takes its event second and gates nothing: only
  // `$past` carries a gating expression (LRM 16.9.3).
  auto history_or = RecordSampledHistory(
      lowerer, frame, call, 1, nullptr, nullptr, name, span);
  if (!history_or) return std::unexpected(std::move(history_or.error()));
  auto operand_or = lowerer.LowerExpr(*call.arguments()[0], frame);
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  return std::optional<hir::Expr>{hir::Expr{
      .type = lowerer.Owner().Unit().builtins.scalar_bit,
      .data =
          hir::CallExpr{
              .callee =
                  hir::ValueChangeRef{
                      .history = *history_or, .reading = change->reading},
              .arguments = {frame.Exprs().Add(*std::move(operand_or))},
          },
      .span = span,
  }};
}

// Maps a frontend ReturnConvention to the builtin HIR TypeId that represents
// it. Local to the calls subsystem (system subroutines are the only consumer).
auto MakeReturnConventionType(
    const hir::BuiltinHirTypes& builtins, support::ReturnConvention conv)
    -> hir::TypeId {
  switch (conv) {
    case support::ReturnConvention::kVoid:
      return builtins.void_type;
    case support::ReturnConvention::kInt32:
      return builtins.int_type;
    case support::ReturnConvention::kIntUnsigned:
      return builtins.int_unsigned;
    case support::ReturnConvention::kBit:
      return builtins.scalar_bit;
    case support::ReturnConvention::kInteger:
      return builtins.integer;
    case support::ReturnConvention::kString:
      return builtins.string;
    case support::ReturnConvention::kTime64:
      return builtins.time;
    case support::ReturnConvention::kRealTime:
      return builtins.realtime;
    case support::ReturnConvention::kOperandType:
      throw InternalError(
          "MakeReturnConventionType: a result that follows its operand has no "
          "type of its own, so the caller reads it off the operand");
  }
  throw InternalError("MakeReturnConventionType: unknown ReturnConvention");
}

// A call to a built-in runtime entry, out of the operands the source wrote with
// the object first. The object the entry acts on becomes the call's receiver
// and the rest its arguments, so no layer below has to know which operand
// position an object was written in; an entry that acts on none -- a factory
// answering with the value it builds -- takes every operand as an argument.
auto BuiltinCall(
    support::BuiltinFn method, std::vector<std::optional<hir::ExprId>> operands,
    std::optional<hir::WithClause> with_clause) -> hir::CallExpr {
  const support::RuntimeEntry entry = support::RuntimeEntryOf(method);
  if (std::holds_alternative<support::StaticFactory>(entry.declaration)) {
    return hir::CallExpr{
        .callee =
            hir::BuiltinMethodRef{.method = method, .receiver = std::nullopt},
        .arguments = std::move(operands),
        .with_clause = std::move(with_clause)};
  }
  if (operands.empty() || !operands.front().has_value()) {
    throw InternalError(
        "AST->HIR call: a built-in entry acting on an object reached lowering "
        "without one");
  }
  const hir::ExprId receiver = *operands.front();
  return hir::CallExpr{
      .callee = hir::BuiltinMethodRef{.method = method, .receiver = receiver},
      .arguments = {operands.begin() + 1, operands.end()},
      .with_clause = std::move(with_clause)};
}

// A method of an imported runtime-library class (LRM 9.7 `process`) is
// recognized by its declaring class, exactly as the handle type is; the runtime
// carries the body out, so the call names a library entry rather than a lowered
// user method.
auto DetectImportedRuntimeMethod(const slang::ast::SubroutineSymbol& method)
    -> std::optional<support::BuiltinFn> {
  const slang::ast::Scope* scope = method.getParentScope();
  if (scope == nullptr) {
    return std::nullopt;
  }
  const slang::ast::Symbol& owner = scope->asSymbol();
  if (owner.kind != slang::ast::SymbolKind::ClassType) {
    return std::nullopt;
  }
  const auto klass = ImportedRuntimeClassOf(owner.as<slang::ast::ClassType>());
  if (!klass.has_value()) {
    return std::nullopt;
  }
  switch (*klass) {
    case support::ImportedRuntimeClass::kProcess:
      return LowerProcessMethodName(method.name);
  }
  throw InternalError("AST->HIR call: unknown imported runtime-library class");
}

// The unit a subroutine is declared directly in when that unit is reached by
// name across the boundary -- a package (LRM 26.2) or the anonymous `$unit`
// scope (LRM 3.12.1) -- or null when the subroutine belongs to this compilation
// unit. Such a target is a receiver-less callable reached by name, not through
// an enclosing-scope binding of this unit. The returned symbol is the declaring
// unit, from which the caller computes its published name.
auto DeclaringUnitOfSubroutine(const slang::ast::SubroutineSymbol& sym)
    -> const slang::ast::Symbol* {
  const slang::ast::Scope* scope = sym.getParentScope();
  if (scope == nullptr) {
    return nullptr;
  }
  const slang::ast::Symbol& owner = scope->asSymbol();
  if (owner.kind != slang::ast::SymbolKind::Package &&
      owner.kind != slang::ast::SymbolKind::CompilationUnit) {
    return nullptr;
  }
  return &owner;
}

// Enables a subroutine of another instance, or of a generate block inside one
// (LRM 25.7, 23.6), reached from `origin`, where the call's name started. The
// route to that scope is the same walk a read of a declaration there takes,
// and the scope published every subroutine it declares, so the call is made on
// what the route lands on and what it passes and awaits is counted out of that
// publication.
auto LowerObjectSubroutineCall(
    UnitLowerer& unit_lowerer, WalkFrame frame, RouteOrigin origin,
    const slang::ast::SubroutineSymbol& sym,
    std::vector<std::optional<hir::ExprId>> arguments, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  const auto refuse = [&](std::string message) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm, std::move(message));
  };
  const slang::ast::Scope* owner = sym.getParentScope();
  if (owner == nullptr) {
    return refuse(
        "a subroutine declared in this scope kind is not yet supported");
  }
  auto walked =
      unit_lowerer.RouteToScopeOrRefuse(frame, *owner, std::move(origin), span);
  if (!walked) return std::unexpected(std::move(walked.error()));
  ScopeRoute route = *std::move(walked);
  const auto unsupported = [&] {
    return refuse(
        "a subroutine of another instance reached this way is not yet "
        "supported");
  };
  const auto* on = std::get_if<InExternalScope>(&route.place);
  if (on == nullptr || !on->within.empty()) return unsupported();
  const hir::ExternalScopeClassId scope_class = on->scope_class;
  const hir::ScopeClassSignature& published =
      unit_lowerer.Unit().external_scope_classes.Get(scope_class).signature;
  const auto callable = published.FindCallable(sym.name);
  if (!callable.has_value()) return unsupported();
  const hir::TypeId result_type =
      published.callables.Get(*callable).result_type;
  const hir::RoutedObjectRef receiver = unit_lowerer.MakeRoutedObjectRef(
      frame.Current(), std::move(route),
      unit_lowerer.ScopeClassTypeOf(scope_class));
  return hir::Expr{
      .type = result_type,
      .data =
          hir::CallExpr{
              .callee =
                  hir::ExternalUnitMethodRef{
                      .receiver = receiver,
                      .scope_class = scope_class,
                      .callable = *callable},
              .arguments = std::move(arguments)},
      .span = span,
  };
}

// A subroutine of the interface instance a virtual interface holds, or of an
// interface that instance instantiates (LRM 25.9). The front end carries the
// virtual interface where a method call carries its handle and resolves the
// name in the interface declaring the subroutine, so the call is made on that
// instance and what it passes and awaits is counted out of what its interface
// published.
template <ExprLowerer Lowerer>
auto LowerHeldInterfaceCall(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::Expression& held,
    const slang::ast::VirtualInterfaceType& handle_type,
    const slang::ast::SubroutineSymbol& sym,
    std::vector<std::optional<hir::ExprId>> arguments, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  auto handle = lowerer.LowerExpr(held, frame);
  if (!handle) return std::unexpected(std::move(handle.error()));
  auto descent = DescendThroughHandle(
      unit_lowerer, frame.Exprs().Add(*std::move(handle)), handle_type,
      *sym.getParentScope(), span);
  if (!descent) return std::unexpected(std::move(descent.error()));
  const hir::ScopeClassSignature& published =
      unit_lowerer.Unit()
          .external_scope_classes.Get(descent->place.scope_class)
          .signature;
  const auto callable = published.FindCallable(sym.name);
  if (!callable.has_value()) {
    throw InternalError(
        "LowerHeldInterfaceCall: a scope publishes every subroutine it "
        "declares, and this one is not among them");
  }
  return hir::Expr{
      .type = published.callables.Get(*callable).result_type,
      .data =
          hir::CallExpr{
              .callee =
                  hir::ExternalUnitMethodRef{
                      .receiver = std::move(descent->instance),
                      .scope_class = descent->place.scope_class,
                      .callable = *callable},
              .arguments = std::move(arguments)},
      .span = span};
}

// A call to a system task or function (LRM 20, 21), or to a method the
// language defines on a value of a built-in type, which the front end spells
// as a system call on that value. `receiver_type` is the type of the first
// operand where it has one, which is what picks a built-in type's method.
template <ExprLowerer Lowerer>
auto LowerSystemCall(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call,
    std::vector<std::optional<hir::ExprId>> arg_ids,
    std::optional<hir::TypeId> receiver_type, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  const hir::TypePool& types = unit_lowerer.Unit().types;
  const auto& info =
      std::get<slang::ast::CallExpression::SystemCallInfo>(call.subroutine);
  const std::string_view name = info.subroutine->name;

  if (receiver_type.has_value() &&
      types.Get(*receiver_type).Is<hir::EnumType>()) {
    if (auto enum_method = LowerEnumMethodName(name); enum_method.has_value()) {
      // `next` / `prev` have an optional `int unsigned step = 1` (LRM
      // 6.19.5.3/4). When the user omits the step, the call keeps the one
      // argument the source wrote and the default is supplied where the
      // method is answered, so no literal is injected here.
      auto type_id = unit_lowerer.InternType(*call.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return hir::Expr{
          .type = *type_id,
          .data =
              hir::CallExpr{
                  .callee = hir::EnumMethodRef{.method = *enum_method},
                  .arguments = std::move(arg_ids),
              },
          .span = span,
      };
    }
  }

  if (receiver_type.has_value() &&
      types.Get(*receiver_type).Is<hir::StringType>()) {
    if (auto kind = LowerStringMethodName(name); kind.has_value()) {
      // LRM 6.16.1 through 6.16.15 -- string intrinsic methods, each acting
      // on the string; the remaining operands are the SV method parameters
      // (e.g. substr's `i, j`).
      auto type_id = unit_lowerer.InternType(*call.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return hir::Expr{
          .type = *type_id,
          .data = BuiltinCall(*kind, std::move(arg_ids), std::nullopt),
          .span = span,
      };
    }
  }

  if (receiver_type.has_value() &&
      types.Get(*receiver_type).Is<hir::EventType>() && name == "triggered") {
    // LRM 15.5.3: `e.triggered` returns true for the duration of the time
    // slot in which the event was last triggered. Result type is bit (1'b0
    // / 1'b1) -- slang already typed the expression; we just route the
    // call through the named-event method.
    auto type_id = unit_lowerer.InternType(*call.type, span);
    if (!type_id) return std::unexpected(std::move(type_id.error()));
    return hir::Expr{
        .type = *type_id,
        .data = BuiltinCall(
            support::BuiltinFn::kTriggered, std::move(arg_ids), std::nullopt),
        .span = span,
    };
  }

  if (receiver_type.has_value() &&
      types.Get(*receiver_type).Is<hir::QueueType>()) {
    // LRM 7.10.2 queue-native methods, each acting on the queue; the method
    // parameters (insert's index and item, push's item) follow. These take no
    // `with` clause and are tried before the array-manipulation family so
    // `size` / `delete` resolve to the queue-native form rather than the LRM
    // 7.12 one.
    if (auto kind = LowerQueueMethodName(name, arg_ids.size() - 1);
        kind.has_value()) {
      auto type_id = unit_lowerer.InternType(*call.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return hir::Expr{
          .type = *type_id,
          .data = BuiltinCall(*kind, std::move(arg_ids), std::nullopt),
          .span = span,
      };
    }
  }

  // LRM 7.9 associative-array native methods, each acting on the array; the
  // index (`exists`, the delete that names one entry) follows. Like the
  // queue's, these are tried before the LRM 7.12 family so a name both define
  // resolves to the one the receiver's own clause states.
  if (receiver_type.has_value() &&
      unit_lowerer.Unit()
          .types.Get(*receiver_type)
          .template Is<hir::AssociativeArrayType>()) {
    if (auto kind = LowerAssociativeMethodName(name, arg_ids.size() - 1);
        kind.has_value()) {
      auto type_id = unit_lowerer.InternType(*call.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return hir::Expr{
          .type = *type_id,
          .data = BuiltinCall(*kind, std::move(arg_ids), std::nullopt),
          .span = span,
      };
    }
  }

  // LRM 7.12 array-manipulation family, defined for any unpacked array: a
  // fixed-size unpacked array (LRM 7.12.1 / 7.12.2 operate on "any unpacked
  // array"), a dynamic array, a queue (LRM 7.10.1 gives it the same
  // operations as an unpacked array), and an associative array (LRM 7.12.1 /
  // 7.12.3 / 7.12.5 reduction / locator / map; the ordering family is
  // rejected on it by slang). A receiver whose own clause names a method --
  // the queue's LRM 7.10.2 set, the associative array's LRM 7.9 set --
  // resolved it above, so only the 7.12 names reach this dispatch. The
  // no-`with` form takes only the receiver; the `with` form (LRM 7.12.4)
  // binds an iterator and a body expression carried as the optional
  // `WithClause`, which HIR -> MIR turns into a closure argument.
  const auto receives_array_method = [&] {
    if (!receiver_type.has_value()) return false;
    const hir::Type& ty = unit_lowerer.Unit().types.Get(*receiver_type);
    return ty.Is<hir::UnpackedArrayType>() || ty.Is<hir::DynamicArrayType>() ||
           ty.Is<hir::QueueType>() || ty.Is<hir::AssociativeArrayType>();
  };
  if (receives_array_method()) {
    if (auto kind = LowerArrayMethodName(name); kind.has_value()) {
      auto type_id = unit_lowerer.InternType(*call.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      std::optional<hir::WithClause> with_clause;
      if (std::holds_alternative<slang::ast::CallExpression::IteratorCallInfo>(
              info.extraInfo)) {
        const auto& iter_info =
            std::get<slang::ast::CallExpression::IteratorCallInfo>(
                info.extraInfo);
        const auto& iter_var =
            iter_info.iterVar->as<slang::ast::VariableSymbol>();
        // The clause's element and index references resolve to this id while
        // the body is lowered; marking the iterator by identity on the frame
        // distinguishes it from a foreach loop variable (also a slang
        // Iterator symbol) and lets a clause nested in the body name this
        // outer one.
        const hir::WithClauseId clause_id = unit_lowerer.NextWithClauseId();
        auto body_or = lowerer.LowerExpr(
            *iter_info.iterExpr,
            frame.WithIterationClause(iter_var, clause_id));
        if (!body_or) return std::unexpected(std::move(body_or.error()));
        const auto body_expr_id = frame.Exprs().Add(*std::move(body_or));
        with_clause = hir::WithClause{
            .id = clause_id,
            .element_name = std::string{iter_var.name},
            .expr = body_expr_id};
      }
      return hir::Expr{
          .type = *type_id,
          .data =
              BuiltinCall(*kind, std::move(arg_ids), std::move(with_clause)),
          .span = span,
      };
    }
  }

  // LRM 20.8.1 `$clog2`: ceil(log2) of the operand read as unsigned. A
  // type-agnostic value query -- every value type exposes it -- so it lowers
  // to the generic instance builtin call on its operand. A constant argument
  // is folded by the downstream optimizer, never in lowering.
  if (info.subroutine != nullptr &&
      info.subroutine->knownNameId == slang::parsing::KnownSystemName::Clog2) {
    auto type_id = unit_lowerer.InternType(*call.type, span);
    if (!type_id) return std::unexpected(std::move(type_id.error()));
    return hir::Expr{
        .type = *type_id,
        .data = BuiltinCall(
            support::BuiltinFn::kClog2, std::move(arg_ids), std::nullopt),
        .span = span,
    };
  }

  // LRM 20.8.2 real mathematics and the LRM 20.5 conversions that read a
  // real's bits or truncate its value. Both families are operations on the
  // operand, so they lower to the generic instance builtin call on it; the
  // result type is slang's, which is what fixes the width a bit pattern
  // lands in and the precision a pattern is read back as. A constant
  // argument is folded before lowering sees the call at all.
  if (info.subroutine != nullptr) {
    const auto known = info.subroutine->knownNameId;
    auto real_fn = LowerRealMathName(known);
    if (!real_fn.has_value()) real_fn = LowerRealConversionName(known);
    if (real_fn.has_value()) {
      auto type_id = unit_lowerer.InternType(*call.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return hir::Expr{
          .type = *type_id,
          .data = BuiltinCall(*real_fn, std::move(arg_ids), std::nullopt),
          .span = span,
      };
    }
  }

  // LRM 20.5 `$itor` asks for the LRM 6.12.1 integral-to-real conversion the
  // result type already names, so it is a value conversion rather than a
  // runtime call.
  if (info.subroutine != nullptr &&
      info.subroutine->knownNameId == slang::parsing::KnownSystemName::Itor) {
    auto type_id = unit_lowerer.InternType(*call.type, span);
    if (!type_id) return std::unexpected(std::move(type_id.error()));
    return hir::Expr{
        .type = *type_id,
        .data =
            hir::ConversionExpr{
                .kind = hir::ConversionKind::kExplicit,
                .operand = *arg_ids.front()},
        .span = span,
    };
  }

  // LRM 7.12.4 `item.index`: slang dresses the iteration index as a method on
  // the iterator (a SystemSubroutine with `KnownSystemName::Index`) whose
  // receiver value is discarded. It is the index iteration parameter -- a
  // value co-equal with the element -- so it lowers to an
  // `IterationBindingRef` value reference, not a call. Its clause is the one
  // whose iterator is the receiver, so a nested clause's `item.index` still
  // names the right clause.
  if (info.subroutine != nullptr &&
      info.subroutine->knownNameId == slang::parsing::KnownSystemName::Index) {
    const auto& receiver = *call.arguments()[0];
    if (receiver.kind != slang::ast::ExpressionKind::NamedValue) {
      throw InternalError("item.index receiver is not a named iterator");
    }
    const auto clause = frame.FindIterationClause(
        receiver.as<slang::ast::NamedValueExpression>().symbol);
    if (!clause) {
      throw InternalError(
          "item.index receiver is not an active with-clause iterator");
    }
    auto type_id = unit_lowerer.InternType(*call.type, span);
    if (!type_id) return std::unexpected(std::move(type_id.error()));
    return hir::Expr{
        .type = *type_id,
        .data =
            hir::PrimaryExpr{
                .data =
                    hir::IterationBindingRef{
                        .clause = *clause,
                        .role = hir::IterationBindingRole::kIndex}},
        .span = span,
    };
  }

  // `$signed` / `$unsigned` reinterpret the operand under the named
  // signedness. The result type already carries that signedness, so this is a
  // value conversion to the result type, not a runtime call.
  if (info.subroutine != nullptr &&
      (info.subroutine->knownNameId ==
           slang::parsing::KnownSystemName::Signed ||
       info.subroutine->knownNameId ==
           slang::parsing::KnownSystemName::Unsigned)) {
    auto type_id = unit_lowerer.InternType(*call.type, span);
    if (!type_id) return std::unexpected(std::move(type_id.error()));
    return hir::Expr{
        .type = *type_id,
        .data =
            hir::ConversionExpr{
                .kind = hir::ConversionKind::kImplicit,
                .operand = *arg_ids.front()},
        .span = span,
    };
  }

  const auto* desc = support::FindSystemSubroutine(name);
  if (desc == nullptr) {
    // slang resolved a system task / function that Lyra's registry does not
    // carry: a legitimate but unimplemented construct, not a compiler bug.
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        std::string{"system task / function '"} + std::string{name} +
            "' is not yet supported");
  }
  const auto frontend_kind = FromSlangSubroutineKind(info.subroutine->kind);
  if (desc->kind != frontend_kind) {
    throw InternalError(
        std::string{"AST->HIR call: registry/frontend kind mismatch for '"} +
        std::string{name} + "'");
  }
  if (!desc->arg_policy.Accepts(arg_ids.size())) {
    throw InternalError(
        std::string{
            "AST->HIR call: arg count outside descriptor policy for '"} +
        std::string{name} + "'");
  }

  if (std::holds_alternative<support::SampledValueSystemSubroutineInfo>(
          desc->semantic)) {
    if (auto recorded = RecordSampledCells(lowerer, frame, call, span);
        !recorded) {
      return std::unexpected(std::move(recorded.error()));
    }
  }

  // A result that follows its operand reads its type off that operand rather
  // than off a builtin, which is what lets one entry serve every type its
  // argument may have (LRM 16.9.3 `$sampled`).
  const auto result_type = [&] {
    if (desc->result_conv != support::ReturnConvention::kOperandType) {
      return MakeReturnConventionType(
          unit_lowerer.Unit().builtins, desc->result_conv);
    }
    if (arg_ids.empty() || !arg_ids.front().has_value()) {
      throw InternalError(
          std::string{"AST->HIR call: '"} + std::string{name} +
          "' takes its result type from an operand it was not given");
    }
    return frame.Exprs().Get(*arg_ids.front()).type;
  }();
  return hir::Expr{
      .type = result_type,
      .data =
          hir::CallExpr{
              .callee = hir::SystemSubroutineRef{.id = desc->id},
              .arguments = std::move(arg_ids),
          },
      .span = span,
  };
}

// A call to a subroutine the source declared: a method of a class or of an
// imported runtime-library class, one of an interface instance, a DPI-C
// import, one another unit's namespace declares, or one of this unit's scopes.
template <ExprLowerer Lowerer>
auto LowerSubroutineCall(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call,
    std::vector<std::optional<hir::ExprId>> arg_ids, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  const auto* sym =
      std::get<const slang::ast::SubroutineSymbol*>(call.subroutine);
  if (sym == nullptr) {
    throw InternalError(
        "AST->HIR call: user call missing resolved SubroutineSymbol");
  }

  // A method of an imported runtime-library class routes to the library entry.
  // A static method acts on no object; an instance method acts on the handle
  // `thisClass` carries.
  if (const auto imported = DetectImportedRuntimeMethod(*sym)) {
    auto result_type = unit_lowerer.InternType(*call.type, span);
    if (!result_type) return std::unexpected(std::move(result_type.error()));
    std::optional<hir::ExprId> receiver;
    if (const slang::ast::Expression* this_class = call.thisClass();
        this_class != nullptr) {
      auto receiver_or = lowerer.LowerExpr(*this_class, frame);
      if (!receiver_or) return std::unexpected(std::move(receiver_or.error()));
      receiver = frame.Exprs().Add(*std::move(receiver_or));
    }
    return hir::Expr{
        .type = *result_type,
        .data =
            hir::CallExpr{
                .callee =
                    hir::BuiltinMethodRef{
                        .method = *imported, .receiver = receiver},
                .arguments = std::move(arg_ids),
            },
        .span = span,
    };
  }

  // A static class method call (LRM 8.10): the target has no receiver, so it
  // lowers to `StaticMethodCallRef` rather than `MethodCallRef` -- a `handle`-
  // qualified call to a static method still discards the handle because the
  // callee never observes it (LRM 8.10 "no access to non-static members").
  // The declaring class comes from slang's resolved callee, so an inherited
  // static call reaches the base's arena the same way an instance-method
  // call does under LRM 8.13.
  bool is_type_associated = false;
  if (sym->getParentScope() != nullptr &&
      sym->getParentScope()->asSymbol().kind ==
          slang::ast::SymbolKind::ClassType) {
    auto answered = unit_lowerer.IsTypeAssociatedMethod(*sym, span);
    if (!answered) return std::unexpected(std::move(answered.error()));
    is_type_associated = *answered;
  }
  if (is_type_associated) {
    const auto& declaring_class =
        sym->getParentScope()->asSymbol().as<slang::ast::ClassType>();
    auto callee = unit_lowerer.MakeMethodCallee(declaring_class, *sym, span);
    if (!callee) return std::unexpected(std::move(callee.error()));
    auto static_result_type = unit_lowerer.InternType(*call.type, span);
    if (!static_result_type) {
      return std::unexpected(std::move(static_result_type.error()));
    }
    auto declaring_instance =
        unit_lowerer.DeclaringInstanceFrom(declaring_class, frame, span);
    if (!declaring_instance) {
      return std::unexpected(std::move(declaring_instance.error()));
    }
    return hir::Expr{
        .type = *static_result_type,
        .data =
            hir::CallExpr{
                .callee =
                    hir::StaticMethodCallRef{
                        .callee = *std::move(callee),
                        .declaring_instance = *std::move(declaring_instance)},
                .arguments = std::move(arg_ids),
            },
        .span = span,
    };
  }

  if (const slang::ast::Expression* this_class = call.thisClass();
      this_class != nullptr) {
    if (const auto* handle_type =
            this_class->type->getCanonicalType()
                .as_if<slang::ast::VirtualInterfaceType>()) {
      return LowerHeldInterfaceCall(
          lowerer, frame, *this_class, *handle_type, *sym, std::move(arg_ids),
          span);
    }
  }

  // Instance-method call (LRM 8.6): every source shape -- `h.foo()`, an
  // unqualified `foo()` from inside a class body, `this.foo()`, and
  // `super.foo()` -- lowers to one `MethodCallRef`, distinguished by which
  // object the classifier says it runs against. The declaring class comes from
  // slang's resolved callee, which already accounts for inheritance and super
  // resolution.
  if (CallReachesInstanceMethod(call, *sym)) {
    auto receiver_or = ClassifyMethodReceiver(lowerer, frame, call);
    if (!receiver_or) return std::unexpected(std::move(receiver_or.error()));
    const auto& declaring_class =
        sym->getParentScope()->asSymbol().as<slang::ast::ClassType>();
    auto callee = unit_lowerer.MakeMethodCallee(declaring_class, *sym, span);
    if (!callee) return std::unexpected(std::move(callee.error()));
    auto method_result_type = unit_lowerer.InternType(*call.type, span);
    if (!method_result_type) {
      return std::unexpected(std::move(method_result_type.error()));
    }
    return hir::Expr{
        .type = *method_result_type,
        .data =
            hir::CallExpr{
                .callee =
                    hir::MethodCallRef{
                        .receiver = *std::move(receiver_or),
                        .callee = *std::move(callee)},
                .arguments = std::move(arg_ids),
            },
        .span = span,
    };
  }

  // A DPI-C import is a bodyless external callable whose symbol is
  // program-global (LRM 35.4), so this unit reaches it through its own record
  // and never across a unit boundary -- which is why a declaration in a
  // package or at `$unit` scope resolves here exactly as one written in this
  // unit's own scope does, rather than through the cross-unit boundary.
  if (sym->flags.has(slang::ast::MethodFlags::DPIImport)) {
    auto import_id = unit_lowerer.EnsureForeignImport(*sym);
    if (!import_id) return std::unexpected(std::move(import_id.error()));
    // The declaration's own instantiated scope, reached from here. It is absent
    // when the declaring scope is a namespace that is never instantiated.
    std::optional<hir::StructuralHops> declaring_scope;
    if (const auto declaring_frame =
            unit_lowerer.LookupForeignImportScope(*sym)) {
      declaring_scope = frame.HopsTo(*declaring_frame);
      if (!declaring_scope.has_value()) {
        throw InternalError(
            "AST->HIR call: the scope declaring this DPI import is not on the "
            "current scope stack");
      }
    }
    auto import_result_type = unit_lowerer.InternType(*call.type, span);
    if (!import_result_type) {
      return std::unexpected(std::move(import_result_type.error()));
    }
    return hir::Expr{
        .type = *import_result_type,
        .data =
            hir::CallExpr{
                .callee =
                    hir::ForeignImportRef{
                        .id = *import_id, .declaring_scope = declaring_scope},
                .arguments = std::move(arg_ids),
            },
        .span = span,
    };
  }

  // A subroutine declared in a package or the `$unit` scope belongs to another
  // compilation unit (LRM 26.3 / 3.12.1). It is reached by name across the unit
  // boundary rather than through an enclosing-scope binding of this unit. Each
  // formal's direction and type is what marshals an output / inout / ref
  // actual at that boundary (LRM 13.5), and the declaring unit's signature is
  // where it is stated. The call's result type is the enclosing expression's
  // own type and is not recorded again here.
  if (const auto* unit = DeclaringUnitOfSubroutine(*sym)) {
    std::string unit_name =
        CompilationUnitName(*unit, unit_lowerer.Specialization());
    auto interface =
        unit_lowerer.NamespaceCalleeInterface(unit_name, *sym, span);
    if (!interface) return std::unexpected(std::move(interface.error()));
    auto result_type = unit_lowerer.InternType(*call.type, span);
    if (!result_type) return std::unexpected(std::move(result_type.error()));
    return hir::Expr{
        .type = *result_type,
        .data =
            hir::CallExpr{
                .callee =
                    hir::ExternalUnitSubroutineRef{
                        .unit_name = std::move(unit_name),
                        .subroutine_name = std::string{sym->name},
                        .interface = *std::move(interface)},
                .arguments = std::move(arg_ids)},
        .span = span,
    };
  }

  // A name leaving the caller's instance starts where it lands or at the port
  // it went through (LRM 23.8, 25.3), whatever declaration the front end
  // resolved it to in the instance being lowered: in another instance of this
  // unit the same text reaches another object. Only a name that stays inside
  // the instance may enable a subroutine this unit declares as its own.
  RouteOrigin start = unit_lowerer.StartOf(frame, call.lookupInfo.hierRef);
  const auto binding = std::holds_alternative<FromReader>(start)
                           ? unit_lowerer.LookupSubroutineBinding(*sym)
                           : std::nullopt;
  // What is left is a subroutine another instance declares -- one an interface
  // offers across a port (LRM 25.7), or one a hierarchical name enables (LRM
  // 23.6) -- and the route to the object it is enabled on starts where the name
  // does.
  if (!binding.has_value()) {
    return LowerObjectSubroutineCall(
        unit_lowerer, frame, std::move(start), *sym, std::move(arg_ids), span);
  }
  // The scope that declares the callee, reached the way a reference to a
  // declaration in that same scope is: a climb to the nearest scope enclosing
  // both, then a descent. A callee of an enclosing scope is the empty descent,
  // which is what a bare climb already was.
  auto reach = unit_lowerer.ReachOwnScope(frame, *sym->getParentScope(), span);
  if (!reach) return std::unexpected(std::move(reach.error()));
  auto result_type = unit_lowerer.InternType(*call.type, span);
  if (!result_type) return std::unexpected(std::move(result_type.error()));
  return hir::Expr{
      .type = *result_type,
      .data =
          hir::CallExpr{
              .callee =
                  hir::StructuralSubroutineRef{
                      .hops = reach->hops,
                      .descent = std::move(reach->descent),
                      .subroutine = binding->subroutine_id},
              .arguments = std::move(arg_ids),
          },
      .span = span,
  };
}

}  // namespace

template <ExprLowerer Lowerer>
auto LowerCallExpr(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  // The LRM 20.6 / 20.7 queries are resolved before the argument loop: their
  // operand is never evaluated and may be a data type, which has no value form
  // to lower.
  auto query = LowerQueryExpr(lowerer, frame, call, span);
  if (!query) return std::unexpected(std::move(query.error()));
  if (query->has_value()) return *std::move(*query);

  auto sampled = LowerSampledHistoryExpr(lowerer, frame, call, span);
  if (!sampled) return std::unexpected(std::move(sampled.error()));
  if (sampled->has_value()) return *std::move(*sampled);

  // LRM 6.24.2: the first actual of a dynamic cast is the destination rather
  // than an operand, so it is resolved before the argument loop. Reached as an
  // expression the call is the function spelling, whose answer is the whole of
  // what an invalid assignment produces; the task spelling is recognized where
  // the statement is.
  if (IsDynamicCast(call)) {
    return LowerDynamicCastExpr(
        lowerer, frame, call, hir::InvalidAssignmentHandling::kAnswered, span);
  }

  std::vector<std::optional<hir::ExprId>> arg_ids;
  arg_ids.reserve(call.arguments().size());
  std::optional<hir::TypeId> receiver_type;
  for (std::size_t i = 0; i < call.arguments().size(); ++i) {
    // LRM 13.5: slang models an `output` / `inout` actual as an
    // AssignmentExpression whose right side is an EmptyArgument placeholder;
    // the actual lvalue is the left side. HIR carries just that lvalue -- the
    // copy-in / copy-out is synthesized at HIR-to-MIR from the formal's
    // direction.
    const slang::ast::Expression* arg = call.arguments()[i];
    if (arg->kind == slang::ast::ExpressionKind::Assignment) {
      const auto& as = arg->as<slang::ast::AssignmentExpression>();
      if (as.right().kind == slang::ast::ExpressionKind::EmptyArgument) {
        arg = &as.left();
      }
    }
    // LRM 21.3.4.4 form 2d: a standalone EmptyArgument marks a positional
    // elision (`$fread(mem, fd, , count)`). Surface as `std::nullopt` so the
    // per-subroutine HIR-to-MIR handler can decide whether elision is valid
    // at this position; positions stay aligned with slang's arg list.
    if (arg->kind == slang::ast::ExpressionKind::EmptyArgument) {
      arg_ids.emplace_back(std::nullopt);
      continue;
    }
    // A clocking event names an event rather than standing for a value, so
    // there is nothing here to read (LRM 16.9.3). What it settles -- which
    // event's ticks a sampled value function counts -- is taken from the call
    // itself, where the rest of the rule for finding it also applies.
    if (arg->kind == slang::ast::ExpressionKind::ClockingEvent) {
      arg_ids.emplace_back(std::nullopt);
      continue;
    }
    auto arg_or = lowerer.LowerExpr(*arg, frame);
    if (!arg_or) return std::unexpected(std::move(arg_or.error()));
    if (i == 0) {
      receiver_type = arg_or->type;
    }
    arg_ids.emplace_back(frame.Exprs().Add(*std::move(arg_or)));
  }

  if (call.isSystemCall()) {
    return LowerSystemCall(
        lowerer, frame, call, std::move(arg_ids), receiver_type, span);
  }
  return LowerSubroutineCall(lowerer, frame, call, std::move(arg_ids), span);
}

template auto LowerCallExpr(
    ProcessLowerer& lowerer, WalkFrame frame,
    const slang::ast::CallExpression& call, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
template auto LowerCallExpr(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const slang::ast::CallExpression& call, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir

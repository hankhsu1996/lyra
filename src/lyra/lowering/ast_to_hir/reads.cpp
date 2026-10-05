#include "lyra/lowering/ast_to_hir/reads.hpp"

#include <algorithm>
#include <cstddef>
#include <expected>
#include <format>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/ASTVisitor.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/Type.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lowering/ast_to_hir/expression/references.hpp"
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/expression/virtual_interface.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// A property of a class object, which is reached through an object and not
// declared in any scope (LRM 8.4). A static one is the one copy its class
// shares (LRM 8.9) and is a variable like any other.
auto IsObjectMember(const slang::ast::Symbol& symbol) -> bool {
  const auto* property = symbol.as_if<slang::ast::ClassPropertySymbol>();
  return property != nullptr &&
         property->lifetime != slang::ast::VariableLifetime::Static;
}

auto IsClassHandle(const slang::ast::Type& type) -> bool {
  return type.getCanonicalType().isClass();
}

auto IsVirtualInterface(const slang::ast::Type& type) -> bool {
  return type.getCanonicalType().isVirtualInterface();
}

// A method the runtime library carries out, which no unit declares a body for.
auto IsRuntimeLibraryMethod(const slang::ast::SubroutineSymbol& method)
    -> bool {
  const slang::ast::Scope* scope = method.getParentScope();
  if (scope == nullptr) return false;
  const auto* owner = scope->asSymbol().as_if<slang::ast::ClassType>();
  return owner != nullptr && ImportedRuntimeClassOf(*owner).has_value();
}

auto IsFormalOf(
    const slang::ast::Symbol& symbol,
    const slang::ast::SubroutineSymbol& function) -> bool {
  return std::ranges::any_of(
      function.getArguments(),
      [&](const slang::ast::FormalArgumentSymbol* formal) {
        return formal == &symbol;
      });
}

// Whether a report can evaluate `expr` where it stands, which is ahead of a
// body: reading what `holds_state` admits -- for a function its formals, the
// object it runs on and storage that exists before the body runs -- and nothing
// else. Automatic variables hold nothing yet, a handle not yet tested may be
// null, and a call may do either.
template <typename HoldsState>
class ReportCanEvaluate
    : public slang::ast::ASTVisitor<
          ReportCanEvaluate<HoldsState>, slang::ast::VisitFlags::Expressions> {
 public:
  explicit ReportCanEvaluate(HoldsState holds_state)
      : holds_state_(std::move(holds_state)) {
  }

  [[nodiscard]] auto Evaluable() const -> bool {
    return evaluable_;
  }

  void handle(const slang::ast::NamedValueExpression& named) {
    if (!holds_state_(named.symbol)) {
      evaluable_ = false;
    }
  }

  void handle(const slang::ast::MemberAccessExpression& access) {
    const slang::ast::Type& base = *access.value().type;
    if ((IsClassHandle(base) || IsVirtualInterface(base)) &&
        !NamesCurrentInstance(access.value())) {
      evaluable_ = false;
      return;
    }
    this->visitDefault(access);
  }

  // A system function answers from its arguments alone; a user one may read
  // anything, including what the report is not standing in.
  void handle(const slang::ast::CallExpression& call) {
    if (!call.isSystemCall()) {
      evaluable_ = false;
      return;
    }
    this->visitDefault(call);
  }

 private:
  HoldsState holds_state_;
  bool evaluable_ = true;
};

// Where the reads are stated, which is ahead of a body either way: in a
// function's report, which can evaluate only what the function holds before
// its body runs; or ahead of an `always_comb` / `always_latch` procedure's
// first run, where its implicit list is collected once and which can evaluate
// what exists before its body runs.
struct InAReport {
  const slang::ast::SubroutineSymbol* function;
};
struct BeforeTheProcedure {
  const slang::ast::ProceduralBlockSymbol* proc;
};
using Standpoint = std::variant<InAReport, BeforeTheProcedure>;

template <typename HoldsState>
auto EvaluableAhead(const slang::ast::Expression& expr, HoldsState holds_state)
    -> bool {
  ReportCanEvaluate<HoldsState> check(std::move(holds_state));
  expr.visit(check);
  return check.Evaluable();
}

auto CanEvaluate(
    const Standpoint& standpoint, const slang::ast::Expression& expr) -> bool {
  return std::visit(
      Overloaded{
          [&](const InAReport& report) {
            const slang::ast::SubroutineSymbol& function = *report.function;
            return EvaluableAhead(expr, [&](const slang::ast::Symbol& symbol) {
              return HoldsStateBeforeItRuns(symbol, function) ||
                     IsFormalOf(symbol, function) ||
                     &symbol == function.thisVar;
            });
          },
          [&](const BeforeTheProcedure& before) {
            return EvaluableAhead(expr, [&](const slang::ast::Symbol& symbol) {
              return HoldsStateBeforeItRuns(symbol, *before.proc);
            });
          }},
      standpoint);
}

// A method run on an object rather than on its class (LRM 8.10).
auto IsInstanceMethod(const slang::ast::SubroutineSymbol& subroutine) -> bool {
  const slang::ast::Scope* declaring = subroutine.getParentScope();
  return declaring != nullptr &&
         declaring->asSymbol().kind == slang::ast::SymbolKind::ClassType &&
         !subroutine.flags.has(slang::ast::MethodFlags::Static);
}

// Walks a function body's or a procedure's statements for what they reach
// beyond the variables they name: the objects and interface variables found
// through a handle, and the calls of functions whose reports say what they
// read.
class ReadsCollector : public slang::ast::ASTVisitor<
                           ReadsCollector, slang::ast::VisitFlags::AllGood> {
 public:
  ReadsCollector(
      ProcessLowerer& proc, WalkFrame frame, Standpoint standpoint,
      diag::SourceSpan span)
      : proc_(&proc),
        frame_(std::move(frame)),
        standpoint_(standpoint),
        span_(std::move(span)) {
  }

  void handle(const slang::ast::MemberAccessExpression& access) {
    if (failure_.has_value()) return;
    // A whole virtual interface is a value the expression reads, not a place
    // it reaches through.
    if (IsVirtualInterface(*access.type)) {
      return;
    }
    if (IsVirtualInterface(*access.value().type)) {
      ReachThroughInterface(access);
      access.value().visit(*this);
      return;
    }
    if (IsObjectMember(access.member)) {
      ReachAlongChain(access);
      return;
    }
    visitDefault(access);
  }

  void handle(const slang::ast::NamedValueExpression& named) {
    if (failure_.has_value()) return;
    // A property named bare is one of the running method's object (LRM 8.4).
    if (IsObjectMember(named.symbol)) {
      AddChain(Root{hir::ReceiverObject{}}, {});
    }
  }

  void handle(const slang::ast::CallExpression& call) {
    if (failure_.has_value()) return;
    AddCall(call);
    visitDefault(call);
  }

  auto TakeReads() && -> diag::Result<hir::Reads> {
    if (failure_.has_value()) return std::unexpected(*std::move(failure_));
    return std::move(reads_);
  }

  void Fail(diag::Diagnostic diagnostic) {
    if (!failure_.has_value()) {
      failure_ = std::move(diagnostic);
    }
  }

 private:
  // Adds `expr`, lowered where the reads are stated, to the frame's
  // expressions; absent, with the failure kept, where it does not lower.
  auto AddLowered(const slang::ast::Expression& expr)
      -> std::optional<hir::ExprId> {
    auto lowered = proc_->LowerExpr(expr, frame_);
    if (!lowered) {
      Fail(std::move(lowered.error()));
      return std::nullopt;
    }
    return frame_.Exprs().Add(*std::move(lowered));
  }

  using Root = std::variant<hir::ExprId, hir::ReceiverObject>;

  // Every object is one leaf however many reads reach it.
  void AddEveryObject() {
    if (std::ranges::none_of(reads_.leaves, [](const hir::WaitLeaf& leaf) {
          return std::holds_alternative<hir::EveryObject>(leaf);
        })) {
      reads_.leaves.emplace_back(hir::EveryObject{});
    }
  }

  // Where a chain of handle reads starts, as the reads can evaluate it: the
  // running method's own object where the source wrote `this`, and otherwise
  // the handle the source wrote. Absent where a report cannot evaluate it.
  auto ChainRoot(const slang::ast::Expression& root) -> std::optional<Root> {
    if (NamesCurrentInstance(root)) {
      return Root{hir::ReceiverObject{}};
    }
    if (!CanEvaluate(standpoint_, root)) {
      return std::nullopt;
    }
    const std::optional<hir::ExprId> lowered = AddLowered(root);
    if (!lowered.has_value()) return std::nullopt;
    return Root{*lowered};
  }

  void AddChain(
      std::optional<Root> root,
      std::vector<const slang::ast::ClassPropertySymbol*> hops) {
    if (failure_.has_value()) return;
    if (!root.has_value()) {
      AddEveryObject();
      return;
    }
    hir::ObjectChain chain{.root = *root, .hops = {}};
    chain.hops.reserve(hops.size());
    for (const slang::ast::ClassPropertySymbol* property : hops) {
      const auto& owner =
          property->getParentScope()->asSymbol().as<slang::ast::ClassType>();
      auto target =
          proc_->Owner().MakeClassPropertyTarget(owner, *property, span_);
      if (!target) {
        Fail(std::move(target.error()));
        return;
      }
      auto type = proc_->Owner().InternType(property->getType(), span_);
      if (!type) {
        Fail(std::move(type.error()));
        return;
      }
      chain.hops.push_back(
          hir::ObjectChain::Hop{
              .property = *std::move(target), .handle_type = *type});
    }
    reads_.leaves.emplace_back(std::move(chain));
  }

  // `access` reads a property of an object; every handle-valued property on
  // the way to it is a hop, and what the first of them is read on is the root.
  void ReachAlongChain(const slang::ast::MemberAccessExpression& access) {
    std::vector<const slang::ast::ClassPropertySymbol*> hops;
    const slang::ast::Expression* root = &access.value();
    while (const auto* inner =
               root->as_if<slang::ast::MemberAccessExpression>()) {
      if (!IsObjectMember(inner->member)) break;
      hops.push_back(&inner->member.as<slang::ast::ClassPropertySymbol>());
      root = &inner->value();
    }
    std::ranges::reverse(hops);
    if (const auto* named = root->as_if<slang::ast::NamedValueExpression>();
        named != nullptr && IsObjectMember(named->symbol)) {
      hops.insert(
          hops.begin(), &named->symbol.as<slang::ast::ClassPropertySymbol>());
      AddChain(Root{hir::ReceiverObject{}}, std::move(hops));
      return;
    }
    AddChain(ChainRoot(*root), std::move(hops));
    root->visit(*this);
  }

  // `access` reads a variable of the interface instance a virtual interface
  // holds (LRM 25.9), which is that instance's as the handle is evaluated.
  void ReachThroughInterface(const slang::ast::MemberAccessExpression& access) {
    const slang::ast::Expression& handle = access.value();
    if (!CanEvaluate(standpoint_, handle)) {
      reads_.unreportable =
          "a variable of the interface instance a virtual interface holds, "
          "reached through what the function's own variables hold, is not yet "
          "watched";
      return;
    }
    const std::optional<hir::ExprId> lowered = AddLowered(handle);
    if (!lowered.has_value()) return;
    auto watched = WatchedThroughHandle(
        proc_->Owner(), *lowered,
        handle.type->getCanonicalType().as<slang::ast::VirtualInterfaceType>(),
        access.member, span_);
    if (!watched) {
      Fail(std::move(watched.error()));
      return;
    }
    for (hir::InterfaceMemberAccessExpr& member : *watched) {
      reads_.leaves.emplace_back(std::move(member));
    }
  }

  // A call of a function made so it reports what it reads. What the call is
  // handed that the reads cannot evaluate goes as its type's default, and a
  // handle so lost leaves every object to cover what it reached.
  void AddCall(const slang::ast::CallExpression& call) {
    if (call.isSystemCall()) return;
    const auto* callee =
        std::get<const slang::ast::SubroutineSymbol*>(call.subroutine);
    // Foreign code and a method of a class the runtime library defines (LRM
    // 9.7 `process`) read nothing a wait can watch, and a task is never called
    // from an expression. A constructor answers with an object nothing else
    // names, so what is read through it is read through a variable of the
    // caller's own, which every object already covers where it is read.
    if (callee->flags.has(slang::ast::MethodFlags::DPIImport) ||
        callee->flags.has(slang::ast::MethodFlags::Constructor) ||
        callee->subroutineKind != slang::ast::SubroutineKind::Function ||
        IsRuntimeLibraryMethod(*callee)) {
      return;
    }
    const std::optional<hir::ExprId> lowered = AddLowered(call);
    if (!lowered.has_value()) return;

    const auto pass =
        [&](const slang::ast::Expression& actual) -> hir::ReportedArgument {
      if (CanEvaluate(standpoint_, actual)) {
        return hir::ReportedArgument::kEvaluated;
      }
      if (IsClassHandle(*actual.type)) {
        AddEveryObject();
      }
      if (IsVirtualInterface(*actual.type)) {
        reads_.unreportable =
            "a virtual interface a call is handed from what the function's own "
            "variables hold is not yet followed";
      }
      return hir::ReportedArgument::kDefaulted;
    };

    hir::ReportingCall reporting{
        .call = *lowered, .receiver = std::nullopt, .arguments = {}};
    if (const slang::ast::Expression* receiver = call.thisClass()) {
      reporting.receiver = pass(*receiver);
    } else if (IsInstanceMethod(*callee)) {
      // A method called bare runs on the running method's own object.
      reporting.receiver = hir::ReportedArgument::kEvaluated;
    }
    const auto formals = callee->getArguments();
    const auto actuals = call.arguments();
    if (formals.size() != actuals.size()) {
      throw InternalError(
          "ReadsCollector: a call's arguments do not match its callee's "
          "formals one for one");
    }
    reporting.arguments.reserve(actuals.size());
    for (std::size_t i = 0; i < actuals.size(); ++i) {
      if (formals[i]->direction == slang::ast::ArgumentDirection::Ref &&
          !CanEvaluate(standpoint_, *actuals[i])) {
        reads_.unreportable =
            "a `ref` argument naming what the function's own variables hold "
            "is not yet followed";
      }
      reporting.arguments.push_back(pass(*actuals[i]));
    }
    reads_.calls.push_back(std::move(reporting));
  }

  ProcessLowerer* proc_;
  WalkFrame frame_;
  Standpoint standpoint_;
  diag::SourceSpan span_;
  hir::Reads reads_;
  std::optional<diag::Diagnostic> failure_;
};

// Adds the cells `sealed` names to `leaves`.
void AddSealed(
    std::vector<hir::WaitLeaf>& leaves,
    std::vector<hir::SensitivityEntry> sealed) {
  leaves.insert(
      leaves.begin(), std::make_move_iterator(sealed.begin()),
      std::make_move_iterator(sealed.end()));
}

// Adds to `reads` the cells a body's own text reads and writes, beside what the
// walk over that text found through handles and calls.
auto AddAccesses(
    ProcessLowerer& proc, const NodeAccesses& accesses, const WalkFrame& frame,
    hir::Reads& reads) -> diag::Result<void> {
  auto sealed = proc.Owner().SensitivityEntriesOf(proc, accesses.reads, frame);
  if (!sealed) return std::unexpected(std::move(sealed.error()));
  auto writes = proc.Owner().SensitivityEntriesOf(proc, accesses.writes, frame);
  if (!writes) return std::unexpected(std::move(writes.error()));
  AddSealed(reads.leaves, *std::move(sealed));
  reads.writes = *std::move(writes);
  return {};
}

}  // namespace

auto CellsOfWaitedExpression(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::Expression& expr)
    -> diag::Result<std::vector<hir::SensitivityEntry>> {
  return proc.Owner().SensitivityEntriesOf(
      proc,
      proc.Owner().Sensitivity().AnalyzeReads(expr, proc.ContainingSymbol()),
      frame);
}

auto ReadsOfFunctionBody(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::SubroutineSymbol& function, diag::SourceSpan span)
    -> diag::Result<hir::Reads> {
  // The body is compiled whether or not a wait calls it, so what its report
  // cannot state yet is the wait's to refuse when it asks, never the body's.
  const auto refused_when_asked = [](diag::Diagnostic diagnostic) {
    return hir::Reads{
        .leaves = {},
        .writes = {},
        .calls = {},
        .unreportable = std::move(diagnostic.primary.message)};
  };
  ReadsCollector collector(proc, frame, InAReport{.function = &function}, span);
  function.getBody().visit(collector);
  auto reads = std::move(collector).TakeReads();
  if (!reads) return refused_when_asked(std::move(reads.error()));
  auto added = AddAccesses(
      proc, proc.Owner().Sensitivity().AnalyzeAccesses(function), frame,
      *reads);
  if (!added) return refused_when_asked(std::move(added.error()));
  return reads;
}

auto ReadsOfProcedure(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ProceduralBlockSymbol& procedure, diag::SourceSpan span)
    -> diag::Result<hir::Reads> {
  // Each function states what a call of it reads and writes, which a report
  // collects ahead of the first run; the procedure states what its own text
  // reads and writes, and what that text reaches through a handle, which the
  // report keeps apart.
  ReadsCollector collector(
      proc, frame, BeforeTheProcedure{.proc = &procedure}, span);
  procedure.getBody().visit(collector);
  auto reads = std::move(collector).TakeReads();
  if (!reads) return std::unexpected(std::move(reads.error()));
  if (reads->unreportable.has_value()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedStatementForm,
        std::format(
            "an always_comb or always_latch reading this is not yet "
            "supported: {}",
            *reads->unreportable));
  }
  auto added = AddAccesses(
      proc, proc.Owner().Sensitivity().AnalyzeProcedureText(procedure), frame,
      *reads);
  if (!added) return std::unexpected(std::move(added.error()));
  return reads;
}

}  // namespace lyra::lowering::ast_to_hir

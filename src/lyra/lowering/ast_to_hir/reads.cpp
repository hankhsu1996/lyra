#include "lyra/lowering/ast_to_hir/reads.hpp"

#include <algorithm>
#include <expected>
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

// Whether a function's report can evaluate `expr` where it stands, which is
// ahead of the body: reading its formals, the object it runs on and storage
// that exists before the body runs, and nothing else. Its automatic variables
// hold nothing yet, a handle it has not tested may be null, and a call may do
// either.
class ReportCanEvaluate
    : public slang::ast::ASTVisitor<
          ReportCanEvaluate, slang::ast::VisitFlags::Expressions> {
 public:
  explicit ReportCanEvaluate(const slang::ast::SubroutineSymbol& function)
      : function_(&function) {
  }

  [[nodiscard]] auto Evaluable() const -> bool {
    return evaluable_;
  }

  void handle(const slang::ast::NamedValueExpression& named) {
    const slang::ast::Symbol& symbol = named.symbol;
    if (!HoldsStateBeforeItRuns(symbol, *function_) &&
        !IsFormalOf(symbol, *function_) && &symbol != function_->thisVar) {
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
    visitDefault(access);
  }

  // A system function answers from its arguments alone; a user one may read
  // anything, including what the report is not standing in.
  void handle(const slang::ast::CallExpression& call) {
    if (!call.isSystemCall()) {
      evaluable_ = false;
      return;
    }
    visitDefault(call);
  }

 private:
  const slang::ast::SubroutineSymbol* function_;
  bool evaluable_ = true;
};

// Whether `function`'s report, which stands ahead of its body, can evaluate
// `expr`.
auto CanEvaluate(
    const slang::ast::SubroutineSymbol& function,
    const slang::ast::Expression& expr) -> bool {
  ReportCanEvaluate check(function);
  expr.visit(check);
  return check.Evaluable();
}

// Walks a function body's statements for what it reaches beyond the variables
// it names: the objects and interface variables found through a handle, and the
// calls of functions whose reports say what they read.
class ReadsCollector : public slang::ast::ASTVisitor<
                           ReadsCollector, slang::ast::VisitFlags::AllGood> {
 public:
  ReadsCollector(
      ProcessLowerer& proc, WalkFrame frame,
      const slang::ast::SubroutineSymbol& function, diag::SourceSpan span)
      : proc_(&proc),
        frame_(std::move(frame)),
        function_(&function),
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
    if (!CanEvaluate(*function_, root)) {
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
      auto target = proc_->Owner().MakeClassPropertyTarget(
          frame_, owner, *property, span_);
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
    if (!CanEvaluate(*function_, handle)) {
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
      if (CanEvaluate(*function_, actual)) {
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
    } else if (const auto* declaring = callee->getParentScope();
               declaring != nullptr &&
               declaring->asSymbol().kind ==
                   slang::ast::SymbolKind::ClassType &&
               !callee->flags.has(slang::ast::MethodFlags::Static)) {
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
          !CanEvaluate(*function_, *actuals[i])) {
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
  const slang::ast::SubroutineSymbol* function_;
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

}  // namespace

auto CellsOfWaitedExpression(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::Expression& expr)
    -> diag::Result<std::vector<hir::SensitivityEntry>> {
  return proc.Owner().TranslateSensitivityReads(
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
        .calls = {},
        .unreportable = std::move(diagnostic.primary.message)};
  };
  ReadsCollector collector(proc, frame, function, span);
  function.getBody().visit(collector);
  auto reads = std::move(collector).TakeReads();
  if (!reads) return refused_when_asked(std::move(reads.error()));

  auto sealed = proc.Owner().TranslateSensitivityReads(
      proc, proc.Owner().Sensitivity().AnalyzeReads(function), frame);
  if (!sealed) return refused_when_asked(std::move(sealed.error()));
  AddSealed(reads->leaves, *std::move(sealed));
  return reads;
}

}  // namespace lyra::lowering::ast_to_hir

#include "lyra/lowering/ast_to_hir/covergroup_access.hpp"

#include <expected>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CoverSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/covergroup_ref.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto IsItem(const slang::ast::Symbol& symbol) -> bool {
  return symbol.kind == slang::ast::SymbolKind::Coverpoint ||
         symbol.kind == slang::ast::SymbolKind::CoverCross;
}

// Where a member the front end declares on a covergroup sits: the covergroup,
// and the coverpoint or cross it is a member of, absent where it is the
// covergroup's own.
struct Located {
  const slang::ast::CovergroupType* covergroup = nullptr;
  const slang::ast::Symbol* item = nullptr;
};

auto Locate(const slang::ast::Symbol& member) -> Located {
  const slang::ast::Symbol* level = &member.getParentScope()->asSymbol();
  Located located;
  if (IsItem(*level)) {
    located.item = level;
    level = &level->getParentScope()->asSymbol();
  }
  if (level->kind == slang::ast::SymbolKind::CovergroupBody) {
    level = &level->getParentScope()->asSymbol();
  }
  located.covergroup = &level->as<slang::ast::CovergroupType>();
  return located;
}

// The member an expression names and the expression it names it through,
// absent where it is named alone.
struct Named {
  const slang::ast::Symbol* member = nullptr;
  const slang::ast::Expression* base = nullptr;
};

auto NamedBy(const slang::ast::Expression& expr) -> std::optional<Named> {
  if (const auto* access = expr.as_if<slang::ast::MemberAccessExpression>()) {
    return Named{.member = &access->member, .base = &access->value()};
  }
  if (const slang::ast::Symbol* symbol = expr.getSymbolReference()) {
    return Named{.member = symbol, .base = nullptr};
  }
  return std::nullopt;
}

// The covergroup instance a member is reached through, and the item it is
// reached for: `inst.member`, or `inst.item.member`. Named alone it is reached
// through no instance the expression states.
struct Reached {
  Located at;
  const slang::ast::Expression* instance = nullptr;
};

auto ReachOf(
    const slang::ast::Symbol& member, const slang::ast::Expression* base)
    -> Reached {
  Reached reached{.at = Locate(member), .instance = nullptr};
  if (base == nullptr) return reached;
  if (reached.at.item == nullptr) {
    reached.instance = base;
    return reached;
  }
  if (const auto* access = base->as_if<slang::ast::MemberAccessExpression>();
      access != nullptr && IsItem(access->member)) {
    reached.instance = &access->value();
  }
  return reached;
}

// The covergroup a reach names: the static type of the instance where there
// is one, since a covergroup extending another is reached through what it
// inherits (LRM 19.4.1), and otherwise the one declaring the member.
auto CovergroupOf(const Reached& reached) -> const slang::ast::CovergroupType& {
  if (reached.instance != nullptr) {
    if (const auto* covergroup = reached.instance->type->getCanonicalType()
                                     .as_if<slang::ast::CovergroupType>()) {
      return *covergroup;
    }
  }
  return *reached.at.covergroup;
}

auto ItemName(const Reached& reached) -> std::optional<std::string> {
  if (reached.at.item == nullptr) return std::nullopt;
  return std::string{reached.at.item->name};
}

auto MethodOf(std::string_view name) -> hir::CovergroupMethod {
  if (name == "sample") return hir::CovergroupMethod::kSample;
  if (name == "get_coverage") return hir::CovergroupMethod::kGetCoverage;
  if (name == "get_inst_coverage") {
    return hir::CovergroupMethod::kGetInstCoverage;
  }
  if (name == "set_inst_name") return hir::CovergroupMethod::kSetInstName;
  if (name == "start") return hir::CovergroupMethod::kStart;
  if (name == "stop") return hir::CovergroupMethod::kStop;
  throw InternalError(
      std::format(
          "LowerCovergroupMethodCall: the front end declares no coverage "
          "method '{}'",
          name));
}

// Whether a call of a coverage method was handed the two counts (LRM 19.8):
// where the source leaves them out the front end fills in their defaults,
// which are constants no variable a `ref` names is.
auto CountsWereGiven(const slang::ast::CallExpression& call) -> bool {
  const auto arguments = call.arguments();
  return !arguments.empty() &&
         arguments[0]->kind != slang::ast::ExpressionKind::IntegerLiteral;
}

// The covergroup's type as a use reaching it with no instance names it, which
// reaches the instance the type belongs to, where it belongs to one (LRM 6.22).
auto TypeReceiver(
    UnitLowerer& unit_lowerer, const slang::ast::CovergroupType& covergroup,
    const WalkFrame& frame, diag::SourceSpan span)
    -> diag::Result<hir::CovergroupReceiver> {
  if (!BelongsToAnInstance(covergroup)) {
    return hir::CovergroupTypeReceiver{.declaring_instance = std::nullopt};
  }
  auto reach = unit_lowerer.ReachDeclaringInstance(covergroup, frame, span);
  if (!reach) return std::unexpected(std::move(reach.error()));
  return hir::CovergroupTypeReceiver{.declaring_instance = *std::move(reach)};
}

// The instance `instance` names, as the receiver of what it is used for.
template <ExprLowerer Lowerer>
auto InstanceReceiver(
    Lowerer& lowerer, const WalkFrame& frame,
    const slang::ast::Expression& instance)
    -> diag::Result<hir::CovergroupReceiver> {
  auto lowered = lowerer.LowerExpr(instance, frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  return hir::CovergroupInstanceReceiver{
      .instance = frame.Exprs().Add(*std::move(lowered))};
}

}  // namespace

auto IsCovergroupMember(const slang::ast::Symbol& member) -> bool {
  if (IsItem(member)) return true;
  if (member.kind != slang::ast::SymbolKind::ClassProperty &&
      member.kind != slang::ast::SymbolKind::Subroutine) {
    return false;
  }
  const slang::ast::Scope* scope = member.getParentScope();
  if (scope == nullptr) return false;
  const slang::ast::SymbolKind kind = scope->asSymbol().kind;
  return kind == slang::ast::SymbolKind::CovergroupBody ||
         kind == slang::ast::SymbolKind::Coverpoint ||
         kind == slang::ast::SymbolKind::CoverCross;
}

auto NamesCoverageOptions(const slang::ast::Expression& options) -> bool {
  const std::optional<Named> named = NamedBy(options);
  return named.has_value() &&
         named->member->kind == slang::ast::SymbolKind::ClassProperty &&
         IsCovergroupMember(*named->member);
}

template <ExprLowerer Lowerer>
auto LowerCoverageOption(
    Lowerer& lowerer, const WalkFrame& frame,
    const slang::ast::Expression& options, std::string_view field,
    const slang::ast::Type& type, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  const Named named = *NamedBy(options);
  const bool type_option = named.member->name == "type_option";
  const Reached reached = ReachOf(*named.member, named.base);
  UnitLowerer& unit_lowerer = lowerer.Owner();
  auto covergroup =
      unit_lowerer.ResolveCovergroupRef(CovergroupOf(reached), span);
  if (!covergroup) return std::unexpected(std::move(covergroup.error()));
  auto type_id = unit_lowerer.InternType(type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  // A type's options are the type's, whatever names them (LRM 19.7).
  if (!type_option && reached.instance == nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedClassFeature,
        "reading an instance's option from the covergroup's own "
        "expressions is not yet supported");
  }
  auto receiver =
      type_option
          ? TypeReceiver(unit_lowerer, CovergroupOf(reached), frame, span)
          : InstanceReceiver(lowerer, frame, *reached.instance);
  if (!receiver) return std::unexpected(std::move(receiver.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::CovergroupOptionExpr{
              .covergroup = *std::move(covergroup),
              .receiver = *std::move(receiver),
              .item = ItemName(reached),
              .field = std::string{field}},
      .span = span};
}

auto ReachedThroughEmbedding(
    const WalkFrame& frame, const slang::ast::Symbol& member) -> bool {
  if (!frame.embedding.has_value()) return false;
  const slang::ast::Scope* declaring = member.getParentScope();
  if (declaring == nullptr) return false;
  // The embedding class, or a class it extends, declares it.
  for (const slang::ast::Type* cls = frame.embedding->cls; cls != nullptr;
       cls =
           cls->getCanonicalType().as<slang::ast::ClassType>().getBaseClass()) {
    if (&cls->getCanonicalType().as<slang::ast::ClassType>() ==
        &declaring->asSymbol()) {
      return true;
    }
  }
  return false;
}

auto EmbeddingObject(const WalkFrame& frame, diag::SourceSpan span)
    -> hir::ExprId {
  const hir::ProceduralVarId object = frame.embedding->object;
  const hir::TypeId type =
      frame.current_procedural_body->procedural_vars.Get(object).type;
  return frame.Exprs().Add(
      hir::MakeRefExpr(hir::ProceduralVarRef{.var = object}, type, span));
}

template <ExprLowerer Lowerer>
auto LowerNewCovergroup(
    Lowerer& lowerer, const WalkFrame& frame,
    const slang::ast::NewCovergroupExpression& expr,
    std::vector<hir::ExprId> arguments, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const auto& covergroup =
      expr.type->getCanonicalType().as<slang::ast::CovergroupType>();
  auto ref = unit_lowerer.ResolveCovergroupRef(covergroup, span);
  if (!ref) return std::unexpected(std::move(ref.error()));
  auto type = unit_lowerer.InternExpressionType(expr, span);
  if (!type) return std::unexpected(std::move(type.error()));
  hir::CovergroupNewExpr created{
      .covergroup = *std::move(ref),
      .arguments = std::move(arguments),
      .embedding = std::nullopt,
      .declaring_instance = std::nullopt};
  // One a class embeds is made in that class's `new`, by the object being
  // made (LRM 19.4). Either is a type of the instance replicating it, where
  // one does (LRM 6.22).
  if (const auto* embedding = covergroup.getParentScope()
                                  ->asSymbol()
                                  .as_if<slang::ast::ClassType>()) {
    auto embedding_type = unit_lowerer.InternType(*embedding, span);
    if (!embedding_type) {
      return std::unexpected(std::move(embedding_type.error()));
    }
    created.embedding = frame.Exprs().Add(
        hir::MakeRefExpr(hir::ThisHandle{}, *embedding_type, span));
  }
  if (BelongsToAnInstance(covergroup)) {
    auto instance =
        unit_lowerer.ReachDeclaringInstance(covergroup, frame, span);
    if (!instance) return std::unexpected(std::move(instance.error()));
    created.declaring_instance = *std::move(instance);
  }
  return hir::Expr{.type = *type, .data = std::move(created), .span = span};
}

template <ExprLowerer Lowerer>
auto LowerCovergroupMethodCall(
    Lowerer& lowerer, const WalkFrame& frame,
    const slang::ast::CallExpression& call,
    std::vector<std::optional<hir::ExprId>> arguments, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  const auto& method =
      *std::get<const slang::ast::SubroutineSymbol*>(call.subroutine);
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const Reached reached = ReachOf(method, call.thisClass());
  auto covergroup =
      unit_lowerer.ResolveCovergroupRef(CovergroupOf(reached), span);
  if (!covergroup) return std::unexpected(std::move(covergroup.error()));
  auto type = unit_lowerer.InternExpressionType(call, span);
  if (!type) return std::unexpected(std::move(type.error()));
  const hir::CovergroupMethod kind = MethodOf(method.name);
  // A static method (LRM 19.8) is the type's, so whatever it was called
  // through is not evaluated.
  const bool of_type = method.flags.has(slang::ast::MethodFlags::Static);
  if (!of_type && reached.instance == nullptr) {
    throw InternalError(
        std::format(
            "LowerCovergroupMethodCall: '{}' is called on an instance, and "
            "this call names none",
            method.name));
  }
  auto receiver =
      of_type ? TypeReceiver(unit_lowerer, CovergroupOf(reached), frame, span)
              : InstanceReceiver(lowerer, frame, *reached.instance);
  if (!receiver) return std::unexpected(std::move(receiver.error()));
  // The counts a coverage query is handed are left to their defaults where
  // the source wrote none.
  if ((kind == hir::CovergroupMethod::kGetCoverage ||
       kind == hir::CovergroupMethod::kGetInstCoverage) &&
      !CountsWereGiven(call)) {
    arguments.clear();
  }
  return hir::Expr{
      .type = *type,
      .data =
          hir::CovergroupMethodCallExpr{
              .covergroup = *std::move(covergroup),
              .receiver = *std::move(receiver),
              .item = ItemName(reached),
              .method = kind,
              .arguments = std::move(arguments)},
      .span = span};
}

template auto LowerCoverageOption(
    ProcessLowerer&, const WalkFrame&, const slang::ast::Expression&,
    std::string_view, const slang::ast::Type&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerCoverageOption(
    StructuralScopeLowerer&, const WalkFrame&, const slang::ast::Expression&,
    std::string_view, const slang::ast::Type&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerNewCovergroup(
    ProcessLowerer&, const WalkFrame&,
    const slang::ast::NewCovergroupExpression&, std::vector<hir::ExprId>,
    diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerNewCovergroup(
    StructuralScopeLowerer&, const WalkFrame&,
    const slang::ast::NewCovergroupExpression&, std::vector<hir::ExprId>,
    diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerCovergroupMethodCall(
    ProcessLowerer&, const WalkFrame&, const slang::ast::CallExpression&,
    std::vector<std::optional<hir::ExprId>>, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerCovergroupMethodCall(
    StructuralScopeLowerer&, const WalkFrame&,
    const slang::ast::CallExpression&, std::vector<std::optional<hir::ExprId>>,
    diag::SourceSpan) -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir

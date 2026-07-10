#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/Type.h>
#include <slang/syntax/AllSyntax.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/foreign_import.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/support/dpi_abi.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto ToHirSubroutineKind(slang::ast::SubroutineKind k) -> hir::SubroutineKind {
  switch (k) {
    case slang::ast::SubroutineKind::Function:
      return hir::SubroutineKind::kFunction;
    case slang::ast::SubroutineKind::Task:
      return hir::SubroutineKind::kTask;
  }
  throw InternalError("ToHirSubroutineKind: unknown SubroutineKind");
}

// slang has no ConstRef direction (LRM 13.5.2): a `const ref` formal carries
// direction Ref with the Const variable flag, so the const-ness must be read
// off the formal rather than the direction enum alone.
auto ParamDirectionOf(const slang::ast::FormalArgumentSymbol& formal)
    -> hir::ParamDirection {
  switch (formal.direction) {
    case slang::ast::ArgumentDirection::In:
      return hir::ParamDirection::kInput;
    case slang::ast::ArgumentDirection::Out:
      return hir::ParamDirection::kOutput;
    case slang::ast::ArgumentDirection::InOut:
      return hir::ParamDirection::kInOut;
    case slang::ast::ArgumentDirection::Ref:
      return formal.flags.has(slang::ast::VariableFlags::Const)
                 ? hir::ParamDirection::kConstRef
                 : hir::ParamDirection::kRef;
  }
  throw InternalError("ParamDirectionOf: unknown ArgumentDirection");
}

// Classifies an SV type into its DPI-C carrier category (LRM 35.5.6). This is
// the one place slang types are inspected for DPI. The supported set is 2-state
// integral scalars up to one machine word, `real`, `string`, and `void`;
// anything else is a located Unsupported so the gap is a clean diagnostic.
auto ClassifyDpiAbi(const slang::ast::Type& type, diag::SourceSpan span)
    -> diag::Result<support::DpiAbiClass> {
  const auto& t = type.getCanonicalType();
  if (t.isVoid()) return support::DpiAbiClass::kVoid;
  if (t.isString()) return support::DpiAbiClass::kString;
  if (t.isFloating()) {
    if (t.as<slang::ast::FloatingType>().floatKind ==
        slang::ast::FloatingType::Real) {
      return support::DpiAbiClass::kReal;
    }
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedDpi,
        "DPI-C shortreal is not yet supported");
  }
  if (t.isIntegral()) {
    if (t.isFourState()) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedDpi,
          "4-state DPI-C value is not yet supported");
    }
    const auto width = t.getBitWidth();
    if (width == 1) return support::DpiAbiClass::kBit;
    if (width <= 8) return support::DpiAbiClass::kByte;
    if (width <= 16) return support::DpiAbiClass::kShortInt;
    if (width <= 32) return support::DpiAbiClass::kInt;
    if (width <= 64) return support::DpiAbiClass::kLongInt;
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedDpi,
        "wide packed DPI-C value is not yet supported");
  }
  if (t.isCHandle()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedDpi,
        "chandle DPI-C argument is not yet supported");
  }
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedDpi, "DPI-C type is not yet supported");
}

// The direction of a DPI-C formal argument (LRM 35.5.1.2). `ref` is illegal in
// import declarations (LRM 35.5.4), so only input / output / inout arrive here.
auto ClassifyDpiDirection(
    const slang::ast::FormalArgumentSymbol& formal, diag::SourceSpan span)
    -> diag::Result<support::DpiDirection> {
  switch (formal.direction) {
    case slang::ast::ArgumentDirection::In:
      return support::DpiDirection::kInput;
    case slang::ast::ArgumentDirection::Out:
      return support::DpiDirection::kOutput;
    case slang::ast::ArgumentDirection::InOut:
      return support::DpiDirection::kInout;
    case slang::ast::ArgumentDirection::Ref:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedDpi,
          "DPI-C ref argument is not allowed in an import declaration");
  }
  throw InternalError("ClassifyDpiDirection: unknown ArgumentDirection");
}

// The resolved C linkage name of a DPI import (LRM 35.5.4): the explicit
// `c_identifier` token when present, otherwise the SV subroutine name. slang
// does not persist this for imports, so it is re-derived from the syntax here.
auto ResolveImportCName(const slang::ast::SubroutineSymbol& sym)
    -> std::string {
  if (const auto* syntax = sym.getSyntax();
      syntax != nullptr &&
      syntax->kind == slang::syntax::SyntaxKind::DPIImport) {
    const auto& dpi = syntax->as<slang::syntax::DPIImportSyntax>();
    if (dpi.c_identifier) {
      return std::string{dpi.c_identifier.valueText()};
    }
  }
  return std::string{sym.name};
}

}  // namespace

auto LowerSubroutineDecl(
    ModuleLowerer& module, const slang::ast::SubroutineSymbol& sym,
    WalkFrame frame) -> diag::Result<hir::SubroutineDecl> {
  const auto& mapper = module.SourceMapper();

  auto return_type_or =
      module.InternType(sym.getReturnType(), mapper.PointSpanOf(sym.location));
  if (!return_type_or) {
    return std::unexpected(std::move(return_type_or.error()));
  }

  hir::ProceduralBody body;
  ProcessLowerer lowerer(module, sym);
  lowerer.AnalyzeLifetimeExtended(sym.getBody());

  // Open the root lexical scope accumulators for this subroutine body. The
  // formal params, the implicit result var, and every body-tree var declared
  // below feed into these vectors and become the root scope's direct
  // declarations. Nested begin/end / fork / foreach scopes push their ids
  // into the root scope's children list.
  std::vector<hir::ProceduralVarId> root_declarations;
  std::vector<hir::ProceduralScopeId> root_children;
  const WalkFrame body_frame =
      frame.WithProceduralBody(&body, &body.exprs)
          .WithProceduralScopeAccumulators(&root_declarations, &root_children);

  std::vector<hir::SubroutineParam> params;
  params.reserve(sym.getArguments().size());
  for (const auto* formal : sym.getArguments()) {
    auto formal_type_or = module.InternType(
        formal->getType(), mapper.PointSpanOf(formal->location));
    if (!formal_type_or) {
      return std::unexpected(std::move(formal_type_or.error()));
    }
    const hir::ProceduralVarId var =
        lowerer.AddProceduralVar(body_frame, body, *formal, *formal_type_or);
    params.push_back(
        hir::SubroutineParam{
            .var = var, .direction = ParamDirectionOf(*formal)});
  }

  std::optional<hir::ProceduralVarId> result_var;
  if (sym.returnValVar != nullptr) {
    result_var = lowerer.AddProceduralVar(
        body_frame, body, *sym.returnValVar, *return_type_or);
  }

  auto body_stmt_or = lowerer.LowerStmt(sym.getBody(), body_frame);
  if (!body_stmt_or) return std::unexpected(std::move(body_stmt_or.error()));
  body.root_stmt = body.stmts.Add(*std::move(body_stmt_or));

  // Append the assembled root scope to the enclosing structural scope's
  // procedural-scope arena when one is available (a class method body has
  // no structural-scope context; its locals are placed directly on the
  // class and the root-scope record has no consumer).
  if (frame.current_structural_scope != nullptr) {
    body.root_scope = frame.current_structural_scope->procedural_scopes.Add(
        hir::ProceduralScopeDecl{
            .label = std::nullopt,
            .direct_declarations = std::move(root_declarations),
            .direct_child_scopes = std::move(root_children)});
  }

  return hir::SubroutineDecl{
      .name = std::string{sym.name},
      .kind = ToHirSubroutineKind(sym.subroutineKind),
      .result_type = *return_type_or,
      .params = std::move(params),
      .result_var = result_var,
      .body = std::move(body)};
}

// Lowers a DPI-C import declaration (LRM 35.4) to a bodyless external callable.
// It has no SV body, so it is not a `SubroutineDecl`; the caller records it
// among the scope's foreign imports, not its subroutines. The ABI
// classification and the foreign name are resolved once here and never
// re-derived downstream. Input-only scalar functions are accepted; the
// remaining forms are located diagnostics.
auto LowerForeignImport(
    ModuleLowerer& module, const slang::ast::SubroutineSymbol& sym)
    -> diag::Result<hir::ForeignImportDecl> {
  const auto& mapper = module.SourceMapper();
  const auto loc = mapper.PointSpanOf(sym.location);
  if (sym.subroutineKind == slang::ast::SubroutineKind::Task) {
    return diag::Fail(
        loc, diag::DiagCode::kUnsupportedDpi,
        "DPI-C import task is not yet supported");
  }
  if (sym.flags.has(slang::ast::MethodFlags::DPIContext)) {
    return diag::Fail(
        loc, diag::DiagCode::kUnsupportedDpi,
        "DPI-C context import is not yet supported");
  }
  auto ret_abi = ClassifyDpiAbi(sym.getReturnType(), loc);
  if (!ret_abi) return std::unexpected(std::move(ret_abi.error()));
  auto ret_type = module.InternType(sym.getReturnType(), loc);
  if (!ret_type) return std::unexpected(std::move(ret_type.error()));

  std::vector<hir::DpiParamAbi> abi_params;
  abi_params.reserve(sym.getArguments().size());
  for (const auto* formal : sym.getArguments()) {
    const auto floc = mapper.PointSpanOf(formal->location);
    auto direction = ClassifyDpiDirection(*formal, floc);
    if (!direction) return std::unexpected(std::move(direction.error()));
    auto abi = ClassifyDpiAbi(formal->getType(), floc);
    if (!abi) return std::unexpected(std::move(abi.error()));
    auto param_type = module.InternType(formal->getType(), floc);
    if (!param_type) return std::unexpected(std::move(param_type.error()));
    abi_params.push_back(
        hir::DpiParamAbi{
            .sv_type = *param_type, .abi = *abi, .direction = *direction});
  }

  return hir::ForeignImportDecl{
      .name = std::string{sym.name},
      .foreign_name = ResolveImportCName(sym),
      .is_pure = sym.flags.has(slang::ast::MethodFlags::Pure),
      .ret_abi = *ret_abi,
      .ret_sv_type = *ret_type,
      .params = std::move(abi_params)};
}

}  // namespace lyra::lowering::ast_to_hir

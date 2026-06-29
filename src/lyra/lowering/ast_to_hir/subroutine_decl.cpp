#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"

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
  const WalkFrame body_frame = frame.WithProceduralBody(&body, &body.exprs);

  std::vector<hir::SubroutineParam> params;
  params.reserve(sym.getArguments().size());
  for (const auto* formal : sym.getArguments()) {
    auto formal_type_or = module.InternType(
        formal->getType(), mapper.PointSpanOf(formal->location));
    if (!formal_type_or) {
      return std::unexpected(std::move(formal_type_or.error()));
    }
    const hir::ProceduralVarId var =
        lowerer.AddProceduralVar(body, *formal, *formal_type_or);
    params.push_back(
        hir::SubroutineParam{
            .var = var, .direction = ParamDirectionOf(*formal)});
  }

  std::optional<hir::ProceduralVarId> result_var;
  if (sym.returnValVar != nullptr) {
    result_var =
        lowerer.AddProceduralVar(body, *sym.returnValVar, *return_type_or);
  }

  auto body_stmt_or = lowerer.LowerStmt(sym.getBody(), body_frame);
  if (!body_stmt_or) return std::unexpected(std::move(body_stmt_or.error()));
  body.root_stmt = body.stmts.Add(*std::move(body_stmt_or));

  return hir::SubroutineDecl{
      .name = std::string{sym.name},
      .kind = ToHirSubroutineKind(sym.subroutineKind),
      .result_type = *return_type_or,
      .params = std::move(params),
      .result_var = result_var,
      .body = std::move(body)};
}

}  // namespace lyra::lowering::ast_to_hir

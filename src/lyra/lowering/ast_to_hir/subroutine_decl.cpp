#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
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
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/support/dpi_abi.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

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

// Classifies an SV type into its DPI-C carrier (LRM 35.5.6, Annex H.10 /
// Table H.1). This is the one place slang types are inspected for DPI. The
// dispatch is on the declared type shape, not the bit width -- WYSIWYG
// (LRM 35.6.1.1) makes `int` (by-value C `int`) and `bit [31:0]` (canonical
// `svBitVecVal*`) different DPI ABIs even though both are 32-bit 2-state.
//
//   scalar `bit` / `logic` (1 bit)        -> by-value `svBit` / `svLogic`
//   predefined `byte`/`shortint`/`int`/`longint` (2-state) -> by-value C int
//   predefined `integer` / `time` (4-state) -> canonical logic vector
//   packed `bit [N:0]`                    -> canonical bit vector
//   packed `logic [N:0]`                  -> canonical logic vector
//
// `real`, `string`, `chandle`, `void` are by-value scalars. `shortreal`, packed
// struct / union, and open arrays are a located Unsupported so the gap stays a
// clean diagnostic.
auto ClassifyDpiCarrier(const slang::ast::Type& type, diag::SourceSpan span)
    -> diag::Result<support::DpiCarrier> {
  const auto& t = type.getCanonicalType();
  const auto scalar = [](support::DpiScalarAbi abi) -> support::DpiCarrier {
    return support::ScalarCarrier{abi};
  };
  if (t.isVoid()) return scalar(support::DpiScalarAbi::kVoid);
  if (t.isString()) return scalar(support::DpiScalarAbi::kString);
  if (t.isCHandle()) return scalar(support::DpiScalarAbi::kChandle);
  if (t.isFloating()) {
    if (t.as<slang::ast::FloatingType>().floatKind ==
        slang::ast::FloatingType::Real) {
      return scalar(support::DpiScalarAbi::kReal);
    }
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedDpi,
        "DPI-C shortreal is not yet supported");
  }
  if (t.isIntegral()) {
    // A 1-bit scalar crosses by value as svBit / svLogic (LRM Table H.1).
    if (t.isScalar()) {
      return scalar(
          t.isFourState() ? support::DpiScalarAbi::kLogicScalar
                          : support::DpiScalarAbi::kBitScalar);
    }
    // A predefined named integer crosses by value if 2-state; the 4-state
    // `integer` / `time` are canonical logic vectors (LRM H.7.3).
    if (t.isPredefinedInteger()) {
      if (t.isFourState()) {
        return support::VectorCarrier{.four_state = true};
      }
      switch (t.getBitWidth()) {
        case 8:
          return scalar(support::DpiScalarAbi::kByte);
        case 16:
          return scalar(support::DpiScalarAbi::kShortInt);
        case 32:
          return scalar(support::DpiScalarAbi::kInt);
        case 64:
          return scalar(support::DpiScalarAbi::kLongInt);
        default:
          break;
      }
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedDpi,
          "DPI-C integer type is not yet supported");
    }
    // A packed array of bit / logic crosses by pointer as a canonical vector.
    if (t.isPackedArray()) {
      return support::VectorCarrier{.four_state = t.isFourState()};
    }
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedDpi,
        "packed struct / union DPI-C arguments are not yet supported");
  }
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedDpi, "DPI-C type is not yet supported");
}

// Classifies a DPI-C function result. LRM 35.5.5 restricts a result to a small
// value -- a by-value scalar; a canonical vector (a packed value, `integer`, or
// `time`) cannot be returned. So a vector carrier here is an error, not a
// by-pointer result.
auto ClassifyDpiScalarResult(
    const slang::ast::Type& type, diag::SourceSpan span)
    -> diag::Result<support::DpiScalarAbi> {
  auto carrier = ClassifyDpiCarrier(type, span);
  if (!carrier) return std::unexpected(std::move(carrier.error()));
  if (const auto* scalar = std::get_if<support::ScalarCarrier>(&*carrier)) {
    return scalar->abi;
  }
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedDpi,
      "a DPI-C function result must be a small value (LRM 35.5.5); a packed "
      "vector, integer, or time cannot be returned");
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

// The subroutine's callable code lowered against its own binding frame plus
// the optional base-call args lowered against that same frame. The two
// halves must share one frame because a base-call arg may reference a formal
// parameter, and that reference resolves through the same procedural-var
// registry the body lowering uses.
struct SubroutineLoweringResult {
  hir::SubroutineDecl decl;
  std::optional<hir::BaseCall> base_call;
};

auto LowerSubroutineDeclImpl(
    UnitLowerer& unit_lowerer, const slang::ast::SubroutineSymbol& sym,
    WalkFrame frame, const slang::ast::Expression* base_call_ast)
    -> diag::Result<SubroutineLoweringResult> {
  const auto& mapper = unit_lowerer.SourceMapper();

  auto return_type_or = unit_lowerer.InternType(
      sym.getReturnType(), mapper.PointSpanOf(sym.location));
  if (!return_type_or) {
    return std::unexpected(std::move(return_type_or.error()));
  }

  hir::ProceduralBody body;
  ConsumedBodyExpressions consumed_body_exprs;
  if (base_call_ast != nullptr) consumed_body_exprs.insert(base_call_ast);
  ProcessLowerer lowerer(unit_lowerer, sym, std::move(consumed_body_exprs));
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
    auto formal_type_or = unit_lowerer.InternType(
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

  // Base-call args (LRM 8.7) evaluate in the constructor's own frame and can
  // reference formals -- for example `super.new(a * 2)` where `a` is the
  // derived ctor's parameter. Lowering them here, before the body statements
  // (which the LRM requires super.new to precede), keeps them ahead of any
  // stateful body computation and ensures they see only the formals -- the
  // ordering the runtime observes when it invokes the base ctor first.
  std::optional<hir::BaseCall> base_call;
  if (base_call_ast != nullptr) {
    const auto& new_expr = base_call_ast->as<slang::ast::NewClassExpression>();
    const auto* ctor_call = new_expr.constructorCall();
    hir::BaseCall lowered;
    if (ctor_call != nullptr) {
      const auto& actuals =
          ctor_call->as<slang::ast::CallExpression>().arguments();
      lowered.arguments.reserve(actuals.size());
      for (const auto* actual : actuals) {
        auto arg_or = lowerer.LowerExpr(*actual, body_frame);
        if (!arg_or) return std::unexpected(std::move(arg_or.error()));
        lowered.arguments.push_back(body.exprs.Add(*std::move(arg_or)));
      }
    }
    base_call = std::move(lowered);
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

  return SubroutineLoweringResult{
      .decl =
          hir::SubroutineDecl{
              .name = std::string{sym.name},
              .kind = ToHirSubroutineKind(sym.subroutineKind),
              .result_type = *return_type_or,
              .params = std::move(params),
              .result_var = result_var,
              .body = std::move(body),
              .is_virtual = false,
              .overrides = std::nullopt},
      .base_call = std::move(base_call)};
}

}  // namespace

auto LowerSubroutineDecl(
    UnitLowerer& unit_lowerer, const slang::ast::SubroutineSymbol& sym,
    WalkFrame frame) -> diag::Result<hir::SubroutineDecl> {
  auto result_or = LowerSubroutineDeclImpl(unit_lowerer, sym, frame, nullptr);
  if (!result_or) return std::unexpected(std::move(result_or.error()));
  return std::move(result_or->decl);
}

auto LowerConstructorDecl(
    UnitLowerer& unit_lowerer, const slang::ast::SubroutineSymbol& sym,
    WalkFrame frame, const slang::ast::Expression* base_call_ast)
    -> diag::Result<ConstructorAndBaseCall> {
  auto result_or =
      LowerSubroutineDeclImpl(unit_lowerer, sym, frame, base_call_ast);
  if (!result_or) return std::unexpected(std::move(result_or.error()));
  return ConstructorAndBaseCall{
      .constructor = std::move(result_or->decl),
      .base_call = std::move(result_or->base_call)};
}

auto LowerMethodPrototypeDecl(
    UnitLowerer& unit_lowerer, const slang::ast::MethodPrototypeSymbol& proto)
    -> diag::Result<hir::SubroutineDecl> {
  const auto& mapper = unit_lowerer.SourceMapper();

  auto return_type_or = unit_lowerer.InternType(
      proto.getReturnType(), mapper.PointSpanOf(proto.location));
  if (!return_type_or) {
    return std::unexpected(std::move(return_type_or.error()));
  }

  hir::ProceduralBody body;
  ProcessLowerer lowerer(unit_lowerer, proto);

  // A pure virtual prototype has no body to walk, but it still has parameter
  // vars whose types the signature must expose. The scope accumulators are
  // written into by `AddProceduralVar` and discarded on return -- a prototype
  // has no lexical body scope for downstream consumers to attach to.
  std::vector<hir::ProceduralVarId> root_declarations;
  std::vector<hir::ProceduralScopeId> root_children;
  const WalkFrame body_frame =
      WalkFrame{}
          .WithProceduralBody(&body, &body.exprs)
          .WithProceduralScopeAccumulators(&root_declarations, &root_children);

  std::vector<hir::SubroutineParam> params;
  params.reserve(proto.getArguments().size());
  for (const auto* formal : proto.getArguments()) {
    auto formal_type_or = unit_lowerer.InternType(
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

  return hir::SubroutineDecl{
      .name = std::string{proto.name},
      .kind = ToHirSubroutineKind(proto.subroutineKind),
      .result_type = *return_type_or,
      .params = std::move(params),
      .result_var = std::nullopt,
      .body = std::move(body),
      .is_virtual = true,
      .is_prototype = true,
      .overrides = std::nullopt,
  };
}

// Classifies each formal argument's ABI projection (LRM 35.5.6): its direction,
// C ABI carrier, and SV type. Import and export declarations project their
// arguments identically, so both build their parameter list here.
auto ClassifyDpiParams(
    UnitLowerer& unit_lowerer, const slang::ast::SubroutineSymbol& sym)
    -> diag::Result<std::vector<hir::DpiParamAbi>> {
  const auto& mapper = unit_lowerer.SourceMapper();
  std::vector<hir::DpiParamAbi> abi_params;
  abi_params.reserve(sym.getArguments().size());
  for (const auto* formal : sym.getArguments()) {
    const auto floc = mapper.PointSpanOf(formal->location);
    auto direction = ClassifyDpiDirection(*formal, floc);
    if (!direction) return std::unexpected(std::move(direction.error()));
    auto carrier = ClassifyDpiCarrier(formal->getType(), floc);
    if (!carrier) return std::unexpected(std::move(carrier.error()));
    auto param_type = unit_lowerer.InternType(formal->getType(), floc);
    if (!param_type) return std::unexpected(std::move(param_type.error()));
    abi_params.push_back(
        hir::DpiParamAbi{
            .sv_type = *param_type,
            .carrier = *carrier,
            .direction = *direction});
  }
  return abi_params;
}

// Lowers a DPI-C import declaration (LRM 35.4) to a bodyless external callable.
// It has no SV body, so it is not a `SubroutineDecl`; the caller records it
// among the scope's foreign imports, not its subroutines. The ABI
// classification and the foreign name are resolved once here and never
// re-derived downstream. A function or a task is accepted; a task carries no
// return, so its `void` return classifies as `kVoid` through the same path. A
// `context` import remains a located diagnostic, its scope surface a separate
// concern.
auto LowerForeignImport(
    UnitLowerer& unit_lowerer, const slang::ast::SubroutineSymbol& sym)
    -> diag::Result<hir::ForeignImportDecl> {
  const auto& mapper = unit_lowerer.SourceMapper();
  const auto loc = mapper.PointSpanOf(sym.location);
  if (sym.flags.has(slang::ast::MethodFlags::DPIContext)) {
    return diag::Fail(
        loc, diag::DiagCode::kUnsupportedDpi,
        "DPI-C context import is not yet supported");
  }
  const bool is_task = sym.subroutineKind == slang::ast::SubroutineKind::Task;
  auto ret_abi = ClassifyDpiScalarResult(sym.getReturnType(), loc);
  if (!ret_abi) return std::unexpected(std::move(ret_abi.error()));
  auto ret_type = unit_lowerer.InternType(sym.getReturnType(), loc);
  if (!ret_type) return std::unexpected(std::move(ret_type.error()));

  auto abi_params = ClassifyDpiParams(unit_lowerer, sym);
  if (!abi_params) return std::unexpected(std::move(abi_params.error()));

  return hir::ForeignImportDecl{
      .name = std::string{sym.name},
      .foreign_name = ResolveImportCName(sym),
      .is_pure = sym.flags.has(slang::ast::MethodFlags::Pure),
      .is_task = is_task,
      .ret_abi = *ret_abi,
      .ret_sv_type = *ret_type,
      .params = std::move(*abi_params)};
}

auto LowerForeignExport(
    UnitLowerer& unit_lowerer, const slang::ast::SubroutineSymbol& sym,
    std::string_view foreign_name) -> diag::Result<hir::ForeignExportDecl> {
  const auto& mapper = unit_lowerer.SourceMapper();
  const auto loc = mapper.PointSpanOf(sym.location);
  auto ret_abi = ClassifyDpiScalarResult(sym.getReturnType(), loc);
  if (!ret_abi) return std::unexpected(std::move(ret_abi.error()));
  auto ret_type = unit_lowerer.InternType(sym.getReturnType(), loc);
  if (!ret_type) return std::unexpected(std::move(ret_type.error()));

  auto abi_params = ClassifyDpiParams(unit_lowerer, sym);
  if (!abi_params) return std::unexpected(std::move(abi_params.error()));

  return hir::ForeignExportDecl{
      .sv_name = std::string{sym.name},
      .foreign_name = std::string{foreign_name},
      .ret_abi = *ret_abi,
      .ret_sv_type = *ret_type,
      .params = std::move(*abi_params)};
}

}  // namespace lyra::lowering::ast_to_hir

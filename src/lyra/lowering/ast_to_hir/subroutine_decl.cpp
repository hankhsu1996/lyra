#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"

#include <algorithm>
#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/Type.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/frontend/slang_source_span.hpp"
#include "lyra/hir/foreign_import.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/name_in_a_path.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/reads.hpp"
#include "lyra/lowering/ast_to_hir/std_classes.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/support/dpi_abi.hpp"

namespace lyra::lowering::ast_to_hir {

auto DeclaringClassOf(const slang::ast::SubroutineSymbol& sym)
    -> const slang::ast::ClassType* {
  const slang::ast::Scope* scope = sym.getParentScope();
  if (scope == nullptr ||
      scope->asSymbol().kind != slang::ast::SymbolKind::ClassType) {
    return nullptr;
  }
  return &scope->asSymbol().as<slang::ast::ClassType>();
}

auto SubroutineOf(const slang::ast::Symbol& member)
    -> const slang::ast::SubroutineSymbol* {
  if (member.kind == slang::ast::SymbolKind::Subroutine) {
    return &member.as<slang::ast::SubroutineSymbol>();
  }
  if (member.kind == slang::ast::SymbolKind::MethodPrototype) {
    return member.as<slang::ast::MethodPrototypeSymbol>().getSubroutine();
  }
  return nullptr;
}

auto IsMadeOnAnObject(
    const slang::ast::CallExpression& call,
    const slang::ast::SubroutineSymbol& callee) -> bool {
  return call.thisClass() != nullptr ||
         (DeclaringClassOf(callee) != nullptr &&
          !callee.flags.has(slang::ast::MethodFlags::Static));
}

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

auto ViewDefinesTheName(const slang::ast::Symbol& item) -> bool {
  const auto* port = item.as_if<slang::ast::ModportPortSymbol>();
  return port != nullptr && port->explicitConnection != nullptr;
}

auto ModportReadName(std::string_view modport, std::string_view port)
    -> std::string {
  return std::format("modport read {} {}", modport, port);
}

auto PortDefaultName(std::string_view port) -> std::string {
  return std::format("port default {}", port);
}

auto ArgumentDefaultName(std::string_view subroutine, std::string_view formal)
    -> std::string {
  return std::format("argument default {} {}", subroutine, formal);
}

namespace {

// The ABI projection of one DPI-C type: the carrier it crosses as, and the SV
// type whose values the boundary marshals. The two differ for an open array,
// whose extent is the actual's (LRM 35.6.1.1) -- the declaration fixes only the
// element type, so that is what the boundary marshals values of.
struct DpiTypeAbi {
  support::DpiCarrier carrier;
  const slang::ast::Type* marshaled_type = nullptr;
};

auto ClassifyDpiType(const slang::ast::Type& type, diag::SourceSpan span)
    -> diag::Result<DpiTypeAbi>;

// Classifies one SV value type into its DPI-C carrier (LRM 35.5.6, Annex H.10 /
// Table H.1). The dispatch is on the declared type shape, not the bit width --
// WYSIWYG (LRM 35.6.1.1) makes `int` (by-value C `int`) and `bit [31:0]`
// (canonical `svBitVecVal*`) different DPI ABIs even though both are 32-bit
// 2-state.
//
//   scalar `bit` / `logic` (1 bit)        -> by-value `svBit` / `svLogic`
//   predefined `byte`/`shortint`/`int`/`longint` (2-state) -> by-value C int
//   predefined `integer` / `time` (4-state) -> canonical logic vector
//   packed `bit [N:0]`                    -> canonical bit vector
//   packed `logic [N:0]`                  -> canonical logic vector
//
// `real`, `string`, `chandle`, `void` are by-value scalars. `shortreal` and
// packed struct / union are a located Unsupported so the gap stays a clean
// diagnostic.
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
        "passing a 'shortreal' through DPI-C is not yet supported");
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
          "passing an integer type of this width through DPI-C is not yet "
          "supported");
    }
    // A packed array of bit / logic crosses by pointer as a canonical vector.
    if (t.isPackedArray()) {
      return support::VectorCarrier{.four_state = t.isFourState()};
    }
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedDpi,
        "passing a packed structure or union through DPI-C is not yet "
        "supported");
  }
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedDpi,
      "passing a value of this type through DPI-C is not yet supported");
}

// LRM 35.5.6.1: a formal leaving at least one dimension unsized crosses as an
// open array. Walks the declared dimensions outermost-first -- a sized one is a
// fixed unpacked layer, an unsized one an open layer -- to the element type,
// recording per dimension the range the declaration fixes or nothing where the
// actual supplies it (Annex H.7.6).
//
// An array with no unsized dimension is not an open array and is rejected:
// Annex H.11.4 requires it to have C-compiler layout, which an SV value does
// not have. An element type Annex H.7.3 puts in C-compatible rather than
// canonical form is rejected for the same reason -- the LRM permits it, Lyra
// cannot yet represent it.
auto ClassifyOpenArray(const slang::ast::Type& type, diag::SourceSpan span)
    -> diag::Result<DpiTypeAbi> {
  support::OpenArrayCarrier open;
  const slang::ast::Type* cursor = &type;
  while (true) {
    const auto& layer = cursor->getCanonicalType();
    if (layer.kind == slang::ast::SymbolKind::FixedSizeUnpackedArrayType) {
      const auto& fixed = layer.as<slang::ast::FixedSizeUnpackedArrayType>();
      open.unpacked.emplace_back(
          support::DpiRange{
              .left = static_cast<std::int64_t>(fixed.range.left),
              .right = static_cast<std::int64_t>(fixed.range.right)});
      cursor = &fixed.elementType;
      continue;
    }
    if (layer.kind != slang::ast::SymbolKind::DPIOpenArrayType) {
      break;
    }
    const auto& unsized = layer.as<slang::ast::DPIOpenArrayType>();
    cursor = &unsized.elementType;
    if (unsized.isPacked) {
      open.packed_unsized = true;
      break;
    }
    open.unpacked.emplace_back();
  }

  const auto dimension_is_unsized =
      [](const std::optional<support::DpiRange>& declared) {
        return !declared.has_value();
      };
  if (!open.packed_unsized &&
      !std::ranges::any_of(open.unpacked, dimension_is_unsized)) {
    return std::unexpected(
        diag::Make(
            span, diag::DiagCode::kUnsupportedDpi,
            "passing a sized unpacked array through DPI-C is not yet supported")
            .WithNote("leave a dimension unsized to pass it as an open array"));
  }
  // The foreign side reaches an element through the indexing entries Annex
  // H.12.3 specializes for one, two, and three indices. Its variable-argument
  // forms, which an array of more dimensions would need, are not published.
  if (open.unpacked.size() > 3) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedDpi,
        "an open array of more than three unpacked dimensions is not yet "
        "supported");
  }

  auto element = ClassifyDpiType(*cursor, span);
  if (!element) return std::unexpected(std::move(element.error()));
  if (const auto* scalar =
          std::get_if<support::ScalarCarrier>(&element->carrier);
      scalar != nullptr && !DpiScalarAbiIsIntegral(scalar->abi)) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedDpi,
        "an open array of this element type is not yet supported; only 2- and "
        "4-state scalar and packed element types are");
  }
  // An unsized packed dimension makes every element a packed vector of the
  // actual's own width, which the foreign side reaches through the canonical
  // representation whatever the declared element atom is (LRM Annex H.12.1).
  // With the packed dimension sized, the element's own ABI family decides.
  open.element_crosses_as_canonical_vector =
      open.packed_unsized ||
      std::holds_alternative<support::VectorCarrier>(element->carrier);
  return DpiTypeAbi{
      .carrier = std::move(open), .marshaled_type = element->marshaled_type};
}

auto ClassifyDpiType(const slang::ast::Type& type, diag::SourceSpan span)
    -> diag::Result<DpiTypeAbi> {
  const slang::ast::SymbolKind kind = type.getCanonicalType().kind;
  if (kind == slang::ast::SymbolKind::DPIOpenArrayType ||
      kind == slang::ast::SymbolKind::FixedSizeUnpackedArrayType) {
    return ClassifyOpenArray(type, span);
  }
  auto carrier = ClassifyDpiCarrier(type, span);
  if (!carrier) return std::unexpected(std::move(carrier.error()));
  return DpiTypeAbi{.carrier = *std::move(carrier), .marshaled_type = &type};
}

// Classifies a DPI-C function result. LRM 35.5.5 restricts a result to a small
// value -- a by-value scalar; a canonical vector (a packed value, `integer`, or
// `time`) and an open array cannot be returned, and the front end refuses a
// subroutine that returns one. So every result here has a scalar carrier.
auto ClassifyDpiScalarResult(
    const slang::ast::Type& type, diag::SourceSpan span)
    -> diag::Result<support::DpiScalarAbi> {
  auto abi = ClassifyDpiType(type, span);
  if (!abi) return std::unexpected(std::move(abi.error()));
  if (const auto* scalar = std::get_if<support::ScalarCarrier>(&abi->carrier)) {
    return scalar->abi;
  }
  throw InternalError("a DPI-C result reached lowering that is no small value");
}

// The direction of a DPI-C formal argument (LRM 35.5.1.2). `ref` is illegal in
// import declarations (LRM 35.5.4), so only input / output / inout arrive here.
auto ClassifyDpiDirection(const slang::ast::FormalArgumentSymbol& formal)
    -> support::DpiDirection {
  switch (formal.direction) {
    case slang::ast::ArgumentDirection::In:
      return support::DpiDirection::kInput;
    case slang::ast::ArgumentDirection::Out:
      return support::DpiDirection::kOutput;
    case slang::ast::ArgumentDirection::InOut:
      return support::DpiDirection::kInout;
    case slang::ast::ArgumentDirection::Ref:
      throw InternalError(
          "ClassifyDpiDirection: a DPI import declares no ref argument, and "
          "the front end refuses one, so reaching this means an import was "
          "accepted that the language forbids");
  }
  throw InternalError("ClassifyDpiDirection: unknown ArgumentDirection");
}

// The subroutine's callable code lowered against its own binding frame plus
// the optional base-call args lowered against that same frame. The two
// halves must share one frame because a base-call arg may reference a formal
// parameter, and that reference resolves through the same procedural-var
// registry the body lowering uses.
struct SubroutineLoweringResult {
  hir::SubroutineDecl decl;
  std::optional<std::vector<hir::ExprId>> base_arguments;
};

auto LowerSubroutineDeclImpl(
    UnitLowerer& unit_lowerer, const slang::ast::SubroutineSymbol& sym,
    WalkFrame frame, const slang::ast::Expression* base_call_ast)
    -> diag::Result<SubroutineLoweringResult> {
  auto return_type_or = unit_lowerer.InternType(
      sym.getReturnType(), frontend::PointSpanOf(sym.location));
  if (!return_type_or) {
    return std::unexpected(std::move(return_type_or.error()));
  }

  hir::ProceduralBody body = unit_lowerer.MakeProceduralBody(sym);
  ConsumedBodyExpressions consumed_body_exprs;
  if (base_call_ast != nullptr) consumed_body_exprs.insert(base_call_ast);
  ProcessLowerer lowerer(unit_lowerer, sym, std::move(consumed_body_exprs));
  std::vector<const slang::ast::VariableSymbol*> declared_ahead(
      sym.getArguments().begin(), sym.getArguments().end());
  if (sym.returnValVar != nullptr) {
    declared_ahead.push_back(sym.returnValVar);
  }
  lowerer.AnalyzeLifetimeExtended(declared_ahead, sym.getBody());

  // A subroutine's root scope is named by the source (LRM 23.9), so a
  // hierarchical path reaches into it and the formals, the implicit result
  // var, and every body-tree declaration below all land in it.
  OpenProceduralScope root{
      unit_lowerer.LookupProceduralScope(sym),
      hir::ProceduralScopeKind::kSubroutineRoot, NameInAPath(sym)};
  const WalkFrame body_frame =
      frame.WithProceduralBody(&body).WithOpenScope(&root);

  std::vector<hir::SubroutineParam> params;
  params.reserve(sym.getArguments().size());
  for (const auto* formal : sym.getArguments()) {
    auto formal_type_or = unit_lowerer.InternType(
        formal->getType(), frontend::PointSpanOf(formal->location));
    if (!formal_type_or) {
      return std::unexpected(std::move(formal_type_or.error()));
    }
    const hir::ProceduralVarId var =
        lowerer.AddProceduralVar(body_frame, body, *formal, *formal_type_or);
    params.push_back(
        hir::SubroutineParam{
            .var = var,
            .direction = ParamDirectionOf(*formal),
            .default_evaluator = unit_lowerer.FindEvaluator(*formal)});
  }

  std::optional<hir::ProceduralVarId> result_var;
  if (sym.returnValVar != nullptr) {
    result_var = lowerer.AddProceduralVar(
        body_frame, body, *sym.returnValVar, *return_type_or);
  }

  auto constants = lowerer.DeclarePerObjectConstants(sym, body_frame);
  if (!constants) return std::unexpected(std::move(constants.error()));

  // Base-call args (LRM 8.7) evaluate in the constructor's own frame and can
  // reference formals -- for example `super.new(a * 2)` where `a` is the
  // derived ctor's parameter. Lowering them here, before the body statements
  // (which the LRM requires super.new to precede), keeps them ahead of any
  // stateful body computation and ensures they see only the formals -- the
  // ordering the runtime observes when it invokes the base ctor first.
  std::optional<std::vector<hir::ExprId>> base_arguments;
  if (base_call_ast != nullptr) {
    std::vector<hir::ExprId> lowered;
    const auto actuals = BaseCallArguments(*base_call_ast);
    lowered.reserve(actuals.size());
    for (const auto* actual : actuals) {
      auto arg_or = lowerer.LowerExpr(*actual, body_frame);
      if (!arg_or) return std::unexpected(std::move(arg_or.error()));
      lowered.push_back(body.exprs.Add(*std::move(arg_or)));
    }
    base_arguments = std::move(lowered);
  }

  auto body_stmt_or = lowerer.LowerStmt(sym.getBody(), body_frame);
  if (!body_stmt_or) return std::unexpected(std::move(body_stmt_or.error()));
  const hir::StmtId root_stmt = body.stmts.Add(*std::move(body_stmt_or));

  // A function may be called from an expression a wait watches (LRM 9.4.2) or
  // by a procedure whose implicit list is collected (LRM 9.2.2.2.1), so it
  // states the cells it reads. A task never is, and a construction states its
  // own, from the class.
  hir::Reads reads;
  if (sym.subroutineKind == slang::ast::SubroutineKind::Function &&
      !sym.flags.has(slang::ast::MethodFlags::Constructor)) {
    reads = ReadsOfFunctionBody(lowerer, body_frame, sym);
  }

  // The registry the root is defined in belongs to whichever declaration scope
  // owns this body -- the enclosing structural scope for a free subroutine, the
  // class for a method.
  body.root_scope = frame.SealScope(std::move(root));

  return SubroutineLoweringResult{
      .decl =
          hir::SubroutineDecl{
              .name = std::string{sym.name},
              .kind = ToHirSubroutineKind(sym.subroutineKind),
              .result_type = *return_type_or,
              .params = std::move(params),
              .result_var = result_var,
              .body = std::move(body),
              .root_stmt = root_stmt,
              .reads = std::move(reads)},
      .base_arguments = std::move(base_arguments)};
}

}  // namespace

auto EvaluatesItsOwnDefaults(const slang::ast::SubroutineSymbol& callee)
    -> bool {
  if (callee.flags.has(slang::ast::MethodFlags::DPIImport) ||
      callee.flags.has(slang::ast::MethodFlags::Constructor) ||
      callee.flags.has(slang::ast::MethodFlags::BuiltIn) ||
      RuntimeCarriedMethodOf(callee).has_value()) {
    return false;
  }
  const slang::ast::ClassType* cls = DeclaringClassOf(callee);
  return cls == nullptr || !cls->isInterface;
}

auto OmittedFormal(const slang::ast::CallExpression& call, std::size_t position)
    -> const slang::ast::FormalArgumentSymbol* {
  const auto* const* sym =
      std::get_if<const slang::ast::SubroutineSymbol*>(&call.subroutine);
  if (sym == nullptr || *sym == nullptr) return nullptr;
  const slang::ast::SubroutineSymbol& callee = **sym;
  if (!EvaluatesItsOwnDefaults(callee)) return nullptr;
  const auto formals = callee.getArguments();
  if (position >= formals.size()) return nullptr;
  const slang::ast::FormalArgumentSymbol& formal = *formals[position];
  if (formal.direction != slang::ast::ArgumentDirection::In ||
      formal.getDefaultValue() == nullptr ||
      formal.getDefaultValue() != call.arguments()[position]) {
    return nullptr;
  }
  return &formal;
}

auto LowerEvaluatorDecl(
    UnitLowerer& unit_lowerer, const slang::ast::Symbol& holder,
    std::string name, const slang::ast::Expression& expr, WalkFrame frame)
    -> diag::Result<hir::SubroutineDecl> {
  const auto span = frontend::PointSpanOf(holder.location);
  auto result_type = unit_lowerer.InternDeclaredType(holder);
  if (!result_type) return std::unexpected(std::move(result_type.error()));

  hir::ProceduralBody body;
  OpenProceduralScope root{
      frame.ProceduralScopes().Declare(),
      hir::ProceduralScopeKind::kSubroutineRoot, name};
  const hir::ProceduralVarId result_var = body.procedural_vars.Declare();
  body.procedural_vars.Define(
      result_var,
      hir::ProceduralVarDecl{.name = std::nullopt, .type = *result_type});
  root.declarations.push_back(result_var);
  ProcessLowerer lowerer(unit_lowerer, holder);
  const WalkFrame body_frame =
      frame.WithProceduralBody(&body).WithOpenScope(&root);
  auto value = lowerer.LowerExpr(expr, body_frame);
  if (!value) return std::unexpected(std::move(value.error()));
  // A wait on the value asks the evaluator what it reads (LRM 9.4.2), and an
  // expression a scope evaluates reads that scope's own storage.
  auto cells = CellsOfWaitedExpression(lowerer, body_frame, expr);
  if (!cells) return std::unexpected(std::move(cells.error()));
  hir::Reads reads{
      .cells = *std::move(cells),
      .writes = {},
      .calls = {},
      .unreportable = std::nullopt};
  const hir::StmtId root_stmt = body.stmts.Add(
      hir::Stmt{
          .label = std::nullopt,
          .data = hir::ReturnStmt{.value = body.exprs.Add(*std::move(value))},
          .span = span});
  body.root_scope = frame.SealScope(std::move(root));
  return hir::SubroutineDecl{
      .name = std::move(name),
      .kind = hir::SubroutineKind::kFunction,
      .result_type = *result_type,
      .params = {},
      .result_var = result_var,
      .body = std::move(body),
      .root_stmt = root_stmt,
      .reads = std::move(reads)};
}

auto BaseCallArguments(const slang::ast::Expression& base_call)
    -> std::span<const slang::ast::Expression* const> {
  if (const auto* written = base_call.as_if<slang::ast::NewClassExpression>()) {
    const auto* invocation = written->constructorCall();
    if (invocation == nullptr) {
      return {};
    }
    return invocation->as<slang::ast::CallExpression>().arguments();
  }
  return base_call.as<slang::ast::CallExpression>().arguments();
}

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
    -> diag::Result<ConstructorAndBaseArguments> {
  auto result_or =
      LowerSubroutineDeclImpl(unit_lowerer, sym, frame, base_call_ast);
  if (!result_or) return std::unexpected(std::move(result_or.error()));
  return ConstructorAndBaseArguments{
      .constructor = std::move(result_or->decl),
      .base_arguments = std::move(result_or->base_arguments)};
}

auto LowerMethodPrototypeDecl(
    UnitLowerer& unit_lowerer, const slang::ast::MethodPrototypeSymbol& proto,
    WalkFrame class_frame) -> diag::Result<hir::SubroutineDecl> {
  auto return_type_or = unit_lowerer.InternType(
      proto.getReturnType(), frontend::PointSpanOf(proto.location));
  if (!return_type_or) {
    return std::unexpected(std::move(return_type_or.error()));
  }

  hir::ProceduralBody body = unit_lowerer.MakeProceduralBody(proto);
  ProcessLowerer lowerer(unit_lowerer, proto);

  // A pure virtual prototype has no body to walk, but it still has parameter
  // vars whose types the signature must expose, and those vars belong to the
  // prototype's root scope the same way a defined method's do. Having no body,
  // it is nothing a `disable` can name, so its identity is minted here rather
  // than ahead of the bodies.
  OpenProceduralScope root{
      class_frame.ProceduralScopes().Declare(),
      hir::ProceduralScopeKind::kSubroutineRoot, NameInAPath(proto)};
  const WalkFrame body_frame =
      class_frame.WithProceduralBody(&body).WithOpenScope(&root);

  std::vector<hir::SubroutineParam> params;
  params.reserve(proto.getArguments().size());
  for (const auto* formal : proto.getArguments()) {
    auto formal_type_or = unit_lowerer.InternType(
        formal->getType(), frontend::PointSpanOf(formal->location));
    if (!formal_type_or) {
      return std::unexpected(std::move(formal_type_or.error()));
    }
    const hir::ProceduralVarId var =
        lowerer.AddProceduralVar(body_frame, body, *formal, *formal_type_or);
    params.push_back(
        hir::SubroutineParam{
            .var = var,
            .direction = ParamDirectionOf(*formal),
            .default_evaluator = unit_lowerer.FindEvaluator(*formal)});
  }

  body.root_scope = class_frame.SealScope(std::move(root));

  return hir::SubroutineDecl{
      .name = std::string{proto.name},
      .kind = ToHirSubroutineKind(proto.subroutineKind),
      .result_type = *return_type_or,
      .params = std::move(params),
      .result_var = std::nullopt,
      .body = std::move(body),
      .root_stmt = std::nullopt,
      .reads = {},
  };
}

// Classifies each formal argument's ABI projection (LRM 35.5.6): its direction,
// C ABI carrier, and the SV type the boundary marshals values of. Import and
// export declarations build their parameter list here alike -- the forms they
// admit differ (an export cannot take an open array, LRM 35.5.6.1), but the
// frontend rejects such a declaration before it reaches this classification, so
// no arm here is direction-specific.
auto ClassifyDpiParams(
    UnitLowerer& unit_lowerer, const slang::ast::SubroutineSymbol& sym)
    -> diag::Result<std::vector<hir::DpiParamAbi>> {
  std::vector<hir::DpiParamAbi> abi_params;
  abi_params.reserve(sym.getArguments().size());
  for (const auto* formal : sym.getArguments()) {
    const auto floc = frontend::PointSpanOf(formal->location);
    const support::DpiDirection direction = ClassifyDpiDirection(*formal);
    auto abi = ClassifyDpiType(formal->getType(), floc);
    if (!abi) return std::unexpected(std::move(abi.error()));
    auto param_type = unit_lowerer.InternType(*abi->marshaled_type, floc);
    if (!param_type) return std::unexpected(std::move(param_type.error()));
    abi_params.push_back(
        hir::DpiParamAbi{
            .sv_type = *param_type,
            .carrier = std::move(abi->carrier),
            .direction = direction});
  }
  return abi_params;
}

// Lowers a DPI-C import declaration (LRM 35.4) to a bodyless external callable.
// It has no SV body, so it is not a `SubroutineDecl`; the unit records it among
// its foreign imports, not in a scope's subroutine arena. The ABI
// classification and the foreign name are resolved once here and never
// re-derived downstream. A function or a task is accepted; a task carries no
// return, so its `void` return classifies as `kVoid` through the same path. A
// `context` import (LRM 35.5.3) carries its context property forward; the call
// site establishes the declaration scope around the foreign call. Every
// diagnostic here is reported against the declaration, so a unit that reaches
// an unsupported signature through a call still names the declaration that
// wrote it.
auto LowerForeignImport(
    UnitLowerer& unit_lowerer, const slang::ast::SubroutineSymbol& sym)
    -> diag::Result<hir::ForeignImportDecl> {
  const auto loc = frontend::PointSpanOf(sym.location);
  const bool is_task = sym.subroutineKind == slang::ast::SubroutineKind::Task;
  auto ret_abi = ClassifyDpiScalarResult(sym.getReturnType(), loc);
  if (!ret_abi) return std::unexpected(std::move(ret_abi.error()));
  auto ret_type = unit_lowerer.InternType(sym.getReturnType(), loc);
  if (!ret_type) return std::unexpected(std::move(ret_type.error()));

  auto abi_params = ClassifyDpiParams(unit_lowerer, sym);
  if (!abi_params) return std::unexpected(std::move(abi_params.error()));

  return hir::ForeignImportDecl{
      .name = std::string{sym.name},
      .foreign_name = std::string{sym.getDPICIdentifier()},
      .is_pure = sym.flags.has(slang::ast::MethodFlags::Pure),
      .is_task = is_task,
      .is_context = sym.flags.has(slang::ast::MethodFlags::DPIContext),
      .ret_abi = *ret_abi,
      .ret_sv_type = *ret_type,
      .params = std::move(*abi_params)};
}

auto LowerForeignExport(
    UnitLowerer& unit_lowerer, const slang::ast::SubroutineSymbol& sym,
    hir::StructuralSubroutineId subroutine, std::string_view foreign_name)
    -> diag::Result<hir::ForeignExportDecl> {
  const auto loc = frontend::PointSpanOf(sym.location);
  auto ret_abi = ClassifyDpiScalarResult(sym.getReturnType(), loc);
  if (!ret_abi) return std::unexpected(std::move(ret_abi.error()));
  auto ret_type = unit_lowerer.InternType(sym.getReturnType(), loc);
  if (!ret_type) return std::unexpected(std::move(ret_type.error()));

  auto abi_params = ClassifyDpiParams(unit_lowerer, sym);
  if (!abi_params) return std::unexpected(std::move(abi_params.error()));

  return hir::ForeignExportDecl{
      .subroutine = subroutine,
      .foreign_name = std::string{foreign_name},
      .ret_abi = *ret_abi,
      .ret_sv_type = *ret_type,
      .params = std::move(*abi_params)};
}

}  // namespace lyra::lowering::ast_to_hir

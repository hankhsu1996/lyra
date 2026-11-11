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
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/Type.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/frontend/slang_source_span.hpp"
#include "lyra/hir/foreign_import.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/name_in_a_path.hpp"
#include "lyra/lowering/ast_to_hir/passed_by_reference.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/reads.hpp"
#include "lyra/lowering/ast_to_hir/std_classes.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
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

auto ViewNameDesignatesStorage(const slang::ast::ModportPortSymbol& port)
    -> bool {
  return port.direction != slang::ast::ArgumentDirection::In;
}

auto ViewExpressionOf(const slang::ast::ModportPortSymbol& port)
    -> const slang::ast::Expression& {
  const slang::ast::Expression* connection = port.getConnectionExpr();
  if (connection == nullptr) {
    throw InternalError(
        "ViewExpressionOf: a name the view defines is the expression it was "
        "written with");
  }
  return *connection;
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

auto ClockingEventName(std::string_view block, std::size_t entry)
    -> std::string {
  return std::format("clocking event {} {}", block, entry);
}

auto ClockingConditionName(std::string_view block, std::size_t entry)
    -> std::string {
  return std::format("clocking condition {} {}", block, entry);
}

auto ClockingLandingName(std::string_view block, std::string_view clockvar)
    -> std::string {
  return std::format("clocking landing {} {}", block, clockvar);
}

auto ClockingBlockEventName() -> std::string {
  return "clocking block event";
}

auto ClockingDrivingVariableName(std::string_view named_after) -> std::string {
  return std::format("{} driving variable", named_after);
}

namespace {

// Whether `place` names bits of a packed value, or a join of places, which no
// reference binds (LRM 13.5.2).
auto NamesNoSingleStorage(const slang::ast::Expression& place) -> bool {
  const auto within = [](const slang::ast::Expression& value) {
    return value.type->isIntegral() || NamesNoSingleStorage(value);
  };
  if (const auto* element =
          place.as_if<slang::ast::ElementSelectExpression>()) {
    return within(element->value());
  }
  if (const auto* range = place.as_if<slang::ast::RangeSelectExpression>()) {
    return within(range->value());
  }
  if (const auto* member = place.as_if<slang::ast::MemberAccessExpression>()) {
    return within(member->value());
  }
  return place.as_if<slang::ast::ConcatenationExpression>() != nullptr ||
         place.as_if<slang::ast::StreamingConcatenationExpression>() != nullptr;
}

// The function evaluating `formal`'s default, or nothing where it has none or
// this unit evaluates it in no subroutine of its own.
auto DefaultEvaluatorOf(
    const UnitLowerer& unit_lowerer,
    const slang::ast::FormalArgumentSymbol& formal)
    -> std::optional<hir::StructuralSubroutineId> {
  const slang::ast::Expression* default_value = formal.getDefaultValue();
  if (default_value == nullptr) return std::nullopt;
  return unit_lowerer.FindEvaluator(*default_value);
}

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

// Classifies one SV value type into its DPI-C carrier (LRM 35.5.6, Annex H.7.3,
// H.7.4, Table H.1). The dispatch is on the declared type shape, not the bit
// width -- WYSIWYG (LRM 35.6.1.1) makes `int` (by-value C `int`) and
// `bit [31:0]` (canonical `svBitVecVal*`) different DPI ABIs even though both
// are 32-bit 2-state.
//
//   `void`, `string`, `chandle`, a real    -> by-value C scalar
//   an enumeration                         -> what its base type crosses as
//   scalar `bit` / `logic` (1 bit)         -> by-value `svBit` / `svLogic`
//   predefined `byte`/`shortint`/`int`/`longint` (2-state) -> by-value C int
//   every other packed type                -> canonical vector of its width
//
// The last arm is the standard's own rule: every packed type is equivalent to
// a one-dimensional packed array, and that is how the foreign side sees it
// (LRM 35.5.6) -- a packed array, a packed structure or union, a tagged one
// with its tag above its members, and `integer` and `time`. The value already
// is that array here, so crossing it needs nothing a packed array does not.
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
    // A `realtime` is a `real` (LRM 6.12).
    switch (t.as<slang::ast::FloatingType>().floatKind) {
      case slang::ast::FloatingType::Real:
      case slang::ast::FloatingType::RealTime:
        return scalar(support::DpiScalarAbi::kReal);
      case slang::ast::FloatingType::ShortReal:
        return scalar(support::DpiScalarAbi::kShortReal);
    }
    throw InternalError("ClassifyDpiCarrier: unknown floating type");
  }
  if (t.isEnum()) {
    return ClassifyDpiCarrier(t.as<slang::ast::EnumType>().baseType, span);
  }
  if (!t.isIntegral()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedDpi,
        "passing an unpacked structure through DPI-C is not yet supported");
  }
  if (t.isScalar()) {
    return scalar(
        t.isFourState() ? support::DpiScalarAbi::kLogicScalar
                        : support::DpiScalarAbi::kBitScalar);
  }
  if (t.isPredefinedInteger()) {
    switch (t.as<slang::ast::PredefinedIntegerType>().integerKind) {
      case slang::ast::PredefinedIntegerType::Byte:
        return scalar(support::DpiScalarAbi::kByte);
      case slang::ast::PredefinedIntegerType::ShortInt:
        return scalar(support::DpiScalarAbi::kShortInt);
      case slang::ast::PredefinedIntegerType::Int:
        return scalar(support::DpiScalarAbi::kInt);
      case slang::ast::PredefinedIntegerType::LongInt:
        return scalar(support::DpiScalarAbi::kLongInt);
      case slang::ast::PredefinedIntegerType::Integer:
      case slang::ast::PredefinedIntegerType::Time:
        break;
    }
  }
  return support::VectorCarrier{.four_state = t.isFourState()};
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

// Whether a formal of `type` crosses the boundary as an open array, as the
// classification of the formal states it.
auto CrossesAsOpenArray(const slang::ast::Type& type) -> bool {
  const diag::Result<DpiTypeAbi> abi = ClassifyDpiType(type, {});
  return abi.has_value() &&
         std::holds_alternative<support::OpenArrayCarrier>(abi->carrier);
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

// The variable `formal` is: one its namespace keeps, named as every name
// reaching it names it (LRM 13.3.2, 6.21), or one the body declares -- an
// automatic formal, or a static one each instance keeps.
auto FormalVariableOf(
    UnitLowerer& unit_lowerer, ProcessLowerer& lowerer, const WalkFrame& frame,
    hir::ProceduralBody& body, const slang::ast::FormalArgumentSymbol& formal)
    -> diag::Result<hir::FormalVariable> {
  const auto span = frontend::PointSpanOf(formal.location);
  const auto of_the_body = [&]() -> diag::Result<hir::FormalVariable> {
    auto type = unit_lowerer.InternType(formal.getType(), span);
    if (!type) return std::unexpected(std::move(type.error()));
    return hir::FormalVariable{
        lowerer.AddProceduralVar(frame, body, formal, *type)};
  };
  std::optional<KeptStatic> kept = KeptStaticOf(formal);
  if (!kept.has_value()) return of_the_body();
  return std::visit(
      Overloaded{
          [&](const KeptByNamespace& by) -> diag::Result<hir::FormalVariable> {
            return hir::FormalVariable{
                unit_lowerer.NamespaceValueRefOf(*by.unit, kept->name)};
          },
          [](const KeptByClass&) -> diag::Result<hir::FormalVariable> {
            throw InternalError(
                "FormalVariableOf: a class keeps a formal, though a class "
                "method is automatic (LRM 8.6) -- please report this as a bug");
          }},
      kept->keeper);
}

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
  // A call of a task every connected module defines runs each definition as a
  // process of its own (LRM 25.7.4), on the formals of the call.
  if (sym.flags.has(slang::ast::MethodFlags::InterfaceExtern) &&
      sym.getPrototype()->flags.has(slang::ast::MethodFlags::ForkJoin)) {
    lowerer.ExtendLifetimes(declared_ahead);
  }

  // A subroutine's root scope is named by the source (LRM 23.9), so a
  // hierarchical path reaches into it and the formals the body declares, the
  // implicit result var, and every body-tree declaration below all land in it.
  OpenProceduralScope root{
      unit_lowerer.LookupProceduralScope(sym),
      ToHirProceduralScopeKind(sym.subroutineKind), NameInAPath(sym)};
  const WalkFrame body_frame =
      frame.WithProceduralBody(&body).WithOpenScope(&root);

  std::vector<hir::SubroutineParam> params;
  params.reserve(sym.getArguments().size());
  for (const auto* formal : sym.getArguments()) {
    auto variable =
        FormalVariableOf(unit_lowerer, lowerer, body_frame, body, *formal);
    if (!variable) return std::unexpected(std::move(variable.error()));
    params.push_back(
        hir::SubroutineParam{
            .variable = *std::move(variable),
            .direction = ParamDirectionOf(*formal),
            .default_evaluator = DefaultEvaluatorOf(unit_lowerer, *formal)});
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
    auto lowered = AddConstructionActuals(
        lowerer, body_frame, ConstructorEnteredBy(*base_call_ast),
        BaseCallArguments(*base_call_ast), frontend::PointSpanOf(sym.location));
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    base_arguments = *std::move(lowered);
  }

  // A subroutine an interface leaves to a connected module has no body of its
  // own (LRM 25.7): a call runs the definition the module bound to the object.
  std::optional<hir::StmtId> root_stmt;
  if (!sym.flags.has(slang::ast::MethodFlags::InterfaceExtern)) {
    auto body_stmt_or = lowerer.LowerStmt(sym.getBody(), body_frame);
    if (!body_stmt_or) return std::unexpected(std::move(body_stmt_or.error()));
    root_stmt = body.stmts.Add(*std::move(body_stmt_or));
  }

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
              .name = SubroutineNameOf(sym),
              .kind = ToHirSubroutineKind(sym.subroutineKind),
              .result_type = *return_type_or,
              .params = std::move(params),
              .result_var = result_var,
              .body = std::move(body),
              .root_stmt = root_stmt,
              .reads = std::move(reads),
              .track_caller = TracksCaller(sym),
              .span = frontend::PointSpanOf(sym.location)},
      .base_arguments = std::move(base_arguments)};
}

// Whether `actual` is the default of `formal`: the front end fills an omitted
// argument in by placing the default's own expression there.
auto IsDefaultOf(
    const slang::ast::FormalArgumentSymbol& formal,
    const slang::ast::Expression& actual) -> bool {
  return formal.getDefaultValue() == &actual;
}

}  // namespace

auto EvaluatesItsOwnDefaults(const slang::ast::SubroutineSymbol& callee)
    -> bool {
  if (callee.flags.has(slang::ast::MethodFlags::BuiltIn) ||
      RuntimeCarriedMethodOf(callee).has_value()) {
    return false;
  }
  const slang::ast::ClassType* cls = DeclaringClassOf(callee);
  return cls == nullptr || !cls->isInterface || !BelongsToAnInstance(*cls);
}

auto TracksCaller(const slang::ast::SubroutineSymbol& callee) -> bool {
  return DeclaredInLibraryText(callee) ||
         callee.flags.has(slang::ast::MethodFlags::InterfaceExtern);
}

auto OmitsArgument(const slang::ast::CallExpression& call, std::size_t position)
    -> bool {
  const auto* const* sym =
      std::get_if<const slang::ast::SubroutineSymbol*>(&call.subroutine);
  if (sym == nullptr || *sym == nullptr) return false;
  const slang::ast::SubroutineSymbol& callee = **sym;
  if (!EvaluatesItsOwnDefaults(callee)) return false;
  const auto formals = callee.getArguments();
  if (position >= formals.size()) return false;
  return IsDefaultOf(*formals[position], *call.arguments()[position]);
}

auto FormalsNamedByDefaults(const slang::ast::SubroutineSymbol& subroutine)
    -> std::span<const slang::ast::FormalArgumentSymbol* const> {
  const slang::ast::MethodPrototypeSymbol* prototype =
      subroutine.getPrototype();
  return prototype != nullptr ? prototype->getArguments()
                              : subroutine.getArguments();
}

auto EvaluatorParametersOf(
    const slang::ast::SubroutineSymbol& subroutine,
    const slang::ast::FormalArgumentSymbol& formal)
    -> std::vector<const slang::ast::FormalArgumentSymbol*> {
  const auto declared = subroutine.getArguments();
  const auto it = std::ranges::find(declared, &formal);
  if (it == declared.end()) {
    throw InternalError(
        "EvaluatorParametersOf: a formal is one of the subroutine declaring "
        "it");
  }
  const bool imported =
      subroutine.flags.has(slang::ast::MethodFlags::DPIImport);
  std::vector<const slang::ast::FormalArgumentSymbol*> parameters;
  for (const slang::ast::FormalArgumentSymbol* before :
       FormalsNamedByDefaults(subroutine)
           .first(static_cast<std::size_t>(it - declared.begin()))) {
    if (!imported || !CrossesAsOpenArray(before->getType())) {
      parameters.push_back(before);
    }
  }
  return parameters;
}

auto EvaluatorParamDirectionOf(const slang::ast::FormalArgumentSymbol& formal)
    -> hir::ParamDirection {
  const hir::ParamDirection direction = ParamDirectionOf(formal);
  switch (direction) {
    case hir::ParamDirection::kConstRef:
      return direction;
    case hir::ParamDirection::kRef:
    case hir::ParamDirection::kInput:
    case hir::ParamDirection::kOutput:
    case hir::ParamDirection::kInOut:
      return hir::ParamDirection::kRef;
  }
  throw InternalError("EvaluatorParamDirectionOf: unknown direction");
}

auto DefaultExpressionOf(const slang::ast::FormalArgumentSymbol& formal)
    -> const slang::ast::Expression& {
  const slang::ast::Expression& bound = *formal.getDefaultValue();
  const slang::ast::Expression* place = ArgLValue(bound);
  return place != nullptr ? *place : bound;
}

auto LowerEvaluatorDecl(
    UnitLowerer& unit_lowerer, const slang::ast::Symbol& holder,
    std::string name, const slang::ast::Expression& expr,
    hir::TypeId result_type, hir::ParamDirection result_direction,
    std::span<const slang::ast::FormalArgumentSymbol* const> parameters,
    WalkFrame frame) -> diag::Result<hir::SubroutineDecl> {
  const auto span = frontend::PointSpanOf(holder.location);
  hir::ProceduralBody body;
  OpenProceduralScope root{
      frame.ProceduralScopes().Declare(), hir::ProceduralScopeKind::kImplicit,
      std::nullopt};
  ProcessLowerer lowerer(unit_lowerer, holder);
  const WalkFrame body_frame =
      frame.WithProceduralBody(&body).WithOpenScope(&root);
  std::vector<hir::SubroutineParam> params;
  params.reserve(parameters.size());
  for (const slang::ast::FormalArgumentSymbol* parameter : parameters) {
    auto type = unit_lowerer.InternType(
        parameter->getType(), frontend::PointSpanOf(parameter->location));
    if (!type) return std::unexpected(std::move(type.error()));
    params.push_back(
        hir::SubroutineParam{
            .variable = lowerer.AddEvaluatorParameter(
                body_frame, body, *parameter, *type),
            .direction = EvaluatorParamDirectionOf(*parameter),
            .default_evaluator = std::nullopt});
  }
  // A result the call takes as a value has the result variable every function
  // has (LRM 13.4.1); one standing as the actual of a formal binding a place
  // names that place, which no variable holds.
  const bool names_a_place = result_direction != hir::ParamDirection::kInput;
  std::optional<hir::ProceduralVarId> result_var;
  if (!names_a_place) {
    result_var = body.procedural_vars.Declare();
    body.procedural_vars.Define(
        *result_var,
        hir::ProceduralVarDecl{.name = std::nullopt, .type = result_type});
    root.declarations.push_back(*result_var);
  }
  const slang::ast::Expression* place = ArgLValue(expr);
  const slang::ast::Expression& evaluated = place != nullptr ? *place : expr;
  switch (result_direction) {
    case hir::ParamDirection::kRef:
    case hir::ParamDirection::kConstRef: {
      auto passed = RefuseUnionPartByReference(evaluated);
      if (!passed) return std::unexpected(std::move(passed.error()));
      break;
    }
    case hir::ParamDirection::kInput:
    case hir::ParamDirection::kOutput:
    case hir::ParamDirection::kInOut:
      break;
  }
  if (names_a_place && NamesNoSingleStorage(evaluated)) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a default of an 'output' or 'inout' formal naming part of a packed "
        "value is not yet supported");
  }
  auto value = lowerer.LowerExpr(evaluated, body_frame);
  if (!value) return std::unexpected(std::move(value.error()));
  // A wait on the value asks the evaluator what it reads (LRM 9.4.2), and an
  // expression a scope evaluates reads that scope's own storage.
  auto cells = CellsOfWaitedExpression(lowerer, body_frame, evaluated);
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
      .result_type = result_type,
      .result_direction = result_direction,
      .params = std::move(params),
      .result_var = result_var,
      .body = std::move(body),
      .root_stmt = root_stmt,
      .reads = std::move(reads),
      .span = span};
}

template <ExprLowerer Lowerer>
auto AddConstructionActuals(
    Lowerer& lowerer, const WalkFrame& frame,
    const slang::ast::SubroutineSymbol* constructor,
    std::span<const slang::ast::Expression* const> actuals,
    diag::SourceSpan span) -> diag::Result<std::vector<hir::ExprId>> {
  std::vector<hir::ExprId> lowered;
  lowered.reserve(actuals.size());
  for (std::size_t i = 0; i < actuals.size(); ++i) {
    if (constructor != nullptr && EvaluatesItsOwnDefaults(*constructor) &&
        IsDefaultOf(*constructor->getArguments()[i], *actuals[i])) {
      auto type = lowerer.Owner().InternType(
          constructor->getArguments()[i]->getType(), span);
      if (!type) return std::unexpected(std::move(type.error()));
      lowered.push_back(frame.Exprs().Add(
          hir::Expr{
              .type = *type, .data = hir::CalleeDefaultExpr{}, .span = span}));
      continue;
    }
    auto actual = lowerer.LowerExpr(*actuals[i], frame);
    if (!actual) return std::unexpected(std::move(actual.error()));
    lowered.push_back(frame.Exprs().Add(*std::move(actual)));
  }
  return lowered;
}

template auto AddConstructionActuals(
    ProcessLowerer&, const WalkFrame&, const slang::ast::SubroutineSymbol*,
    std::span<const slang::ast::Expression* const>, diag::SourceSpan)
    -> diag::Result<std::vector<hir::ExprId>>;
template auto AddConstructionActuals(
    StructuralScopeLowerer&, const WalkFrame&,
    const slang::ast::SubroutineSymbol*,
    std::span<const slang::ast::Expression* const>, diag::SourceSpan)
    -> diag::Result<std::vector<hir::ExprId>>;

auto ConstructorEnteredBy(const slang::ast::Expression& base_call)
    -> const slang::ast::SubroutineSymbol* {
  const slang::ast::Expression* invocation = &base_call;
  if (const auto* written = base_call.as_if<slang::ast::NewClassExpression>()) {
    invocation = written->constructorCall();
    if (invocation == nullptr) return nullptr;
  }
  const auto* const* callee = std::get_if<const slang::ast::SubroutineSymbol*>(
      &invocation->as<slang::ast::CallExpression>().subroutine);
  return callee == nullptr ? nullptr : *callee;
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
      ToHirProceduralScopeKind(proto.subroutineKind), NameInAPath(proto)};
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
            .variable = var,
            .direction = ParamDirectionOf(*formal),
            .default_evaluator = DefaultEvaluatorOf(unit_lowerer, *formal)});
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
      .track_caller = DeclaredInLibraryText(proto),
      .span = frontend::PointSpanOf(proto.location),
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
      .name = NameInAPath(sym),
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

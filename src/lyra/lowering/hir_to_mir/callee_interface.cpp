#include "lyra/lowering/hir_to_mir/callee_interface.hpp"

#include <cstdint>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

auto SubroutineCallType(
    mir::CompilationUnit& unit, hir::SubroutineKind kind,
    mir::TypeId result_type) -> mir::TypeId {
  return kind == hir::SubroutineKind::kTask
             ? unit.types.Intern(
                   mir::Type{mir::CoroutineType{.payload = result_type}})
             : result_type;
}

auto CompletionPayloadType(
    mir::CompilationUnit& unit, const std::vector<mir::TypeId>& components)
    -> mir::TypeId {
  return unit.types.Intern(mir::Type{mir::TupleType{.elements = components}});
}

auto BuildCompletionLayout(
    const std::vector<CalleeFormal>& formals,
    std::optional<mir::TypeId> result_type) -> CompletionLayout {
  CompletionLayout layout;
  if (result_type.has_value()) {
    layout.components.push_back(*result_type);
  }
  layout.formals.reserve(formals.size());
  for (const CalleeFormal& formal : formals) {
    CompletionLayout::Formal out{
        .direction = formal.direction,
        .type = formal.type,
        .component = std::nullopt};
    if (hir::RequiresWriteback(formal.direction)) {
      out.component = base::ComponentIndex{
          static_cast<std::uint32_t>(layout.components.size())};
      layout.components.push_back(formal.type);
    }
    layout.formals.push_back(out);
  }
  return layout;
}

auto ParamTypeOf(
    UnitLowerer& unit_lowerer, hir::TypeId value_type,
    hir::ParamDirection direction) -> std::optional<mir::TypeId> {
  const mir::TypeId mir_value = unit_lowerer.TranslateType(value_type);
  switch (direction) {
    case hir::ParamDirection::kOutput:
      return std::nullopt;
    case hir::ParamDirection::kInput:
    case hir::ParamDirection::kInOut:
      return mir_value;
    case hir::ParamDirection::kRef:
    case hir::ParamDirection::kConstRef:
      return unit_lowerer.Unit().types.Intern(
          mir::Type{mir::RefType{
              .pointee = mir_value,
              .mutability = direction == hir::ParamDirection::kConstRef
                                ? mir::Mutability::kReadOnly
                                : mir::Mutability::kMutable}});
  }
  throw InternalError("ParamTypeOf: unknown parameter direction");
}

auto ReportParamTypeOf(
    const mir::CompilationUnit& unit, hir::SubroutineKind kind)
    -> std::optional<mir::TypeId> {
  switch (kind) {
    case hir::SubroutineKind::kFunction:
      return unit.builtins.read_report_ptr;
    case hir::SubroutineKind::kTask:
      return std::nullopt;
  }
  throw InternalError("ReportParamTypeOf: unknown subroutine kind");
}

auto CalleeFormalsOf(UnitLowerer& unit_lowerer, const hir::SubroutineDecl& decl)
    -> std::vector<CalleeFormal> {
  std::vector<CalleeFormal> formals;
  formals.reserve(decl.params.size());
  for (const hir::SubroutineParam& param : decl.params) {
    formals.push_back(
        CalleeFormal{
            .direction = param.direction,
            .type = unit_lowerer.TranslateType(
                decl.body.procedural_vars.Get(param.var).type)});
  }
  return formals;
}

auto CalleeFormalsOf(
    UnitLowerer& unit_lowerer, const hir::ExternalCalleeInterface& interface)
    -> std::vector<CalleeFormal> {
  std::vector<CalleeFormal> formals;
  formals.reserve(interface.params.size());
  for (const hir::ExternalCalleeParam& param : interface.params) {
    formals.push_back(
        CalleeFormal{
            .direction = param.direction,
            .type = unit_lowerer.TranslateType(param.type)});
  }
  return formals;
}

namespace {

// The type a call yields to a callee of `kind` taking `formals` and returning
// `result_type`.
auto CallTypeOf(
    UnitLowerer& unit_lowerer, hir::SubroutineKind kind,
    hir::TypeId result_type, const std::vector<CalleeFormal>& formals)
    -> mir::TypeId {
  const mir::TypeId result = unit_lowerer.TranslateType(result_type);
  const CompletionLayout layout = BuildCompletionLayout(
      formals, result == unit_lowerer.Unit().builtins.void_type
                   ? std::nullopt
                   : std::optional<mir::TypeId>{result});
  return SubroutineCallType(
      unit_lowerer.Unit(), kind,
      CompletionPayloadType(unit_lowerer.Unit(), layout.components));
}

}  // namespace

auto SubroutineCallTypeOf(
    UnitLowerer& unit_lowerer, const hir::SubroutineDecl& decl) -> mir::TypeId {
  return CallTypeOf(
      unit_lowerer, decl.kind, decl.result_type,
      CalleeFormalsOf(unit_lowerer, decl));
}

auto SubroutineCallTypeOf(
    UnitLowerer& unit_lowerer, const hir::ExternalCalleeInterface& interface,
    hir::TypeId result_type) -> mir::TypeId {
  return CallTypeOf(
      unit_lowerer, interface.kind, result_type,
      CalleeFormalsOf(unit_lowerer, interface));
}

auto ParamTypesOf(UnitLowerer& unit_lowerer, const hir::SubroutineDecl& decl)
    -> std::vector<mir::TypeId> {
  std::vector<mir::TypeId> params;
  params.reserve(decl.params.size());
  for (const hir::SubroutineParam& formal : decl.params) {
    if (const std::optional<mir::TypeId> param = ParamTypeOf(
            unit_lowerer, decl.body.procedural_vars.Get(formal.var).type,
            formal.direction)) {
      params.push_back(*param);
    }
  }
  if (const std::optional<mir::TypeId> report =
          ReportParamTypeOf(unit_lowerer.Unit(), decl.kind)) {
    params.push_back(*report);
  }
  return params;
}

auto ParamTypesOf(
    UnitLowerer& unit_lowerer, const hir::ExternalCalleeInterface& interface)
    -> std::vector<mir::TypeId> {
  std::vector<mir::TypeId> params;
  params.reserve(interface.params.size());
  for (const hir::ExternalCalleeParam& formal : interface.params) {
    if (const std::optional<mir::TypeId> param =
            ParamTypeOf(unit_lowerer, formal.type, formal.direction)) {
      params.push_back(*param);
    }
  }
  if (const std::optional<mir::TypeId> report =
          ReportParamTypeOf(unit_lowerer.Unit(), interface.kind)) {
    params.push_back(*report);
  }
  return params;
}

auto ProjectCompletionComponent(
    mir::Block& block, mir::LocalId completion, mir::TypeId payload_type,
    base::ComponentIndex index, mir::TypeId component_type) -> mir::ExprId {
  const mir::ExprId tuple_ref =
      block.exprs.Add(mir::MakeLocalRefExpr(completion, payload_type));
  return block.exprs.Add(
      mir::MakeComponentExpr(tuple_ref, index, component_type));
}

auto BindCompletion(
    mir::CompilationUnit& unit, const WalkFrame& frame, mir::Expr call,
    mir::TypeId payload_type, std::span<const CompletionWriteback> writebacks)
    -> mir::LocalId {
  mir::Block& body = *frame.current_block;
  const mir::ExprId call_id = body.exprs.Add(std::move(call));
  const mir::ExprId completion_value =
      unit.types.Get(body.exprs.Get(call_id).type).Is<mir::CoroutineType>()
          ? body.exprs.Add(
                mir::Expr{
                    .data = mir::AwaitExpr{.execution = call_id},
                    .type = payload_type})
          : call_id;
  const mir::LocalId completion =
      frame.bindings->DeclareAnonymous(payload_type);
  body.AppendStmt(
      mir::LocalDeclStmt{.target = completion, .init = completion_value});

  for (const CompletionWriteback& writeback : writebacks) {
    const mir::ExprId component = ProjectCompletionComponent(
        body, completion, payload_type, writeback.component, writeback.type);
    body.AppendStmt(
        mir::ExprStmt{
            .expr = body.exprs.Add(BuildStoreExpr(
                unit, body, writeback.place, component, std::nullopt,
                writeback.type))});
  }
  return completion;
}

}  // namespace lyra::lowering::hir_to_mir

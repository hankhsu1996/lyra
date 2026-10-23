#include "lyra/lowering/hir_to_mir/callee_interface.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/class_decl.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/external_class.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
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
    const mir::CompilationUnit& unit, mir::TypeId value_type,
    hir::ParamDirection direction) -> std::optional<mir::TypeId> {
  switch (direction) {
    case hir::ParamDirection::kOutput:
      return std::nullopt;
    case hir::ParamDirection::kInput:
    case hir::ParamDirection::kInOut:
      return value_type;
    // A reference to a value of any type refers to the value where it lies,
    // whatever its type, rather than to something holding one, as Rust's
    // `&mut dyn Any` and `Box<dyn Any>` hold one unsized type two ways.
    case hir::ParamDirection::kRef:
    case hir::ParamDirection::kConstRef:
      return unit.types.Intern(
          mir::Type{mir::VariableRefType{
              .pointee =
                  unit.types.Get(value_type).Is<mir::AnyType>()
                      ? unit.types.Intern(mir::Type{mir::ErasedValueType{}})
                      : value_type,
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

auto ConstructorFormalsOf(UnitLowerer& unit_lowerer, const hir::ClassRef& cls)
    -> std::vector<CalleeFormal> {
  return std::visit(
      Overloaded{
          [&](const hir::LocalClassRef& local) {
            return CalleeFormalsOf(
                unit_lowerer,
                unit_lowerer.Hir().classes.Get(local.class_id).constructor);
          },
          [&](const hir::ExternalClassRef& external) {
            const hir::ExternalClass* published = hir::FindExternalClass(
                unit_lowerer.Hir().external_classes, external.unit_name,
                external.class_path);
            if (published == nullptr) {
              throw InternalError(
                  "ConstructorFormalsOf: a class another unit declares is "
                  "constructed through what that unit published, which this "
                  "unit records");
            }
            if (!published->constructor.has_value()) {
              throw InternalError(
                  "ConstructorFormalsOf: an interface class is never "
                  "constructed (LRM 8.26.5)");
            }
            return CalleeFormalsOf(unit_lowerer, *published->constructor);
          }},
      cls);
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

namespace {

// The variable a subroutine's declaration states for one of its formals.
struct FormalVariable {
  hir::ProceduralVarId var;
  std::optional<std::string> name;
};

// One formal as a prototype declares it: its value type and direction, and its
// variable where the prototype is read off a declaration rather than off what
// a unit published.
struct PrototypeFormal {
  hir::TypeId type;
  hir::ParamDirection direction;
  std::optional<FormalVariable> declared;
};

// A prototype taking `self`, then each of `formals` that is a parameter, then
// where a call of `kind` reports what it reads, and yielding `call_type`.
auto BuildPrototype(
    UnitLowerer& unit_lowerer, mir::TypeId self, hir::SubroutineKind kind,
    std::span<const PrototypeFormal> formals, mir::TypeId call_type)
    -> mir::CallableCode {
  mir::CallableCode code;
  CallableBindings bindings(unit_lowerer.Unit(), code);
  std::vector<mir::LocalId> params{bindings.Declare(ReceiverOrigin{}, self)};
  params.reserve(formals.size() + 2);
  for (const PrototypeFormal& formal : formals) {
    const std::optional<mir::TypeId> type = ParamTypeOf(
        unit_lowerer.Unit(), unit_lowerer.TranslateType(formal.type),
        formal.direction);
    if (!type.has_value()) continue;
    params.push_back(
        formal.declared.has_value()
            ? bindings.DeclareProcedural(
                  ProceduralOrigin{.var = formal.declared->var},
                  formal.declared->name, *type)
            : bindings.DeclareAnonymous(*type));
  }
  if (const std::optional<mir::TypeId> report =
          ReportParamTypeOf(unit_lowerer.Unit(), kind)) {
    params.push_back(bindings.DeclareAnonymous(*report));
  }
  code.params = std::move(params);
  code.result_type = call_type;
  return code;
}

}  // namespace

auto PrototypeOf(
    UnitLowerer& unit_lowerer, const hir::SubroutineDecl& decl,
    mir::TypeId self) -> mir::CallableCode {
  std::vector<PrototypeFormal> formals;
  formals.reserve(decl.params.size());
  for (const hir::SubroutineParam& param : decl.params) {
    const hir::ProceduralVarDecl& var =
        decl.body.procedural_vars.Get(param.var);
    formals.push_back(
        PrototypeFormal{
            .type = var.type,
            .direction = param.direction,
            .declared = FormalVariable{.var = param.var, .name = var.name}});
  }
  return BuildPrototype(
      unit_lowerer, self, decl.kind, formals,
      SubroutineCallTypeOf(unit_lowerer, decl));
}

auto PrototypeOf(
    UnitLowerer& unit_lowerer, const hir::PublishedCallable& published,
    mir::TypeId self) -> mir::CallableCode {
  std::vector<PrototypeFormal> formals;
  formals.reserve(published.interface.params.size());
  for (const hir::ExternalCalleeParam& param : published.interface.params) {
    formals.push_back(
        PrototypeFormal{
            .type = param.type,
            .direction = param.direction,
            .declared = std::nullopt});
  }
  return BuildPrototype(
      unit_lowerer, self, published.interface.kind, formals,
      SubroutineCallTypeOf(
          unit_lowerer, published.interface, published.result_type));
}

auto ProjectCompletionComponent(
    mir::Block& block, mir::LocalId completion, mir::TypeId payload_type,
    base::ComponentIndex index, mir::TypeId component_type) -> mir::ExprId {
  const mir::ExprId payload = block.LocalPlace(completion, payload_type);
  return block.Read(block.exprs.Add(
      mir::MakeFieldAccessExpr(
          payload, mir::ComponentFieldTarget{.position = index},
          component_type)));
}

auto BindCompletion(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::Expr call,
    mir::TypeId payload_type, std::span<const CompletionWriteback> writebacks)
    -> diag::Result<mir::LocalId> {
  const mir::CompilationUnit& unit = unit_lowerer.Unit();
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
    auto stored =
        AppendStores(unit_lowerer, frame, writeback.target, component);
    if (!stored) return std::unexpected(std::move(stored.error()));
  }
  return completion;
}

}  // namespace lyra::lowering::hir_to_mir

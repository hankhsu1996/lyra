#include "lyra/lowering/hir_to_mir/expression/system/value_dump.hpp"

#include <cstddef>
#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/call_operands.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// A call of `entry`, handed the runtime and then `operands`.
auto DumpCall(
    ProcessLowerer& process, mir::Block& block, support::BuiltinFn entry,
    std::vector<mir::ExprId> operands) -> mir::Expr {
  std::vector<mir::ExprId> arguments{
      block.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner()))};
  arguments.insert(arguments.end(), operands.begin(), operands.end());
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = entry},
              .arguments = std::move(arguments)},
      .type = process.Owner().Unit().builtins.void_type};
}

// A count or a size the source wrote, read out as the machine integer the
// runtime takes it as.
auto MachineIntOperand(
    ProcessLowerer& process, const WalkFrame& frame, hir::ExprId operand)
    -> diag::Result<mir::ExprId> {
  auto lowered = process.LowerExpr(process.HirBody().exprs.Get(operand), frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  mir::Block& block = *frame.current_block;
  return BuildToInt64Call(
      process.Owner().Unit(), block, block.exprs.Add(*std::move(lowered)));
}

// The instance an argument names, none where it names something else.
auto NamedInstance(const hir::Expr& named) -> const hir::RoutedObjectRef* {
  const auto* primary = std::get_if<hir::PrimaryExpr>(&named.data);
  return primary != nullptr ? std::get_if<hir::RoutedObjectRef>(&primary->data)
                            : nullptr;
}

// The file an operand names, as a string, or `otherwise` where the call names
// none.
auto FileNameOperand(
    ProcessLowerer& process, const WalkFrame& frame,
    std::optional<hir::ExprId> named, std::string_view otherwise)
    -> diag::Result<mir::ExprId> {
  auto& unit = process.Owner().Unit();
  mir::Block& block = *frame.current_block;
  if (!named.has_value()) {
    return BuildStringValueExpr(unit, block, std::string(otherwise));
  }
  auto lowered = process.LowerExpr(process.HirBody().exprs.Get(*named), frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  return ConvertToType(
      unit, block, block.exprs.Add(*std::move(lowered)), unit.builtins.string);
}

// One item a `$dumpvars` names after its levels (LRM 21.7.1.2): an instance,
// as the pointer to the object it is that the route to it ends at, or a
// variable, as the address of the member it is of the instance declaring it.
// A part of a variable is no item (LRM 21.7.2.1).
auto DumpVarsItem(
    ProcessLowerer& process, const WalkFrame& frame, mir::ExprId levels,
    hir::ExprId item) -> diag::Result<mir::Expr> {
  const hir::Expr& named = process.HirBody().exprs.Get(item);
  mir::Block& block = *frame.current_block;
  if (const hir::RoutedObjectRef* instance = NamedInstance(named)) {
    return DumpCall(
        process, block, support::BuiltinFn::kDumpVarsOf,
        {levels, block.AsValue(
                     process.Owner().Unit().types,
                     process.RouteEnd(frame, instance->id))});
  }
  auto path = process.LowerAccessPath(named, frame);
  if (!path) return std::unexpected(std::move(path.error()));
  if (!path->descent.empty()) {
    return diag::Fail(
        named.span, diag::DiagCode::kUnsupportedExpressionForm,
        "a part of a variable cannot be dumped; name the whole variable");
  }
  const mir::ExprId member = OwnerPlace(*path);
  auto& unit = process.Owner().Unit();
  const mir::TypeId pointer = unit.types.Intern(
      mir::Type{mir::PointerType{
          .pointee = block.exprs.Get(member).type,
          .ownership = mir::PointerOwnership::kBorrowed}});
  return DumpCall(
      process, block, support::BuiltinFn::kDumpVarsOfVariable,
      {block.exprs.Add(mir::MakeAddressOfExpr(member, pointer))});
}

// LRM 21.7.1.2: no arguments is the whole design, levels alone that many
// levels of it from its tops, and otherwise the levels and then each instance
// and variable.
auto LowerDumpVars(
    ProcessLowerer& process, const WalkFrame& frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  mir::Block& block = *frame.current_block;
  const std::vector<hir::ExprId> operands = RequiredOperands(call);
  if (operands.empty()) {
    return DumpCall(process, block, support::BuiltinFn::kDumpVars, {});
  }
  auto levels = MachineIntOperand(process, frame, operands.front());
  if (!levels) return std::unexpected(std::move(levels.error()));
  if (operands.size() == 1) {
    return DumpCall(
        process, block, support::BuiltinFn::kDumpVarsLevels, {*levels});
  }
  std::optional<mir::Expr> last;
  for (std::size_t i = 1; i < operands.size(); ++i) {
    if (last.has_value()) {
      block.AppendStmt(
          mir::ExprStmt{.expr = block.exprs.Add(*std::move(last))});
    }
    auto item = DumpVarsItem(process, frame, *levels, operands[i]);
    if (!item) return std::unexpected(std::move(item.error()));
    last = *std::move(item);
  }
  return *std::move(last);
}

// A `$dumpports` control task naming no file acts on every file, which the
// runtime is told by the empty name (LRM 21.7.3.7).
constexpr std::string_view kEveryPortFile = "";

// The instance a body stands in, which a `$dumpports` naming none dumps the
// ports of (LRM 21.7.3.1): the instance of the unit its code belongs to.
auto CallingInstance(
    ProcessLowerer& process, const WalkFrame& frame, diag::SourceSpan span)
    -> diag::Result<mir::ExprId> {
  if (!process.HasEnclosingScope()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a '$dumpports' naming no instance outside a module's own code is not "
        "yet supported");
  }
  return BuildEnclosingScopeReceiver(
      frame, process.Owner().Unit(), frame.HopsToUnitRoot());
}

// LRM 21.7.3.1: the instances named, or the one calling where none is, each
// sending its ports to the file named last, or to `dumpports.vcd` where none
// is. A name standing alone is an instance where it reaches one, and is the
// file otherwise.
auto LowerDumpPorts(
    ProcessLowerer& process, const WalkFrame& frame, const hir::CallExpr& call,
    diag::SourceSpan span) -> diag::Result<mir::Expr> {
  auto& unit = process.Owner().Unit();
  mir::Block& block = *frame.current_block;
  std::vector<mir::ExprId> instances;
  std::optional<hir::ExprId> file;
  for (const std::optional<hir::ExprId>& argument : call.arguments) {
    if (!argument.has_value()) {
      continue;
    }
    const hir::RoutedObjectRef* instance =
        NamedInstance(process.HirBody().exprs.Get(*argument));
    if (instance == nullptr) {
      file = *argument;
      continue;
    }
    instances.push_back(
        block.AsValue(unit.types, process.RouteEnd(frame, instance->id)));
  }
  auto name = FileNameOperand(process, frame, file, "dumpports.vcd");
  if (!name) return std::unexpected(std::move(name.error()));
  if (instances.empty()) {
    auto calling = CallingInstance(process, frame, span);
    if (!calling) return std::unexpected(std::move(calling.error()));
    instances.push_back(*calling);
  }
  for (std::size_t i = 0; i + 1 < instances.size(); ++i) {
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.exprs.Add(DumpCall(
                process, block, support::BuiltinFn::kDumpPortsOf,
                {instances[i], *name}))});
  }
  return DumpCall(
      process, block, support::BuiltinFn::kDumpPortsOf,
      {instances.back(), *name});
}

// A control task of the extended dumps, on the file its operand at `at`
// names.
auto LowerDumpPortsControl(
    ProcessLowerer& process, const WalkFrame& frame, const hir::CallExpr& call,
    support::BuiltinFn entry, std::size_t at) -> diag::Result<mir::Expr> {
  auto name = FileNameOperand(
      process, frame, OptionalOperand(call, at), kEveryPortFile);
  if (!name) return std::unexpected(std::move(name.error()));
  return DumpCall(process, *frame.current_block, entry, {*name});
}

}  // namespace

auto LowerValueDumpSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    const support::ValueDumpSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  mir::Block& block = *frame.current_block;
  switch (info.task) {
    case support::ValueDumpTask::kDumpFile: {
      // LRM 21.7.1.1: the file is `dump.vcd` where the call names none.
      auto name =
          FileNameOperand(process, frame, OptionalOperand(call, 0), "dump.vcd");
      if (!name) return std::unexpected(std::move(name.error()));
      return DumpCall(process, block, support::BuiltinFn::kDumpFile, {*name});
    }
    case support::ValueDumpTask::kDumpVars:
      return LowerDumpVars(process, frame, call);
    case support::ValueDumpTask::kDumpOff:
      return DumpCall(process, block, support::BuiltinFn::kDumpOff, {});
    case support::ValueDumpTask::kDumpOn:
      return DumpCall(process, block, support::BuiltinFn::kDumpOn, {});
    case support::ValueDumpTask::kDumpAll:
      return DumpCall(process, block, support::BuiltinFn::kDumpAll, {});
    case support::ValueDumpTask::kDumpLimit: {
      auto bytes =
          MachineIntOperand(process, frame, RequiredOperands(call, 1)[0]);
      if (!bytes) return std::unexpected(std::move(bytes.error()));
      return DumpCall(process, block, support::BuiltinFn::kDumpLimit, {*bytes});
    }
    case support::ValueDumpTask::kDumpFlush:
      return DumpCall(process, block, support::BuiltinFn::kDumpFlush, {});
    case support::ValueDumpTask::kDumpPorts:
      return LowerDumpPorts(process, frame, call, span);
    case support::ValueDumpTask::kDumpPortsOff:
      return LowerDumpPortsControl(
          process, frame, call, support::BuiltinFn::kDumpPortsOff, 0);
    case support::ValueDumpTask::kDumpPortsOn:
      return LowerDumpPortsControl(
          process, frame, call, support::BuiltinFn::kDumpPortsOn, 0);
    case support::ValueDumpTask::kDumpPortsAll:
      return LowerDumpPortsControl(
          process, frame, call, support::BuiltinFn::kDumpPortsAll, 0);
    case support::ValueDumpTask::kDumpPortsFlush:
      return LowerDumpPortsControl(
          process, frame, call, support::BuiltinFn::kDumpPortsFlush, 0);
    case support::ValueDumpTask::kDumpPortsLimit: {
      auto bytes = MachineIntOperand(
          process, frame, RequiredLeadingOperands(call, 1)[0]);
      if (!bytes) return std::unexpected(std::move(bytes.error()));
      auto name = FileNameOperand(
          process, frame, OptionalOperand(call, 1), kEveryPortFile);
      if (!name) return std::unexpected(std::move(name.error()));
      return DumpCall(
          process, block, support::BuiltinFn::kDumpPortsLimit, {*bytes, *name});
    }
  }
  throw InternalError("LowerValueDumpSystemSubroutineCall: unknown task");
}

}  // namespace lyra::lowering::hir_to_mir

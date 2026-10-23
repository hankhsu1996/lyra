#include "lyra/lowering/hir_to_mir/expression/system/mem_file.hpp"

#include <array>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/param_direction.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/call_operands.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/enum_table.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The leaf element type, the addressing operands a memory shape hands to the
// runtime call, and the address a run starts from where the source names none.
// Each shape states its addressing differently -- a fixed array of any depth
// by every dimension's range, and an associative array, a dynamic array or a
// queue by nothing (its keys are of its declared index type, and its
// `[0, size-1]` range comes from the container).
struct MemAddressing {
  hir::TypeId element;
  std::vector<mir::ExprId> operands;
  mir::ExprId lowest_address;
};

auto DescribeMemory(
    ProcessLowerer& process, WalkFrame wrapper_frame, hir::TypeId mem_type,
    const std::optional<SliceIndices>& slice, std::string_view task,
    diag::SourceSpan span) -> diag::Result<MemAddressing> {
  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  auto& wrapper = *wrapper_frame.current_block;
  const auto not_a_memory = [&]() -> diag::Result<MemAddressing> {
    throw InternalError(
        std::format(
            "{} reached lowering with a target that is no memory", task));
  };
  const auto from_zero = [&](hir::TypeId element) {
    return MemAddressing{
        .element = element,
        .operands = {},
        .lowest_address = mir::BuildMachineIntLiteral(unit, wrapper, 0)};
  };
  return unit_lowerer.Hir().types.Get(mem_type).Visit(
      Overloaded{
          [&](const hir::UnpackedArrayType&) -> diag::Result<MemAddressing> {
            // Every dimension's indices ride in one operand, highest dimension
            // first, so the runtime traverses row-major by ascending address
            // (LRM 21.4.3) and a one-dimensional memory is the one-dimension
            // case of the same traversal.
            const hir::UnpackedShape shape =
                hir::UnpackedShapeOf(unit_lowerer.Hir().types, mem_type);
            const MemoryIndices indices =
                MemoryIndicesOf(unit, wrapper_frame, shape.dims, slice);
            return MemAddressing{
                .element = shape.element_type,
                .operands = {indices.ranges},
                .lowest_address = indices.lowest};
          },
          [&](const hir::DynamicArrayType& d) -> diag::Result<MemAddressing> {
            return from_zero(d.element_type);
          },
          [&](const hir::QueueType& q) -> diag::Result<MemAddressing> {
            return from_zero(q.element_type);
          },
          [&](const hir::AssociativeArrayType& a)
              -> diag::Result<MemAddressing> {
            // LRM 21.4.1: an associative memory is addressed by key, and a load
            // builds its keys at the type the memory declares for them. A
            // wildcard index (LRM 7.8.1) declares none.
            const mir::Type& key = unit_lowerer.Unit().types.Get(
                unit_lowerer.TranslateType(a.key_type));
            if (key.Is<mir::WildcardIndexType>()) {
              return diag::Fail(
                  span, diag::DiagCode::kUnsupportedSubroutineArgument,
                  "'{}' with an associative memory of wildcard index is not "
                  "yet supported",
                  task);
            }
            if (!key.IsIntegral()) {
              throw InternalError(
                  std::format(
                      "{} reached lowering with an associative memory whose "
                      "index type is not integral",
                      task));
            }
            return from_zero(a.element_type);
          },
          // LRM 21.4 / 21.5 name a memory as an array of integral elements, and
          // the front end refuses a task given any other form.
          [&](const hir::ScalarBitType&) { return not_a_memory(); },
          [&](const hir::PackedArrayType&) { return not_a_memory(); },
          [&](const hir::PackedStructType&) { return not_a_memory(); },
          [&](const hir::PackedUnionType&) { return not_a_memory(); },
          [&](const hir::EnumType&) { return not_a_memory(); },
          [&](const hir::UnpackedStructType&) { return not_a_memory(); },
          [&](const hir::UnpackedUnionType&) { return not_a_memory(); },
          [&](const hir::WildcardIndexType&) { return not_a_memory(); },
          [&](const hir::StringType&) { return not_a_memory(); },
          [&](const hir::EventType&) { return not_a_memory(); },
          [&](const hir::RealType&) { return not_a_memory(); },
          [&](const hir::ShortRealType&) { return not_a_memory(); },
          [&](const hir::RealTimeType&) { return not_a_memory(); },
          [&](const hir::ChandleType&) { return not_a_memory(); },
          [&](const hir::ClassHandleType&) { return not_a_memory(); },
          [&](const hir::ImportedClassHandleType&) { return not_a_memory(); },
          [&](const hir::CovergroupHandleType&) { return not_a_memory(); },
          [&](const hir::UnitObjectType&) { return not_a_memory(); },
          [&](const hir::UnitObjectsType&) { return not_a_memory(); },
          [&](const hir::VirtualInterfaceType&) { return not_a_memory(); },
          [&](const hir::NullType&) { return not_a_memory(); },
          [&](const hir::DynamicType&) { return not_a_memory(); },
          [&](const hir::VoidType&) { return not_a_memory(); },
      });
}

// Which dump a call means: whether it named a window (LRM 21.5).
auto DumpTask(bool windowed) -> support::BuiltinFn {
  return windowed ? support::BuiltinFn::kWriteMemWithin
                  : support::BuiltinFn::kWriteMem;
}

// Which load a call means: whether it named a window, and whether its words are
// an enumeration's members (LRM 21.4, 21.4.2).
auto LoadTask(bool windowed, bool enumerated) -> support::BuiltinFn {
  if (enumerated) {
    return windowed ? support::BuiltinFn::kReadEnumeratedMemWithin
                    : support::BuiltinFn::kReadEnumeratedMem;
  }
  return windowed ? support::BuiltinFn::kReadMemWithin
                  : support::BuiltinFn::kReadMem;
}

}  // namespace

auto NameMemory(
    ProcessLowerer& process, const WalkFrame& frame, const hir::Expr& memory)
    -> diag::Result<NamedMemory> {
  UnitLowerer& unit_lowerer = process.Owner();
  const auto* sel = std::get_if<hir::RangeSelectExpr>(&memory.data);
  if (sel != nullptr &&
      unit_lowerer.Hir().types.Get(memory.type).Is<hir::UnpackedArrayType>()) {
    return LowerHirArraySliceWithIndices(
        process, frame, *sel, unit_lowerer.TranslateType(memory.type));
  }
  auto path = process.LowerAccessPath(memory, frame);
  if (!path) return std::unexpected(std::move(path.error()));
  return NamedMemory{.path = *std::move(path), .slice = std::nullopt};
}

auto MemoryWriteTarget(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame, AccessPath path,
    const hir::Expr& memory) -> Lvalue {
  SettleForTagChecks(unit_lowerer, frame, path);
  return Lvalue{
      .form = std::move(path),
      .type = unit_lowerer.TranslateType(memory.type),
      .source_type = memory.type,
      .span = memory.span};
}

auto MemoryIndicesOf(
    mir::CompilationUnit& unit, const WalkFrame& frame,
    std::span<const hir::UnpackedRange> dims,
    const std::optional<SliceIndices>& slice) -> MemoryIndices {
  mir::Block& block = *frame.current_block;
  const auto number = [&](std::int64_t index) {
    return mir::BuildMachineIntLiteral(unit, block, index);
  };
  const hir::UnpackedRange& highest = dims.front();
  const SliceIndices top = [&] {
    if (!slice.has_value()) {
      return SliceIndices{
          .left = number(highest.left), .right = number(highest.right)};
    }
    return SliceIndices{
        .left = EvaluatedOnce(frame, slice->left),
        .right = EvaluatedOnce(frame, slice->right)};
  }();
  std::vector<mir::ExprId> bounds{top.left, top.right};
  for (const hir::UnpackedRange& below : dims.subspan(1)) {
    bounds.push_back(number(below.left));
    bounds.push_back(number(below.right));
  }
  const mir::TypeId type = mir::MachineArrayOf(
      unit.types, unit.builtins.machine_int64, bounds.size());
  // A slice runs the way the array it is taken from does, which its zero-based
  // type keeps (LRM 7.4.5).
  const bool descending = highest.left > highest.right;
  return MemoryIndices{
      .ranges = block.exprs.Add(
          mir::Expr{
              .data = mir::CompositeExpr{.parts = std::move(bounds)},
              .type = type}),
      .lowest = descending ? top.right : top.left};
}

auto LowerMemFileSystemSubroutineCallStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CallExpr& call, const support::MemFileSystemSubroutineInfo& info)
    -> diag::Result<mir::Stmt> {
  auto& unit_lowerer = process.Owner();
  auto& unit = unit_lowerer.Unit();
  const auto& hir_proc = process.HirBody();
  const bool is_store = info.direction == support::MemFileDirection::kStore;
  const bool is_binary = info.base == 2U;
  const std::string_view task = [&]() -> std::string_view {
    if (is_store) return is_binary ? "$writememb" : "$writememh";
    return is_binary ? "$readmemb" : "$readmemh";
  }();

  // LRM 21.4 / 21.5: arg[0] is the file name, arg[1] the memory, arg[2] /
  // arg[3] the optional start / finish addresses.
  const std::vector<hir::ExprId> head = RequiredLeadingOperands(call, 2);
  const auto& mem_hir = hir_proc.exprs.Get(head[1]);

  BlockBuilder steps(frame);
  const WalkFrame& step_frame = steps.Frame();
  mir::Block& body = steps.Body();

  auto name_or = process.LowerExpr(hir_proc.exprs.Get(head[0]), step_frame);
  if (!name_or) return std::unexpected(std::move(name_or.error()));
  // The file name is an SV string; a string literal reaches here as a packed
  // value, so route it to the runtime's String type (LRM 21.4 / 21.5).
  const mir::ExprId name_id = ConvertToType(
      unit, body, body.exprs.Add(*std::move(name_or)), unit.builtins.string);

  auto memory = NameMemory(process, step_frame, mem_hir);
  if (!memory) return std::unexpected(std::move(memory.error()));
  auto addressing = DescribeMemory(
      process, step_frame, mem_hir.type, memory->slice, task, mem_hir.span);
  if (!addressing) return std::unexpected(std::move(addressing.error()));

  // A word is a single packed vector (LRM 21.4.1 / 21.5.1): a bit vector, or a
  // packed struct / union / enum. The front end also accepts a memory of
  // several dimensions with one of variable size, whose words are then arrays.
  const mir::TypeId elem_mir = unit_lowerer.TranslateType(addressing->element);
  if (!unit.types.Get(elem_mir).IsIntegral()) {
    return diag::Fail(
        mem_hir.span, diag::DiagCode::kUnsupportedSubroutineArgument,
        "'{}' with a memory of more than one dimension, one of them not of "
        "fixed size, is not yet supported",
        task);
  }

  // A load's memory is an `inout`: it crosses in because a word the file does
  // not address keeps what it held, and it rides the completion back out (LRM
  // 13.5, 21.4). A dump only reads it.
  const mir::TypeId mem_type = unit_lowerer.TranslateType(mem_hir.type);
  std::optional<Lvalue> mem_place;
  mir::ExprId mem_value{};
  if (is_store) {
    AppendReadChecks(unit, body, memory->path);
    mem_value = PathValue(unit, body, memory->path);
  } else {
    auto read = ReadThenWrite(
        unit_lowerer, step_frame,
        MemoryWriteTarget(
            unit_lowerer, step_frame, std::move(memory->path), mem_hir));
    if (!read) return std::unexpected(std::move(read.error()));
    mem_place = std::move(read->lvalue);
    mem_value = read->incoming;
  }

  // A word of an enumerated memory is a member's value and nothing else (LRM
  // 21.4.2), so its load names the member list it reads the words as.
  const bool enumerated =
      !is_store && unit.types.Get(elem_mir).Is<mir::EnumType>();

  std::vector<mir::ExprId> operands;
  operands.push_back(body.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer)));
  operands.push_back(mem_value);
  if (enumerated) {
    operands.push_back(mir::BuildEnumTableRef(unit, body, elem_mir));
  }
  operands.push_back(name_id);
  for (const mir::ExprId operand : addressing->operands) {
    operands.push_back(operand);
  }
  operands.push_back(mir::BuildMachineIntLiteral(unit, body, info.base));

  // The addresses start where the source said, or at the memory's lowest
  // address where it said nothing -- which is the same addresses the clause's
  // no-address form describes, so the two need no separate entry. A finish
  // makes it the other request.
  const std::optional<hir::ExprId> start = OptionalOperand(call, 2);
  const std::optional<hir::ExprId> finish = OptionalOperand(call, 3);
  if (start.has_value()) {
    auto start_or = process.LowerExpr(hir_proc.exprs.Get(*start), step_frame);
    if (!start_or) return std::unexpected(std::move(start_or.error()));
    operands.push_back(
        BuildToInt64Call(unit, body, body.exprs.Add(*std::move(start_or))));
  } else {
    if (finish.has_value()) {
      throw InternalError(
          std::format(
              "{} reached lowering with a finish address and no start, which "
              "the front end refuses",
              task));
    }
    operands.push_back(addressing->lowest_address);
  }
  if (finish.has_value()) {
    auto finish_or = process.LowerExpr(hir_proc.exprs.Get(*finish), step_frame);
    if (!finish_or) return std::unexpected(std::move(finish_or.error()));
    operands.push_back(
        BuildToInt64Call(unit, body, body.exprs.Add(*std::move(finish_or))));
  }

  const bool windowed = finish.has_value();
  const support::BuiltinFn target =
      is_store ? DumpTask(windowed) : LoadTask(windowed, enumerated);

  if (is_store) {
    body.AppendStmt(
        mir::ExprStmt{
            .expr = body.exprs.Add(
                mir::Expr{
                    .data =
                        mir::CallExpr{
                            .callee = mir::Direct{.target = target},
                            .arguments = std::move(operands)},
                    .type = unit.builtins.void_type})});
  } else {
    const CompletionLayout layout = BuildCompletionLayout(
        {CalleeFormal{
            .direction = hir::ParamDirection::kInOut, .type = mem_type}},
        std::nullopt);
    const mir::TypeId payload = CompletionPayloadType(unit, layout.components);
    const std::array writebacks{CompletionWriteback{
        .target = *mem_place,
        .component = *layout.formals.front().component,
        .type = mem_type}};
    auto completion = BindCompletion(
        unit_lowerer, step_frame,
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee = mir::Direct{.target = target},
                    .arguments = std::move(operands)},
            .type = payload},
        payload, writebacks);
    if (!completion) return std::unexpected(std::move(completion.error()));
  }

  mir::Stmt stmt = steps.BuildStatement();
  stmt.label = std::move(label);
  return stmt;
}

}  // namespace lyra::lowering::hir_to_mir

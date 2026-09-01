#include "lyra/lowering/hir_to_mir/expression/system/mem_file.hpp"

#include <array>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/param_direction.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/call_operands.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The leaf element type, the addressing operands a memory shape hands to the
// runtime call, and the address a run starts from where the source names none.
// Each shape describes its addressing differently -- a fixed array of any depth
// by every dimension's bounds as one array, an associative array by a key
// prototype that carries the index width, and a dynamic array or queue by
// nothing (its `[0, size-1]` range comes from the container). `DescribeMemory`
// is the one place that knows this, so the call assembly stays uniform.
struct MemAddressing {
  hir::TypeId element;
  std::vector<mir::ExprId> operands;
  std::int64_t lowest_address;
};

auto DescribeMemory(
    ProcessLowerer& process, WalkFrame wrapper_frame, hir::TypeId mem_type,
    bool is_store, std::string_view task) -> diag::Result<MemAddressing> {
  UnitLowerer& unit_lowerer = process.Owner();
  auto& wrapper = *wrapper_frame.current_block;
  const mir::TypeId int_type = unit_lowerer.Unit().builtins.int_type;
  const auto int_literal = [&](std::int64_t value) {
    return BuildIntLiteral(unit_lowerer.Unit(), wrapper, value);
  };
  return unit_lowerer.Hir().types.Get(mem_type).Visit(
      Overloaded{
          [&](const hir::UnpackedArrayType&) -> diag::Result<MemAddressing> {
            // Walk the nested unpacked dimensions to the leaf element. Every
            // dimension's bounds ride as one array, highest dimension first, so
            // the runtime traverses row-major by ascending address (LRM 21.4.3)
            // and a one-dimensional memory is the two-element case of the same
            // traversal.
            std::vector<hir::UnpackedRange> dims;
            hir::TypeId cursor = mem_type;
            while (const auto* nested = unit_lowerer.Hir()
                                            .types.Get(cursor)
                                            .As<hir::UnpackedArrayType>()) {
              dims.push_back(nested->dim);
              cursor = nested->element_type;
            }
            std::vector<mir::ExprId> bounds;
            bounds.reserve(dims.size() * 2);
            for (const hir::UnpackedRange& dim : dims) {
              bounds.push_back(int_literal(dim.left));
              bounds.push_back(int_literal(dim.right));
            }
            const mir::TypeId bounds_type = mir::MachineArrayOf(
                unit_lowerer.Unit().types, int_type, bounds.size());
            return MemAddressing{
                .element = cursor,
                .operands = {wrapper.exprs.Add(
                    mir::Expr{
                        .data =
                            mir::ArrayLiteralExpr{
                                .elements = std::move(bounds)},
                        .type = bounds_type})},
                .lowest_address =
                    std::min(dims.front().left, dims.front().right)};
          },
          [&](const hir::DynamicArrayType& d) -> diag::Result<MemAddressing> {
            return MemAddressing{
                .element = d.element_type, .operands = {}, .lowest_address = 0};
          },
          [&](const hir::QueueType& q) -> diag::Result<MemAddressing> {
            return MemAddressing{
                .element = q.element_type, .operands = {}, .lowest_address = 0};
          },
          [&](const hir::AssociativeArrayType& a)
              -> diag::Result<MemAddressing> {
            // LRM 21.4.1: an associative memory is addressed by key, so its
            // index type must be integral. A load builds its keys at that
            // declared width, carried by a default value of the key type; a
            // dump reads the stored keys and needs no prototype.
            const mir::TypeId key = unit_lowerer.TranslateType(a.key_type);
            if (!unit_lowerer.Unit()
                     .types.Get(key)
                     .Is<mir::PackedArrayType>()) {
              return diag::Fail(
                  diag::DiagCode::kUnsupportedSubroutineArgument,
                  std::format(
                      "{} associative memory: the index type must be integral "
                      "(LRM 21.4.1)",
                      task));
            }
            std::vector<mir::ExprId> operands;
            if (!is_store) {
              operands.push_back(wrapper.exprs.Add(
                  BuildDefaultValueExpr(unit_lowerer, wrapper_frame, key)));
            }
            return MemAddressing{
                .element = a.element_type,
                .operands = std::move(operands),
                .lowest_address = 0};
          },
          [&](const auto&) -> diag::Result<MemAddressing> {
            return diag::Fail(
                diag::DiagCode::kUnsupportedSubroutineArgument,
                std::format(
                    "{} target must be an unpacked, dynamic-array, queue, or "
                    "associative memory (LRM 21.4 / 21.5)",
                    task));
          },
      });
}

// Which of the four entries a call means: the direction the source wrote, and
// whether it named a window (LRM 21.4 / 21.5).
auto MemoryTask(bool is_store, bool windowed) -> support::BuiltinFn {
  if (is_store) {
    return windowed ? support::BuiltinFn::kWriteMemWithin
                    : support::BuiltinFn::kWriteMem;
  }
  return windowed ? support::BuiltinFn::kReadMemWithin
                  : support::BuiltinFn::kReadMem;
}

}  // namespace

auto LowerMemFileSystemSubroutineCallStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CallExpr& call, const support::MemFileSystemSubroutineInfo& info)
    -> diag::Result<mir::Stmt> {
  auto& unit_lowerer = process.Owner();
  auto& unit = unit_lowerer.Unit();
  const auto& hir_proc = process.HirBody();
  const bool is_store = info.direction == support::MemFileDirection::kStore;
  const std::string_view task =
      is_store ? "$writememh / $writememb" : "$readmemh / $readmemb";

  // LRM 21.4 / 21.5: arg[0] is the file name, arg[1] the memory, arg[2] /
  // arg[3] the optional start / finish addresses.
  const std::vector<hir::ExprId> head = RequiredLeadingOperands(call, 2);
  const auto& mem_hir = hir_proc.exprs.Get(head[1]);

  BlockBuilder steps(frame);
  const WalkFrame& step_frame = steps.Frame();
  mir::Block& body = steps.Body();

  auto addressing =
      DescribeMemory(process, step_frame, mem_hir.type, is_store, task);
  if (!addressing) return std::unexpected(std::move(addressing.error()));

  // The element must lower to a single packed vector (LRM 21.4.1 / 21.5.1): a
  // bit vector, or a packed struct / union / enum. A non-packed leaf (an
  // unpacked struct, say) is not a memory word.
  const mir::TypeId elem_mir = unit_lowerer.TranslateType(addressing->element);
  if (!unit.types.Get(elem_mir).Is<mir::PackedArrayType>()) {
    return diag::Fail(
        diag::DiagCode::kUnsupportedSubroutineArgument,
        std::format(
            "{} memory: the element must be a packed vector (LRM 21.4.1 / "
            "21.5.1)",
            task));
  }

  auto name_or = process.LowerExpr(hir_proc.exprs.Get(head[0]), step_frame);
  if (!name_or) return std::unexpected(std::move(name_or.error()));
  // The file name is an SV string; a string literal reaches here as a packed
  // value, so route it to the runtime's String type (LRM 21.4 / 21.5).
  const mir::ExprId name_id = ConvertToType(
      unit, body, body.exprs.Add(*std::move(name_or)), unit.builtins.string);

  // A load's memory is an `inout`: it crosses in because a word the file does
  // not address keeps what it held, and it rides the completion back out (LRM
  // 13.5, 21.4). A dump only reads it. Either way its place is bound here,
  // which is the once it is evaluated.
  std::optional<mir::ExprId> mem_place;
  if (!is_store) {
    auto place_or = process.LowerLhsExpr(mem_hir, step_frame);
    if (!place_or) return std::unexpected(std::move(place_or.error()));
    mem_place = body.exprs.Add(*std::move(place_or));
  }
  auto mem_or = process.LowerExpr(mem_hir, step_frame);
  if (!mem_or) return std::unexpected(std::move(mem_or.error()));

  std::vector<mir::ExprId> operands;
  operands.push_back(body.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer)));
  operands.push_back(body.exprs.Add(*std::move(mem_or)));
  operands.push_back(name_id);
  for (const mir::ExprId operand : addressing->operands) {
    operands.push_back(operand);
  }
  operands.push_back(
      BuildIntLiteral(unit, body, static_cast<std::int64_t>(info.base)));

  // The run starts where the source said, or at the memory's lowest address
  // where it said nothing -- which is the same run the clause's no-address form
  // describes, so the two need no separate entry. A finish makes it the other
  // request.
  const std::optional<hir::ExprId> start = OptionalOperand(call, 2);
  const std::optional<hir::ExprId> finish = OptionalOperand(call, 3);
  if (start.has_value()) {
    auto start_or = process.LowerExpr(hir_proc.exprs.Get(*start), step_frame);
    if (!start_or) return std::unexpected(std::move(start_or.error()));
    operands.push_back(body.exprs.Add(*std::move(start_or)));
  } else {
    if (finish.has_value()) {
      return diag::Fail(
          diag::DiagCode::kUnsupportedSubroutineArgument,
          std::format(
              "{}: a finish address with the start elided is not supported "
              "(LRM 21.4 / 21.5)",
              task));
    }
    operands.push_back(BuildIntLiteral(unit, body, addressing->lowest_address));
  }
  if (finish.has_value()) {
    auto finish_or = process.LowerExpr(hir_proc.exprs.Get(*finish), step_frame);
    if (!finish_or) return std::unexpected(std::move(finish_or.error()));
    operands.push_back(body.exprs.Add(*std::move(finish_or)));
  }

  const bool windowed = finish.has_value();
  const support::BuiltinFn target = MemoryTask(is_store, windowed);

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
    const mir::TypeId mem_type = unit_lowerer.TranslateType(mem_hir.type);
    const CompletionLayout layout = BuildCompletionLayout(
        {CalleeFormal{
            .direction = hir::ParamDirection::kInOut, .type = mem_type}},
        std::nullopt);
    const mir::TypeId payload = CompletionPayloadType(unit, layout.components);
    const std::array writebacks{CompletionWriteback{
        .place = *mem_place,
        .component = *layout.formals.front().component,
        .type = mem_type}};
    BindCompletion(
        unit, step_frame,
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee = mir::Direct{.target = target},
                    .arguments = std::move(operands)},
            .type = payload},
        payload, writebacks);
  }

  mir::Stmt stmt = steps.BuildStatement();
  stmt.label = std::move(label);
  return stmt;
}

}  // namespace lyra::lowering::hir_to_mir

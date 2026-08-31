#include "lyra/lowering/hir_to_mir/expression/system/mem_file.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/call_operands.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/copy_out_desugar.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The leaf element type plus the addressing operands a memory shape hands to
// the runtime call. Each shape describes its addressing differently -- a
// one-dimensional fixed array by its declared bounds, a multidimensional one by
// every dimension's bounds (as a single bounds array), an associative array by
// a key prototype that carries the index width, and a dynamic array or queue by
// nothing (its `[0, size-1]` range comes from the container). `DescribeMemory`
// is the one place that knows this, so the call assembly stays uniform.
struct MemAddressing {
  hir::TypeId element;
  std::vector<mir::ExprId> operands;
};

auto DescribeMemory(
    ProcessLowerer& process, WalkFrame wrapper_frame, hir::TypeId mem_type,
    bool is_store, std::string_view task) -> diag::Result<MemAddressing> {
  auto& unit = process.Owner();
  auto& wrapper = *wrapper_frame.current_block;
  const mir::TypeId int_type = unit.Unit().builtins.int_type;
  const auto int_literal = [&](std::int64_t value) {
    return BuildIntLiteral(unit.Unit(), wrapper, value);
  };
  return std::visit(
      Overloaded{
          [&](const hir::UnpackedArrayType&) -> diag::Result<MemAddressing> {
            // Walk the nested unpacked dimensions to the leaf element. One
            // dimension passes its declared bounds; two or more (LRM 21.4.3)
            // pass every dimension's bounds as one array, highest dimension
            // first, so the runtime traverses row-major by ascending address.
            std::vector<hir::UnpackedRange> dims;
            hir::TypeId cursor = mem_type;
            while (const auto* nested = std::get_if<hir::UnpackedArrayType>(
                       &unit.Hir().types.Get(cursor).data)) {
              dims.push_back(nested->dim);
              cursor = nested->element_type;
            }
            if (dims.size() == 1) {
              return MemAddressing{
                  .element = cursor,
                  .operands = {
                      int_literal(dims.front().left),
                      int_literal(dims.front().right)}};
            }
            std::vector<mir::ExprId> bounds;
            bounds.reserve(dims.size() * 2);
            for (const hir::UnpackedRange& dim : dims) {
              bounds.push_back(int_literal(dim.left));
              bounds.push_back(int_literal(dim.right));
            }
            const mir::TypeId bounds_type =
                unit.Unit().types.MachineArrayOf(int_type, bounds.size());
            return MemAddressing{
                .element = cursor,
                .operands = {wrapper.exprs.Add(
                    mir::Expr{
                        .data =
                            mir::ArrayLiteralExpr{
                                .elements = std::move(bounds)},
                        .type = bounds_type})}};
          },
          [&](const hir::DynamicArrayType& d) -> diag::Result<MemAddressing> {
            return MemAddressing{.element = d.element_type, .operands = {}};
          },
          [&](const hir::QueueType& q) -> diag::Result<MemAddressing> {
            return MemAddressing{.element = q.element_type, .operands = {}};
          },
          [&](const hir::AssociativeArrayType& a)
              -> diag::Result<MemAddressing> {
            // LRM 21.4.1: an associative memory is addressed by key, so its
            // index type must be integral. A load builds its keys at that
            // declared width, carried by a default value of the key type; a
            // dump reads the stored keys and needs no prototype.
            const mir::TypeId key = unit.TranslateType(a.key_type);
            if (!std::holds_alternative<mir::PackedArrayType>(
                    unit.Unit().types.Get(key).data)) {
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
                  BuildDefaultValueExpr(unit, wrapper_frame, key)));
            }
            return MemAddressing{
                .element = a.element_type, .operands = std::move(operands)};
          },
          [&](const auto&) -> diag::Result<MemAddressing> {
            return diag::Fail(
                diag::DiagCode::kUnsupportedSubroutineArgument,
                std::format(
                    "{} target must be an unpacked, dynamic-array, queue, or "
                    "associative memory (LRM 21.4 / 21.5)",
                    task));
          },
      },
      unit.Hir().types.Get(mem_type).data);
}

}  // namespace

auto LowerMemFileSystemSubroutineCallStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CallExpr& call, const support::MemFileSystemSubroutineInfo& info)
    -> diag::Result<mir::Stmt> {
  const auto& unit_lowerer = process.Owner();
  const auto& hir_proc = process.HirBody();
  const auto& builtins = unit_lowerer.Unit().builtins;
  const bool is_store = info.direction == support::MemFileDirection::kStore;
  const std::string_view task =
      is_store ? "$writememh / $writememb" : "$readmemh / $readmemb";

  // LRM 21.4 / 21.5: arg[0] is the file name, arg[1] the memory, arg[2] /
  // arg[3] the optional start / finish addresses.
  const std::vector<hir::ExprId> head = RequiredLeadingOperands(call, 2);
  const auto& mem_hir = hir_proc.exprs.Get(head[1]);

  mir::Block wrapper;
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);

  auto addressing =
      DescribeMemory(process, wrapper_frame, mem_hir.type, is_store, task);
  if (!addressing) return std::unexpected(std::move(addressing.error()));

  // The element must lower to a single packed vector (LRM 21.4.1 / 21.5.1): a
  // bit vector, or a packed struct / union / enum. A non-packed leaf (an
  // unpacked struct, say) is not a memory word.
  const mir::TypeId elem_mir = unit_lowerer.TranslateType(addressing->element);
  if (!std::holds_alternative<mir::PackedArrayType>(
          unit_lowerer.Unit().types.Get(elem_mir).data)) {
    return diag::Fail(
        diag::DiagCode::kUnsupportedSubroutineArgument,
        std::format(
            "{} memory: the element must be a packed vector (LRM 21.4.1 / "
            "21.5.1)",
            task));
  }

  auto name_or = process.LowerExpr(hir_proc.exprs.Get(head[0]), wrapper_frame);
  if (!name_or) return std::unexpected(std::move(name_or.error()));
  // The file name is an SV string; a string literal reaches here as a packed
  // value, so route it to the runtime's String type (LRM 21.4 / 21.5).
  const mir::ExprId name_id = ConvertToType(
      unit_lowerer.Unit(), wrapper, wrapper.exprs.Add(*std::move(name_or)),
      builtins.string);

  // A load writes the memory (an output argument copy-out desugared per LRM
  // 13.5, so words the file does not address survive); a dump reads it (a plain
  // input read with no writeback -- an empty slot list makes the shared block
  // builder emit a bare call).
  std::vector<OutputArgSlot> slots;
  mir::ExprId mem_arg_id{};
  if (is_store) {
    auto mem_or = process.LowerExpr(mem_hir, wrapper_frame);
    if (!mem_or) return std::unexpected(std::move(mem_or.error()));
    mem_arg_id = wrapper.exprs.Add(*std::move(mem_or));
  } else {
    auto slot_or = BuildOutputArgSlot(
        process, wrapper_frame, head[1], "_lyra_readmem_dest");
    if (!slot_or) return std::unexpected(std::move(slot_or.error()));
    slots.push_back(*slot_or);
    mem_arg_id = wrapper.exprs.Add(
        mir::MakeLocalRefExpr(slots.front().temp, slots.front().type));
  }

  // Uniform assembly: [runtime, memory, filename], the shape's addressing
  // operands, the radix, then the optional start / finish addresses.
  std::vector<mir::ExprId> operands;
  operands.push_back(
      wrapper.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer)));
  operands.push_back(mem_arg_id);
  operands.push_back(name_id);
  for (const mir::ExprId operand : addressing->operands) {
    operands.push_back(operand);
  }
  operands.push_back(BuildIntLiteral(
      unit_lowerer.Unit(), wrapper, static_cast<std::int64_t>(info.base)));
  for (std::size_t i = 2; i < call.arguments.size(); ++i) {
    // Unlike the two above, a bound this lowering cannot realize is the
    // program's shape, not the compiler's, so it earns a diagnostic.
    const std::optional<hir::ExprId> bound = OptionalOperand(call, i);
    if (!bound.has_value()) {
      return diag::Fail(
          diag::DiagCode::kUnsupportedSubroutineArgument,
          std::format(
              "{}: an elided start / finish argument is not supported "
              "(LRM 21.4 / 21.5)",
              task));
    }
    auto arg_or = process.LowerExpr(hir_proc.exprs.Get(*bound), wrapper_frame);
    if (!arg_or) return std::unexpected(std::move(arg_or.error()));
    operands.push_back(wrapper.exprs.Add(*std::move(arg_or)));
  }

  mir::Expr call_expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = is_store ? support::BuiltinFn::kWriteMem
                                         : support::BuiltinFn::kReadMem},
              .arguments = std::move(operands)},
      .type = builtins.void_type};

  return BuildCopyOutBlock(
      unit_lowerer.Unit(), frame, std::move(wrapper), std::move(label),
      builtins.void_type, std::move(call_expr), std::nullopt, slots);
}

}  // namespace lyra::lowering::hir_to_mir

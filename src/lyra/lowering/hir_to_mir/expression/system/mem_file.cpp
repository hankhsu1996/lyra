#include "lyra/lowering/hir_to_mir/expression/system/mem_file.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/copy_out_desugar.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

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
  const auto& mem_hir = hir_proc.exprs.Get(*call.arguments[1]);
  const auto& mem_hir_ty = unit_lowerer.Hir().types.Get(mem_hir.type);
  const auto* unpacked = std::get_if<hir::UnpackedArrayType>(&mem_hir_ty.data);
  if (unpacked == nullptr) {
    return diag::Fail(
        diag::DiagCode::kUnsupportedSubroutineArgument,
        std::format(
            "{} target must be an unpacked memory (LRM 21.4 / 21.5)", task));
  }
  // The element must lower to a single packed vector (LRM 21.4.1 / 21.5.1):
  // a bit vector, or a packed struct / union / enum, each one PackedArray at
  // runtime. A nested unpacked element (a multidimensional memory) or a
  // non-packed element is not yet supported.
  const mir::TypeId elem_mir =
      unit_lowerer.TranslateType(unpacked->element_type);
  if (!std::holds_alternative<mir::PackedArrayType>(
          unit_lowerer.Unit().types.Get(elem_mir).data)) {
    return diag::Fail(
        diag::DiagCode::kUnsupportedSubroutineArgument,
        std::format(
            "{} memory: only a one-dimensional unpacked array of packed "
            "elements is supported (LRM 21.4.1 / 21.5.1)",
            task));
  }

  mir::Block wrapper;
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);

  auto name_or =
      process.LowerExpr(hir_proc.exprs.Get(*call.arguments[0]), wrapper_frame);
  if (!name_or) return std::unexpected(std::move(name_or.error()));
  // The file name is an SV string; a string literal reaches here as a packed
  // value, so route it to the runtime's String type (LRM 21.4 / 21.5).
  const mir::ExprId name_id = ConvertToType(
      unit_lowerer.Unit(), wrapper, wrapper.exprs.Add(*std::move(name_or)),
      builtins.string);

  // The memory argument and the writeback differ by direction: a load writes
  // the memory (an output argument copy-out desugared per LRM 13.5, so words
  // the file does not address survive), a dump reads it (a plain input read
  // with no writeback -- an empty slot list makes the shared block builder emit
  // a bare call).
  std::vector<OutputArgSlot> slots;
  mir::ExprId mem_arg_id{};
  if (is_store) {
    auto mem_or = process.LowerExpr(mem_hir, wrapper_frame);
    if (!mem_or) return std::unexpected(std::move(mem_or.error()));
    mem_arg_id = wrapper.exprs.Add(*std::move(mem_or));
  } else {
    auto slot_or = BuildOutputArgSlot(
        process, wrapper_frame, *call.arguments[1], "_lyra_readmem_dest");
    if (!slot_or) return std::unexpected(std::move(slot_or.error()));
    slots.push_back(*slot_or);
    mem_arg_id = wrapper.exprs.Add(
        mir::MakeLocalRefExpr(slots.front().temp, slots.front().type));
  }

  std::vector<mir::ExprId> operands;
  operands.push_back(
      wrapper.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer)));
  operands.push_back(mem_arg_id);
  operands.push_back(name_id);
  operands.push_back(wrapper.exprs.Add(
      mir::MakeIntLiteral(builtins.int_type, unpacked->dim.left)));
  operands.push_back(wrapper.exprs.Add(
      mir::MakeIntLiteral(builtins.int_type, unpacked->dim.right)));
  operands.push_back(wrapper.exprs.Add(
      mir::MakeIntLiteral(
          builtins.int_type, static_cast<std::int64_t>(info.base))));
  for (std::size_t i = 2; i < call.arguments.size(); ++i) {
    if (!call.arguments[i].has_value()) {
      return diag::Fail(
          diag::DiagCode::kUnsupportedSubroutineArgument,
          std::format(
              "{}: an elided start / finish argument is not supported "
              "(LRM 21.4 / 21.5)",
              task));
    }
    auto arg_or = process.LowerExpr(
        hir_proc.exprs.Get(*call.arguments[i]), wrapper_frame);
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

  const mir::ExprId runtime_id =
      wrapper.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer));
  return BuildCopyOutBlock(
      unit_lowerer.Unit(), runtime_id, frame, std::move(wrapper),
      std::move(label), builtins.void_type, std::move(call_expr), false,
      std::nullopt, slots);
}

}  // namespace lyra::lowering::hir_to_mir

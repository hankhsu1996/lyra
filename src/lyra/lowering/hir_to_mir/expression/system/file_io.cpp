#include "lyra/lowering/hir_to_mir/expression/system/file_io.hpp"

#include <array>
#include <cstddef>
#include <expected>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
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
#include "lyra/lowering/hir_to_mir/expression/calls.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/mem_file.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_interface.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/language_trait.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// A file-IO call acts on the runtime's file broker, which is its receiver; the
// operands follow in the entry's own order.
auto BuildFileIoCall(
    const ProcessLowerer& process, const WalkFrame& frame,
    support::BuiltinFn builtin_fn, std::vector<mir::ExprId> operands,
    mir::TypeId result_type) -> mir::Expr {
  mir::Builder block = frame.Builder();
  const mir::ExprId files =
      block.Add(BuildFilesCallExpr(process.Owner(), block));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = builtin_fn, .receiver = files},
              .arguments = std::move(operands)},
      .type = result_type};
}

auto LowerOperand(
    ProcessLowerer& process, const WalkFrame& frame, hir::ExprId operand)
    -> diag::Result<mir::Expr> {
  return process.LowerExpr(process.HirBody().exprs.Get(operand), frame);
}

// A descriptor, a position, a count or a character the source wrote, read out
// as the machine integer the file broker works in (LRM 21.3).
auto BuildMachineIntOperand(
    ProcessLowerer& process, const WalkFrame& frame, hir::ExprId operand)
    -> diag::Result<mir::ExprId> {
  auto operand_or = LowerOperand(process, frame, operand);
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));
  mir::Builder block = frame.Builder();
  return BuildToInt64Call(
      process.Owner().Unit(), block, block.Add(*std::move(operand_or)));
}

// Every operand of the calls built here is a machine integer (LRM 21.3).
auto LowerFixedOperandCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    support::BuiltinFn builtin_fn, std::size_t operand_count,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const std::vector<hir::ExprId> operands =
      RequiredOperands(call, operand_count);
  std::vector<mir::ExprId> lowered;
  lowered.reserve(operands.size());
  for (const hir::ExprId operand : operands) {
    auto operand_or = BuildMachineIntOperand(process, frame, operand);
    if (!operand_or) return std::unexpected(std::move(operand_or.error()));
    lowered.push_back(*operand_or);
  }
  return BuildFileIoCall(
      process, frame, builtin_fn, std::move(lowered), result_type);
}

// Opening with a mode yields a file descriptor and opening without one yields a
// multichannel descriptor (LRM 21.3.1). Which was written is known here, so the
// call names the form it means and nothing downstream recovers it by counting
// operands.
auto LowerFileOpenCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  mir::Builder block = frame.Builder();
  const std::vector<hir::ExprId> head = RequiredLeadingOperands(call, 1);
  auto name_or = LowerOperand(process, frame, head[0]);
  if (!name_or) return std::unexpected(std::move(name_or.error()));
  std::vector<mir::ExprId> operands{block.Add(*std::move(name_or))};
  const std::optional<hir::ExprId> mode = OptionalOperand(call, 1);
  if (mode) {
    auto mode_or = LowerOperand(process, frame, *mode);
    if (!mode_or) return std::unexpected(std::move(mode_or.error()));
    operands.push_back(block.Add(*std::move(mode_or)));
  }
  return BuildFileIoCall(
      process, frame,
      mode ? support::BuiltinFn::kFileOpenMode : support::BuiltinFn::kFileOpen,
      std::move(operands), process.Owner().Unit().builtins.int_type);
}

// Flushing an addressed channel and flushing every open one (LRM 21.3.6) are
// two requests, told apart here by which the source wrote.
auto LowerFileFlushCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  std::vector<mir::ExprId> operands;
  const std::optional<hir::ExprId> fd = OptionalOperand(call, 0);
  if (fd) {
    auto fd_or = BuildMachineIntOperand(process, frame, *fd);
    if (!fd_or) return std::unexpected(std::move(fd_or.error()));
    operands.push_back(*fd_or);
  }
  return BuildFileIoCall(
      process, frame,
      fd ? support::BuiltinFn::kFileFlush : support::BuiltinFn::kFileFlushAll,
      std::move(operands), process.Owner().Unit().builtins.void_type);
}

// The text a read delivers is an SV string; LRM 21.3.4.2 also admits an
// integral destination, whose width bounds the read, and no entry carries that
// sizing rule yet.
auto RequireStringDestination(
    const mir::CompilationUnit& unit, mir::TypeId destination,
    std::string_view name, diag::SourceSpan span) -> diag::Result<void> {
  if (unit.types.Get(destination).IsDeclaredBy(support::LibraryDecl::kString))
    return {};
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedSubroutineArgument,
      "'{}' into an integral destination, whose width bounds the read, is not "
      "yet supported",
      name);
}

// How many a read reached, as the `integer` the call is: the entry answers it
// at the type `layout` gives the result, and the front end types `$fgets`,
// `$ferror` and `$fread` as the `integer` the code of LRM 21.3.4 is read into.
auto ReadCount(
    mir::CompilationUnit& unit, mir::Builder block, mir::LocalId completion,
    const CompletionLayout& layout) -> mir::ExprId {
  return ConvertToType(
      unit, block,
      ProjectCompletionComponent(
          block, completion, CompletionPayloadType(unit, layout.components),
          kCompletionResult, layout.components.at(kCompletionResult.value)),
      unit.builtins.integer);
}

// Emits the call, stores the component it settles for the destination into the
// place the source named, and yields the count it settled for itself -- the
// steps of one block expression, so a read that writes back stands wherever the
// source wrote it.
//
// The destination is an `output` for either of these, so nothing crosses in
// (LRM 13.5): what the place held before says nothing about the text being
// delivered, and all of it is replaced.
auto BuildTextRead(
    ProcessLowerer& process, BlockBuilder& steps, support::BuiltinFn builtin_fn,
    std::vector<mir::ExprId> operands, Lvalue destination,
    mir::TypeId destination_type) -> diag::Result<mir::Expr> {
  auto& unit = process.Owner().Unit();
  const CompletionLayout layout = BuildCompletionLayout(
      {CalleeFormal{
          .direction = hir::ParamDirection::kOutput, .type = destination_type}},
      unit.builtins.int_type);
  const mir::TypeId payload = CompletionPayloadType(unit, layout.components);
  const std::array writebacks{CompletionWriteback{
      .target = std::move(destination),
      .component = *layout.formals.front().component,
      .type = destination_type}};
  auto completion = BindCompletion(
      process.Owner(), steps.Frame(),
      BuildFileIoCall(
          process, steps.Frame(), builtin_fn, std::move(operands), payload),
      payload, writebacks);
  if (!completion) return std::unexpected(std::move(completion.error()));
  return steps.Build(ReadCount(unit, steps.Body(), *completion, layout));
}

// $fgets(str, fd) -- LRM 21.3.4.2.
auto LowerFileGetsCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    std::string_view name, diag::SourceSpan span) -> diag::Result<mir::Expr> {
  const std::vector<hir::ExprId> operands = RequiredOperands(call, 2);
  BlockBuilder steps(frame);

  const hir::Expr& line_hir = process.HirExprs().Get(operands[0]);
  const mir::TypeId line_type = process.Owner().TranslateType(line_hir.type);
  auto valid_or =
      RequireStringDestination(process.Owner().Unit(), line_type, name, span);
  if (!valid_or) return std::unexpected(std::move(valid_or.error()));
  auto line_or = LowerLvalue(process, line_hir, steps.Frame());
  if (!line_or) return std::unexpected(std::move(line_or.error()));

  auto fd_or = BuildMachineIntOperand(process, steps.Frame(), operands[1]);
  if (!fd_or) return std::unexpected(std::move(fd_or.error()));

  return BuildTextRead(
      process, steps, support::BuiltinFn::kFileGets, {*fd_or},
      *std::move(line_or), line_type);
}

// A memory `$fread` loads (LRM 21.3.4.4): its own element, which is one word,
// and where its type declares one, the range its indices run over. A
// fixed-size array is addressed by its indices; a dynamic array and a queue
// from 0 across the elements they hold, as a memory task addresses them (LRM
// 21.4).
struct FileReadMemory {
  hir::TypeId element;
  std::optional<hir::UnpackedRange> declared;
};

// $fread into a memory -- LRM 21.3.4.4: the words the file holds next, from
// the start the source named or the lowest index, as many as it named or the
// whole memory, which the memory's own code then loads as a memory task's.
// The memory is read and then written: a word the file does not reach keeps
// what it held, and the memory takes the whole of what the load made through
// its own write. A slice is addressed by the indices it holds of the array it
// is taken from. The call answers how many bytes it read.
auto LowerFileReadMemory(
    ProcessLowerer& process, BlockBuilder& steps, const hir::CallExpr& call,
    const hir::Expr& dest_hir, const FileReadMemory& memory,
    mir::TypeId dest_type) -> diag::Result<mir::Expr> {
  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const WalkFrame& step_frame = steps.Frame();
  mir::Builder body = steps.Body();
  auto named = NameMemory(process, step_frame, dest_hir);
  if (!named) return std::unexpected(std::move(named.error()));
  auto changed = ReadThenWrite(
      unit, step_frame,
      MemoryWriteTarget(unit_lowerer, step_frame, named->path, dest_hir));
  if (!changed) return std::unexpected(std::move(changed.error()));

  const std::vector<hir::ExprId> head = RequiredLeadingOperands(call, 2);
  auto fd = BuildMachineIntOperand(process, step_frame, head[1]);
  if (!fd) return std::unexpected(std::move(fd.error()));
  std::optional<mir::ExprId> start;
  if (const std::optional<hir::ExprId> given = OptionalOperand(call, 2)) {
    auto start_or = BuildMachineIntOperand(process, step_frame, *given);
    if (!start_or) return std::unexpected(std::move(start_or.error()));
    start = *start_or;
  }
  std::optional<mir::ExprId> count;
  if (const std::optional<hir::ExprId> given = OptionalOperand(call, 3)) {
    auto count_or = BuildMachineIntOperand(process, step_frame, *given);
    if (!count_or) return std::unexpected(std::move(count_or.error()));
    count = *count_or;
  }
  // The memory is read after every other operand, as the packed form reads
  // its destination.
  const mir::LocalId loaded = DeclareLocal(step_frame, changed->incoming);
  // The start the source left out is the lowest index, and the count it left
  // out is the whole memory -- which the read then clamps to what stands
  // between the start and the end, so one request serves every form.
  const auto [ranges, lowest, whole] =
      [&]() -> std::tuple<mir::ExprId, mir::ExprId, mir::ExprId> {
    if (memory.declared.has_value()) {
      const MemoryIndices indices = MemoryIndicesOf(
          unit, step_frame,
          std::span<const hir::UnpackedRange>{&*memory.declared, 1},
          named->slice);
      return {
          indices.ranges, indices.lowest,
          mir::BuildMachineCount(unit, body, memory.declared->ElementCount())};
    }
    const MemoryIndices from_zero = MemoryIndicesFromZero(unit, body);
    return {
        from_zero.ranges, from_zero.lowest,
        BuildToInt64Call(
            unit, body,
            body.Add(BuildBuiltinMethodCall(
                unit, body, support::BuiltinFn::kSize,
                body.LocalPlace(loaded, dest_type), {},
                unit.builtins.int_type)))};
  }();
  const mir::TypeId word_type = unit_lowerer.TranslateType(memory.element);
  const mir::TypeId words_type =
      mir::LibraryTypeOf(unit.types, support::LibraryDecl::kMemoryWords);
  const mir::LocalId words = DeclareLocal(
      step_frame,
      body.Add(BuildFileIoCall(
          process, step_frame, support::BuiltinFn::kFileReadWords,
          {*fd, ranges, start.value_or(lowest), count.value_or(whole),
           mir::BuildMachineIntLiteral(
               unit, body,
               static_cast<std::int64_t>(
                   unit.types.Get(word_type).Integral().bit_width))},
          words_type)));

  body.AppendStmt(
      mir::ExprStmt{
          .expr = body.Add(BuildBuiltinMethodCall(
              unit, body, support::BuiltinFn::kLoadWords,
              body.LocalPlace(loaded, dest_type),
              {body.LocalPlace(words, words_type)}, unit.builtins.void_type))});
  auto stored = AppendStores(
      unit_lowerer, step_frame, changed->lvalue,
      body.ReadLocal(loaded, dest_type));
  if (!stored) return std::unexpected(std::move(stored.error()));
  const mir::ExprId bytes = body.Add(BuildBuiltinMethodCall(
      unit, body, support::BuiltinFn::kMemoryWordsBytesRead,
      body.LocalPlace(words, words_type), {}, unit.builtins.machine_int64));
  return steps.Build(body.Add(
      mir::MakeBuiltinCall(
          unit, body.Body(), support::BuiltinFn::kIntegralFromInt, std::nullopt,
          {bytes}, unit.builtins.integer)));
}

// The memory a `$fread` destination of `type` is, which implements a memory's
// side of the task; nothing where it is an associative array, whose keyed load
// is not yet carried out from a file's bytes.
auto FileReadMemoryOf(const hir::Type& type) -> std::optional<FileReadMemory> {
  if (const auto* fixed = type.As<hir::UnpackedArrayType>()) {
    return FileReadMemory{
        .element = fixed->element_type, .declared = fixed->dim};
  }
  if (const auto* dynamic = type.As<hir::DynamicArrayType>()) {
    return FileReadMemory{
        .element = dynamic->element_type, .declared = std::nullopt};
  }
  if (const auto* queue = type.As<hir::QueueType>()) {
    return FileReadMemory{
        .element = queue->element_type, .declared = std::nullopt};
  }
  return std::nullopt;
}

// $fread -- LRM 21.3.4.4. Which destination the source named says which of the
// two requests it is: a packed variable takes the bytes whole, a memory --
// whatever implements a memory's side of a memory task -- takes them word by
// word from an address it is told, and the clause gives the two different
// addressing. The destination crosses in as well as back, because its own
// shape decides how many bytes a word takes and what the file does not reach
// keeps what it held (LRM 13.5).
auto LowerFileReadCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  auto& unit_lowerer = process.Owner();
  auto& unit = unit_lowerer.Unit();
  const auto& hir_proc = process.HirBody();
  const std::vector<hir::ExprId> head = RequiredLeadingOperands(call, 2);
  const hir::Expr& dest_hir = hir_proc.exprs.Get(head[0]);
  const mir::TypeId dest_type = unit_lowerer.TranslateType(dest_hir.type);
  const bool is_memory =
      mir::ImplementsLanguageTrait(unit, dest_type, support::kMemoryWords);

  if (!is_memory) {
    // LRM 21.3.4.4 says start and count "are ignored if $fread is loading an
    // integral variable"; refusing rather than dropping them surfaces the
    // mistake instead of running a call the source did not write.
    if (call.arguments.size() != 2) {
      return std::unexpected(
          diag::Make(
              diag::DiagCode::kUnsupportedSubroutineArgument,
              "a start or a count argument on '$fread' into an integral "
              "variable is not yet supported")
              .WithNote("leave them out, since the standard ignores them"));
    }
  }

  BlockBuilder steps(frame);
  const WalkFrame& step_frame = steps.Frame();
  mir::Builder body = steps.Body();

  if (is_memory) {
    const std::optional<FileReadMemory> memory =
        FileReadMemoryOf(unit_lowerer.Hir().types.Get(dest_hir.type));
    if (!memory.has_value()) {
      return diag::Fail(
          diag::DiagCode::kUnsupportedSubroutineArgument,
          "'$fread' into an associative array is not yet supported");
    }
    // LRM 21.3.4.4.
    if (!unit_lowerer.Hir().types.Get(memory->element).IsIntegral()) {
      return diag::Fail(
          diag::DiagCode::kUnsupportedSubroutineArgument,
          "'$fread' into a memory whose words are not integral is not yet "
          "supported");
    }
    return LowerFileReadMemory(
        process, steps, call, dest_hir, *memory, dest_type);
  }

  auto dest_or = LowerLvalue(process, dest_hir, step_frame);
  if (!dest_or) return std::unexpected(std::move(dest_or.error()));
  auto dest =
      ReadThenWrite(unit_lowerer.Unit(), step_frame, *std::move(dest_or));
  if (!dest) return std::unexpected(std::move(dest.error()));
  auto fd_or = BuildMachineIntOperand(process, step_frame, head[1]);
  if (!fd_or) return std::unexpected(std::move(fd_or.error()));
  const CompletionLayout layout = BuildCompletionLayout(
      {CalleeFormal{
          .direction = hir::ParamDirection::kInOut, .type = dest_type}},
      unit.builtins.int_type);
  const mir::TypeId payload = CompletionPayloadType(unit, layout.components);
  const std::array writebacks{CompletionWriteback{
      .target = std::move(dest->lvalue),
      .component = *layout.formals.front().component,
      .type = dest_type}};
  auto completion = BindCompletion(
      unit_lowerer, step_frame,
      BuildFileIoCall(
          process, step_frame, support::BuiltinFn::kFileRead,
          {dest->incoming, *fd_or}, payload),
      payload, writebacks);
  if (!completion) return std::unexpected(std::move(completion.error()));
  return steps.Build(ReadCount(unit, body, *completion, layout));
}

// $ferror(fd, str) -- LRM 21.3.7.
auto LowerFileErrorCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    std::string_view name, diag::SourceSpan span) -> diag::Result<mir::Expr> {
  const std::vector<hir::ExprId> operands = RequiredOperands(call, 2);
  BlockBuilder steps(frame);

  auto fd_or = BuildMachineIntOperand(process, steps.Frame(), operands[0]);
  if (!fd_or) return std::unexpected(std::move(fd_or.error()));
  const mir::ExprId fd_id = *fd_or;

  const hir::Expr& message_hir = process.HirExprs().Get(operands[1]);
  const mir::TypeId message_type =
      process.Owner().TranslateType(message_hir.type);
  auto valid_or = RequireStringDestination(
      process.Owner().Unit(), message_type, name, span);
  if (!valid_or) return std::unexpected(std::move(valid_or.error()));
  auto message_or = LowerLvalue(process, message_hir, steps.Frame());
  if (!message_or) return std::unexpected(std::move(message_or.error()));

  return BuildTextRead(
      process, steps, support::BuiltinFn::kFileError, {fd_id},
      *std::move(message_or), message_type);
}

}  // namespace

auto LowerFileIOSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    std::string_view name, const support::FileIOSystemSubroutineInfo& info,
    diag::SourceSpan span) -> diag::Result<mir::Expr> {
  const auto& builtins = process.Owner().Unit().builtins;
  switch (info.builtin_fn) {
    case support::BuiltinFn::kFileOpen:
      return LowerFileOpenCall(process, frame, call);
    case support::BuiltinFn::kFileClose:
      return LowerFixedOperandCall(
          process, frame, call, info.builtin_fn, 1, builtins.void_type);
    case support::BuiltinFn::kFileGetc:
      return LowerFixedOperandCall(
          process, frame, call, info.builtin_fn, 1, builtins.int_type);
    case support::BuiltinFn::kFileUngetc:
      return LowerFixedOperandCall(
          process, frame, call, info.builtin_fn, 2, builtins.int_type);
    case support::BuiltinFn::kFileSeek:
      return LowerFixedOperandCall(
          process, frame, call, info.builtin_fn, 3, builtins.int_type);
    case support::BuiltinFn::kFileRewind:
      return LowerFixedOperandCall(
          process, frame, call, info.builtin_fn, 1, builtins.int_type);
    case support::BuiltinFn::kFileTell:
      return LowerFixedOperandCall(
          process, frame, call, info.builtin_fn, 1, builtins.int_type);
    case support::BuiltinFn::kFileEof:
      return LowerFixedOperandCall(
          process, frame, call, info.builtin_fn, 1, builtins.int_type);
    case support::BuiltinFn::kFileFlush:
      return LowerFileFlushCall(process, frame, call);
    case support::BuiltinFn::kFileGets:
      return LowerFileGetsCall(process, frame, call, name, span);
    case support::BuiltinFn::kFileError:
      return LowerFileErrorCall(process, frame, call, name, span);
    case support::BuiltinFn::kFileRead:
      return LowerFileReadCall(process, frame, call);
    default:
      throw InternalError(
          "LowerFileIOSystemSubroutineCall: unexpected file-IO BuiltinFn");
  }
}

}  // namespace lyra::lowering::hir_to_mir

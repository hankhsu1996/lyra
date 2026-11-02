#include "lyra/backend/llvm/runtime_entry.hpp"

#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/backend/llvm/entry_realizations.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/operator.hpp"
#include "lyra/lir/value_step.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/declared_member_storage.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::backend::llvm_backend {

namespace {

auto Symbol(std::string_view operation) -> std::string {
  return std::format("{}{}", kRuntimeSymbolPrefix, operation);
}

auto Symbol(support::ValueDomain domain, std::string_view operation)
    -> std::string {
  return std::format(
      "{}{}_{}", kRuntimeSymbolPrefix, support::ValueDomainName(domain),
      operation);
}

}  // namespace

// This is an interface contract, not a display string: changing a spelling
// renames a linked symbol.
auto RuntimeOpName(RuntimeOp op) -> std::string_view {
  switch (op) {
    case RuntimeOp::kCellRefer:
      return "cell_refer";
    case RuntimeOp::kReferStorage:
      return "refer_storage";
    case RuntimeOp::kRunProgram:
      return "run_program";
    case RuntimeOp::kSequenceMake:
      return "sequence_make";
    case RuntimeOp::kSequenceElement:
      return "sequence_element";
    case RuntimeOp::kClosureBoxEnd:
      return "closure_box_end";
    case RuntimeOp::kObjectAdopt:
      return "object_adopt";
    case RuntimeOp::kSharedCellMake:
      return "shared_cell_make";
    case RuntimeOp::kSharedPointerDeref:
      return "shared_pointer_deref";
    case RuntimeOp::kInterfaceTable:
      return "interface_table";
    case RuntimeOp::kBeginLibraryPart:
      return "begin_library_part";
    case RuntimeOp::kEndLibraryPart:
      return "end_library_part";
    case RuntimeOp::kToBool:
      return "to_bool";
    case RuntimeOp::kDefault:
      return "default";
    case RuntimeOp::kFromElements:
      return "from_elements";
    case RuntimeOp::kSettleDeparture:
      return "settle_departure";
    case RuntimeOp::kStreamWrite:
      return "stream_write";
    case RuntimeOp::kStreamBuild:
      return "stream_build";
    case RuntimeOp::kRunWrite:
      return "run_write";
    case RuntimeOp::kRunBuild:
      return "run_build";
    case RuntimeOp::kInsertBits:
      return "insert_bits";
    case RuntimeOp::kInsertDesignatedBits:
      return "insert_designated_bits";
    case RuntimeOp::kElementDefault:
      return "element_default";
    case RuntimeOp::kDiscardedWrite:
      return "discarded_write";
    case RuntimeOp::kConstruct:
      return "construct";
    case RuntimeOp::kDestroy:
      return "destroy";
    case RuntimeOp::kCopy:
      return "copy";
    case RuntimeOp::kMove:
      return "move";
    case RuntimeOp::kAssign:
      return "assign";
  }
  throw InternalError("llvm codegen: unknown runtime operation");
}

auto WrapperName(WrapperKind wrapper) -> std::string_view {
  switch (wrapper) {
    case WrapperKind::kCell:
      return "cell";
    case WrapperKind::kNet:
      return "net";
    case WrapperKind::kDriver:
      return "driver";
    case WrapperKind::kRef:
      return "ref";
  }
  throw InternalError("llvm codegen: unknown capability wrapper");
}

auto RuntimeOpOf(lir::ValueStep step) -> RuntimeOp {
  switch (step) {
    case lir::ValueStep::kCopy:
      return RuntimeOp::kCopy;
    case lir::ValueStep::kMove:
      return RuntimeOp::kMove;
    case lir::ValueStep::kEnd:
      return RuntimeOp::kDestroy;
    case lir::ValueStep::kAssign:
      return RuntimeOp::kAssign;
  }
  throw InternalError("llvm codegen: unknown value step");
}

auto KeyedElementEntries() -> ElementEntries {
  return {
      .read =
          {.entry = support::BuiltinFn::kAssocElement,
           .symbol = RuntimeSymbol(support::BuiltinFn::kAssocElement)},
      .write = {
          .entry = support::BuiltinFn::kAssocElementMut,
          .symbol = RuntimeSymbol(support::BuiltinFn::kAssocElementMut)}};
}

auto QueueElementEntries() -> ElementEntries {
  return {
      .read =
          {.entry = support::BuiltinFn::kElement,
           .symbol = RuntimeSymbol(
               support::ValueDomain::kQueue, support::BuiltinFn::kElement)},
      .write = {
          .entry = support::BuiltinFn::kQueueElementMut,
          .symbol = RuntimeSymbol(support::BuiltinFn::kQueueElementMut)}};
}

auto PositionedElementEntries(support::ValueDomain domain) -> ElementEntries {
  return {
      .read =
          {.entry = support::BuiltinFn::kElement,
           .symbol = RuntimeSymbol(domain, support::BuiltinFn::kElement)},
      .write = {
          .entry = support::BuiltinFn::kElementMut,
          .symbol = RuntimeSymbol(domain, support::BuiltinFn::kElementMut)}};
}

auto RuntimeSymbol(RuntimeOp op) -> std::string {
  return Symbol(RuntimeOpName(op));
}

auto RuntimeSymbol(support::ValueDomain domain, RuntimeOp op) -> std::string {
  ToldOf(domain, op);
  return Symbol(domain, RuntimeOpName(op));
}

auto ValueTypeSymbol(support::ValueDomain domain) -> std::string {
  if (!StatesValueType(domain)) {
    throw InternalError(
        std::format(
            "llvm codegen: the library states no value type of {} -- please "
            "report this as a bug",
            support::ValueDomainName(domain)));
  }
  return Symbol(domain, "value_type");
}

auto IndexOrderSymbol(support::ValueDomain domain) -> std::string {
  constexpr std::string_view kIndexOrder = "index_order";
  const std::optional<IndexOrder> order = IndexOrderOf(domain);
  if (!order.has_value()) {
    throw InternalError(
        "llvm codegen: an associative array is indexed by values the "
        "language admits as no index -- please report this as a bug");
  }
  switch (*order) {
    case IndexOrder::kIntegral:
      return std::format("{}integral_{}", kRuntimeSymbolPrefix, kIndexOrder);
    case IndexOrder::kOwn:
      return Symbol(domain, kIndexOrder);
  }
  throw InternalError("llvm codegen: unknown index order");
}

auto OperationFunctionsSymbol(support::ValueDomain domain) -> std::string {
  if (!StatesOperationFunctions(domain)) {
    throw InternalError(
        "llvm codegen: the library states no operations of values it holds "
        "this way -- please report this as a bug");
  }
  return Symbol(domain, "operation_functions");
}

auto ValueOperationsSymbol(support::ValueDomain domain) -> std::string {
  if (!StatesValueOperations(domain)) {
    throw InternalError(
        "llvm codegen: the library states no operations of values it holds "
        "this way -- please report this as a bug");
  }
  return Symbol(domain, "value_operations");
}

auto RuntimeSymbol(support::RuntimeObject object, RuntimeOp op) -> std::string {
  return std::format(
      "{}{}_{}", kRuntimeSymbolPrefix, support::RuntimeObjectName(object),
      RuntimeOpName(op));
}

auto RuntimeSymbol(support::DeclaredMemberStorage storage, RuntimeOp op)
    -> std::string {
  const std::string kind_op = std::format(
      "{}_{}", support::MemberStorageName(storage), RuntimeOpName(op));
  const auto over = [&](support::ValueDomain domain) {
    return Symbol(domain, kind_op);
  };
  const auto alone = [&](const auto&) { return Symbol(kind_op); };
  return std::visit(
      Overloaded{
          [&](const support::ObservableCellMember& m) {
            return over(m.domain);
          },
          [&](const support::ResolvedNetMember& m) { return over(m.domain); },
          [&](const support::SampledHistoryMember& m) {
            return over(m.domain);
          },
          [&](const support::ValueCellMember& m) { return over(m.domain); },
          // Storage holding a value inline is that value, so what builds and
          // ends it is what builds and ends a value of its domain.
          [&](const support::InlineValueMember& m) {
            return RuntimeSymbol(m.domain, op);
          },
          [](const support::InlineBytesMember&) -> std::string {
            throw InternalError(
                "llvm codegen: an entry is named for storage generated code "
                "lays out itself, which no entry builds or ends -- please "
                "report this as a bug");
          },
          [&](const support::BorrowedHandleMember& m) { return alone(m); },
          [&](const support::LibraryDeclMember& m) { return alone(m); },
          [&](const support::LibraryObjectMember& m) { return alone(m); }},
      storage);
}

auto RuntimeSymbol(support::ValueDomain domain, lir::BinaryOp op)
    -> std::string {
  return Symbol(domain, lir::BinaryOpName(op));
}

auto RuntimeSymbol(lir::ControlEffectTarget::Op op) -> std::string {
  return Symbol(lir::ControlEffectOpName(op));
}

auto RuntimeSymbol(lir::CoroutineTarget::Op op) -> std::string {
  return Symbol(lir::CoroutineOpName(op));
}

auto RuntimeSymbol(support::ValueDomain domain, lir::ValueCellTarget::Op op)
    -> std::string {
  ToldOf(domain, op);
  return Symbol(domain, lir::ValueCellOpName(op));
}

auto RuntimeSymbol(support::ValueDomain domain, lir::OpenWriteTarget::Op op)
    -> std::string {
  ToldOf(domain, op);
  return Symbol(domain, lir::OpenWriteOpName(op));
}

auto RuntimeSymbol(support::BuiltinFn fn) -> std::string {
  return Symbol(support::RuntimeEntryOf(fn).name);
}

auto RuntimeSymbol(support::ValueDomain domain, support::BuiltinFn fn)
    -> std::string {
  ToldOf(domain, fn);
  return Symbol(domain, support::RuntimeEntryOf(fn).name);
}

auto RuntimeSymbol(
    support::ValueDomain destination, support::BuiltinFn fn,
    support::ValueDomain source) -> std::string {
  const std::string_view name = support::RuntimeEntryOf(fn).name;
  if (!RealizesConversion(fn, destination, source)) {
    throw InternalError(
        std::format(
            "llvm codegen: the library realizes no {} for {} out of {} -- "
            "please report this as a bug",
            name, support::ValueDomainName(destination),
            support::ValueDomainName(source)));
  }
  return Symbol(
      destination,
      std::format("{}_{}", name, support::ValueDomainName(source)));
}

auto RuntimeSymbol(
    support::ValueDomain domain, WrapperKind wrapper, support::BuiltinFn fn)
    -> std::string {
  ToldOf(domain, wrapper, fn);
  return Symbol(
      domain,
      std::format(
          "{}_{}", WrapperName(wrapper), support::RuntimeEntryOf(fn).name));
}

auto OperandReadingsOf(RuntimeOp op) -> support::OperandReadings {
  using enum support::OperandReading;
  switch (op) {
    // The function answering a value's stream, then that value, which crosses
    // with how wide its stream is and whether the stream holds x or z.
    case RuntimeOp::kStreamWrite:
      return {kMachine, kBits};
    // The function answering a value's run of bits, then that value.
    case RuntimeOp::kRunWrite:
      return {kMachine, kMachine};
    // The value written into, the parts the bits lie in, where the bits
    // start, and the bits.
    case RuntimeOp::kInsertBits:
      return {kBits, kMachine, kPosition, kBits};
    // The designation, the parts the bits lie in, where the bits start, and
    // the bits.
    case RuntimeOp::kInsertDesignatedBits:
      return {kMachine, kMachine, kPosition, kBits};
    case RuntimeOp::kCellRefer:
    case RuntimeOp::kReferStorage:
    case RuntimeOp::kRunProgram:
    case RuntimeOp::kSequenceMake:
    case RuntimeOp::kSequenceElement:
    case RuntimeOp::kClosureBoxEnd:
    case RuntimeOp::kObjectAdopt:
    case RuntimeOp::kSharedCellMake:
    case RuntimeOp::kSharedPointerDeref:
    case RuntimeOp::kInterfaceTable:
    case RuntimeOp::kBeginLibraryPart:
    case RuntimeOp::kEndLibraryPart:
    case RuntimeOp::kToBool:
    case RuntimeOp::kDefault:
    case RuntimeOp::kSettleDeparture:
    case RuntimeOp::kStreamBuild:
    case RuntimeOp::kRunBuild:
    case RuntimeOp::kFromElements:
    case RuntimeOp::kElementDefault:
    case RuntimeOp::kDiscardedWrite:
    case RuntimeOp::kConstruct:
    case RuntimeOp::kDestroy:
    case RuntimeOp::kCopy:
    case RuntimeOp::kMove:
    case RuntimeOp::kAssign:
      return {};
  }
  throw InternalError("llvm codegen: unknown runtime operation");
}

auto AnswerToldOf(RuntimeOp op) -> support::AnswerTold {
  switch (op) {
    // The integral value a stream is read into is laid out at a width and
    // with the states no operand fixes (LRM 6.24.3).
    case RuntimeOp::kStreamBuild:
      return support::AnswerTold::kIntegralExtent;
    // A position naming no element is answered for out of the element type,
    // which no array in hand states.
    case RuntimeOp::kElementDefault:
    case RuntimeOp::kDiscardedWrite:
      return support::AnswerTold::kElementType;
    case RuntimeOp::kCellRefer:
    case RuntimeOp::kReferStorage:
    case RuntimeOp::kRunProgram:
    case RuntimeOp::kSequenceMake:
    case RuntimeOp::kSequenceElement:
    case RuntimeOp::kClosureBoxEnd:
    case RuntimeOp::kObjectAdopt:
    case RuntimeOp::kSharedCellMake:
    case RuntimeOp::kSharedPointerDeref:
    case RuntimeOp::kInterfaceTable:
    case RuntimeOp::kBeginLibraryPart:
    case RuntimeOp::kEndLibraryPart:
    case RuntimeOp::kToBool:
    case RuntimeOp::kDefault:
    case RuntimeOp::kSettleDeparture:
    case RuntimeOp::kStreamWrite:
    case RuntimeOp::kRunWrite:
    case RuntimeOp::kRunBuild:
    case RuntimeOp::kInsertBits:
    case RuntimeOp::kInsertDesignatedBits:
    case RuntimeOp::kFromElements:
    case RuntimeOp::kConstruct:
    case RuntimeOp::kDestroy:
    case RuntimeOp::kCopy:
    case RuntimeOp::kMove:
    case RuntimeOp::kAssign:
      return support::AnswerTold::kNothing;
  }
  throw InternalError("llvm codegen: unknown runtime operation");
}

auto OperandReadingsOf(lir::ValueCellTarget::Op op)
    -> support::OperandReadings {
  using enum support::OperandReading;
  switch (op) {
    case lir::ValueCellTarget::Op::kAllocate:
    case lir::ValueCellTarget::Op::kLoad:
      return {};
    // The cell, then the value it is to hold.
    case lir::ValueCellTarget::Op::kStore:
      return {kMachine, kHeld};
  }
  throw InternalError("llvm codegen: unknown value cell operation");
}

auto OperandReadingsOf(lir::OpenWriteTarget::Op op)
    -> support::OperandReadings {
  switch (op) {
    case lir::OpenWriteTarget::Op::kLand:
      return {};
    // A run of elements written within a write takes what the container's own
    // slice write takes, the designation standing for the container.
    case lir::OpenWriteTarget::Op::kAssignElementSlice:
      return support::RuntimeEntryOf(support::BuiltinFn::kAssignElementSlice)
          .operands;
    case lir::OpenWriteTarget::Op::kAssignQueueSlice:
      return support::RuntimeEntryOf(support::BuiltinFn::kAssignQueueSlice)
          .operands;
  }
  throw InternalError("llvm codegen: unknown open-write operation");
}

}  // namespace lyra::backend::llvm_backend

#include "lyra/backend/llvm/constant_record.hpp"

#include <bit>
#include <cstdint>
#include <format>
#include <type_traits>
#include <vector>

#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/runtime/class_definition.hpp"
#include "lyra/runtime/scope_info.hpp"

namespace lyra::backend::llvm_backend {

namespace {

// A field's size where its type is one a constant record holds: an address of
// the host, an integer of whole bytes, or a record or an array of them built
// the same way.
auto TypeSize(const llvm::Type* type) -> std::uint64_t {
  if (type->isPointerTy()) {
    return sizeof(void*);
  }
  if (const auto* integer = llvm::dyn_cast<llvm::IntegerType>(type)) {
    if (integer->getBitWidth() % 8 != 0) {
      throw InternalError(
          "llvm codegen: a constant record holds an integer of no whole "
          "number of bytes -- please report this as a bug");
    }
    return integer->getBitWidth() / 8;
  }
  if (const auto* array = llvm::dyn_cast<llvm::ArrayType>(type)) {
    return array->getNumElements() * TypeSize(array->getElementType());
  }
  if (const auto* record = llvm::dyn_cast<llvm::StructType>(type);
      record != nullptr && record->isPacked()) {
    std::uint64_t size = 0;
    for (const llvm::Type* element : record->elements()) {
      size += TypeSize(element);
    }
    return size;
  }
  throw InternalError(
      "llvm codegen: a constant record holds a field whose size depends on "
      "LLVM's own layout -- please report this as a bug");
}

// A structure's fields are read off the structure: a structured binding takes
// them in declaration order, each binding's address says where the field is,
// and its type says what it holds. So the structure's own declaration is the
// only statement of its fields, and a binding list of the wrong length does not
// compile.

template <typename Record, typename Field>
auto OffsetIn(const Record& record, const Field& field) -> std::uint64_t {
  return std::bit_cast<std::uintptr_t>(&field) -
         std::bit_cast<std::uintptr_t>(&record);
}

template <typename Field>
void AppendField(
    std::vector<FieldLayout>& out, std::uint64_t at, const Field& field);

template <typename Record, typename... Fields>
void AppendFieldsOf(
    std::vector<FieldLayout>& out, std::uint64_t at, const Record& record,
    const Fields&... fields) {
  (AppendField(out, at + OffsetIn(record, fields), fields), ...);
}

template <typename T>
void AppendRecord(
    std::vector<FieldLayout>& out, std::uint64_t at,
    const runtime::ConstantSpan<T>& record) {
  const auto& [data, size] = record;
  AppendFieldsOf(out, at, record, data, size);
}

void AppendRecord(
    std::vector<FieldLayout>& out, std::uint64_t at,
    const runtime::ScopeMetadata& record) {
  const auto& [time_unit_power, time_precision_power] = record;
  AppendFieldsOf(out, at, record, time_unit_power, time_precision_power);
}

void AppendRecord(
    std::vector<FieldLayout>& out, std::uint64_t at,
    const runtime::ScopeCallable& record) {
  const auto& [name, entry] = record;
  AppendFieldsOf(out, at, record, name, entry);
}

void AppendRecord(
    std::vector<FieldLayout>& out, std::uint64_t at,
    const runtime::ScopeInfo& record) {
  const auto& [metadata, exports] = record;
  AppendFieldsOf(out, at, record, metadata, exports);
}

void AppendRecord(
    std::vector<FieldLayout>& out, std::uint64_t at,
    const runtime::ObjectDefinition& record) {
  const auto& [base, scope] = record;
  AppendFieldsOf(out, at, record, base, scope);
}

// A field that is itself a structure stands for its own fields.
template <typename Field>
void AppendField(
    std::vector<FieldLayout>& out, std::uint64_t at, const Field& field) {
  if constexpr (std::is_integral_v<Field>) {
    out.push_back(
        {.offset = at, .kind = FieldKind::kInteger, .size = sizeof(Field)});
  } else if constexpr (std::is_pointer_v<Field>) {
    out.push_back(
        {.offset = at, .kind = FieldKind::kPointer, .size = sizeof(void*)});
  } else {
    AppendRecord(out, at, field);
  }
}

template <typename Record>
auto LayoutOf() -> LibraryRecordLayout {
  const Record record{};
  LibraryRecordLayout layout{
      .size = sizeof(Record), .alignment = alignof(Record), .fields = {}};
  AppendRecord(layout.fields, 0, record);
  return layout;
}

}  // namespace

auto LayoutOfLibraryRecord(lir::RuntimeLibraryKind kind)
    -> LibraryRecordLayout {
  switch (kind) {
    case lir::RuntimeLibraryKind::kObjectDefinition:
      return LayoutOf<runtime::ObjectDefinition>();
    case lir::RuntimeLibraryKind::kScopeInfo:
      return LayoutOf<runtime::ScopeInfo>();
    case lir::RuntimeLibraryKind::kScopeCallable:
      return LayoutOf<runtime::ScopeCallable>();
    case lir::RuntimeLibraryKind::kPackedType:
    case lir::RuntimeLibraryKind::kPackedRange:
    case lir::RuntimeLibraryKind::kUnpackedRange:
    case lir::RuntimeLibraryKind::kEnumeration:
    case lir::RuntimeLibraryKind::kPrintItem:
    case lir::RuntimeLibraryKind::kPrintLiteralItem:
    case lir::RuntimeLibraryKind::kPrintValueItem:
    case lir::RuntimeLibraryKind::kFormatSpec:
    case lir::RuntimeLibraryKind::kFormatArg:
    case lir::RuntimeLibraryKind::kChannelCancellation:
    case lir::RuntimeLibraryKind::kTimeFormat:
    case lir::RuntimeLibraryKind::kHierarchySegment:
    case lir::RuntimeLibraryKind::kDpiBitBuffer:
    case lir::RuntimeLibraryKind::kDpiLogicBuffer:
    case lir::RuntimeLibraryKind::kDpiBitChunk:
    case lir::RuntimeLibraryKind::kDpiLogicChunk:
    case lir::RuntimeLibraryKind::kDpiOpenArray:
    case lir::RuntimeLibraryKind::kDpiOpenArrayHandle:
    case lir::RuntimeLibraryKind::kTrigger:
    case lir::RuntimeLibraryKind::kObservation:
    case lir::RuntimeLibraryKind::kReadReport:
    case lir::RuntimeLibraryKind::kWait:
    case lir::RuntimeLibraryKind::kObjectWrite:
    case lir::RuntimeLibraryKind::kCancellationTarget:
    case lir::RuntimeLibraryKind::kControlEffect:
      break;
  }
  throw InternalError(
      "llvm codegen: a constant is stated as a library structure no constant "
      "is made of -- please report this as a bug");
}

ConstantRecord::ConstantRecord(llvm::LLVMContext& context, std::uint64_t size)
    : context_(&context), size_(size) {
}

void ConstantRecord::PadTo(std::uint64_t offset) {
  if (offset > end_) {
    fields_.push_back(
        llvm::ConstantAggregateZero::get(
            llvm::ArrayType::get(
                llvm::Type::getInt8Ty(*context_), offset - end_)));
    end_ = offset;
  }
}

void ConstantRecord::Place(std::uint64_t offset, llvm::Constant* value) {
  if (offset < end_) {
    throw InternalError(
        std::format(
            "llvm codegen: a constant record places a field at {} inside the "
            "one ending at {} -- please report this as a bug",
            offset, end_));
  }
  PadTo(offset);
  fields_.push_back(value);
  end_ += TypeSize(value->getType());
}

auto ConstantRecord::Build() && -> llvm::Constant* {
  if (end_ > size_) {
    throw InternalError(
        std::format(
            "llvm codegen: a constant record of {} bytes holds fields ending "
            "at {} -- please report this as a bug",
            size_, end_));
  }
  PadTo(size_);
  return llvm::ConstantStruct::getAnon(*context_, fields_, true);
}

}  // namespace lyra::backend::llvm_backend

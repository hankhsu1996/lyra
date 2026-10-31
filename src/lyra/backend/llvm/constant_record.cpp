#include "lyra/backend/llvm/constant_record.hpp"

#include <bit>
#include <cstddef>
#include <cstdint>
#include <format>
#include <type_traits>
#include <vector>

#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "lyra/abi/records.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/value/value_type.hpp"

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

// A structure's fields are read off the structure, as the library declares
// them: each field's address says where it lies, and its type what it holds.
template <typename Record>
auto LayoutOf() -> LibraryRecordLayout {
  const Record record{};
  LibraryRecordLayout layout{
      .layout = {.size = sizeof(Record), .align = alignof(Record)},
      .fields = {}};
  abi::ForEachField(record, [&]<typename Field>(const Field& field) {
    const std::uint64_t offset = std::bit_cast<std::uintptr_t>(&field) -
                                 std::bit_cast<std::uintptr_t>(&record);
    if constexpr (std::is_integral_v<Field>) {
      layout.fields.push_back(
          {.offset = offset, .kind = IntegerField{.size = sizeof(Field)}});
    } else if constexpr (std::is_pointer_v<Field>) {
      layout.fields.push_back({.offset = offset, .kind = PointerField{}});
    } else if constexpr (abi::kIsConstantSpan<Field>) {
      static_assert(
          offsetof(Field, data) == 0 && sizeof(Field::size) == 8,
          "a view of entries is laid out as other than where the first is "
          "and then a count of eight bytes");
      layout.fields.push_back(
          {.offset = offset,
           .kind = SpanField{.count_at = offsetof(Field, size)}});
    } else {
      layout.fields.push_back({.offset = offset, .kind = RecordField{}});
    }
  });
  return layout;
}

}  // namespace

auto LayoutOfLibraryRecord(lir::RuntimeLibraryKind kind)
    -> LibraryRecordLayout {
  switch (kind) {
    case lir::RuntimeLibraryKind::kObjectDefinition:
      return LayoutOf<abi::ObjectDefinition>();
    case lir::RuntimeLibraryKind::kScopeInfo:
      return LayoutOf<abi::ScopeInfo>();
    case lir::RuntimeLibraryKind::kScopeCallable:
      return LayoutOf<abi::ScopeCallable>();
    case lir::RuntimeLibraryKind::kClassObjectInfo:
      return LayoutOf<abi::ClassObjectInfo>();
    case lir::RuntimeLibraryKind::kInterfaceRow:
      return LayoutOf<abi::InterfaceRow>();
    case lir::RuntimeLibraryKind::kScopeMetadata:
      return LayoutOf<abi::ScopeMetadata>();
    case lir::RuntimeLibraryKind::kConstructionTable:
      return LayoutOf<abi::ConstructionTable>();
    case lir::RuntimeLibraryKind::kInstantiation:
      return LayoutOf<abi::Instantiation>();
    case lir::RuntimeLibraryKind::kGeneratePlaces:
      return LayoutOf<abi::GeneratePlaces>();
    case lir::RuntimeLibraryKind::kDumpEntry:
      return LayoutOf<abi::DumpEntry>();
    case lir::RuntimeLibraryKind::kDumpBlock:
      return LayoutOf<abi::DumpBlock>();
    case lir::RuntimeLibraryKind::kBitVec:
    case lir::RuntimeLibraryKind::kEnumeration:
    case lir::RuntimeLibraryKind::kPrintItem:
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
    case lir::RuntimeLibraryKind::kWatchedPlace:
    case lir::RuntimeLibraryKind::kTrigger:
    case lir::RuntimeLibraryKind::kObservation:
    case lir::RuntimeLibraryKind::kReadReport:
    case lir::RuntimeLibraryKind::kWait:
    case lir::RuntimeLibraryKind::kLeasedReference:
    case lir::RuntimeLibraryKind::kObjectWrite:
    case lir::RuntimeLibraryKind::kCancellationTarget:
    case lir::RuntimeLibraryKind::kTimer:
    case lir::RuntimeLibraryKind::kDeferredEffects:
    case lir::RuntimeLibraryKind::kViolationReportKeeper:
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

void ConstantRecord::PlaceInteger(value::StatedAt at, std::uint64_t value) {
  Place(
      at.offset,
      llvm::ConstantInt::get(
          llvm::Type::getIntNTy(*context_, static_cast<unsigned>(8 * at.bytes)),
          value));
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

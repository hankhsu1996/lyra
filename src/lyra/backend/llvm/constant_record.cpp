#include "lyra/backend/llvm/constant_record.hpp"

#include <bit>
#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <type_traits>
#include <utility>
#include <vector>

#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "lyra/abi/library_records.hpp"
#include "lyra/abi/records.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/support/library_decl.hpp"
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

auto LayoutOfLibraryRecord(support::LibraryDecl decl) -> LibraryRecordLayout {
  std::optional<LibraryRecordLayout> layout;
  abi::ForEachLibraryRecord([&]<typename Record>(support::LibraryDecl listed) {
    if (listed == decl) {
      layout = LayoutOf<Record>();
    }
  });
  if (!layout.has_value()) {
    throw InternalError(
        "llvm codegen: a constant is stated as a library structure no "
        "constant is made of -- please report this as a bug");
  }
  return *std::move(layout);
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

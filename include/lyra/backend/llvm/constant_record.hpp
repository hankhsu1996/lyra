#pragma once

#include <cstdint>
#include <vector>

#include "lyra/lir/type.hpp"

namespace llvm {
class Constant;
class LLVMContext;
}  // namespace llvm

namespace lyra::backend::llvm_backend {

// The type of one field of a library structure a constant is stated as, as far
// as emitting data needs it: an integer of some bytes, or a pointer to data or
// to code.
enum class FieldKind : std::uint8_t { kInteger, kPointer };

struct FieldLayout {
  std::uint64_t offset = 0;
  FieldKind kind = FieldKind::kPointer;
  std::uint64_t size = 0;
};

// The record layout of one structure of the runtime library, as the host's C++
// compiler computed it: its size and alignment, and the offset and type of each
// field in declaration order, a nested structure's fields standing where it
// does. It is what a C++ compiler holds for a class it has seen the definition
// of; this compiler reads no C++, so it is taken from the structure itself
// where this compiler is built against the library.
struct LibraryRecordLayout {
  std::uint64_t size = 0;
  std::uint64_t alignment = 1;
  std::vector<FieldLayout> fields;
};

// Throws for a kind no constant is stated as.
[[nodiscard]] auto LayoutOfLibraryRecord(lir::RuntimeLibraryKind kind)
    -> LibraryRecordLayout;

// A constant the runtime library reads as one of its own structures, laid out
// where the host's C++ compiler laid that structure out: each field at the
// offset the compiler gave the member, and the gaps between them, and after
// the last, filled with zero bytes. The offsets and the size come from the
// structure itself, so the two sides agree because they read one declaration,
// and the constant is packed so that no layout rule of LLVM's own moves a
// field.
class ConstantRecord {
 public:
  ConstantRecord(llvm::LLVMContext& context, std::uint64_t size);

  // Places `value` at `offset`, which lies at or past where the field placed
  // before it ends.
  void Place(std::uint64_t offset, llvm::Constant* value);

  [[nodiscard]] auto Build() && -> llvm::Constant*;

 private:
  void PadTo(std::uint64_t offset);

  llvm::LLVMContext* context_;
  std::uint64_t size_;
  std::uint64_t end_ = 0;
  std::vector<llvm::Constant*> fields_;
};

}  // namespace lyra::backend::llvm_backend

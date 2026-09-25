#pragma once

#include <cstdint>
#include <unordered_map>
#include <vector>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "lyra/lir/type_id.hpp"
#include "lyra/support/runtime_object.hpp"

namespace lyra::lir {
struct CompilationUnit;
}  // namespace lyra::lir

namespace lyra::backend::llvm_backend {

// Where each component of a product value sits in the storage the value
// occupies. A product is laid out the way a C record is: each component at the
// first offset its own alignment allows after the one before, and the whole
// rounded to the widest alignment among them, so a component's position follows
// from the components ahead of it and nothing else.
struct ProductLayout {
  std::vector<lir::TypeId> components;
  std::vector<std::uint32_t> offsets;
  support::ObjectLayout storage;
};

// Maps a LIR type to the LLVM type a value of it is held in. A machine type
// names itself, because a target's own type system already spells what the
// target computes with; every other type answers with an address, because what
// it names lives in storage rather than in a register.
class CodeGenTypes {
 public:
  CodeGenTypes(llvm::LLVMContext& ctx, const lir::CompilationUnit& unit);

  auto Map(lir::TypeId id) -> llvm::Type*;

  // The storage a value of an owned type occupies. A product's is composed from
  // its components'; every other owned value is one runtime object, whose
  // storage the library states.
  auto StorageOf(lir::TypeId type) -> support::ObjectLayout;
  // Where each component of a product sits. Asked only of a product.
  auto LayoutOfProduct(lir::TypeId product) -> const ProductLayout&;

  auto Ptr() const -> llvm::PointerType* {
    return ptr_ty_;
  }
  // A contiguous run of values named by its first-element pointer and length,
  // which is the shape a run of them crosses the runtime boundary in.
  auto Span() const -> llvm::StructType* {
    return span_ty_;
  }
  auto Void() const -> llvm::Type*;

 private:
  llvm::LLVMContext* ctx_;
  const lir::CompilationUnit* unit_;
  llvm::PointerType* ptr_ty_;
  llvm::StructType* span_ty_;
  std::unordered_map<lir::TypeId, llvm::Type*> cache_;
  std::unordered_map<lir::TypeId, ProductLayout> products_;
};

}  // namespace lyra::backend::llvm_backend

#include "lyra/backend/llvm/codegen_types.hpp"

#include <llvm/IR/LLVMContext.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type.hpp"

namespace lyra::backend::llvm_backend {

CodeGenTypes::CodeGenTypes(
    llvm::LLVMContext& ctx, const lir::CompilationUnit& unit)
    : ctx_(&ctx),
      unit_(&unit),
      ptr_ty_(llvm::PointerType::getUnqual(ctx)),
      span_ty_(
          llvm::StructType::get(ctx, {ptr_ty_, llvm::Type::getInt64Ty(ctx)})) {
}

auto CodeGenTypes::Void() const -> llvm::Type* {
  return llvm::Type::getVoidTy(*ctx_);
}

auto CodeGenTypes::Map(lir::TypeId id) -> llvm::Type* {
  if (auto it = cache_.find(id); it != cache_.end()) {
    return it->second;
  }
  // An address, which is the whole of what a machine register holds for such a
  // type. Three kinds of type answer this way, and the arms below are grouped
  // by which: a value the runtime realizes as an object of its own, storage
  // whose every operation consumes where it lives, and a value that already is
  // an address.
  const auto address = [&](const auto&) -> llvm::Type* { return ptr_ty_; };
  llvm::Type* mapped = unit_->types.Get(id).Visit(
      Overloaded{
          // The machine types, which name themselves: what a target's own type
          // system spells is what the target computes with.
          [&](const lir::VoidType&) -> llvm::Type* { return Void(); },
          [&](const lir::MachineBoolType&) -> llvm::Type* {
            return llvm::Type::getInt1Ty(*ctx_);
          },
          [&](const lir::MachineIntType& m) -> llvm::Type* {
            return llvm::IntegerType::get(*ctx_, lir::BitsOf(m.width));
          },
          [&](const lir::MachineFloatType& m) -> llvm::Type* {
            switch (m.width) {
              case lir::MachineFloatWidth::k32:
                return llvm::Type::getFloatTy(*ctx_);
              case lir::MachineFloatWidth::k64:
                return llvm::Type::getDoubleTy(*ctx_);
            }
            throw InternalError("llvm codegen: unknown MachineFloatWidth");
          },
          [&](const lir::MachineArrayType& m) -> llvm::Type* {
            return llvm::ArrayType::get(Map(m.element), m.size);
          },
          [&](const lir::MachineCStringType& t) { return address(t); },

          // A simulation value the runtime realizes as an object it owns. Every
          // operation on one is a library call handed where the object lives,
          // so generated code never holds the object's shape and would have
          // nothing to do with it if it did.
          [&](const lir::PackedArrayType& t) { return address(t); },
          [&](const lir::EnumType& t) { return address(t); },
          [&](const lir::UnpackedArrayType& t) { return address(t); },
          [&](const lir::DynamicArrayType& t) { return address(t); },
          [&](const lir::QueueType& t) { return address(t); },
          [&](const lir::AssociativeArrayType& t) { return address(t); },
          [&](const lir::StringType& t) { return address(t); },
          [&](const lir::RealType& t) { return address(t); },
          [&](const lir::ShortRealType& t) { return address(t); },
          [&](const lir::RealTimeType& t) { return address(t); },
          [&](const lir::TupleType& t) { return address(t); },
          [&](const lir::UnionType& t) { return address(t); },
          [&](const lir::TaggedUnionType& t) { return address(t); },
          [&](const lir::EmptyType& t) { return address(t); },
          [&](const lir::EventType& t) { return address(t); },
          // A wildcard index (LRM 7.8.1) is the one type this runtime realizes
          // no value of. Naming a machine type for it settles nothing, because
          // what refuses is the operation reached over such a value; this
          // answer only has to exist for that refusal to be the one a reader
          // meets.
          [&](const lir::WildcardIndexType& t) { return address(t); },

          // Storage, whose operations consume where it lives rather than what
          // it holds: a node of the object tree, a record the compiler
          // generated, and the cells a declaration installs over a value.
          [&](const lir::ObjectType& t) { return address(t); },
          [&](const lir::ExternalUnitObjectType& t) { return address(t); },
          [&](const lir::CrossUnitClassType& t) { return address(t); },
          [&](const lir::RuntimeClassType& t) { return address(t); },
          [&](const lir::StructType& t) { return address(t); },
          [&](const lir::ClosureType& t) { return address(t); },
          [&](const lir::ObservableType& t) { return address(t); },
          [&](const lir::ResolvedType& t) { return address(t); },
          [&](const lir::SampledHistoryType& t) { return address(t); },
          [&](const lir::EvaluationAttemptsType& t) { return address(t); },
          [&](const lir::RuntimeEffectsType& t) { return address(t); },
          [&](const lir::FilesType& t) { return address(t); },
          [&](const lir::DiagnosticType& t) { return address(t); },
          [&](const lir::RuntimeLibraryType& t) { return address(t); },

          // A value that already is an address: a handle onto storage of some
          // other lifetime, an execution's own frame, or -- for a chandle (LRM
          // 6.14) and a class handle (LRM 8.3) -- a value whose whole content
          // is what it refers to.
          [&](const lir::RefType& t) { return address(t); },
          [&](const lir::PointerType& t) { return address(t); },
          [&](const lir::ManagedRefType& t) { return address(t); },
          [&](const lir::ChandleType& t) { return address(t); },
          [&](const lir::DriverType& t) { return address(t); },
          [&](const lir::VectorType& t) { return address(t); },
          [&](const lir::CoroutineType& t) { return address(t); },
      });
  cache_.emplace(id, mapped);
  return mapped;
}

}  // namespace lyra::backend::llvm_backend

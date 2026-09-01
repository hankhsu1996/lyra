#include <cstdint>
#include <format>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_builders.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::mir_to_lir {

namespace {

auto TranslateStateKind(mir::IntegralStateKind s) -> lir::IntegralStateKind {
  switch (s) {
    case mir::IntegralStateKind::kTwoState:
      return lir::IntegralStateKind::kTwoState;
    case mir::IntegralStateKind::kFourState:
      return lir::IntegralStateKind::kFourState;
  }
  throw InternalError("TranslateStateKind: unknown IntegralStateKind");
}

auto TranslateSignedness(mir::Signedness s) -> lir::Signedness {
  return s == mir::Signedness::kSigned ? lir::Signedness::kSigned
                                       : lir::Signedness::kUnsigned;
}

auto TranslateNetResolution(mir::NetResolution r) -> lir::NetResolution {
  switch (r) {
    case mir::NetResolution::kTriState:
      return lir::NetResolution::kTriState;
  }
  throw InternalError("TranslateNetResolution: unknown NetResolution");
}

auto TranslatePointerOwnership(mir::PointerOwnership o)
    -> lir::PointerOwnership {
  switch (o) {
    case mir::PointerOwnership::kUnique:
      return lir::PointerOwnership::kUnique;
    case mir::PointerOwnership::kShared:
      return lir::PointerOwnership::kShared;
    case mir::PointerOwnership::kBorrowed:
      return lir::PointerOwnership::kBorrowed;
  }
  throw InternalError("TranslatePointerOwnership: unknown PointerOwnership");
}

auto TranslateMutability(mir::Mutability m) -> lir::Mutability {
  return m == mir::Mutability::kReadOnly ? lir::Mutability::kReadOnly
                                         : lir::Mutability::kMutable;
}

auto TranslatePackedArray(const mir::PackedArrayType& pa)
    -> lir::PackedArrayType {
  std::vector<lir::PackedRange> dims;
  dims.reserve(pa.dims.size());
  for (const mir::PackedRange& d : pa.dims) {
    dims.push_back(lir::PackedRange{.left = d.left, .right = d.right});
  }
  return lir::PackedArrayType{
      .state_kind = TranslateStateKind(pa.state_kind),
      .signedness = TranslateSignedness(pa.signedness),
      .dims = std::move(dims)};
}

}  // namespace

auto UnitLowerer::TranslateType(mir::TypeId id) -> lir::TypeId {
  if (const auto it = type_memo_.find(id); it != type_memo_.end()) {
    return it->second;
  }
  const lir::TypeId lir_id =
      out_.types.Intern(TranslateType(mir_->types.Get(id)));
  type_memo_.emplace(id, lir_id);
  return lir_id;
}

auto UnitLowerer::TranslateType(const mir::Type& ty) -> lir::Type {
  return ty.Visit(
      Overloaded{
          [&](const mir::PackedArrayType& pa) -> lir::Type {
            return lir::Type{TranslatePackedArray(pa)};
          },
          [&](const mir::EnumType& e) -> lir::Type {
            std::vector<lir::EnumMember> members;
            members.reserve(e.members.size());
            for (const mir::EnumMember& m : e.members) {
              members.push_back(
                  lir::EnumMember{.name = m.name, .value = m.value});
            }
            return lir::Type{lir::EnumType{
                .base = TranslatePackedArray(e.base),
                .members = std::move(members)}};
          },
          [&](const mir::UnpackedArrayType& ua) -> lir::Type {
            return lir::Type{lir::UnpackedArrayType{
                .element_type = TranslateType(ua.element_type),
                .size = ua.Size()}};
          },
          [&](const mir::DynamicArrayType& da) -> lir::Type {
            return lir::Type{lir::DynamicArrayType{
                .element_type = TranslateType(da.element_type)}};
          },
          [&](const mir::QueueType& q) -> lir::Type {
            return lir::Type{lir::QueueType{
                .element_type = TranslateType(q.element_type),
                .max_bound = q.max_bound}};
          },
          [&](const mir::AssociativeArrayType& aa) -> lir::Type {
            return lir::Type{lir::AssociativeArrayType{
                .element_type = TranslateType(aa.element_type),
                .key_type = TranslateType(aa.key_type)}};
          },
          [](const mir::WildcardIndexType&) -> lir::Type {
            return lir::Type{lir::WildcardIndexType{}};
          },
          [](const mir::StringType&) -> lir::Type {
            return lir::Type{lir::StringType{}};
          },
          [](const mir::MachineCStringType&) -> lir::Type {
            return lir::Type{lir::MachineCStringType{}};
          },
          [](const mir::MachineBoolType&) -> lir::Type {
            return lir::Type{lir::MachineBoolType{}};
          },
          [](const mir::MachineIntType& mi) -> lir::Type {
            return lir::Type{lir::MachineIntType{
                .bit_width = mi.bit_width,
                .signedness = TranslateSignedness(mi.signedness)}};
          },
          [](const mir::MachineFloatType& mf) -> lir::Type {
            return lir::Type{lir::MachineFloatType{.bit_width = mf.bit_width}};
          },
          [&](const mir::MachineArrayType& ma) -> lir::Type {
            return lir::Type{lir::MachineArrayType{
                .element = TranslateType(ma.element), .size = ma.size}};
          },
          [&](const mir::MachineFunctionType&) -> lir::Type {
            // A code address is named only in a unit's definition constant,
            // which the backend consumes directly rather than through
            // MIR-to-LIR.
            throw InternalError(
                "TranslateType: a machine function type does not flow "
                "through MIR-to-LIR");
          },
          [](const mir::EventType&) -> lir::Type {
            return lir::Type{lir::EventType{}};
          },
          [](const mir::RealType&) -> lir::Type {
            return lir::Type{lir::RealType{}};
          },
          [](const mir::ShortRealType&) -> lir::Type {
            return lir::Type{lir::ShortRealType{}};
          },
          [](const mir::RealTimeType&) -> lir::Type {
            return lir::Type{lir::RealTimeType{}};
          },
          [](const mir::ChandleType&) -> lir::Type {
            return lir::Type{lir::ChandleType{}};
          },
          [](const mir::VoidType&) -> lir::Type {
            return lir::Type{lir::VoidType{}};
          },
          [](const mir::EmptyType&) -> lir::Type {
            return lir::Type{lir::EmptyType{}};
          },
          [&](const mir::ObjectType& ob) -> lir::Type {
            return lir::Type{lir::ObjectType{
                .class_id = class_identities_.Get(ob.class_id).lir_class}};
          },
          [&](const mir::ExternalUnitObjectType& eu) -> lir::Type {
            return lir::Type{lir::ExternalUnitObjectType{
                .object = external_unit_object_identities_.Get(eu.object)}};
          },
          [](const mir::RuntimeClassType& e) -> lir::Type {
            return lir::Type{lir::RuntimeClassType{.symbol = e.symbol}};
          },
          [](const mir::CrossUnitClassType& e) -> lir::Type {
            return lir::Type{lir::CrossUnitClassType{
                .unit_name = e.unit_name, .class_name = e.class_name}};
          },
          [](const mir::RuntimeEffectsType&) -> lir::Type {
            return lir::Type{lir::RuntimeEffectsType{}};
          },
          [](const mir::FilesType&) -> lir::Type {
            return lir::Type{lir::FilesType{}};
          },
          [](const mir::DiagnosticType&) -> lir::Type {
            return lir::Type{lir::DiagnosticType{}};
          },
          [&](const mir::RuntimeLibraryType& rl) -> lir::Type {
            return TranslateRuntimeLibrary(rl.kind);
          },
          [&](const mir::CoroutineType& co) -> lir::Type {
            return lir::Type{
                lir::CoroutineType{.payload = TranslateType(co.payload)}};
          },
          [&](const mir::RefType& r) -> lir::Type {
            return out_.types.Get(
                lir::ReferenceToCellOf(
                    out_.types, TranslateType(r.pointee),
                    TranslateMutability(r.mutability)));
          },
          [&](const mir::PointerType& pt) -> lir::Type {
            return lir::Type{lir::PointerType{
                .pointee = TranslateType(pt.pointee),
                .ownership = TranslatePointerOwnership(pt.ownership),
                .mutability = TranslateMutability(pt.mutability)}};
          },
          [&](const mir::ManagedRefType& mr) -> lir::Type {
            return lir::Type{
                lir::ManagedRefType{.pointee = TranslateType(mr.pointee)}};
          },
          [&](const mir::VectorType& v) -> lir::Type {
            return lir::Type{
                lir::VectorType{.element = TranslateType(v.element)}};
          },
          [&](const mir::TupleType& t) -> lir::Type {
            std::vector<lir::TypeId> elements;
            elements.reserve(t.elements.size());
            for (const mir::TypeId element : t.elements) {
              elements.push_back(TranslateType(element));
            }
            return lir::Type{lir::TupleType{.elements = std::move(elements)}};
          },
          [&](const mir::UnionType& u) -> lir::Type {
            std::vector<lir::TypeId> elements;
            elements.reserve(u.elements.size());
            for (const mir::TypeId element : u.elements) {
              elements.push_back(TranslateType(element));
            }
            return lir::Type{lir::UnionType{.elements = std::move(elements)}};
          },
          [&](const mir::TaggedUnionType& u) -> lir::Type {
            std::vector<lir::TypeId> elements;
            elements.reserve(u.elements.size());
            for (const mir::TypeId element : u.elements) {
              elements.push_back(TranslateType(element));
            }
            return lir::Type{
                lir::TaggedUnionType{.elements = std::move(elements)}};
          },
          [&](const mir::ResolvedType& r) -> lir::Type {
            return lir::Type{lir::ResolvedType{
                .value = TranslateType(r.value),
                .resolution = TranslateNetResolution(r.resolution)}};
          },
          [&](const mir::DriverType& d) -> lir::Type {
            return lir::Type{lir::DriverType{
                .value = TranslateType(d.value),
                .resolution = TranslateNetResolution(d.resolution)}};
          },
          [&](const mir::ObservableType& ob) -> lir::Type {
            return lir::Type{
                lir::ObservableType{.value = TranslateType(ob.value)}};
          },
          [&](const mir::StructType&) -> lir::Type {
            return RecordUnsupportedType("a nominal struct");
          },
          [&](const mir::ClosureType& c) -> lir::Type {
            return lir::Type{lir::ClosureType{
                .closure_id = ClosureDeclaration(c.closure_id)}};
          }});
}

auto UnitLowerer::ControlEffectType() -> lir::TypeId {
  return out_.types.Intern(
      TranslateRuntimeLibrary(mir::RuntimeLibraryKind::kControlEffect));
}

auto UnitLowerer::TranslateRuntimeLibrary(mir::RuntimeLibraryKind kind)
    -> lir::Type {
  const auto mirror = [](lir::RuntimeLibraryKind k) {
    return lir::Type{lir::RuntimeLibraryType{.kind = k}};
  };
  switch (kind) {
    case mir::RuntimeLibraryKind::kPrintItem:
      return mirror(lir::RuntimeLibraryKind::kPrintItem);
    case mir::RuntimeLibraryKind::kPackedType:
      return mirror(lir::RuntimeLibraryKind::kPackedType);
    case mir::RuntimeLibraryKind::kPackedRange:
      return mirror(lir::RuntimeLibraryKind::kPackedRange);
    case mir::RuntimeLibraryKind::kPrintLiteralItem:
      return mirror(lir::RuntimeLibraryKind::kPrintLiteralItem);
    case mir::RuntimeLibraryKind::kPrintValueItem:
      return mirror(lir::RuntimeLibraryKind::kPrintValueItem);
    case mir::RuntimeLibraryKind::kFormatSpec:
      return mirror(lir::RuntimeLibraryKind::kFormatSpec);
    case mir::RuntimeLibraryKind::kFormatArg:
      return mirror(lir::RuntimeLibraryKind::kFormatArg);
    case mir::RuntimeLibraryKind::kChannelCancellation:
      return mirror(lir::RuntimeLibraryKind::kChannelCancellation);
    case mir::RuntimeLibraryKind::kTimeFormat:
      return mirror(lir::RuntimeLibraryKind::kTimeFormat);
    case mir::RuntimeLibraryKind::kHierarchySegment:
      return mirror(lir::RuntimeLibraryKind::kHierarchySegment);
    case mir::RuntimeLibraryKind::kDpiBitBuffer:
      return mirror(lir::RuntimeLibraryKind::kDpiBitBuffer);
    case mir::RuntimeLibraryKind::kDpiLogicBuffer:
      return mirror(lir::RuntimeLibraryKind::kDpiLogicBuffer);
    case mir::RuntimeLibraryKind::kDpiBitChunk:
      return mirror(lir::RuntimeLibraryKind::kDpiBitChunk);
    case mir::RuntimeLibraryKind::kDpiLogicChunk:
      return mirror(lir::RuntimeLibraryKind::kDpiLogicChunk);
    case mir::RuntimeLibraryKind::kDpiOpenArray:
      return mirror(lir::RuntimeLibraryKind::kDpiOpenArray);
    case mir::RuntimeLibraryKind::kDpiOpenArrayHandle:
      return mirror(lir::RuntimeLibraryKind::kDpiOpenArrayHandle);
    case mir::RuntimeLibraryKind::kTrigger:
      return mirror(lir::RuntimeLibraryKind::kTrigger);
    case mir::RuntimeLibraryKind::kCancellationTarget:
      return mirror(lir::RuntimeLibraryKind::kCancellationTarget);
    case mir::RuntimeLibraryKind::kDpiScopeGuard:
      return RecordUnsupportedType(
          "the scope a DPI-C context import makes current");
    case mir::RuntimeLibraryKind::kForeignTaskAwaitable:
      return RecordUnsupportedType("the fiber a DPI-C task import runs on");
    case mir::RuntimeLibraryKind::kControlEffect:
      return mirror(lir::RuntimeLibraryKind::kControlEffect);
    case mir::RuntimeLibraryKind::kScopeProgram:
    case mir::RuntimeLibraryKind::kScopeDefinition:
    case mir::RuntimeLibraryKind::kScopeMetadata:
    case mir::RuntimeLibraryKind::kAbiStringRef:
    case mir::RuntimeLibraryKind::kScopeExport:
    case mir::RuntimeLibraryKind::kScopeExportTable:
      throw InternalError(
          "TranslateRuntimeLibrary: a unit-definition record type is a "
          "compile-time constant consumed by the backend directly and does not "
          "flow through MIR-to-LIR");
  }
  throw InternalError("TranslateRuntimeLibrary: unknown RuntimeLibraryKind");
}

auto UnitLowerer::RecordUnsupportedType(std::string_view what) -> lir::Type {
  if (!type_error_.has_value()) {
    type_error_ = diag::Make(
        diag::DiagCode::kUnsupportedTypeKind,
        std::format("mir_to_lir: {} is not yet lowerable to LIR", what));
  }
  return lir::Type{lir::VoidType{}};
}

}  // namespace lyra::lowering::mir_to_lir

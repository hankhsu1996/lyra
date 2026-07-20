#include <format>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::mir_to_lir {

namespace {

auto TranslateBitAtom(mir::BitAtom a) -> lir::BitAtom {
  switch (a) {
    case mir::BitAtom::kBit:
      return lir::BitAtom::kBit;
    case mir::BitAtom::kLogic:
      return lir::BitAtom::kLogic;
    case mir::BitAtom::kReg:
      return lir::BitAtom::kReg;
  }
  throw InternalError("TranslateBitAtom: unknown BitAtom");
}

auto TranslateSignedness(mir::Signedness s) -> lir::Signedness {
  return s == mir::Signedness::kSigned ? lir::Signedness::kSigned
                                       : lir::Signedness::kUnsigned;
}

auto TranslatePackedArrayForm(mir::PackedArrayForm f) -> lir::PackedArrayForm {
  switch (f) {
    case mir::PackedArrayForm::kExplicit:
      return lir::PackedArrayForm::kExplicit;
    case mir::PackedArrayForm::kByte:
      return lir::PackedArrayForm::kByte;
    case mir::PackedArrayForm::kShortInt:
      return lir::PackedArrayForm::kShortInt;
    case mir::PackedArrayForm::kInt:
      return lir::PackedArrayForm::kInt;
    case mir::PackedArrayForm::kLongInt:
      return lir::PackedArrayForm::kLongInt;
    case mir::PackedArrayForm::kInteger:
      return lir::PackedArrayForm::kInteger;
    case mir::PackedArrayForm::kTime:
      return lir::PackedArrayForm::kTime;
  }
  throw InternalError("TranslatePackedArrayForm: unknown PackedArrayForm");
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

auto TranslateRuntimeLibraryKind(mir::RuntimeLibraryKind k)
    -> lir::RuntimeLibraryKind {
  switch (k) {
    case mir::RuntimeLibraryKind::kPrintItem:
      return lir::RuntimeLibraryKind::kPrintItem;
    case mir::RuntimeLibraryKind::kPrintLiteralItem:
      return lir::RuntimeLibraryKind::kPrintLiteralItem;
    case mir::RuntimeLibraryKind::kPrintValueItem:
      return lir::RuntimeLibraryKind::kPrintValueItem;
    case mir::RuntimeLibraryKind::kFormatSpec:
      return lir::RuntimeLibraryKind::kFormatSpec;
    case mir::RuntimeLibraryKind::kFormatArg:
      return lir::RuntimeLibraryKind::kFormatArg;
    case mir::RuntimeLibraryKind::kChannelCancellation:
      return lir::RuntimeLibraryKind::kChannelCancellation;
    case mir::RuntimeLibraryKind::kTimeFormat:
      return lir::RuntimeLibraryKind::kTimeFormat;
    case mir::RuntimeLibraryKind::kHierarchySegment:
      return lir::RuntimeLibraryKind::kHierarchySegment;
    case mir::RuntimeLibraryKind::kDpiBitBuffer:
      return lir::RuntimeLibraryKind::kDpiBitBuffer;
    case mir::RuntimeLibraryKind::kDpiLogicBuffer:
      return lir::RuntimeLibraryKind::kDpiLogicBuffer;
    case mir::RuntimeLibraryKind::kDpiBitChunk:
      return lir::RuntimeLibraryKind::kDpiBitChunk;
    case mir::RuntimeLibraryKind::kDpiLogicChunk:
      return lir::RuntimeLibraryKind::kDpiLogicChunk;
    case mir::RuntimeLibraryKind::kTrigger:
      return lir::RuntimeLibraryKind::kTrigger;
    case mir::RuntimeLibraryKind::kScopeProgram:
    case mir::RuntimeLibraryKind::kUnitDefinition:
    case mir::RuntimeLibraryKind::kScopeMetadata:
    case mir::RuntimeLibraryKind::kAbiStringRef:
    case mir::RuntimeLibraryKind::kScopeEntry:
      throw InternalError(
          "TranslateRuntimeLibraryKind: a unit-definition record type is a "
          "compile-time constant consumed by the backend directly and does not "
          "flow through MIR-to-LIR");
    case mir::RuntimeLibraryKind::kDpiScopeGuard:
      throw InternalError(
          "TranslateRuntimeLibraryKind: the DPI context scope guard is a "
          "C++-backend marshaling artifact; the execution backend does not "
          "consume the DPI context surface");
  }
  throw InternalError(
      "TranslateRuntimeLibraryKind: unknown RuntimeLibraryKind");
}

auto TranslatePackedArray(const mir::PackedArrayType& pa)
    -> lir::PackedArrayType {
  std::vector<lir::PackedRange> dims;
  dims.reserve(pa.dims.size());
  for (const mir::PackedRange& d : pa.dims) {
    dims.push_back(lir::PackedRange{.left = d.left, .right = d.right});
  }
  return lir::PackedArrayType{
      .atom = TranslateBitAtom(pa.atom),
      .signedness = TranslateSignedness(pa.signedness),
      .dims = std::move(dims),
      .form = TranslatePackedArrayForm(pa.form)};
}

}  // namespace

auto UnitLowerer::TranslateType(mir::TypeId id) -> lir::TypeId {
  if (const auto it = type_memo_.find(id); it != type_memo_.end()) {
    return it->second;
  }
  lir::TypeData data = TranslateTypeData(mir_->types.Get(id));
  const lir::TypeId lir_id = out_.types.Add(lir::Type{.data = std::move(data)});
  type_memo_.emplace(id, lir_id);
  return lir_id;
}

auto UnitLowerer::TranslateTypeData(const mir::Type& ty) -> lir::TypeData {
  return std::visit(
      Overloaded{
          [&](const mir::PackedArrayType& pa) -> lir::TypeData {
            return lir::TypeData{TranslatePackedArray(pa)};
          },
          [&](const mir::EnumType& e) -> lir::TypeData {
            std::vector<lir::EnumMember> members;
            members.reserve(e.members.size());
            for (const mir::EnumMember& m : e.members) {
              members.push_back(
                  lir::EnumMember{.name = m.name, .value = m.value});
            }
            return lir::TypeData{lir::EnumType{
                .base = TranslatePackedArray(e.base),
                .members = std::move(members)}};
          },
          [&](const mir::UnpackedArrayType& ua) -> lir::TypeData {
            return lir::TypeData{lir::UnpackedArrayType{
                .element_type = TranslateType(ua.element_type),
                .size = ua.Size()}};
          },
          [&](const mir::DynamicArrayType& da) -> lir::TypeData {
            return lir::TypeData{lir::DynamicArrayType{
                .element_type = TranslateType(da.element_type)}};
          },
          [&](const mir::QueueType& q) -> lir::TypeData {
            return lir::TypeData{lir::QueueType{
                .element_type = TranslateType(q.element_type),
                .max_bound = q.max_bound}};
          },
          [&](const mir::AssociativeArrayType& aa) -> lir::TypeData {
            return lir::TypeData{lir::AssociativeArrayType{
                .element_type = TranslateType(aa.element_type),
                .key_type = TranslateType(aa.key_type)}};
          },
          [](const mir::WildcardIndexType&) -> lir::TypeData {
            return lir::TypeData{lir::WildcardIndexType{}};
          },
          [](const mir::StringType&) -> lir::TypeData {
            return lir::TypeData{lir::StringType{}};
          },
          [](const mir::MachineCStringType&) -> lir::TypeData {
            return lir::TypeData{lir::MachineCStringType{}};
          },
          [](const mir::MachineIntType& mi) -> lir::TypeData {
            return lir::TypeData{lir::MachineIntType{
                .bit_width = mi.bit_width,
                .signedness = TranslateSignedness(mi.signedness)}};
          },
          [](const mir::MachineFloatType& mf) -> lir::TypeData {
            return lir::TypeData{
                lir::MachineFloatType{.bit_width = mf.bit_width}};
          },
          [](const mir::EventType&) -> lir::TypeData {
            return lir::TypeData{lir::EventType{}};
          },
          [](const mir::RealType&) -> lir::TypeData {
            return lir::TypeData{lir::RealType{}};
          },
          [](const mir::ShortRealType&) -> lir::TypeData {
            return lir::TypeData{lir::ShortRealType{}};
          },
          [](const mir::RealTimeType&) -> lir::TypeData {
            return lir::TypeData{lir::RealTimeType{}};
          },
          [](const mir::ChandleType&) -> lir::TypeData {
            return lir::TypeData{lir::ChandleType{}};
          },
          [](const mir::VoidType&) -> lir::TypeData {
            return lir::TypeData{lir::VoidType{}};
          },
          [](const mir::ObjectType& ob) -> lir::TypeData {
            return lir::TypeData{
                lir::ObjectType{.class_id = lir::ClassId{ob.class_id.value}}};
          },
          [](const mir::ExternalUnitObjectType& eu) -> lir::TypeData {
            return lir::TypeData{
                lir::ExternalUnitObjectType{.unit_name = eu.unit_name}};
          },
          [](const mir::ExternalClassType& e) -> lir::TypeData {
            return lir::TypeData{
                lir::ExternalClassType{.qualified_name = e.qualified_name}};
          },
          [](const mir::ServicesType&) -> lir::TypeData {
            return lir::TypeData{lir::ServicesType{}};
          },
          [](const mir::FilesType&) -> lir::TypeData {
            return lir::TypeData{lir::FilesType{}};
          },
          [](const mir::DiagnosticType&) -> lir::TypeData {
            return lir::TypeData{lir::DiagnosticType{}};
          },
          [](const mir::RuntimeLibraryType& rl) -> lir::TypeData {
            return lir::TypeData{lir::RuntimeLibraryType{
                .kind = TranslateRuntimeLibraryKind(rl.kind)}};
          },
          [&](const mir::CoroutineType& co) -> lir::TypeData {
            return lir::TypeData{
                lir::CoroutineType{.payload = TranslateType(co.payload)}};
          },
          [&](const mir::RefType& r) -> lir::TypeData {
            return lir::TypeData{lir::RefType{
                .pointee = TranslateType(r.pointee),
                .mutability = TranslateMutability(r.mutability)}};
          },
          [&](const mir::PointerType& pt) -> lir::TypeData {
            return lir::TypeData{lir::PointerType{
                .pointee = TranslateType(pt.pointee),
                .ownership = TranslatePointerOwnership(pt.ownership),
                .mutability = TranslateMutability(pt.mutability)}};
          },
          [&](const mir::ManagedRefType& mr) -> lir::TypeData {
            return lir::TypeData{
                lir::ManagedRefType{.pointee = TranslateType(mr.pointee)}};
          },
          [&](const mir::VectorType& v) -> lir::TypeData {
            return lir::TypeData{
                lir::VectorType{.element = TranslateType(v.element)}};
          },
          [&](const mir::TupleType& t) -> lir::TypeData {
            std::vector<lir::TypeId> elements;
            elements.reserve(t.elements.size());
            for (const mir::TypeId element : t.elements) {
              elements.push_back(TranslateType(element));
            }
            return lir::TypeData{
                lir::TupleType{.elements = std::move(elements)}};
          },
          [&](const mir::UnionType& u) -> lir::TypeData {
            std::vector<lir::TypeId> elements;
            elements.reserve(u.elements.size());
            for (const mir::TypeId element : u.elements) {
              elements.push_back(TranslateType(element));
            }
            return lir::TypeData{
                lir::UnionType{.elements = std::move(elements)}};
          },
          [&](const mir::ResolvedType& r) -> lir::TypeData {
            return lir::TypeData{
                lir::ResolvedType{.value = TranslateType(r.value)}};
          },
          [&](const mir::DriverType& d) -> lir::TypeData {
            return lir::TypeData{
                lir::DriverType{.value = TranslateType(d.value)}};
          },
          [&](const mir::ObservableType& ob) -> lir::TypeData {
            return lir::TypeData{
                lir::ObservableType{.value = TranslateType(ob.value)}};
          },
          [&](const mir::StructType&) -> lir::TypeData {
            return RecordUnsupportedType("a nominal struct");
          },
          [&](const mir::ClosureType&) -> lir::TypeData {
            return RecordUnsupportedType("a closure");
          }},
      ty.data);
}

auto UnitLowerer::RecordUnsupportedType(std::string_view what)
    -> lir::TypeData {
  if (!type_error_.has_value()) {
    type_error_ = diag::Make(
        diag::DiagCode::kUnsupportedTypeKind,
        std::format("mir_to_lir: {} is not yet lowerable to LIR", what));
  }
  return lir::TypeData{lir::VoidType{}};
}

}  // namespace lyra::lowering::mir_to_lir

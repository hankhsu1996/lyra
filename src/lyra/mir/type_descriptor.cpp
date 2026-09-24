#include "lyra/mir/type_descriptor.hpp"

#include <cstdint>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_record.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

namespace {

// The record describing one integral type: its dimension stack, then whether it
// is signed and whether it carries the unknown states. A one-dimensional type
// is a one-element stack rather than a scalar special case, because the runtime
// dispatches element access on the outer dimension's width at any rank.
//
// Naming the dimension stack interns that stack's type, which grows the pool
// the descriptions are read out of, and a reference into that pool does not
// survive the growth -- so every caller hands over a shape it holds a copy of.
auto IntegralRecord(RuntimeRecordBuilder& record, const PackedArrayType& pa)
    -> ExprId {
  std::vector<ExprId> dims;
  dims.reserve(pa.dims.size());
  for (const PackedRange& dim : pa.dims) {
    dims.push_back(record.Construct(
        RuntimeLibraryKind::kPackedRange,
        {record.MachineInt(dim.left), record.MachineInt(dim.right)}));
  }
  return record.Construct(
      RuntimeLibraryKind::kPackedType,
      {record.MachineArray(
           record.Type(RuntimeLibraryKind::kPackedRange), std::move(dims)),
       record.Bool(pa.signedness == Signedness::kSigned),
       record.Bool(pa.state_kind == IntegralStateKind::kFourState)});
}

auto BuildIntegralDescriptor(const CompilationUnit& unit, PackedArrayType pa)
    -> ValueBuild {
  ValueBuild description;
  RuntimeRecordBuilder record(unit, description.body.exprs);
  description.value = IntegralRecord(record, pa);
  return description;
}

// The record describing one enumeration: its base's description, then every
// member's planes in declared order -- its value plane and, over a 4-state
// base, its unknown plane -- and the members' names in the same order. A member
// is already in the canonical form of its base, so each contributes the same
// number of words, and a 2-state base contributes no unknown plane at all.
auto BuildEnumerationDescriptor(const CompilationUnit& unit, EnumType e)
    -> ValueBuild {
  ValueBuild description;
  RuntimeRecordBuilder record(unit, description.body.exprs);
  const TypeId word = unit.builtins.machine_word;
  const TypeId text = unit.types.Intern(mir::Type{MachineCStringType{}});
  std::vector<ExprId> planes;
  std::vector<ExprId> names;
  const auto add_words = [&](const std::vector<std::uint64_t>& words) {
    for (const std::uint64_t bits : words) {
      planes.push_back(record.Add(
          Expr{
              .data =
                  MachineIntLiteral{.value = static_cast<std::int64_t>(bits)},
              .type = word}));
    }
  };
  for (const EnumMember& member : e.members) {
    add_words(member.value.value_words);
    add_words(member.value.state_words);
    names.push_back(record.Add(
        Expr{.data = StringLiteral{.value = member.name}, .type = text}));
  }
  description.value = record.Construct(
      RuntimeLibraryKind::kEnumeration,
      {IntegralRecord(record, e.base),
       record.MachineArray(word, std::move(planes)),
       record.MachineArray(text, std::move(names))});
  return description;
}

// The record describing one unpacked array: the two endpoints its declared
// range spans. Element order runs left to right (LRM 7.6), so which endpoint is
// which is what says whether a source index counts up or down.
auto BuildUnpackedDescriptor(const CompilationUnit& unit, UnpackedRange dim)
    -> ValueBuild {
  ValueBuild description;
  RuntimeRecordBuilder record(unit, description.body.exprs);
  description.value = record.Construct(
      RuntimeLibraryKind::kUnpackedRange,
      {record.MachineInt(dim.left), record.MachineInt(dim.right)});
  return description;
}

// A reference to the description the unit holds for `description`, which
// naming it is what puts it in the unit.
auto BuildDescriptorRef(
    const CompilationUnit& unit, Block& block, TypeDescription description)
    -> ExprId {
  const TypeDescriptorId descriptor =
      unit.type_descriptors.Intern(std::move(description));
  return block.exprs.Add(
      Expr{
          .data =
              ReferenceExpr{
                  .target = TypeDescriptorRef{.descriptor = descriptor}},
          .type = TypeDescriptorTypeOf(unit, descriptor)});
}

}  // namespace

auto DescriptionOf(const CompilationUnit& unit, TypeId type)
    -> std::optional<TypeDescription> {
  const Type& ty = unit.types.Get(type);
  if (ty.IsIntegralPacked()) {
    return TypeDescription{ty.PackedShape()};
  }
  if (const auto* unpacked = ty.As<UnpackedArrayType>()) {
    return TypeDescription{unpacked->dim};
  }
  return std::nullopt;
}

auto HasTypeDescriptor(const CompilationUnit& unit, TypeId type) -> bool {
  return DescriptionOf(unit, type).has_value();
}

auto ValueTypeOf(const CompilationUnit& unit, TypeId type) -> TypeId {
  for (;;) {
    const Type& ty = unit.types.Get(type);
    if (ty.IsCapabilityWrapper()) {
      type = ty.WrappedValueType();
    } else if (const auto* ptr = ty.As<PointerType>()) {
      type = ptr->pointee;
    } else {
      return type;
    }
  }
}

auto TypeDescriptorTypeOf(
    const CompilationUnit& unit, TypeDescriptorId descriptor) -> TypeId {
  return std::visit(
      Overloaded{
          [&unit](const PackedArrayType&) { return unit.builtins.packed_type; },
          [&unit](const UnpackedRange&) {
            return unit.builtins.unpacked_range;
          },
          [&unit](const EnumType&) { return unit.builtins.enumeration; }},
      unit.type_descriptors.Get(descriptor));
}

auto BuildTypeDescriptorRef(
    const CompilationUnit& unit, Block& block, TypeId described) -> ExprId {
  std::optional<TypeDescription> description = DescriptionOf(unit, described);
  if (!description) {
    throw InternalError(
        "mir: this type's declaration says nothing an operation on a value of "
        "it needs");
  }
  return BuildDescriptorRef(unit, block, *std::move(description));
}

auto BuildEnumerationDescriptorRef(
    const CompilationUnit& unit, Block& block, TypeId enumeration) -> ExprId {
  const auto* declared = unit.types.Get(enumeration).As<EnumType>();
  if (declared == nullptr) {
    throw InternalError("mir: only an enumeration declares members");
  }
  return BuildDescriptorRef(unit, block, TypeDescription{*declared});
}

auto DescribeType(const CompilationUnit& unit, TypeDescriptorId descriptor)
    -> ValueBuild {
  return std::visit(
      Overloaded{
          [&unit](const PackedArrayType& packed) {
            return BuildIntegralDescriptor(unit, packed);
          },
          [&unit](const UnpackedRange& range) {
            return BuildUnpackedDescriptor(unit, range);
          },
          [&unit](const EnumType& enumeration) {
            return BuildEnumerationDescriptor(unit, enumeration);
          }},
      unit.type_descriptors.Get(descriptor));
}

}  // namespace lyra::mir

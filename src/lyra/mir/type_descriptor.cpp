#include "lyra/mir/type_descriptor.hpp"

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
// The shape arrives by value because naming its dimension stack interns
// that stack's type, which grows the pool the descriptions are read out of, and
// a reference into that pool does not survive the growth.
auto BuildIntegralDescriptor(const CompilationUnit& unit, PackedArrayType pa)
    -> ValueBuild {
  ValueBuild description;
  RuntimeRecordBuilder record(unit, description.body.exprs);
  std::vector<ExprId> dims;
  dims.reserve(pa.dims.size());
  for (const PackedRange& dim : pa.dims) {
    dims.push_back(record.Construct(
        RuntimeLibraryKind::kPackedRange,
        {record.MachineInt(dim.left), record.MachineInt(dim.right)}));
  }
  description.value = record.Construct(
      RuntimeLibraryKind::kPackedType,
      {record.MachineArray(
           record.Type(RuntimeLibraryKind::kPackedRange), std::move(dims)),
       record.Bool(pa.signedness == Signedness::kSigned),
       record.Bool(pa.state_kind == IntegralStateKind::kFourState)});
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
          }},
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
  const TypeDescriptorId descriptor =
      unit.type_descriptors.Intern(*std::move(description));
  return block.exprs.Add(
      Expr{
          .data =
              ReferenceExpr{
                  .target = TypeDescriptorRef{.descriptor = descriptor}},
          .type = TypeDescriptorTypeOf(unit, descriptor)});
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
          }},
      unit.type_descriptors.Get(descriptor));
}

}  // namespace lyra::mir

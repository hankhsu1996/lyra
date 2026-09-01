#include "lyra/mir/packed_type_descriptor.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_record.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

auto PackedTypeDescriptionName(TypeId integral) -> std::string {
  return std::format("_lyra_packed_type_{}", integral.value);
}

namespace {

// The record describing one integral type: its dimension stack, then whether it
// is signed and whether it carries the unknown states. A one-dimensional type
// is a one-element stack rather than a scalar special case, because the runtime
// dispatches element access on the outer dimension's width at any rank.
//
// The shape arrives by value because naming its dimension stack interns
// that stack's type, which grows the pool the descriptions are read out of, and
// a reference into that pool does not survive the growth.
auto BuildDescriptor(const CompilationUnit& unit, PackedArrayType pa)
    -> PackedTypeDescription {
  PackedTypeDescription description;
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

}  // namespace

auto BuildPackedTypeRef(
    const CompilationUnit& unit, Block& block, TypeId integral) -> ExprId {
  if (!unit.types.Get(integral).IsIntegralPacked()) {
    throw InternalError(
        "BuildPackedTypeRef: only an integral type has a packed "
        "representation");
  }
  return block.exprs.Add(
      Expr{
          .data = PackedTypeRef{.integral = integral},
          .type = unit.builtins.packed_type});
}

auto DescribedPackedTypes(const CompilationUnit& unit) -> std::vector<TypeId> {
  std::vector<TypeId> described;
  for (const TypeId id : unit.types.Ids()) {
    if (unit.types.Get(id).IsIntegralPacked()) {
      described.push_back(id);
    }
  }
  return described;
}

auto DescribePackedType(const CompilationUnit& unit, TypeId integral)
    -> PackedTypeDescription {
  const Type& type = unit.types.Get(integral);
  if (!type.IsIntegralPacked()) {
    throw InternalError(
        "DescribePackedType: only an integral type has a packed "
        "representation");
  }
  return BuildDescriptor(unit, type.PackedShape());
}

}  // namespace lyra::mir

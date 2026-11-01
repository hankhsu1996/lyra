#include "lyra/lowering/hir_to_mir/container_construction.hpp"

#include <cstdint>
#include <format>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/expression/references.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The elements a construction is handed, as one value: the plain-data array of
// the container's element type holding them in order. The container is what
// the construction makes of the list, never what the list itself is.
auto BuildElementList(
    const mir::CompilationUnit& unit, mir::Builder block,
    mir::TypeId array_type, std::vector<mir::ExprId> elements) -> mir::ExprId {
  const mir::TypeId element_type =
      RequiredContainerElementType(unit, array_type);
  // Each item is assigned to the element it lands in (LRM 10.8, 10.9.1).
  for (mir::ExprId& element : elements) {
    element = AtType(unit, block, element, element_type);
  }
  const mir::TypeId list_type =
      mir::MachineArrayOf(unit.types, element_type, elements.size());
  return block.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(elements)},
          .type = list_type});
}

// Builds the queue or the dynamic array an element list feeds. Neither has a
// size of its own, so its construction states the list and how many times it
// repeats. What a position no element fills holds, and how many a queue may
// hold (LRM 7.4.5, 7.10.5), are the type's to say, so nothing here states
// either.
auto BuildContainerFromElements(
    const mir::CompilationUnit& unit, mir::TypeId array_type,
    mir::ExprId elements, mir::ExprId count) -> mir::Expr {
  if (unit.types.Get(array_type).Is<mir::UnpackedArrayType>()) {
    throw InternalError(
        "BuildContainerFromElements: a fixed-size unpacked array's element "
        "count is its type's, so its construction states no repeat count -- "
        "please report this as a bug");
  }
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Construct{}, .arguments = {elements, count}},
      .type = array_type};
}

}  // namespace

auto RequiredContainerElementType(
    const mir::CompilationUnit& unit, mir::TypeId container) -> mir::TypeId {
  const std::optional<mir::TypeId> element =
      unit.types.Get(container).ContainerElementType();
  if (!element.has_value()) {
    throw InternalError(
        "RequiredContainerElementType: the type holds no elements, and the "
        "caller reached it only because its own construction said it would -- "
        "please report this as a bug");
  }
  return *element;
}

auto BuildFixedArrayConstructionCall(
    const mir::CompilationUnit& unit, mir::Builder block,
    mir::TypeId array_type, std::vector<mir::ExprId> repeat_unit) -> mir::Expr {
  const std::uint64_t element_count =
      unit.types.Get(array_type).Get<mir::UnpackedArrayType>().Size();
  if (repeat_unit.empty() || element_count % repeat_unit.size() != 0) {
    throw InternalError(
        std::format(
            "BuildFixedArrayConstructionCall: {} elements laid down again and "
            "again do not fill an array of {} -- please report this as a bug",
            repeat_unit.size(), element_count));
  }
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Construct{},
              .arguments = {BuildElementList(
                  unit, block, array_type, std::move(repeat_unit))}},
      .type = array_type};
}

auto BuildArrayConstructionCall(
    const mir::CompilationUnit& unit, mir::Builder block,
    mir::TypeId array_type, std::vector<mir::ExprId> elements) -> mir::Expr {
  if (unit.types.Get(array_type).Is<mir::UnpackedArrayType>()) {
    return BuildFixedArrayConstructionCall(
        unit, block, array_type, std::move(elements));
  }
  const mir::ExprId list_id =
      BuildElementList(unit, block, array_type, std::move(elements));
  return BuildContainerFromElements(
      unit, array_type, list_id, mir::BuildMachineIntLiteral(unit, block, 1));
}

auto BuildArrayRepeatCall(
    const mir::CompilationUnit& unit, mir::Builder block,
    mir::TypeId array_type, std::vector<mir::ExprId> repeat_unit,
    mir::ExprId count_id) -> mir::Expr {
  const mir::ExprId repeat_unit_id =
      BuildElementList(unit, block, array_type, std::move(repeat_unit));
  return BuildContainerFromElements(unit, array_type, repeat_unit_id, count_id);
}

auto BuildSequenceConstructionCall(
    const mir::CompilationUnit& unit, mir::Builder block,
    mir::TypeId sequence_type, std::vector<mir::ExprId> elements) -> mir::Expr {
  const mir::TypePool& types = unit.types;
  const mir::TypeId list_type = mir::MachineArrayOf(
      types, types.Get(sequence_type).Get<mir::VectorType>().element,
      elements.size());
  const mir::ExprId list_id = block.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(elements)},
          .type = list_type});
  return mir::Expr{
      .data = mir::CallExpr{.callee = mir::Construct{}, .arguments = {list_id}},
      .type = sequence_type};
}

auto BuildAssociativeConstructionCall(
    const mir::CompilationUnit& unit, mir::Builder block,
    mir::TypeId assoc_type,
    std::vector<std::pair<mir::ExprId, mir::ExprId>> entries,
    std::optional<mir::ExprId> stated_default) -> mir::Expr {
  const mir::Type& assoc = unit.types.Get(assoc_type);
  const std::optional<mir::TypeId> index = assoc.IndexType();
  if (!index.has_value()) {
    throw InternalError(
        "BuildAssociativeConstructionCall: what is built is no associative "
        "array -- please report this as a bug");
  }
  const mir::TypeId key_type = *index;
  const mir::TypeId element_type =
      RequiredContainerElementType(unit, assoc_type);

  const mir::TypeId tuple_type = unit.types.Intern(
      mir::Type{mir::TupleType{.elements = {key_type, element_type}}});
  std::vector<mir::ExprId> tuple_ids;
  tuple_ids.reserve(entries.size());
  // Each index and each value is assigned to where it lands (LRM 7.9.11,
  // 10.8), and so is a `default:`, which the front end leaves unconverted
  // since LRM 10.8 leaves a default correspondence out.
  for (const auto& [key_id, value_id] : entries) {
    tuple_ids.push_back(block.Add(
        mir::Expr{
            .data =
                mir::CompositeExpr{
                    .parts =
                        {AtType(unit, block, key_id, key_type),
                         AtType(unit, block, value_id, element_type)}},
            .type = tuple_type}));
  }
  const mir::TypeId entries_type =
      mir::MachineArrayOf(unit.types, tuple_type, tuple_ids.size());
  const mir::ExprId entries_id = block.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(tuple_ids)},
          .type = entries_type});

  mir::Expr built{
      .data =
          mir::CallExpr{.callee = mir::Construct{}, .arguments = {entries_id}},
      .type = assoc_type};
  if (!stated_default.has_value()) {
    return built;
  }
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kAssocWithDefault,
                      .receiver = block.Add(std::move(built))},
              .arguments = {ConvertToType(
                  unit, block, *stated_default, element_type)}},
      .type = assoc_type};
}

}  // namespace lyra::lowering::hir_to_mir

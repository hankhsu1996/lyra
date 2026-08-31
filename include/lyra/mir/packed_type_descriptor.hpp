#pragma once

#include <string>
#include <vector>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/packed_type_description.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// The name a described type's description is emitted under. Derived from the
// type, so a consumer reaches the same name without being told it, and two
// types never collide within a unit.
[[nodiscard]] auto PackedTypeDescriptionName(TypeId integral) -> std::string;

// The declared representation a packed value-layer call lands its result into,
// stated as an operand so it reaches the runtime through the argument list
// rather than being composed by a backend from type payload. A select uses it
// to state the shape its result takes, which a receiver whose own dimensions
// differ (an aggregate's flat base) can never supply.
//
// `integral` must be a packed array or an enumeration; nothing else has a
// representation this describes.
[[nodiscard]] auto BuildPackedTypeRef(
    const CompilationUnit& unit, Block& block, TypeId integral) -> ExprId;

// The unit's types that have a run-time description, in pool order. Answered
// once and handed over whole: describing a type names its dimension stack,
// which interns a type of its own, so the pool a consumer would otherwise be
// walking grows underneath it while it works.
[[nodiscard]] auto DescribedPackedTypes(const CompilationUnit& unit)
    -> std::vector<TypeId>;

// How `integral` is described at run time, as the expression that builds the
// description. A description is a function of the type alone -- it reads no
// body and no other type -- so a consumer asks for the one it is emitting, and
// two consumers asking get the same answer without either inventing it.
//
// `integral` must be a packed array or an enumeration; nothing else has a
// representation this describes.
[[nodiscard]] auto DescribePackedType(
    const CompilationUnit& unit, TypeId integral) -> PackedTypeDescription;

}  // namespace lyra::mir

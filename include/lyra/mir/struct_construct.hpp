#pragma once

#include <vector>

#include "lyra/mir/field.hpp"
#include "lyra/mir/struct_id.hpp"

namespace lyra::mir {

// Construction of a struct value by named-field aggregate initialization
// (`S{ .x = a, .y = b }`). This is the general way any nominal `StructType` is
// built by value from field values -- a generic programming-language primitive,
// the struct analogue of a tuple literal, independent of any source feature or
// lowering strategy. Distinct from the constructor/call protocol (a `CallExpr`
// with a `Construct{}` callee, `T(args)` / `make_shared<T>()`): this carries
// two facts a positional constructor cannot -- which value initializes which
// named field (by stable `FieldId`), and the initializers' source evaluation
// order. `struct_id` names the declaration; the `Expr::type` is the
// `StructType` naming it.
//
// Invariant: `field_inits` are evaluated in listed order, each targeting an
// explicit `FieldId`. A field's declaration / emission order and its physical
// layout are separate from this initializer order.
//
// Reference / shared ownership (a promoted scope's `Shared<StructType>` handle)
// is a wrapper around a construction, not part of it.
struct StructConstructExpr {
  StructId struct_id{};
  std::vector<FieldInit> field_inits;
};

}  // namespace lyra::mir

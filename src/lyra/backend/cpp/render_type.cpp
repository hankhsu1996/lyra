#include "lyra/backend/cpp/render_type.hpp"

#include <cstddef>
#include <format>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/structural_scope.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

auto RenderPackedShapeLiteral(const std::vector<mir::PackedRange>& dims)
    -> std::string {
  if (dims.empty()) {
    return "lyra::runtime::PackedShape<0>{}";
  }
  // Explicit `std::array<PackedDim, N>{...}` so the dims member's array type
  // is unambiguous: a bare `.dims = {...}` relies on brace-elision through
  // std::array's wrapper, which is allowed but easy to get wrong.
  std::string out = std::format(
      "lyra::runtime::PackedShape<{}>{{ .dims = "
      "std::array<lyra::runtime::PackedDim, {}>{{",
      dims.size(), dims.size());
  for (std::size_t i = 0; i < dims.size(); ++i) {
    if (i != 0) {
      out += ", ";
    }
    out += std::format(
        "lyra::runtime::PackedDim{{.left = {}, .right = {}}}", dims[i].left,
        dims[i].right);
  }
  // Two closes: inner `std::array<...>{...}` and outer `PackedShape<...>{...}`.
  out += "} }";
  return out;
}

auto RenderTypeAsCpp(
    const mir::CompilationUnit& unit, const mir::StructuralScope& owner_scope,
    mir::TypeId type_id) -> std::string {
  return std::visit(
      Overloaded{
          [](const mir::PackedArrayType& p) -> std::string {
            // `int`/`integer` retain native std::int32_t because current
            // expression lowering uses raw C++ operators; switching them to
            // runtime Bit<...> requires operator overloads, which belong to
            // the packed-operations cut. `integer`'s 4-state semantics are
            // not blessed correct here -- the mapping is preserved only to
            // keep current tests green; runtime typing of `integer` is
            // unresolved.
            if (p.form == mir::PackedArrayForm::kInt ||
                p.form == mir::PackedArrayForm::kInteger) {
              return "std::int32_t";
            }
            if (p.form == mir::PackedArrayForm::kExplicit) {
              const char* signed_token =
                  (p.signedness == mir::Signedness::kSigned)
                      ? "lyra::runtime::Signedness::kSigned"
                      : "lyra::runtime::Signedness::kUnsigned";
              const std::string shape = RenderPackedShapeLiteral(p.dims);
              switch (p.atom) {
                case mir::BitAtom::kBit:
                  return std::format(
                      "lyra::runtime::Bit<{}, {}>", shape, signed_token);
                case mir::BitAtom::kLogic:
                  return std::format(
                      "lyra::runtime::Logic<{}, {}>", shape, signed_token);
                case mir::BitAtom::kReg:
                  return std::format(
                      "lyra::runtime::Reg<{}, {}>", shape, signed_token);
              }
              throw InternalError(
                  "RenderTypeAsCpp: unknown BitAtom in kExplicit form");
            }
            throw InternalError(
                "RenderTypeAsCpp: unsupported MIR type for current C++ render "
                "cut");
          },
          [](const mir::StringType&) -> std::string { return "std::string"; },
          [&](const mir::ObjectType& o) -> std::string {
            return {owner_scope.GetChildStructuralScope(o.target).name};
          },
          [&](const mir::OwningPtrType& o) -> std::string {
            return "std::unique_ptr<" +
                   RenderTypeAsCpp(unit, owner_scope, o.pointee) + ">";
          },
          [&](const mir::VectorType& v) -> std::string {
            return "std::vector<" +
                   RenderTypeAsCpp(unit, owner_scope, v.element) + ">";
          },
          [](const auto&) -> std::string {
            throw InternalError(
                "RenderTypeAsCpp: unsupported MIR type for current C++ render "
                "cut");
          },
      },
      unit.GetType(type_id).data);
}

}  // namespace lyra::backend::cpp

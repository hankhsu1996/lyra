#include "lyra/backend/cpp/render_type.hpp"

#include <string>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/structural_scope.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

auto RenderPackedArrayCtorArgs(const mir::PackedArrayType& pa) -> std::string {
  const char* signed_lit =
      pa.signedness == mir::Signedness::kSigned ? "true" : "false";
  const char* four_state_lit = pa.atom != mir::BitAtom::kBit ? "true" : "false";
  return std::to_string(pa.BitWidth()) + ", " + signed_lit + ", " +
         four_state_lit;
}

auto RenderTypeAsCpp(
    const mir::CompilationUnit& unit, const mir::StructuralScope& owner_scope,
    mir::TypeId type_id) -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
          [](const mir::PackedArrayType&) -> diag::Result<std::string> {
            return std::string{"lyra::value::PackedArray"};
          },
          [](const mir::StringType&) -> diag::Result<std::string> {
            return std::string{"std::string"};
          },
          [&](const mir::ObjectType& o) -> diag::Result<std::string> {
            return std::string{
                owner_scope.GetChildStructuralScope(o.target).name};
          },
          [&](const mir::OwningPtrType& o) -> diag::Result<std::string> {
            auto inner_or = RenderTypeAsCpp(unit, owner_scope, o.pointee);
            if (!inner_or) return std::unexpected(std::move(inner_or.error()));
            return "std::unique_ptr<" + *inner_or + ">";
          },
          [&](const mir::VectorType& v) -> diag::Result<std::string> {
            auto inner_or = RenderTypeAsCpp(unit, owner_scope, v.element);
            if (!inner_or) return std::unexpected(std::move(inner_or.error()));
            return "std::vector<" + *inner_or + ">";
          },
          [](const auto&) -> diag::Result<std::string> {
            throw InternalError(
                "RenderTypeAsCpp: unsupported MIR type for current C++ render "
                "cut");
          },
      },
      unit.GetType(type_id).data);
}

}  // namespace lyra::backend::cpp

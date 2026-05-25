#include "lyra/backend/cpp/render_type.hpp"

#include <string>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/kind.hpp"
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
          [](const mir::PackedArrayType& p) -> diag::Result<std::string> {
            // TODO(hankhsu): `int`/`integer` still emit as `std::int32_t` so
            // the surrounding native expression path keeps compiling.
            // Migrating them to `PackedArray` requires routing every native
            // expression form through PackedArray ops; tracked under the
            // integral redirect cut plan.
            if (p.form == mir::PackedArrayForm::kInt ||
                p.form == mir::PackedArrayForm::kInteger) {
              return std::string{"std::int32_t"};
            }
            if (p.form == mir::PackedArrayForm::kExplicit) {
              return std::string{"lyra::value::PackedArray"};
            }
            return diag::Unsupported(
                diag::DiagCode::kCppEmitPackedRuntimeNotSupported,
                "this packed array form is not yet supported in cpp emit",
                diag::UnsupportedCategory::kFeature);
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

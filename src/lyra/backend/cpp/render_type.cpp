#include "render_type.hpp"

#include <string>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

auto RenderTypeAsCpp(
    const mir::CompilationUnit& unit, const mir::ClassDecl& owner_class,
    mir::TypeId type_id) -> std::string {
  return std::visit(
      Overloaded{
          [](const mir::PackedArrayType& p) -> std::string {
            if (p.form == mir::PackedArrayForm::kInt ||
                p.form == mir::PackedArrayForm::kInteger) {
              return "std::int32_t";
            }
            throw InternalError(
                "RenderTypeAsCpp: unsupported MIR type for current C++ render "
                "cut");
          },
          [](const mir::StringType&) -> std::string { return "std::string"; },
          [&](const mir::ObjectType& o) -> std::string {
            return {owner_class.GetClass(o.target).Name()};
          },
          [&](const mir::OwningPtrType& o) -> std::string {
            return "std::unique_ptr<" +
                   RenderTypeAsCpp(unit, owner_class, o.pointee) + ">";
          },
          [&](const mir::VectorType& v) -> std::string {
            return "std::vector<" +
                   RenderTypeAsCpp(unit, owner_class, v.element) + ">";
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

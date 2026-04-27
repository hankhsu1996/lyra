#include "render_type.hpp"

#include <string>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

auto RenderTypeAsCpp(const mir::CompilationUnit& unit, mir::TypeId type_id)
    -> std::string {
  return std::visit(
      Overloaded{
          [](const mir::PackedArrayType& p) -> std::string {
            if (p.form == mir::PackedArrayForm::kInt) {
              return "std::int32_t";
            }
            throw InternalError(
                "RenderTypeAsCpp: unsupported MIR type for current C++ render "
                "cut");
          },
          [](const mir::StringType&) -> std::string { return "std::string"; },
          [](const auto&) -> std::string {
            throw InternalError(
                "RenderTypeAsCpp: unsupported MIR type for current C++ render "
                "cut");
          },
      },
      unit.GetType(type_id).data);
}

}  // namespace lyra::backend::cpp

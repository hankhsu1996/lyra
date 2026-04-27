#include "render_type.hpp"

#include <string>
#include <variant>

#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/internal_error.hpp"
#include "lyra/support/overloaded.hpp"

namespace lyra::backend::cpp {

auto RenderTypeAsCpp(const mir::CompilationUnit& unit, mir::TypeId type_id)
    -> std::string {
  return std::visit(
      support::Overloaded{
          [](const mir::PackedArrayType& p) -> std::string {
            if (p.form == mir::PackedArrayForm::kInt) {
              return "std::int32_t";
            }
            throw support::InternalError(
                "RenderTypeAsCpp: unsupported MIR type for current C++ render "
                "cut");
          },
          [](const auto&) -> std::string {
            throw support::InternalError(
                "RenderTypeAsCpp: unsupported MIR type for current C++ render "
                "cut");
          },
      },
      unit.GetType(type_id).data);
}

}  // namespace lyra::backend::cpp

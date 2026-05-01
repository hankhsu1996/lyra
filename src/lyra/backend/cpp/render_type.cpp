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

auto RenderTypeAsCpp(
    const mir::CompilationUnit& unit, const mir::StructuralScope& owner_scope,
    mir::TypeId type_id) -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
          [](const mir::PackedArrayType& p) -> diag::Result<std::string> {
            // `int`/`integer` retain native std::int32_t because current
            // expression lowering uses raw C++ operators; switching them to
            // a runtime packed type requires operator overloads that belong
            // to the erased packed storage model. `integer`'s 4-state
            // semantics are not blessed correct here -- the mapping is
            // preserved only to keep current tests green; runtime typing of
            // `integer` is unresolved.
            if (p.form == mir::PackedArrayForm::kInt ||
                p.form == mir::PackedArrayForm::kInteger) {
              return std::string{"std::int32_t"};
            }
            if (p.form == mir::PackedArrayForm::kExplicit) {
              if (p.atom == mir::BitAtom::kBit) {
                return std::string{"lyra::runtime::BitValue"};
              }
              if (p.atom == mir::BitAtom::kLogic ||
                  p.atom == mir::BitAtom::kReg) {
                return std::string{"lyra::runtime::LogicValue"};
              }
              throw InternalError(
                  "RenderTypeAsCpp: unknown BitAtom for kExplicit packed type");
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

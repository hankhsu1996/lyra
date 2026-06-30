#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"

#include <cstddef>
#include <cstdint>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lowering/mir_to_lir/function_lowerer.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/method.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::mir_to_lir {

auto UnitLowerer::Run() -> diag::Result<lir::CompilationUnit> {
  for (std::size_t i = 0; i < mir_->classes.size(); ++i) {
    const mir::ClassId id{static_cast<std::uint32_t>(i)};
    if (!mir_->classes.IsDefined(id)) {
      return diag::Fail(
          diag::DiagCode::kUnsupportedTypeKind,
          "mir_to_lir: undefined class in unit");
    }
    auto cls = LowerClass(
        lir::ClassId{static_cast<std::uint32_t>(i)}, mir_->GetClass(id));
    if (!cls) {
      return std::unexpected(std::move(cls.error()));
    }
    const lir::ClassId added = out_.classes.Add(*std::move(cls));
    if (id.value == mir_->root.value) {
      out_.root = added;
    }
  }
  return std::move(out_);
}

auto UnitLowerer::LowerClass(lir::ClassId class_id, const mir::Class& cls)
    -> diag::Result<lir::Class> {
  lir::Class out;
  out.name = cls.name;
  if (cls.base.has_value()) {
    out.base = LowerBase(*cls.base);
  }

  auto constructor = FunctionLowerer(
                         *this, cls.constructor_block, {mir::LocalId{0}},
                         "constructor", mir_->builtins.void_type, class_id)
                         .Run();
  if (!constructor) {
    return std::unexpected(std::move(constructor.error()));
  }
  out.constructor = *std::move(constructor);

  for (std::size_t i = 0; i < cls.methods.size(); ++i) {
    const mir::MethodDecl& method =
        cls.methods.Get(mir::MethodId{static_cast<std::uint32_t>(i)});
    auto fn = FunctionLowerer(
                  *this, method.code.body, method.code.params, method.name,
                  method.code.result_type, class_id)
                  .Run();
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    out.methods.push_back(*std::move(fn));
  }
  return out;
}

auto UnitLowerer::LowerBase(const mir::ClassRef& base) -> lir::Base {
  return std::visit(
      Overloaded{[&](const mir::RuntimeLibraryClassRef& r) -> lir::Base {
        const mir::Type& base_type = mir_->types.Get(r.base_type);
        return std::visit(
            Overloaded{
                [](const mir::InstanceType&) -> lir::Base {
                  return lir::Base{lir::RuntimeLibraryBase{
                      .kind = lir::RuntimeBaseKind::kInstance}};
                },
                [](const mir::GenScopeType&) -> lir::Base {
                  return lir::Base{lir::RuntimeLibraryBase{
                      .kind = lir::RuntimeBaseKind::kGenScope}};
                },
                [](const mir::ScopeType&) -> lir::Base {
                  return lir::Base{lir::RuntimeLibraryBase{
                      .kind = lir::RuntimeBaseKind::kScope}};
                },
                [](const auto&) -> lir::Base {
                  throw InternalError(
                      "mir_to_lir: runtime base is not a tree-node base type");
                }},
            base_type.data);
      }},
      base);
}

}  // namespace lyra::lowering::mir_to_lir

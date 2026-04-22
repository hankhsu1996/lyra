#include <gtest/gtest.h>

#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/dpi_name_checks.hpp"
#include "lyra/lowering/hir_to_mir/dpi_registry.hpp"
#include "lyra/mir/call.hpp"

namespace lyra::lowering::hir_to_mir {
namespace {

// Build a minimal DpiSignature for testing (void return, no params).
auto MakeVoidSignature() -> mir::DpiSignature {
  return mir::DpiSignature{
      .result =
          mir::DpiReturnDesc{
              .sv_type = TypeId{0},
              .abi_type = DpiAbiTypeClass::kVoid,
              .kind = mir::DpiReturnKind::kVoid,
          },
      .params = {},
  };
}

TEST(DpiVisibleNameCollision, NoCollisionWhenNamesDistinct) {
  DesignDeclarations decls;

  DpiExportInfo exp{
      .symbol = SymbolId{1},
      .span = SourceSpan{},
      .c_name = "export_fn",
      .signature = MakeVoidSignature(),
  };
  decls.dpi_exports.Insert(std::move(exp));

  DpiImportInfo imp{
      .symbol = SymbolId{2},
      .span = SourceSpan{},
      .sv_name = "import_fn",
      .c_name = "import_fn",
      .return_type_id = TypeId{0},
      .return_abi_type = DpiAbiTypeClass::kVoid,
      .params = {},
  };
  decls.dpi_imports.Insert(std::move(imp));

  CheckDpiVisibleNameCollisions(decls);
  EXPECT_TRUE(decls.export_diagnostics.empty());
}

TEST(DpiVisibleNameCollision, DetectsImportExportCollision) {
  DesignDeclarations decls;

  DpiExportInfo exp{
      .symbol = SymbolId{1},
      .span = SourceSpan{},
      .c_name = "foo",
      .signature = MakeVoidSignature(),
  };
  decls.dpi_exports.Insert(std::move(exp));

  DpiImportInfo imp{
      .symbol = SymbolId{2},
      .span = SourceSpan{},
      .sv_name = "foo",
      .c_name = "foo",
      .return_type_id = TypeId{0},
      .return_abi_type = DpiAbiTypeClass::kVoid,
      .params = {},
  };
  decls.dpi_imports.Insert(std::move(imp));

  CheckDpiVisibleNameCollisions(decls);
  ASSERT_EQ(decls.export_diagnostics.size(), 1);
  EXPECT_NE(
      decls.export_diagnostics[0].message.find("collision"), std::string::npos);
  EXPECT_NE(decls.export_diagnostics[0].message.find("foo"), std::string::npos);
}

TEST(DpiVisibleNameCollision, SameSymbolNoCollision) {
  DesignDeclarations decls;

  // Same symbol ID for both import and export -- not a collision.
  DpiExportInfo exp{
      .symbol = SymbolId{1},
      .span = SourceSpan{},
      .c_name = "foo",
      .signature = MakeVoidSignature(),
  };
  decls.dpi_exports.Insert(std::move(exp));

  DpiImportInfo imp{
      .symbol = SymbolId{1},
      .span = SourceSpan{},
      .sv_name = "foo",
      .c_name = "foo",
      .return_type_id = TypeId{0},
      .return_abi_type = DpiAbiTypeClass::kVoid,
      .params = {},
  };
  decls.dpi_imports.Insert(std::move(imp));

  CheckDpiVisibleNameCollisions(decls);
  EXPECT_TRUE(decls.export_diagnostics.empty());
}

}  // namespace
}  // namespace lyra::lowering::hir_to_mir

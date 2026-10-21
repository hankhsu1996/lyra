// The front end declares the classes of `std` and checks every call and every
// class extending one against that declaration, while Lyra's own text
// defines them, so each definition has to be what its declaration says: the
// same methods, each a task or a function as declared, with the same return
// type and the same arguments -- name, direction, type and default. A compiler
// checks a definition against its declaration the same way; nothing a design
// runs would notice a mismatch until a call through it went wrong.

#include <filesystem>
#include <fstream>
#include <gtest/gtest.h>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <slang/ast/ASTContext.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/EvalContext.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Lookup.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/driver/Driver.h>

#include "lyra/compiler/compile.hpp"
#include "lyra/frontend/load.hpp"
#include "lyra/lowering/ast_to_hir/std_classes.hpp"

namespace {

using lyra::lowering::ast_to_hir::DefineStdClasses;

// A design naming every class Lyra's `std` definitions hold, so the front end
// makes a specialization of each generic one.
constexpr std::string_view kNamesEveryClass = R"(
module Top;
  semaphore s;
  mailbox #(int) m;
  mailbox any;
endmodule
)";

class Elaborated {
 public:
  explicit Elaborated(std::string_view source) {
    const std::filesystem::path path =
        std::filesystem::temp_directory_path() /
        std::filesystem::path{
            ::testing::UnitTest::GetInstance()->current_test_info()->name()}
            .concat(".sv");
    {
      std::ofstream out(path);
      out << source;
    }
    driver_.addStandardArgs();
    const std::string path_text = path.string();
    const std::vector<const char*> args{"lyra", path_text.c_str()};
    EXPECT_TRUE(
        driver_.parseCommandLine(static_cast<int>(args.size()), args.data()));
    lyra::compiler::FrontEndResult front = lyra::compiler::RunFrontEnd(driver_);
    EXPECT_TRUE(front.elaborated.has_value())
        << "the front end rejected the probe design: " << front.diagnostics;
    design_ = std::move(front.elaborated);
  }

  [[nodiscard]] auto Compilation() const -> slang::ast::Compilation& {
    return *design_->compilation;
  }

 private:
  slang::driver::Driver driver_;
  std::optional<lyra::frontend::ParseResult> design_;
};

// The value an argument's default evaluates to, or nothing where it has none.
auto DefaultOf(const slang::ast::FormalArgumentSymbol& arg)
    -> std::optional<std::string> {
  const slang::ast::Expression* value = arg.getDefaultValue();
  if (value == nullptr) return std::nullopt;
  slang::ast::EvalContext context(
      slang::ast::ASTContext(
          *arg.getParentScope(), slang::ast::LookupLocation::max));
  return value->eval(context).toString();
}

void ExpectDefinedAsDeclared(
    const slang::ast::ClassType& declared,
    const slang::ast::ClassType& defined) {
  int methods = 0;
  for (const auto& sub :
       declared.membersOfType<slang::ast::SubroutineSymbol>()) {
    ++methods;
    SCOPED_TRACE(std::string(declared.name) + "::" + std::string(sub.name));
    const slang::ast::Symbol* found = defined.find(sub.name);
    ASSERT_NE(found, nullptr) << "Lyra's std definitions do not define it";
    const auto* def = found->as_if<slang::ast::SubroutineSymbol>();
    ASSERT_NE(def, nullptr) << "Lyra's std definitions define it as no method";
    EXPECT_EQ(def->subroutineKind, sub.subroutineKind);
    EXPECT_TRUE(def->getReturnType().isMatching(sub.getReturnType()))
        << def->getReturnType().toString() << " against "
        << sub.getReturnType().toString();
    ASSERT_EQ(def->getArguments().size(), sub.getArguments().size());
    for (std::size_t i = 0; i < sub.getArguments().size(); ++i) {
      const auto& want = *sub.getArguments()[i];
      const auto& got = *def->getArguments()[i];
      SCOPED_TRACE(std::string(want.name));
      EXPECT_EQ(got.name, want.name);
      EXPECT_EQ(got.direction, want.direction);
      EXPECT_TRUE(got.getType().isMatching(want.getType()))
          << got.getType().toString() << " against "
          << want.getType().toString();
      EXPECT_EQ(DefaultOf(got), DefaultOf(want));
    }
  }
  EXPECT_GT(methods, 0) << "the front end declares no method of "
                        << declared.name;
  // Nothing a design may call is defined without being declared. The methods
  // every class has (LRM 18.6, 18.8, 18.13) come with the definition and are
  // no part of it.
  for (const auto& sub :
       defined.membersOfType<slang::ast::SubroutineSymbol>()) {
    if (sub.visibility == slang::ast::Visibility::Local ||
        sub.flags.has(slang::ast::MethodFlags::BuiltIn)) {
      continue;
    }
    EXPECT_NE(declared.find(sub.name), nullptr)
        << "Lyra's std definitions define '" << defined.name << "::" << sub.name
        << "', which the front end does not declare";
  }
}

TEST(StdDefinitions, EveryDefinitionIsWhatItsDeclarationSays) {
  const Elaborated design(kNamesEveryClass);
  const auto defined = DefineStdClasses(design.Compilation());
  // `semaphore`, and the two specializations of `mailbox` the design names.
  EXPECT_EQ(defined.size(), 3U);
  for (const auto& [declared, definition] : defined) {
    ExpectDefinedAsDeclared(*declared, *definition);
  }
}

}  // namespace

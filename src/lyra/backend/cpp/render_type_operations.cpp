#include "lyra/backend/cpp/render_type_operations.hpp"

#include <algorithm>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/backend/cpp/formatting.hpp"
#include "lyra/backend/cpp/render_decl.hpp"
#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/backend/cpp/type_operation_bodies.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/canonical_type_name.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/type_interface.hpp"
#include "lyra/stable_hash/fingerprint.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/value_operation.hpp"
#include "lyra/support/value_step.hpp"

namespace lyra::backend::cpp {

namespace {

auto HashOf(std::string_view text) -> std::string {
  return stable_hash::HexOf(stable_hash::FingerprintOfText(text));
}

// What a type's name, and its implementation's struct and files, are named
// from: its name as MIR states it, the same in every unit naming the type, so
// every unit writes the same files under the same names and the project keeps
// one of each.
auto IdentityOf(const mir::CompilationUnit& unit, mir::TypeId type)
    -> std::string {
  return HashOf(mir::CanonicalTypeName(unit, type));
}

// How the operation reads in a name: its stable spelling, and a word for an
// operator, whose token no name holds.
auto OperationWord(support::ValueOperation answers) -> std::string {
  return std::visit(
      Overloaded{
          [](support::EqualityItem item) -> std::string {
            switch (item) {
              case support::EqualityItem::kEqual:
                return "equality";
              case support::EqualityItem::kNotEqual:
                return "inequality";
              case support::EqualityItem::kCaseEqual:
                return "case_equal";
            }
            throw InternalError("backend::cpp: unknown equality item");
          },
          [&](support::LibraryAnsweredItem) {
            return std::string(support::ValueOperationName(answers));
          },
          [&](support::BoundFreeItem) {
            return std::string(support::ValueOperationName(answers));
          },
          [&](support::BuiltinFn) {
            return std::string(support::ValueOperationName(answers));
          },
          [&](support::ValueStep) {
            return std::string(support::ValueOperationName(answers));
          }},
      answers);
}

}  // namespace

auto TypeSymbolName(const mir::CompilationUnit& unit, mir::TypeId type)
    -> std::string {
  return "sv_type_" + IdentityOf(unit, type);
}

auto ImplStructName(const mir::CompilationUnit& unit, mir::TypeId type)
    -> std::string {
  return TypeSymbolName(unit, type) + "_impl";
}

auto ImplHeaderFile(const mir::CompilationUnit& unit, mir::TypeId type)
    -> std::string {
  return "impl." + IdentityOf(unit, type) + ".hpp";
}

auto ImplCodeFile(const mir::CompilationUnit& unit, mir::TypeId type)
    -> std::string {
  return "impl." + IdentityOf(unit, type) + ".cpp";
}

auto IsImplItem(
    const mir::CompilationUnit& unit, const mir::TypeOperationSource& source)
    -> bool {
  if (!source.arguments.empty()) {
    return false;
  }
  const std::optional<mir::Impl> impl = mir::ImplOf(unit, source.type);
  return impl.has_value() &&
         std::ranges::contains(impl->Items(), source.answers);
}

auto IsNonMemberBody(
    const mir::CompilationUnit& unit, const mir::TypeOperationSource& source)
    -> bool {
  const std::optional<mir::TypeOperationKind> kind =
      mir::TypeOperationKindOf(unit, source.type, source.answers);
  if (!kind.has_value()) {
    return false;
  }
  switch (*kind) {
    case mir::TypeOperationKind::kContainer:
      return !IsImplItem(unit, source);
    case mir::TypeOperationKind::kToBoundFree:
    case mir::TypeOperationKind::kFromBoundFree:
    case mir::TypeOperationKind::kStructural:
      return true;
    case mir::TypeOperationKind::kDefault:
      return false;
  }
  throw InternalError("backend::cpp: unknown kind of type operation");
}

auto TypeFunctionName(
    const mir::CompilationUnit& unit, const mir::TypeOperationSource& source)
    -> std::string {
  std::string name =
      TypeSymbolName(unit, source.type) + "_" + OperationWord(source.answers);
  if (!source.arguments.empty()) {
    std::string arguments;
    for (const mir::TypeId argument : source.arguments) {
      arguments += mir::CanonicalTypeName(unit, argument);
    }
    name += "_" + HashOf(arguments);
  }
  return name;
}

auto RenderTypeOperations(
    const mir::CompilationUnit& unit, const TypeOperationBodies& bodies,
    UnitRenderReport& report) -> TypeOperationTexts {
  // A file's free functions are its own, each declared ahead of every body
  // that may call it and defined after them.
  const auto functions = [&](std::span<const mir::TypeOperationBody> defined,
                             TargetText& declarations,
                             TargetText& definitions) {
    TargetText declared;
    TargetText written;
    for (const mir::TypeOperationBody& body : defined) {
      const std::string name = TypeFunctionName(unit, body.source);
      RenderFreeFunctionDeclaration(unit, name, body.code, declared);
      RenderFreeFunctionDefinition(unit, report, name, body.code, written);
    }
    AppendSectionInUnnamedNamespace(declarations, declared);
    AppendSectionInUnnamedNamespace(definitions, written);
  };
  TypeOperationTexts texts;
  for (const ImplBodies& of : bodies.impls) {
    const mir::TypeId type = of.self_type;
    texts.headers.push_back(
        TypeOperationFileText{
            .relpath = ImplHeaderFile(unit, type), .text = [&] {
              TargetText declared;
              RenderImpl(unit, type, of.items, declared);
              return declared;
            }()});
    // Each item is defined once in the program, in the type's code file,
    // which every unit naming the type writes alike, so a file using one
    // needs only the header.
    TargetText code;
    code.Require(ImplHeaderFile(unit, type));
    TargetText definitions;
    functions(of.functions, code, definitions);
    for (const mir::TypeOperationBody& item : of.items) {
      RenderImplItemDefinition(unit, report, item, code);
    }
    code += definitions;
    texts.code_files.push_back(
        TypeOperationFileText{
            .relpath = ImplCodeFile(unit, type), .text = std::move(code)});
  }
  functions(bodies.functions, texts.declarations, texts.definitions);
  return texts;
}

}  // namespace lyra::backend::cpp

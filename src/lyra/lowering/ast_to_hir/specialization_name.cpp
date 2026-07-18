#include "lyra/lowering/ast_to_hir/specialization_name.hpp"

#include <cstdint>
#include <format>
#include <string>
#include <string_view>

#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/types/DeclaredType.h>
#include <slang/ast/types/Type.h>
#include <slang/numeric/ConstantValue.h>

namespace lyra::lowering::ast_to_hir {

namespace {

// FNV-1a, so producer and consumer agree on the name across separately
// compiled units and across sessions. A process-seeded or pointer-derived
// hash would not be reproducible across runs; this folds only the bytes.
auto Fnv1a64(std::string_view bytes) -> std::uint64_t {
  std::uint64_t hash = 0xcbf29ce484222325ULL;
  for (const unsigned char byte : bytes) {
    hash ^= byte;
    hash *= 0x100000001b3ULL;
  }
  return hash;
}

// Position-free encoding of one binding: the parameter name, then its resolved
// value or resolved type, on the same value/type split slang uses to decide
// canonical-body equivalence (ParameterSymbolBase::allMatching). The parameter
// arrives as its concrete Symbol so this serves both spaces slang exposes
// bindings through -- a module body's ParameterSymbolBase span (`.symbol` per
// entry) and a class specialization's genericParameters (a Symbol span).
void EncodeBinding(const slang::ast::Symbol& symbol, std::string& out) {
  out += symbol.name;
  out += '=';
  if (symbol.kind == slang::ast::SymbolKind::Parameter) {
    out += symbol.as<slang::ast::ParameterSymbol>().getValue().toString();
  } else {
    out += symbol.as<slang::ast::TypeParameterSymbol>()
               .targetType.getType()
               .toString();
  }
  out += ';';
}

}  // namespace

auto SpecializationName(const slang::ast::InstanceBodySymbol& body)
    -> std::string {
  std::string name{body.getDefinition().name};
  const auto params = body.getParameters();
  if (params.empty()) {
    return name;
  }
  std::string encoding;
  for (const auto* param : params) {
    EncodeBinding(param->symbol, encoding);
  }
  return std::format("{}__{:016x}", name, Fnv1a64(encoding));
}

auto SpecializationName(const slang::ast::InstanceSymbol& inst) -> std::string {
  const auto* canonical = inst.getCanonicalBody();
  return SpecializationName(canonical != nullptr ? *canonical : inst.body);
}

auto SpecializationName(const slang::ast::ClassType& cls) -> std::string {
  std::string name{cls.name};
  if (cls.genericClass == nullptr) {
    return name;
  }
  std::string encoding;
  for (const auto* sym : cls.genericParameters) {
    EncodeBinding(*sym, encoding);
  }
  return std::format("{}__{:016x}", name, Fnv1a64(encoding));
}

}  // namespace lyra::lowering::ast_to_hir

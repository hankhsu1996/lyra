#include "lyra/lowering/ast_to_hir/specialization_name.hpp"

#include <algorithm>
#include <cstdint>
#include <format>
#include <string>
#include <string_view>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/types/DeclaredType.h>
#include <slang/ast/types/Type.h>
#include <slang/numeric/ConstantValue.h>
#include <slang/text/SourceManager.h>

#include "lyra/base/internal_error.hpp"

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

// The generate blocks (LRM 27.6) between a declaration and the compilation unit
// that owns it, outermost first. Each is a declaration scope of its own, so two
// of them may declare the same class name; the path is what tells those
// declarations apart in a name space that has no nesting of its own.
auto DeclaringBlockPath(const slang::ast::Symbol& decl)
    -> std::vector<std::string_view> {
  std::vector<std::string_view> path;
  for (const slang::ast::Scope* scope = decl.getParentScope(); scope != nullptr;
       scope = scope->asSymbol().getParentScope()) {
    const slang::ast::Symbol& sym = scope->asSymbol();
    if (sym.kind == slang::ast::SymbolKind::GenerateBlock) {
      path.push_back(sym.name);
    }
  }
  std::ranges::reverse(path);
  return path;
}

auto SpecializationName(const slang::ast::ClassType& cls) -> std::string {
  // A class carries one identifier through compilation, and a compilation unit
  // holds every class it declares in one flat name space, so the identifier has
  // to be unique there. The source name alone is not: SystemVerilog scopes the
  // declaration, so sibling generate blocks may each declare the same one.
  std::string name;
  for (const std::string_view block : DeclaringBlockPath(cls)) {
    name += block;
    name += '_';
  }
  name += cls.name;
  if (cls.genericClass == nullptr) {
    return name;
  }
  std::string encoding;
  for (const auto* sym : cls.genericParameters) {
    EncodeBinding(*sym, encoding);
  }
  return std::format("{}__{:016x}", name, Fnv1a64(encoding));
}

auto CompilationUnitName(const slang::ast::Symbol& unit) -> std::string {
  using slang::ast::SymbolKind;
  if (unit.kind == SymbolKind::Package) {
    return std::string(unit.name);
  }
  if (unit.kind == SymbolKind::InstanceBody) {
    return SpecializationName(unit.as<slang::ast::InstanceBodySymbol>());
  }
  if (unit.kind == SymbolKind::CompilationUnit) {
    // The anonymous $unit scope has no source name; its distinguishing identity
    // is the compilation-unit input it belongs to, named by the source buffer
    // its declarations live in. Folding the resolved path yields a name stable
    // across edits to the scope's body (the property a module's specialization
    // name has) and distinct per input, with no shared table. The scope symbol
    // itself carries no source location, but every member of one input shares
    // its buffer, so the first located member names it.
    const auto& cu = unit.as<slang::ast::CompilationUnitSymbol>();
    const slang::SourceManager* sources =
        cu.getCompilation().getSourceManager();
    if (sources == nullptr) {
      throw InternalError(
          "CompilationUnitName: compilation has no source manager");
    }
    for (const auto& member : cu.members()) {
      if (member.location.valid()) {
        const std::string path =
            sources->getFullPath(member.location.buffer()).string();
        return std::format("$unit__{:016x}", Fnv1a64(path));
      }
    }
    throw InternalError(
        "CompilationUnitName: compilation unit has no located member");
  }
  throw InternalError(
      "CompilationUnitName: symbol is not a package, module body, or "
      "compilation unit");
}

}  // namespace lyra::lowering::ast_to_hir

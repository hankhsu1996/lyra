#include "lyra/lowering/ast_to_hir/unit_identity.hpp"

#include <algorithm>
#include <cstdint>
#include <format>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/DeclaredType.h>
#include <slang/ast/types/Type.h>
#include <slang/numeric/ConstantValue.h>
#include <slang/text/SourceManager.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"

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

// One type's identity, as this compiler tells types apart. SystemVerilog
// identifies most types by their shape, and the frontend renders a shape
// faithfully, so those answer with that rendering. A class is the exception: it
// is identified by its declaration (LRM 8.3), so two with identical members are
// still two types, and its identity is the unit that declares it together with
// its own name -- the same pair every cross-unit reference to a class carries.
//
// A type built out of others answers with its own form over the identities of
// what it holds, so a class inside one is named the way it would be alone. Only
// an unpacked type can hold a class handle -- a packed type is a bit vector
// (LRM 7.4.1) and a handle is not packable -- so those are the forms spelled
// here, and every other kind's rendering is already its identity.
//
// Every arm answers with an identity rather than adding to one under
// construction, so a form this does not spell cannot contribute nothing and
// leave two types looking alike.
auto TypeIdentity(const slang::ast::Type& type) -> std::string {
  using slang::ast::SymbolKind;
  const slang::ast::Type& canonical = type.getCanonicalType();
  const auto field_identities = [](const slang::ast::Scope& scope) {
    std::string out{'{'};
    for (const auto& field : scope.members()) {
      if (const auto* value = field.as_if<slang::ast::ValueSymbol>()) {
        out +=
            std::format("{}:{};", value->name, TypeIdentity(value->getType()));
      }
    }
    return out + '}';
  };

  switch (canonical.kind) {
    case SymbolKind::ClassType: {
      const auto& cls = canonical.as<slang::ast::ClassType>();
      return std::format(
          "{}::{}", CompilationUnitName(DeclaringCompilationUnit(cls)),
          SpecializationName(cls));
    }
    case SymbolKind::FixedSizeUnpackedArrayType: {
      const auto& array =
          canonical.as<slang::ast::FixedSizeUnpackedArrayType>();
      return std::format(
          "[{}:{}]{}", array.range.left, array.range.right,
          TypeIdentity(array.elementType));
    }
    case SymbolKind::DynamicArrayType:
      return "[]" +
             TypeIdentity(
                 canonical.as<slang::ast::DynamicArrayType>().elementType);
    case SymbolKind::QueueType: {
      const auto& queue = canonical.as<slang::ast::QueueType>();
      return std::format(
          "[${}]{}", queue.maxBound, TypeIdentity(queue.elementType));
    }
    case SymbolKind::AssociativeArrayType: {
      const auto& assoc = canonical.as<slang::ast::AssociativeArrayType>();
      const std::string index = assoc.indexType == nullptr
                                    ? std::string{"*"}
                                    : TypeIdentity(*assoc.indexType);
      return std::format("[{}]{}", index, TypeIdentity(assoc.elementType));
    }
    case SymbolKind::UnpackedStructType:
      return "struct" +
             field_identities(canonical.as<slang::ast::UnpackedStructType>());
    case SymbolKind::UnpackedUnionType: {
      const auto& u = canonical.as<slang::ast::UnpackedUnionType>();
      return (u.isTagged ? "tagged union" : "union") + field_identities(u);
    }
    default:
      return canonical.toString();
  }
}

// What one parameter was fixed to, on the same value/type split slang uses to
// decide canonical-body equivalence (ParameterSymbolBase::allMatching). The
// parameter arrives as its concrete Symbol so this serves both spaces slang
// exposes bindings through -- a module body's ParameterSymbolBase span
// (`.symbol` per entry) and a class specialization's genericParameters (a
// Symbol span).
auto ParameterInput(const slang::ast::Symbol& symbol) -> SpecializationInput {
  if (symbol.kind == slang::ast::SymbolKind::Parameter) {
    return SpecializationInput{
        .name = std::string{symbol.name},
        .kind = FixedValue{
            .value = symbol.as<slang::ast::ParameterSymbol>()
                         .getValue()
                         .toString()}};
  }
  return SpecializationInput{
      .name = std::string{symbol.name},
      .kind = FixedType{
          .type = TypeIdentity(symbol.as<slang::ast::TypeParameterSymbol>()
                                   .targetType.getType())}};
}

// Which interface an interface port carries (LRM 25.3), named the way the unit
// that interface instantiates is. Everything reached through the port takes its
// types and positions from there, so two instantiations bound to different
// interfaces build different objects and are different units, exactly as two
// parameter bindings are. This is the same axis slang splits a module body on,
// so a body it did not share keys apart here. A modport narrows what the port
// reaches (LRM 25.5), so it belongs to the same answer.
auto InterfacePortInput(const slang::ast::InterfacePortSymbol& port)
    -> SpecializationInput {
  const auto [connected, modport] = port.getConnection();
  const auto* instance = connected == nullptr
                             ? nullptr
                             : connected->as_if<slang::ast::InstanceSymbol>();
  return SpecializationInput{
      .name = std::string{port.name},
      .kind = FixedInterface{
          .unit_name = instance == nullptr ? std::string{}
                                           : SpecializationName(*instance),
          .modport =
              modport == nullptr ? std::string{} : std::string{modport->name}}};
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

// The bytes a key folds to. Every part is written with its own delimiter, so
// two keys that differ anywhere differ here; nothing rests on this being read
// back, and nothing compares keys through it -- a key answers that itself.
auto KeyBytes(const SpecializationKey& key) -> std::string {
  std::string bytes;
  for (const SpecializationInput& input : key.inputs) {
    bytes += input.name;
    bytes += '=';
    bytes += std::visit(
        Overloaded{
            [](const FixedValue& v) { return v.value; },
            [](const FixedType& v) { return v.type; },
            [](const FixedInterface& v) {
              return v.modport.empty()
                         ? v.unit_name
                         : std::format("{}.{}", v.unit_name, v.modport);
            }},
        input.kind);
    bytes += ';';
  }
  return bytes;
}

}  // namespace

auto SpecializationKeyOf(const slang::ast::InstanceBodySymbol& body)
    -> SpecializationKey {
  SpecializationKey key{
      .definition = std::string{body.getDefinition().name}, .inputs = {}};
  for (const auto* param : body.getParameters()) {
    key.inputs.push_back(ParameterInput(param->symbol));
  }
  for (const slang::ast::Symbol* port : body.getPortList()) {
    if (port->kind == slang::ast::SymbolKind::InterfacePort) {
      key.inputs.push_back(
          InterfacePortInput(port->as<slang::ast::InterfacePortSymbol>()));
    }
  }
  return key;
}

auto SpecializationName(const SpecializationKey& key) -> std::string {
  if (key.inputs.empty()) {
    return key.definition;
  }
  return std::format("{}__{:016x}", key.definition, Fnv1a64(KeyBytes(key)));
}

auto SpecializationName(const slang::ast::InstanceBodySymbol& body)
    -> std::string {
  return SpecializationName(SpecializationKeyOf(body));
}

auto SpecializationName(const slang::ast::InstanceSymbol& inst) -> std::string {
  const auto* canonical = inst.getCanonicalBody();
  return SpecializationName(canonical != nullptr ? *canonical : inst.body);
}

auto SpecializationKeyOf(const slang::ast::ClassType& cls)
    -> SpecializationKey {
  // A class carries one identifier through compilation, and a compilation unit
  // holds every class it declares in one flat name space, so the identifier has
  // to be unique there. The source name alone is not: SystemVerilog scopes the
  // declaration, so sibling generate blocks may each declare the same one.
  std::string definition;
  for (const std::string_view block : DeclaringBlockPath(cls)) {
    definition += block;
    definition += '_';
  }
  definition += cls.name;

  SpecializationKey key{.definition = std::move(definition), .inputs = {}};
  if (cls.genericClass == nullptr) {
    return key;
  }
  for (const auto* sym : cls.genericParameters) {
    key.inputs.push_back(ParameterInput(*sym));
  }
  return key;
}

auto SpecializationName(const slang::ast::ClassType& cls) -> std::string {
  return SpecializationName(SpecializationKeyOf(cls));
}

auto DeclaringCompilationUnit(const slang::ast::Symbol& decl)
    -> const slang::ast::Symbol& {
  for (const slang::ast::Scope* scope = decl.getParentScope(); scope != nullptr;
       scope = scope->asSymbol().getParentScope()) {
    const slang::ast::Symbol& owner = scope->asSymbol();
    if (owner.kind == slang::ast::SymbolKind::Package ||
        owner.kind == slang::ast::SymbolKind::InstanceBody ||
        owner.kind == slang::ast::SymbolKind::CompilationUnit) {
      return owner;
    }
  }
  throw InternalError(
      "DeclaringCompilationUnit: every declaration lies in a package, a design "
      "element's body, or the file-set scope");
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

#include "lyra/lowering/ast_to_hir/unit_identity.hpp"

#include <algorithm>
#include <bit>
#include <cstdint>
#include <format>
#include <limits>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
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
#include "lyra/lowering/ast_to_hir/connected_interface.hpp"

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
auto TypeIdentity(
    const slang::ast::Type& type, const SpecializationPolicy& policy)
    -> std::string {
  using slang::ast::SymbolKind;
  const slang::ast::Type& canonical = type.getCanonicalType();
  const auto field_identities = [&](const slang::ast::Scope& scope) {
    std::string out{'{'};
    for (const auto& field : scope.members()) {
      if (const auto* value = field.as_if<slang::ast::ValueSymbol>()) {
        out += std::format(
            "{}:{};", value->name, TypeIdentity(value->getType(), policy));
      }
    }
    return out + '}';
  };

  switch (canonical.kind) {
    case SymbolKind::ClassType: {
      const auto& cls = canonical.as<slang::ast::ClassType>();
      return std::format(
          "{}::{}", CompilationUnitName(DeclaringCompilationUnit(cls), policy),
          SpecializationName(cls, policy));
    }
    case SymbolKind::FixedSizeUnpackedArrayType: {
      const auto& array =
          canonical.as<slang::ast::FixedSizeUnpackedArrayType>();
      return std::format(
          "[{}:{}]{}", array.range.left, array.range.right,
          TypeIdentity(array.elementType, policy));
    }
    case SymbolKind::DynamicArrayType:
      return "[]" +
             TypeIdentity(
                 canonical.as<slang::ast::DynamicArrayType>().elementType,
                 policy);
    case SymbolKind::QueueType: {
      const auto& queue = canonical.as<slang::ast::QueueType>();
      return std::format(
          "[${}]{}", queue.maxBound, TypeIdentity(queue.elementType, policy));
    }
    case SymbolKind::AssociativeArrayType: {
      const auto& assoc = canonical.as<slang::ast::AssociativeArrayType>();
      const std::string index = assoc.indexType == nullptr
                                    ? std::string{"*"}
                                    : TypeIdentity(*assoc.indexType, policy);
      return std::format(
          "[{}]{}", index, TypeIdentity(assoc.elementType, policy));
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

}  // namespace

// Every spelling ends where it can be seen to end, so one joined into an
// aggregate cannot run into its neighbour. A real is spelled by its bits,
// because a decimal rendering spells every NaN alike; a string by its length
// and its text, because a quote and a comma are ordinary characters in one; an
// aggregate through its elements, so each of those holds inside it too.
auto ValueIdentity(const slang::ConstantValue& value) -> std::string {
  if (value.isString()) {
    return std::format("t{}:{}", value.str().size(), value.str());
  }
  if (value.isReal()) {
    return std::format(
        "r{:016x}", std::bit_cast<std::uint64_t>(double{value.real()}));
  }
  if (value.isShortReal()) {
    return std::format(
        "s{:08x}", std::bit_cast<std::uint32_t>(float{value.shortReal()}));
  }
  const auto elements = [](const auto& values) {
    std::string out{'['};
    for (const slang::ConstantValue& element : values) {
      out += ValueIdentity(element);
      out += ',';
    }
    return out + ']';
  };
  if (value.isUnpacked()) {
    return elements(value.elements());
  }
  if (value.isQueue()) {
    return "q" + elements(*value.queue());
  }
  if (value.isMap()) {
    std::string out{"m["};
    for (const auto& [key, element] : *value.map()) {
      out += std::format("{}:{},", ValueIdentity(key), ValueIdentity(element));
    }
    if (value.map()->defaultValue) {
      out += "default:" + ValueIdentity(value.map()->defaultValue);
    }
    return out + ']';
  }
  if (value.isUnion()) {
    const auto& u = *value.unionVal();
    return u.activeMember.has_value()
               ? std::format("u{}:{}", *u.activeMember, ValueIdentity(u.value))
               : std::string{"u-"};
  }
  constexpr slang::bitwidth_t kNeverShortened =
      std::numeric_limits<slang::bitwidth_t>::max();
  constexpr bool kExactUnknowns = true;
  return value.toString(kNeverShortened, kExactUnknowns);
}

namespace {

// What one parameter was fixed to. A parameter is fixed to a value or to a type
// (LRM 6.20.2, 6.20.3), and that is the whole of the split. The parameter
// arrives as its concrete Symbol so this serves both spaces the frontend
// exposes bindings through -- a module body's parameter list and a class
// specialization's generic parameters.
//
// Reading the settled value is not a choice about where it enters the artifact
// here: what the value varies with is the specialization, and the
// specialization is what this computes.
auto ParameterInput(
    const slang::ast::Symbol& symbol, const SpecializationPolicy& policy)
    -> SpecializationInput {
  if (symbol.kind == slang::ast::SymbolKind::Parameter) {
    return SpecializationInput{
        .name = std::string{symbol.name},
        .kind = FixedValue{
            .value = ValueIdentity(
                symbol.as<slang::ast::ParameterSymbol>().getValue())}};
  }
  return SpecializationInput{
      .name = std::string{symbol.name},
      .kind = FixedType{
          .type = TypeIdentity(
              symbol.as<slang::ast::TypeParameterSymbol>().targetType.getType(),
              policy)}};
}

// Which interface an interface port carries (LRM 25.3), named the way the unit
// that interface instantiates is. Everything reached through the port takes its
// types and positions from there, so two instantiations bound to different
// interfaces build different objects and are different units, exactly as two
// parameter bindings are. A modport belongs to the same answer, and not merely
// because it narrows: a view also names things of its own (LRM 25.5.4), and two
// views may give one name different storage, so a unit bound through each
// reaches a different place under the same spelling.
auto InterfacePortInput(
    const slang::ast::PortConnection& connection,
    const SpecializationPolicy& policy) -> SpecializationInput {
  const auto [instance, modport] =
      ConnectedInterfaceOf(connection.getIfaceConn());
  return SpecializationInput{
      .name = std::string{connection.port.name},
      .kind = FixedInterface{
          .unit_name = instance == nullptr
                           ? std::string{}
                           : SpecializationName(*instance, policy),
          .modport =
              modport == nullptr ? std::string{} : std::string{modport->name}}};
}

// The generate blocks (LRM 27.6) between a declaration and the compilation unit
// that owns it, outermost first. Each is a declaration scope of its own, so two
// of them may declare the same class name; the path is what tells those
// declarations apart in a name space that has no nesting of its own.
//
// A block is spelled the way the hierarchy spells it, which is what makes the
// path tell two of them apart. An `if` or `case` arm answers to its own label;
// a loop's block carries no label of its own and answers to the construct's
// label together with the index it elaborated at (LRM 27.4), so both halves are
// needed for it.
auto DeclaringBlockPath(const slang::ast::Symbol& decl)
    -> std::vector<std::string> {
  std::vector<std::string> path;
  for (const slang::ast::Scope* scope = decl.getParentScope(); scope != nullptr;
       scope = scope->asSymbol().getParentScope()) {
    const slang::ast::Symbol& sym = scope->asSymbol();
    if (sym.kind != slang::ast::SymbolKind::GenerateBlock) {
      continue;
    }
    const auto& block = sym.as<slang::ast::GenerateBlockSymbol>();
    const slang::SVInt* index = block.getArrayIndex();
    if (index == nullptr) {
      path.emplace_back(sym.name);
      continue;
    }
    const slang::ast::Scope* array = sym.getHierarchicalParent();
    path.push_back(
        std::format(
            "{}_{}",
            array == nullptr ? std::string_view{} : array->asSymbol().name,
            index->as<std::int64_t>().value_or(0)));
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
            },
            [](const SuppliedAtConstruction&) {
              return std::string{"<supplied>"};
            }},
        input.kind);
    bytes += ';';
  }
  return bytes;
}

}  // namespace

auto InstantiationOf(const slang::ast::InstanceBodySymbol& body)
    -> const slang::ast::InstanceSymbol& {
  if (body.parentInstance == nullptr) {
    throw InternalError(
        "InstantiationOf: a body is elaborated for an application of a "
        "definition, so one that belongs to none was never built");
  }
  return *body.parentInstance;
}

auto SpecializationKeyOf(
    const slang::ast::InstanceSymbol& inst, const SpecializationPolicy& policy)
    -> SpecializationKey {
  SpecializationKey key{
      .definition = std::string{inst.getDefinition().name}, .inputs = {}};
  for (const auto* param : inst.body.getParameters()) {
    const auto* value = param->symbol.as_if<slang::ast::ParameterSymbol>();
    if (value == nullptr) {
      key.inputs.push_back(ParameterInput(param->symbol, policy));
      continue;
    }
    switch (policy.ValueSourceOf(inst, *value)) {
      case ParameterValueSource::kFixedBySpecialization:
        key.inputs.push_back(ParameterInput(*value, policy));
        break;
      case ParameterValueSource::kSuppliedAtConstruction:
        key.inputs.push_back(
            SpecializationInput{
                .name = std::string{value->name},
                .kind = SuppliedAtConstruction{}});
        break;
      // Its value follows from what is supplied and its own declaration, which
      // the definition already names.
      case ParameterValueSource::kComputedAtConstruction:
        break;
    }
  }
  for (const auto* connection : inst.getPortConnections()) {
    if (connection->port.kind == slang::ast::SymbolKind::InterfacePort) {
      key.inputs.push_back(InterfacePortInput(*connection, policy));
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

auto SpecializationName(
    const slang::ast::InstanceSymbol& inst, const SpecializationPolicy& policy)
    -> std::string {
  return SpecializationName(SpecializationKeyOf(inst, policy));
}

auto SpecializationKeyOf(
    const slang::ast::ClassType& cls, const SpecializationPolicy& policy)
    -> SpecializationKey {
  // A class carries one identifier through compilation, and a compilation unit
  // holds every class it declares in one flat name space, so the identifier has
  // to be unique there. The source name alone is not: SystemVerilog scopes the
  // declaration, so sibling generate blocks may each declare the same one.
  std::string definition;
  for (const std::string& block : DeclaringBlockPath(cls)) {
    definition += block;
    definition += '_';
  }
  definition += cls.name;

  SpecializationKey key{.definition = std::move(definition), .inputs = {}};
  if (cls.genericClass == nullptr) {
    return key;
  }
  for (const auto* sym : cls.genericParameters) {
    key.inputs.push_back(ParameterInput(*sym, policy));
  }
  return key;
}

auto SpecializationName(
    const slang::ast::ClassType& cls, const SpecializationPolicy& policy)
    -> std::string {
  return SpecializationName(SpecializationKeyOf(cls, policy));
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

auto IsDesignElement(const slang::ast::Symbol& unit) -> bool {
  return unit.kind == slang::ast::SymbolKind::InstanceBody;
}

auto CompilationUnitName(
    const slang::ast::Symbol& unit, const SpecializationPolicy& policy)
    -> std::string {
  using slang::ast::SymbolKind;
  if (unit.kind == SymbolKind::Package) {
    return std::string(unit.name);
  }
  if (unit.kind == SymbolKind::InstanceBody) {
    return SpecializationName(
        InstantiationOf(unit.as<slang::ast::InstanceBodySymbol>()), policy);
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

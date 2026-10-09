#include "lyra/lowering/ast_to_hir/unit_identity.hpp"

#include <algorithm>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <format>
#include <iterator>
#include <limits>
#include <string>
#include <string_view>
#include <utility>
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
#include "lyra/lowering/ast_to_hir/generate_construct.hpp"
#include "lyra/lowering/ast_to_hir/hierarchy_override.hpp"

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
// faithfully, so those answer with that rendering. A class and an unpacked
// structure are the exceptions: each is identified by its declaration (LRM 8.3,
// 6.22.1), so two with identical members are still two types, and its identity
// is the unit that declares it together with its own name -- the same pair
// every cross-unit reference to one carries.
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
          "{}::{}", CompilationUnitName(UnitHomeOf(cls), policy),
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
      return std::format(
          "struct {}::{}", CompilationUnitName(UnitHomeOf(canonical), policy),
          TypeDeclarationName(canonical, policy));
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
// parameter bindings are. A port carrying a range carries one interface per
// element, each its own, so the answer is which unit stands at each position.
// A modport belongs to the same answer, and not merely because it narrows: a
// view also names things of its own (LRM 25.5.4), and two views may give one
// name different storage, so a unit bound through each reaches a different
// place under the same spelling.
auto InterfacePortInput(
    const slang::ast::PortConnection& connection,
    const SpecializationPolicy& policy) -> SpecializationInput {
  const auto [instances, modport] =
      ConnectedInterfaceOf(connection.getIfaceConn());
  FixedInterface fixed{
      .units = {},
      .taken = {},
      .modport =
          modport == nullptr ? std::string{} : std::string{modport->name}};
  fixed.taken.reserve(instances.size());
  for (const slang::ast::InstanceSymbol* instance : instances) {
    std::string unit = policy.NameOf(*instance);
    const auto known = std::ranges::find(fixed.units, unit);
    fixed.taken.push_back(
        static_cast<std::uint32_t>(known - fixed.units.begin()));
    if (known == fixed.units.end()) fixed.units.push_back(std::move(unit));
  }
  return SpecializationInput{
      .name = std::string{connection.port.name}, .kind = std::move(fixed)};
}

// Whether `symbol` is the scope a compilation unit is: a package (LRM 26), a
// design element's body (LRM 23.2, 25), or the `$unit` file-set scope (LRM
// 3.12.1).
auto IsCompilationUnit(const slang::ast::Symbol& symbol) -> bool {
  return symbol.kind == slang::ast::SymbolKind::Package ||
         symbol.kind == slang::ast::SymbolKind::InstanceBody ||
         symbol.kind == slang::ast::SymbolKind::CompilationUnit;
}

auto IsInstanceScope(const slang::ast::Scope& scope) -> bool {
  const slang::ast::SymbolKind kind = scope.asSymbol().kind;
  return kind == slang::ast::SymbolKind::InstanceBody ||
         kind == slang::ast::SymbolKind::GenerateBlock;
}

// A generate block (LRM 27.6) as a path to a declaration inside it spells it,
// the way the hierarchy does, which is what tells two of them apart. A block
// standing on its own answers to its own label; a loop's block carries no
// label of its own and answers to the construct's label together with the
// index it elaborated at (LRM 27.4), so both halves are needed for it. The
// alternatives of one conditional may share a label (LRM 27.5), and one loop
// body can hold several of them where its blocks selected differently, so an
// alternative is told apart by its position among them as well.
auto GenerateBlockStep(const slang::ast::GenerateBlockSymbol& block)
    -> std::string {
  if (block.getArrayIndex() == nullptr) {
    if (!IsAlternative(block)) return std::string{block.name};
    const auto alternatives = AlternativesOfConstruct(block);
    const auto position = std::ranges::find(alternatives, &block);
    return std::format(
        "{}#{}", block.name, std::distance(alternatives.begin(), position));
  }
  const slang::ast::Scope* array = block.getHierarchicalParent();
  return std::format(
      "{}[{}]", array == nullptr ? std::string_view{} : array->asSymbol().name,
      LoopIndexOf(block));
}

// The generate blocks between a declaration and the compilation unit that owns
// it, outermost first. Each is a declaration scope of its own, so two of them
// may declare the same class name; the path is what tells those declarations
// apart in a name space that has no nesting of its own.
auto DeclaringBlockPath(const slang::ast::Symbol& decl)
    -> std::vector<std::string> {
  std::vector<std::string> path;
  for (const slang::ast::Scope* scope = decl.getParentScope(); scope != nullptr;
       scope = scope->asSymbol().getParentScope()) {
    const slang::ast::Symbol& sym = scope->asSymbol();
    if (sym.kind == slang::ast::SymbolKind::GenerateBlock) {
      path.push_back(
          GenerateBlockStep(sym.as<slang::ast::GenerateBlockSymbol>()));
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
              std::string at = "<";
              for (const std::string& unit : v.units) {
                at += unit;
                at += ',';
              }
              at += '|';
              for (const std::uint32_t taken : v.taken) {
                at += std::format("{},", taken);
              }
              return std::format("{}|{}>", at, v.modport);
            },
            [](const SuppliedAtConstruction&) {
              return std::string{"<supplied>"};
            },
            [](const LandsIn& v) { return std::format("<in {}>", v.scope); },
            [](const LandsBack& v) {
              std::string at = std::format("<back {}", v.levels);
              for (const std::string& block : v.blocks) {
                at += ' ';
                at += block;
              }
              return at + '>';
            },
            [](const BindInstantiation& v) {
              return std::format("<bound by {}>", v.directive);
            },
            [](const BoundToCell& v) {
              return std::format("<cell {}>", v.cell);
            }},
        input.kind);
    bytes += ';';
  }
  return bytes;
}

// Where a name that left an instance lands, as one input to that instance's
// key: the class of the scope it lands in, or where that scope stands relative
// to an instance whose name is being worked out.
auto LandingOf(
    const slang::ast::Scope& scope, const SpecializationPolicy& policy)
    -> SpecializationInputKind {
  std::vector<std::string> blocks;
  const slang::ast::Symbol* at = &scope.asSymbol();
  while (at->kind != slang::ast::SymbolKind::InstanceBody) {
    if (const auto* block = at->as_if<slang::ast::GenerateBlockSymbol>()) {
      blocks.push_back(GenerateBlockStep(*block));
    }
    at = &at->getHierarchicalParent()->asSymbol();
  }
  if (const auto levels =
          policy.LevelsOutTo(at->as<slang::ast::InstanceBodySymbol>())) {
    std::ranges::reverse(blocks);
    return LandsBack{.levels = *levels, .blocks = std::move(blocks)};
  }
  return LandsIn{.scope = ScopeClassName(scope, policy)};
}

// One instance as a path spells it: its name, or its array's name with the
// element's indices (LRM 23.3.2).
auto InstanceStep(const slang::ast::InstanceSymbol& inst) -> std::string {
  std::string step{inst.getArrayName()};
  for (const std::uint32_t at : inst.arrayPath) {
    step += std::format("[{}]", at);
  }
  return step;
}

void AddEffectsBelow(
    const slang::ast::Scope& scope, const std::string& prefix,
    const SpecializationPolicy& policy, std::vector<SpecializationInput>& out);

// What was written elsewhere about `inst`, which stands at `path` below the
// instance being keyed, and about every instance below it.
void AddEffectsAt(
    const slang::ast::InstanceSymbol& inst, const std::string& path,
    const SpecializationPolicy& policy, std::vector<SpecializationInput>& out) {
  for (const OverrideEffect& effect : OverridesOn(inst)) {
    std::visit(
        Overloaded{
            [&](const ParameterGivenElsewhere& given) {
              SpecializationInput input =
                  ParameterInput(*given.parameter, policy);
              input.name = std::format("{}.{}", path, input.name);
              out.push_back(std::move(input));
            },
            [&](const InsertedByBind& bound) {
              out.push_back(
                  SpecializationInput{
                      .name = path,
                      .kind = BindInstantiation{.directive = bound.directive}});
            },
            [&](const CellChosenByConfiguration& chosen) {
              out.push_back(
                  SpecializationInput{
                      .name = path, .kind = BoundToCell{.cell = chosen.cell}});
            }},
        effect);
  }
  if (OverridesMayReachBelow(inst)) {
    AddEffectsBelow(inst.body, path + ".", policy, out);
  }
}

// An instance, or every element of an instance array, with what was written
// elsewhere about each.
void AddEffectsOfInstances(
    const slang::ast::Symbol& symbol, const std::string& prefix,
    const SpecializationPolicy& policy, std::vector<SpecializationInput>& out) {
  if (const auto* inst = symbol.as_if<slang::ast::InstanceSymbol>()) {
    AddEffectsAt(*inst, prefix + InstanceStep(*inst), policy, out);
  } else if (
      const auto* array = symbol.as_if<slang::ast::InstanceArraySymbol>()) {
    for (const slang::ast::Symbol* element : array->elements) {
      AddEffectsOfInstances(*element, prefix, policy, out);
    }
  }
}

// Every instance `scope` holds, through its generate blocks and arrays, with
// what was written elsewhere about each.
void AddEffectsBelow(
    const slang::ast::Scope& scope, const std::string& prefix,
    const SpecializationPolicy& policy, std::vector<SpecializationInput>& out) {
  for (const auto& member : scope.members()) {
    if (const auto* block = member.as_if<slang::ast::GenerateBlockSymbol>()) {
      if (!block->isUninstantiated) {
        AddEffectsBelow(
            *block, prefix + GenerateBlockStep(*block) + ".", policy, out);
      }
    } else if (
        const auto* blocks =
            member.as_if<slang::ast::GenerateBlockArraySymbol>()) {
      for (const slang::ast::GenerateBlockSymbol* entry : blocks->entries) {
        AddEffectsBelow(
            *entry, prefix + GenerateBlockStep(*entry) + ".", policy, out);
      }
    } else {
      AddEffectsOfInstances(member, prefix, policy, out);
    }
  }
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

namespace {

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
  // A name is no parameter and declares no name of its own, so it is told
  // apart from the body's other climbs by the order the body writes them in.
  policy.EnterNaming(inst.body);
  std::size_t written = 0;
  for (const ClimbAnchor& climb : policy.ClimbsOutOf(inst)) {
    key.inputs.push_back(
        SpecializationInput{
            .name = std::format("^{}", written++),
            .kind = LandingOf(*climb.scope, policy)});
  }
  policy.LeaveNaming();
  if (OverridesMayReachBelow(inst)) {
    AddEffectsBelow(inst.body, "", policy, key.inputs);
  }
  return key;
}

}  // namespace

auto SpecializationPolicy::AnswersNow(const KeptName& kept) const -> bool {
  return std::ranges::none_of(
      naming_, [&](const slang::ast::InstanceBodySymbol* body) {
        return kept.depends_on.contains(body);
      });
}

auto SpecializationPolicy::NameOf(const slang::ast::InstanceSymbol& inst) const
    -> std::string {
  if (const auto kept = names_.find(&inst);
      kept != names_.end() && AnswersNow(kept->second)) {
    for (NameInProgress& asking : names_in_progress_) {
      asking.depends_on.insert(
          kept->second.depends_on.begin(), kept->second.depends_on.end());
    }
    return kept->second.name;
  }
  names_in_progress_.push_back(
      NameInProgress{
          .chain_at_start = naming_.size(),
          .met_nothing_outside = true,
          .depends_on = {}});
  SpecializationKey key = SpecializationKeyOf(inst, *this);
  NameInProgress worked_out = std::move(names_in_progress_.back());
  names_in_progress_.pop_back();
  std::string name = SpecializationName(key);
  if (!worked_out.met_nothing_outside) return name;
  if (const auto folded = folded_.find(name); folded == folded_.end()) {
    folded_.emplace(name, std::move(key));
  } else if (folded->second != key) {
    throw InternalError(
        "SpecializationPolicy::NameOf: two specializations reached one name, "
        "so the name no longer tells the units apart");
  }
  names_.insert_or_assign(
      &inst,
      KeptName{.name = name, .depends_on = std::move(worked_out.depends_on)});
  return name;
}

auto SpecializationPolicy::ClimbsOutOf(const slang::ast::InstanceSymbol& inst)
    const -> std::span<const ClimbAnchor> {
  auto cached = climbs_.find(&inst);
  if (cached == climbs_.end()) {
    cached = climbs_.emplace(&inst, ast_to_hir::ClimbsOutOf(inst.body)).first;
  }
  return cached->second;
}

void SpecializationPolicy::EnterNaming(
    const slang::ast::InstanceBodySymbol& body) const {
  naming_.push_back(&body);
}

void SpecializationPolicy::LeaveNaming() const {
  naming_.pop_back();
}

auto SpecializationPolicy::LevelsOutTo(
    const slang::ast::InstanceBodySymbol& body) const
    -> std::optional<std::uint32_t> {
  for (std::size_t out = 0; out < naming_.size(); ++out) {
    const std::size_t at = naming_.size() - 1 - out;
    if (naming_[at] == &body) {
      for (NameInProgress& asking : names_in_progress_) {
        if (asking.chain_at_start > at) asking.met_nothing_outside = false;
      }
      return static_cast<std::uint32_t>(out);
    }
  }
  for (NameInProgress& asking : names_in_progress_) {
    asking.depends_on.insert(&body);
  }
  return std::nullopt;
}

auto SpecializationName(const SpecializationKey& key) -> std::string {
  if (key.inputs.empty()) {
    return key.definition;
  }
  return std::format("{}__{:016x}", key.definition, Fnv1a64(KeyBytes(key)));
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

auto ScopeClassName(
    const slang::ast::Scope& scope, const SpecializationPolicy& policy)
    -> std::string {
  const slang::ast::Symbol& symbol = scope.asSymbol();
  if (const auto* body = symbol.as_if<slang::ast::InstanceBodySymbol>()) {
    return policy.NameOf(InstantiationOf(*body));
  }
  const auto* block = symbol.as_if<slang::ast::GenerateBlockSymbol>();
  if (block == nullptr) {
    throw InternalError(
        "ScopeClassName: an object stands for an instance or a generate "
        "block, and for no other scope");
  }
  const slang::ast::Scope* holder = block->getParentScope();
  while (holder != nullptr && !IsInstanceScope(*holder)) {
    holder = holder->asSymbol().getParentScope();
  }
  if (holder == nullptr) {
    throw InternalError(
        "ScopeClassName: a generate block stands in an instance's body or in "
        "another generate block");
  }
  return BlockClassName(ScopeClassName(*holder, policy), *block);
}

auto BlockClassName(
    std::string_view holder, const slang::ast::GenerateBlockSymbol& block)
    -> std::string {
  return std::format("{}::{}", holder, GenerateBlockStep(block));
}

auto DeclaringCompilationUnit(const slang::ast::Symbol& decl)
    -> const slang::ast::Symbol& {
  for (const slang::ast::Scope* scope = decl.getParentScope(); scope != nullptr;
       scope = scope->asSymbol().getParentScope()) {
    const slang::ast::Symbol& owner = scope->asSymbol();
    if (IsCompilationUnit(owner)) {
      return owner;
    }
  }
  throw InternalError(
      "DeclaringCompilationUnit: every declaration lies in a package, a design "
      "element's body, or the file-set scope");
}

namespace {

auto Encloses(const slang::ast::Scope& outer, const slang::ast::Scope& inner)
    -> bool {
  for (const slang::ast::Scope* level = &inner; level != nullptr;
       level = level->asSymbol().getParentScope()) {
    if (level == &outer) return true;
  }
  return false;
}

// Of two scopes replicating parts of one type, the one replicating the whole:
// an instance's scope over a namespace's, and the inner of two instances'
// scopes. Two instance scopes neither of which encloses the other cannot both
// reach one type, since a type an instance declares is named only inside it
// (LRM 6.22).
auto Innermost(const slang::ast::Scope* held, const slang::ast::Scope* other)
    -> const slang::ast::Scope* {
  if (other == nullptr || !IsInstanceScope(*other)) return held;
  if (!IsInstanceScope(*held) || Encloses(*held, *other)) return other;
  if (Encloses(*other, *held)) return held;
  throw InternalError(
      "ReplicatingScope: two instance scopes neither enclosing the other "
      "replicate parts of one type");
}

auto ReplicatingScopeOfType(const slang::ast::Type& type)
    -> const slang::ast::Scope* {
  using slang::ast::SymbolKind;
  const slang::ast::Type& canonical = type.getCanonicalType();
  switch (canonical.kind) {
    case SymbolKind::ClassType:
    case SymbolKind::UnpackedStructType:
    case SymbolKind::UnpackedUnionType:
    case SymbolKind::PackedStructType:
    case SymbolKind::PackedUnionType:
    case SymbolKind::EnumType:
      return &ReplicatingScope(canonical);
    case SymbolKind::AssociativeArrayType: {
      const auto& assoc = canonical.as<slang::ast::AssociativeArrayType>();
      const slang::ast::Scope* element =
          ReplicatingScopeOfType(assoc.elementType);
      if (assoc.indexType == nullptr) return element;
      const slang::ast::Scope* index = ReplicatingScopeOfType(*assoc.indexType);
      if (element == nullptr) return index;
      return Innermost(element, index);
    }
    default:
      break;
  }
  if (const slang::ast::Type* element = canonical.getArrayElementType()) {
    return ReplicatingScopeOfType(*element);
  }
  return nullptr;
}

// The innermost specialization of a generic class `decl` is or lies in, short
// of its compilation unit.
auto InnermostSpecialization(const slang::ast::Symbol& decl)
    -> const slang::ast::ClassType* {
  for (const slang::ast::Symbol* level = &decl;
       level != nullptr && !IsCompilationUnit(*level);
       level = level->getParentScope() == nullptr
                   ? nullptr
                   : &level->getParentScope()->asSymbol()) {
    const auto* cls = level->as_if<slang::ast::ClassType>();
    if (cls != nullptr && cls->genericClass != nullptr) return cls;
  }
  return nullptr;
}

auto InstanceBodyOf(const slang::ast::Scope& scope)
    -> const slang::ast::Symbol& {
  for (const slang::ast::Scope* level = &scope; level != nullptr;
       level = level->asSymbol().getParentScope()) {
    if (level->asSymbol().kind == slang::ast::SymbolKind::InstanceBody) {
      return level->asSymbol();
    }
  }
  throw InternalError(
      "UnitHomeOf: an instance's scope lies in a design element's body");
}

}  // namespace

auto ReplicatingScope(const slang::ast::Symbol& decl)
    -> const slang::ast::Scope& {
  const slang::ast::Scope* replicating = nullptr;
  std::vector<const slang::ast::ClassType*> specializations;
  for (const slang::ast::Symbol* level = &decl; replicating == nullptr;) {
    if (const auto* cls = level->as_if<slang::ast::ClassType>();
        cls != nullptr && cls->genericClass != nullptr) {
      specializations.push_back(cls);
    }
    const slang::ast::Scope* parent = level->getParentScope();
    if (parent == nullptr) {
      throw InternalError(
          "ReplicatingScope: every declaration lies inside a structural "
          "scope");
    }
    const slang::ast::SymbolKind kind = parent->asSymbol().kind;
    if (kind == slang::ast::SymbolKind::InstanceBody ||
        kind == slang::ast::SymbolKind::GenerateBlock ||
        kind == slang::ast::SymbolKind::Package ||
        kind == slang::ast::SymbolKind::CompilationUnit) {
      replicating = parent;
    }
    level = &parent->asSymbol();
  }
  const slang::ast::Scope* innermost = replicating;
  for (const slang::ast::ClassType* spec : specializations) {
    for (const auto* param : spec->genericParameters) {
      if (const auto* type_param =
              param->as_if<slang::ast::TypeParameterSymbol>()) {
        innermost = Innermost(
            innermost,
            ReplicatingScopeOfType(type_param->targetType.getType()));
      }
    }
  }
  return *innermost;
}

auto UnitHomeOf(const slang::ast::Symbol& decl) -> const slang::ast::Symbol& {
  const slang::ast::Scope& replicating = ReplicatingScope(decl);
  if (IsInstanceScope(replicating)) return InstanceBodyOf(replicating);
  if (const slang::ast::ClassType* spec = InnermostSpecialization(decl)) {
    return *spec;
  }
  return DeclaringCompilationUnit(decl);
}

auto IsDesignElement(const slang::ast::Symbol& unit) -> bool {
  return unit.kind == slang::ast::SymbolKind::InstanceBody;
}

auto BelongsToAnInstance(const slang::ast::ClassType& cls) -> bool {
  return IsInstanceScope(ReplicatingScope(cls));
}

auto CompilationUnitName(
    const slang::ast::Symbol& unit, const SpecializationPolicy& policy)
    -> std::string {
  using slang::ast::SymbolKind;
  if (unit.kind == SymbolKind::Package) {
    return std::string(unit.name);
  }
  if (unit.kind == SymbolKind::InstanceBody) {
    return policy.NameOf(
        InstantiationOf(unit.as<slang::ast::InstanceBodySymbol>()));
  }
  if (const auto* spec = unit.as_if<slang::ast::ClassType>()) {
    return std::format(
        "{}__{}", CompilationUnitName(DeclaringCompilationUnit(*spec), policy),
        SpecializationName(*spec, policy));
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
      "CompilationUnitName: symbol is not a package, module body, compilation "
      "unit, or specialization");
}

namespace {

// What a scope between a declaration and its unit is called on the path to it:
// a generate block as the hierarchy spells it, a class as its specialization,
// and a block with no label as where it sits, since it has nothing else.
auto ScopeStep(
    const slang::ast::Symbol& scope, const SpecializationPolicy& policy)
    -> std::string {
  if (scope.kind == slang::ast::SymbolKind::GenerateBlock) {
    return GenerateBlockStep(scope.as<slang::ast::GenerateBlockSymbol>());
  }
  if (scope.kind == slang::ast::SymbolKind::ClassType) {
    return SpecializationName(scope.as<slang::ast::ClassType>(), policy);
  }
  if (!scope.name.empty()) {
    return std::string{scope.name};
  }
  return std::format("${}", static_cast<std::uint32_t>(scope.getIndex()));
}

// Whether `candidate` is the type the declaration `type` came from declares. A
// type written in place of a name is one type for every data object its
// declaration statement declares (LRM 6.22.1 c), while the front end elaborates
// it once per object; what the objects share is the text that wrote it.
auto DeclaredBySameText(
    const slang::ast::Type& candidate, const slang::ast::Type& type) -> bool {
  const slang::ast::Type& canonical = candidate.getCanonicalType();
  return &canonical == &type || (type.getSyntax() != nullptr &&
                                 canonical.getSyntax() == type.getSyntax());
}

// The name a type answers to in the scope that declares it: the typedef
// declaring it, or else the first data object its declaration statement
// declares (LRM 6.22.1 c). A type declared some other way has nothing to answer
// to but where it sits.
auto NameInScope(const slang::ast::Type& type) -> std::string {
  for (const auto& member : type.getParentScope()->members()) {
    if (const auto* alias = member.as_if<slang::ast::TypeAliasType>()) {
      if (DeclaredBySameText(alias->targetType.getType(), type)) {
        return std::string{alias->name};
      }
      continue;
    }
    if (const auto* value = member.as_if<slang::ast::ValueSymbol>()) {
      if (DeclaredBySameText(value->getType(), type)) {
        return std::string{value->name};
      }
    }
  }
  return std::format("${}", static_cast<std::uint32_t>(type.getIndex()));
}

}  // namespace

auto TypeDeclarationName(
    const slang::ast::Type& type, const SpecializationPolicy& policy)
    -> std::string {
  std::vector<std::string> path{NameInScope(type)};
  const slang::ast::Symbol& home = UnitHomeOf(type);
  for (const slang::ast::Scope* scope = type.getParentScope(); scope != nullptr;
       scope = scope->asSymbol().getParentScope()) {
    const slang::ast::Symbol& owner = scope->asSymbol();
    if (IsCompilationUnit(owner) || &owner == &home) {
      break;
    }
    // A type written in place inside another is named inside that one's name.
    if (owner.kind == slang::ast::SymbolKind::UnpackedStructType ||
        owner.kind == slang::ast::SymbolKind::UnpackedUnionType) {
      path.push_back(TypeDeclarationName(owner.as<slang::ast::Type>(), policy));
      break;
    }
    path.push_back(ScopeStep(owner, policy));
  }
  std::ranges::reverse(path);
  std::string name;
  for (const std::string& step : path) {
    if (!name.empty()) {
      name += '.';
    }
    name += step;
  }
  return name;
}

}  // namespace lyra::lowering::ast_to_hir

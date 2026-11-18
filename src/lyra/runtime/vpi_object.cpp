#include "lyra/runtime/vpi_object.hpp"

#include <algorithm>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/abi/records.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/hierarchical_name.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/vpi_time.hpp"
#include "lyra/runtime/vpi_variable.hpp"
#include "lyra/support/scope_kind.hpp"
#include "sv_vpi_user.h"
#include "vpi_user.h"

namespace lyra::runtime {

auto VpiNotYet(std::string_view what) -> std::unexpected<std::string> {
  return std::unexpected(std::string(what) + " is not yet supported");
}

auto VpiNotAProperty() -> std::unexpected<std::string> {
  return std::unexpected<std::string>("the object has no such property");
}

auto ScopeDeclarationsOf(const ScopeHolder& holder)
    -> const abi::ScopeDeclarations& {
  return std::visit(
      Overloaded{
          [](Scope* scope) -> const abi::ScopeDeclarations& {
            return scope->Declarations();
          },
          [](const NamespaceObject& ns) -> const abi::ScopeDeclarations& {
            return *ns.scope_declarations;
          }},
      holder);
}

namespace {

using support::ElaboratedScopeKind;
using support::LexicalScopeKind;

// What holds `object`'s declaration: a constructed scope or a namespace holds
// itself. A typespec, and what a relation from one answers, is a constant of
// no scope.
auto HolderOf(const VpiObject& object) -> std::optional<ScopeHolder> {
  using Held = std::optional<ScopeHolder>;
  return std::visit(
      Overloaded{
          [](Scope* scope) -> Held { return ScopeHolder{scope}; },
          [](const NamespaceObject& ns) -> Held { return ScopeHolder{ns}; },
          [](const ScopeArray& array) -> Held {
            return ScopeHolder{array.holder};
          },
          [](const LexicalScope& scope) -> Held { return scope.holder; },
          [](const VariableObject& variable) -> Held {
            return variable.holder;
          },
          [](const TypespecObject&) -> Held { return std::nullopt; },
          [](const TypespecMemberObject&) -> Held { return std::nullopt; },
          [](const RangeObject&) -> Held { return std::nullopt; },
          [](const ConstantObject&) -> Held { return std::nullopt; },
          [](const EnumConstantObject&) -> Held { return std::nullopt; }},
      object);
}

// Whether names are spelled as a version of 1364 spelled them, which named a
// logic variable a `reg` (LRM 37.17 detail 19).
auto SpellsVerilogNames(VpiMode mode) -> bool {
  switch (mode) {
    case VpiMode::k1364v1995:
    case VpiMode::k1364v2001:
    case VpiMode::k1364v2005:
      return true;
    case VpiMode::k1800v2005:
    case VpiMode::k1800v2009:
    case VpiMode::k1800v2012:
      return false;
  }
  throw InternalError("SpellsVerilogNames: unknown VPI mode");
}

// The constructed scopes `scope` holds, in the order they were attached.
auto ChildrenOf(Scope& scope) -> std::vector<Scope*> {
  std::vector<Scope*> children;
  scope.ForEachChild([&](Scope& child) { children.push_back(&child); });
  return children;
}

// The scopes `array` holds, in the order they were attached.
auto ElementsOf(const ScopeArray& array) -> std::vector<Scope*> {
  std::vector<Scope*> elements;
  array.holder->ForEachChild([&](Scope& child) {
    if (&child.Segment().Declaration() == array.declaration) {
      elements.push_back(&child);
    }
  });
  return elements;
}

// The type of an array of scopes a declaration builds and of each scope in it
// (LRM 37.11, 37.85).
struct ArrayTypes {
  PLI_INT32 array;
  PLI_INT32 element;
};

// The types of the array a declaration of `kind` builds, none where it builds
// one scope.
auto ArrayTypesOf(ElaboratedScopeKind kind) -> std::optional<ArrayTypes> {
  switch (kind) {
    case ElaboratedScopeKind::kModuleArray:
      return ArrayTypes{.array = vpiModuleArray, .element = vpiModule};
    case ElaboratedScopeKind::kInterfaceArray:
      return ArrayTypes{.array = vpiInterfaceArray, .element = vpiInterface};
    case ElaboratedScopeKind::kProgramArray:
      return ArrayTypes{.array = vpiProgramArray, .element = vpiProgram};
    case ElaboratedScopeKind::kGenerateLoop:
      return ArrayTypes{.array = vpiGenScopeArray, .element = vpiGenScope};
    case ElaboratedScopeKind::kModule:
    case ElaboratedScopeKind::kInterface:
    case ElaboratedScopeKind::kProgram:
    case ElaboratedScopeKind::kGenerateBlock:
    case ElaboratedScopeKind::kDesignRoot:
      return std::nullopt;
  }
  throw InternalError("ArrayTypesOf: unknown kind");
}

// The type of a scope the run constructed, which its declaration states: an
// instance's, an element of an array of them as much, and a generate scope's.
auto ScopeType(const Scope& scope) -> PLI_INT32 {
  switch (scope.Segment().Declaration().Kind()) {
    case ElaboratedScopeKind::kModule:
    case ElaboratedScopeKind::kModuleArray:
      return vpiModule;
    case ElaboratedScopeKind::kInterface:
    case ElaboratedScopeKind::kInterfaceArray:
      return vpiInterface;
    case ElaboratedScopeKind::kProgram:
    case ElaboratedScopeKind::kProgramArray:
      return vpiProgram;
    case ElaboratedScopeKind::kGenerateBlock:
    case ElaboratedScopeKind::kGenerateLoop:
      return vpiGenScope;
    case ElaboratedScopeKind::kDesignRoot:
      break;
  }
  throw InternalError(
      "ScopeType: the design's root is no object -- please report this as a "
      "bug");
}

// Whether `scope` is a block a generate built, which answers what a generate
// scope does rather than what its definition states (LRM 37.85).
auto IsGenerateScope(const Scope& scope) -> bool {
  switch (scope.Segment().Declaration().Kind()) {
    case ElaboratedScopeKind::kGenerateBlock:
    case ElaboratedScopeKind::kGenerateLoop:
      return true;
    case ElaboratedScopeKind::kModule:
    case ElaboratedScopeKind::kInterface:
    case ElaboratedScopeKind::kProgram:
    case ElaboratedScopeKind::kModuleArray:
    case ElaboratedScopeKind::kInterfaceArray:
    case ElaboratedScopeKind::kProgramArray:
      return false;
    case ElaboratedScopeKind::kDesignRoot:
      break;
  }
  throw InternalError(
      "IsGenerateScope: the design's root is no object -- please report this "
      "as a bug");
}

auto LexicalScopeType(const abi::LexicalScopeRecord& declaration) -> PLI_INT32 {
  const bool named = declaration.Named();
  switch (declaration.Kind()) {
    case LexicalScopeKind::kBegin:
      return named ? vpiNamedBegin : vpiBegin;
    case LexicalScopeKind::kForkJoin:
    case LexicalScopeKind::kForkJoinAny:
    case LexicalScopeKind::kForkJoinNone:
      return named ? vpiNamedFork : vpiFork;
    case LexicalScopeKind::kFor:
      return vpiFor;
    case LexicalScopeKind::kForeach:
      return vpiForeachStmt;
    case LexicalScopeKind::kTask:
      return vpiTask;
    case LexicalScopeKind::kFunction:
      return vpiFunction;
    case LexicalScopeKind::kClass:
      return vpiClassDefn;
    case LexicalScopeKind::kClockingBlock:
      return vpiClockingBlock;
  }
  throw InternalError("LexicalScopeType: unknown kind");
}

// The types of `array` and of its elements, which its declaration states.
auto TypesOf(const ScopeArray& array) -> ArrayTypes {
  const std::optional<ArrayTypes> types =
      ArrayTypesOf(array.declaration->Kind());
  if (!types.has_value()) {
    throw InternalError(
        "TypesOf: an array is declared as one scope -- please report this as "
        "a bug");
  }
  return *types;
}

auto IsTopLevel(const Scope& scope) -> bool {
  return scope.Parent() != nullptr && scope.Parent()->Parent() == nullptr;
}

// The scopes the run makes no object for that `holder` declares directly in
// it, or directly in `enclosing` where one is given.
auto LexicalScopesIn(
    const ScopeHolder& holder, const abi::LexicalScopeRecord* enclosing)
    -> std::vector<VpiObject> {
  std::vector<VpiObject> found;
  for (const abi::LexicalScopeRecord* declaration :
       ScopeDeclarationsOf(holder).lexical.Entries()) {
    if (declaration->enclosing == enclosing) {
      found.emplace_back(
          LexicalScope{.holder = holder, .declaration = declaration});
    }
  }
  return found;
}

// The arrays of instances or of blocks `scope` declares.
auto ArraysIn(Scope& scope) -> std::vector<VpiObject> {
  std::vector<VpiObject> found;
  for (const abi::ElaboratedScopeRecord* declaration :
       scope.Declarations().elaborated.Entries()) {
    if (ArrayTypesOf(declaration->Kind()).has_value()) {
      found.emplace_back(
          ScopeArray{.holder = &scope, .declaration = declaration});
    }
  }
  return found;
}

auto OfType(std::vector<VpiObject> objects, PLI_INT32 type)
    -> std::vector<VpiObject> {
  std::erase_if(objects, [&](const VpiObject& object) {
    return TypeOf(object, VpiMode::k1800v2012) != type;
  });
  return objects;
}

auto FromDesign(RuntimeEffects& run, PLI_INT32 relation)
    -> VpiAnswer<std::vector<VpiObject>> {
  Runtime& runtime = AsRuntime(run);
  std::vector<VpiObject> tops;
  for (Scope* top : ChildrenOf(runtime.DesignRoot())) tops.emplace_back(top);
  switch (relation) {
    case vpiModule:
    case vpiInterface:
    case vpiProgram:
      return OfType(std::move(tops), relation);
    case vpiInstance:
      for (const abi::ScopeDeclarations* ns : runtime.Namespaces()) {
        tops.emplace_back(NamespaceObject{.scope_declarations = ns});
      }
      return tops;
    default:
      return VpiNotYet("this relation from no object");
  }
}

// What a scope or a namespace declares at its own level and answers by its
// own relations, beside its internal scopes (LRM 37.10): its tasks and
// functions, and its classes. Any other relation from `holder`, which `from`
// says what it is, is not yet supported.
auto DeclaredAtLevel(
    const ScopeHolder& holder, PLI_INT32 relation, std::string_view from)
    -> VpiAnswer<std::vector<VpiObject>> {
  std::vector<VpiObject> declared = LexicalScopesIn(holder, nullptr);
  switch (relation) {
    case vpiTaskFunc:
      std::erase_if(declared, [](const VpiObject& object) {
        const PLI_INT32 type = TypeOf(object, VpiMode::k1800v2012);
        return type != vpiTask && type != vpiFunction;
      });
      return declared;
    case vpiClassDefn:
      return OfType(std::move(declared), vpiClassDefn);
    default:
      return VpiNotYet(std::string("this relation from ").append(from));
  }
}

auto FromScope(Scope& scope, PLI_INT32 relation, VpiMode mode)
    -> VpiAnswer<std::vector<VpiObject>> {
  if (std::optional<std::vector<VpiObject>> variables =
          VariablesDeclaredIn(&scope, nullptr, relation, mode)) {
    return *std::move(variables);
  }
  std::vector<VpiObject> children;
  for (Scope* child : ChildrenOf(scope)) children.emplace_back(child);
  switch (relation) {
    case vpiModule:
    case vpiInterface:
    case vpiProgram:
      return OfType(std::move(children), relation);
    case vpiModuleArray:
    case vpiInterfaceArray:
    case vpiProgramArray:
    case vpiGenScopeArray:
      return OfType(ArraysIn(scope), relation);
    case vpiInternalScope: {
      std::vector<VpiObject> declared = LexicalScopesIn(&scope, nullptr);
      children.insert(children.end(), declared.begin(), declared.end());
      return children;
    }
    default:
      return DeclaredAtLevel(&scope, relation, "a scope");
  }
}

auto FromArray(const ScopeArray& array, PLI_INT32 relation)
    -> VpiAnswer<std::vector<VpiObject>> {
  if (relation != TypesOf(array).element) {
    return VpiNotYet("this relation from an array");
  }
  std::vector<VpiObject> elements;
  for (Scope* scope : ElementsOf(array)) elements.emplace_back(scope);
  return elements;
}

auto FromLexicalScope(
    const LexicalScope& scope, PLI_INT32 relation, VpiMode mode)
    -> VpiAnswer<std::vector<VpiObject>> {
  if (std::optional<std::vector<VpiObject>> variables = VariablesDeclaredIn(
          scope.holder, scope.declaration, relation, mode)) {
    return *std::move(variables);
  }
  if (relation != vpiInternalScope) {
    return VpiNotYet(
        "this relation from a block, a subroutine, a class or a clocking "
        "block");
  }
  return LexicalScopesIn(scope.holder, scope.declaration);
}

auto RelatedToObject(PLI_INT32 relation, const VpiObject& from, VpiMode mode)
    -> VpiAnswer<std::vector<VpiObject>> {
  using Answer = VpiAnswer<std::vector<VpiObject>>;
  const auto of_variable = [&]() -> Answer {
    return RelatedToVariablePart(relation, from, mode);
  };
  return std::visit(
      Overloaded{
          [&](Scope* scope) { return FromScope(*scope, relation, mode); },
          [&](const NamespaceObject& ns) -> Answer {
            if (std::optional<std::vector<VpiObject>> variables =
                    VariablesDeclaredIn(ns, nullptr, relation, mode)) {
              return *std::move(variables);
            }
            if (relation == vpiInternalScope) {
              return LexicalScopesIn(ns, nullptr);
            }
            return DeclaredAtLevel(ns, relation, "a package");
          },
          [&](const ScopeArray& array) { return FromArray(array, relation); },
          [&](const LexicalScope& scope) {
            return FromLexicalScope(scope, relation, mode);
          },
          [&](const VariableObject&) { return of_variable(); },
          [&](const TypespecObject&) { return of_variable(); },
          [&](const TypespecMemberObject&) { return of_variable(); },
          [&](const RangeObject&) { return of_variable(); },
          [&](const ConstantObject&) { return of_variable(); },
          [&](const EnumConstantObject&) { return of_variable(); }},
      from);
}

// The nearest instance of a type `is` admits holding what stands at `at`, `at`
// included.
auto InstanceAround(Scope* at, bool (*is)(PLI_INT32 type))
    -> std::optional<VpiObject> {
  for (; at != nullptr && at->Parent() != nullptr; at = at->Parent()) {
    if (is(ScopeType(*at))) return VpiObject{at};
  }
  return std::nullopt;
}

// The nearest instance of a type `is` admits around `object`: above a
// constructed scope, and from the scope holding an array or a declaration on,
// which for one a package holds is the package (LRM 37.10 details 2, 3). A
// namespace lies in none.
auto InstanceAroundObject(const VpiObject& object, bool (*is)(PLI_INT32 type))
    -> std::optional<VpiObject> {
  if (std::holds_alternative<NamespaceObject>(object)) return std::nullopt;
  if (Scope* const* scope = std::get_if<Scope*>(&object)) {
    return InstanceAround((*scope)->Parent(), is);
  }
  const std::optional<ScopeHolder> holder = HolderOf(object);
  if (!holder.has_value()) return std::nullopt;
  return std::visit(
      Overloaded{
          [&](Scope* held) { return InstanceAround(held, is); },
          [&](const NamespaceObject& ns) -> std::optional<VpiObject> {
            if (!is(vpiPackage)) return std::nullopt;
            return VpiObject{ns};
          }},
      *holder);
}

auto IsModule(PLI_INT32 type) -> bool {
  return type == vpiModule;
}

// What LRM 37.10 detail 3 counts an instance: a package, a module, an
// interface or a program.
auto IsInstance(PLI_INT32 type) -> bool {
  return type == vpiPackage || type == vpiModule || type == vpiInterface ||
         type == vpiProgram;
}

// The array `identifier` names that `scope` declares, none where it declares
// none so named.
auto ArrayNamed(Scope& scope, std::string_view identifier)
    -> std::optional<VpiObject> {
  for (const abi::ElaboratedScopeRecord* declaration :
       scope.Declarations().elaborated.Entries()) {
    if (ArrayTypesOf(declaration->Kind()).has_value() &&
        IdentifierOf(declaration->name) == identifier) {
      return ScopeArray{.holder = &scope, .declaration = declaration};
    }
  }
  return std::nullopt;
}

// The lexical scope `identifier` names directly in `enclosing` of what
// `holder` declares, none where none is so named.
auto LexicalScopeNamed(
    const ScopeHolder& holder, const abi::LexicalScopeRecord* enclosing,
    std::string_view identifier) -> std::optional<VpiObject> {
  const abi::LexicalScopeRecord* declaration = LexicalDeclarationNamed(
      ScopeDeclarationsOf(holder).lexical.Entries(), enclosing, identifier);
  if (declaration == nullptr) return std::nullopt;
  return LexicalScope{.holder = holder, .declaration = declaration};
}

// What `part` names inside `at`. An instance or a block the scope built is a
// child, and one it did not build is not there; a variable a scope or a block
// declares is named in it, and a member in a structure or a union variable.
auto Step(const VpiObject& at, const NamePart& part)
    -> VpiAnswer<std::optional<VpiObject>> {
  using Answer = VpiAnswer<std::optional<VpiObject>>;
  const Answer none = std::optional<VpiObject>{};
  return std::visit(
      Overloaded{
          [&](Scope* scope) -> Answer {
            if (Scope* child = ChildNamed(*scope, part)) {
              return std::optional<VpiObject>{child};
            }
            if (part.indices.empty()) {
              if (auto array = ArrayNamed(*scope, part.identifier)) {
                return array;
              }
              if (auto lexical =
                      LexicalScopeNamed(scope, nullptr, part.identifier)) {
                return lexical;
              }
            }
            return VariableNamed(scope, nullptr, part);
          },
          [&](const NamespaceObject& ns) -> Answer {
            if (part.indices.empty()) {
              if (auto lexical =
                      LexicalScopeNamed(ns, nullptr, part.identifier)) {
                return lexical;
              }
            }
            return VariableNamed(ns, nullptr, part);
          },
          [&](const ScopeArray&) { return none; },
          [&](const LexicalScope& scope) -> Answer {
            if (part.indices.empty()) {
              if (auto lexical = LexicalScopeNamed(
                      scope.holder, scope.declaration, part.identifier)) {
                return lexical;
              }
            }
            return VariableNamed(scope.holder, scope.declaration, part);
          },
          [&](const VariableObject& variable) {
            return MemberNamed(variable, part);
          },
          [&](const TypespecObject&) { return none; },
          [&](const TypespecMemberObject&) { return none; },
          [&](const RangeObject&) { return none; },
          [&](const ConstantObject&) { return none; },
          [&](const EnumConstantObject&) { return none; }},
      at);
}

// What `parts` name in turn from `at`.
auto Walk(std::optional<VpiObject> at, std::span<const NamePart> parts)
    -> VpiAnswer<std::optional<VpiObject>> {
  for (const NamePart& part : parts) {
    if (!at.has_value()) return at;
    VpiAnswer<std::optional<VpiObject>> stepped = Step(*at, part);
    if (!stepped.has_value()) return stepped;
    at = *std::move(stepped);
  }
  return at;
}

// The package named `identifier`.
auto PackageObjectNamed(RuntimeEffects& run, std::string_view identifier)
    -> std::optional<VpiObject> {
  const abi::ScopeDeclarations* declared =
      PackageNamed(AsRuntime(run).Namespaces(), identifier);
  if (declared == nullptr) {
    return std::nullopt;
  }
  return NamespaceObject{.scope_declarations = declared};
}

// Whether `object` is a compilation unit's scope or lies within one.
auto InCompilationUnit(const VpiObject& object) -> bool {
  const std::optional<ScopeHolder> holder = HolderOf(object);
  if (!holder.has_value()) return false;
  const auto* ns = std::get_if<NamespaceObject>(&*holder);
  return ns != nullptr &&
         support::IsCompilationUnit(ns->scope_declarations->definition->Kind());
}

// The object `parts` name from the design's tops, or a package's name alone
// where no top-level instance has that name.
auto NamedFromDesign(RuntimeEffects& run, const HierarchicalName& name)
    -> VpiAnswer<std::optional<VpiObject>> {
  if (name.package.has_value()) {
    return Walk(PackageObjectNamed(run, *name.package), name.parts);
  }
  std::span<const NamePart> parts = name.parts;
  std::optional<VpiObject> at;
  const ScopeReached reached =
      LongestNamedPrefix(AsRuntime(run).DesignRoot(), parts);
  if (reached.parts > 0) {
    at = reached.scope;
    parts = parts.subspan(reached.parts);
  } else if (parts.size() == 1 && parts.front().indices.empty()) {
    return PackageObjectNamed(run, parts.front().identifier);
  }
  return Walk(at, parts);
}

// The object `name` names within `within` and nowhere else (LRM 38.21), so
// one naming a package from there names nothing.
auto NamedWithin(const VpiObject& within, const HierarchicalName& name)
    -> VpiAnswer<std::optional<VpiObject>> {
  if (name.package.has_value() || InCompilationUnit(within)) {
    return std::optional<VpiObject>{};
  }
  return Walk(within, name.parts);
}

auto NamespaceFullName(const NamespaceObject& ns) -> std::string {
  return std::string(ns.scope_declarations->definition->name) + "::";
}

auto HolderFullName(const ScopeHolder& holder) -> std::string {
  return std::visit(
      Overloaded{
          [](Scope* scope) {
            return std::string(scope->HierarchicalPath().View()) + ".";
          },
          [](const NamespaceObject& ns) { return NamespaceFullName(ns); }},
      holder);
}

// A scope inside an object answers the name `%m` prints inside it: its
// holder's, then each named declaration around it, then its own (LRM
// 21.2.1.5, 37.10). One the source gave no name answers none, and one around
// it adds nothing; a full name is shown rather than looked up, so it need not
// be one that reaches the scope (LRM 9.3.4, 37.10 detail 5).
auto LexicalScopeFullName(const LexicalScope& scope)
    -> std::optional<std::string> {
  if (!scope.declaration->Named()) return std::nullopt;
  std::string full = HolderFullName(scope.holder);
  const char* separator = "";
  for (const abi::LexicalScopeRecord* named :
       NamedEnclosingScopes(scope.declaration)) {
    full.append(separator).append(named->name);
    separator = ".";
  }
  return full;
}

auto JoinType(LexicalScopeKind kind) -> std::optional<PLI_INT64> {
  switch (kind) {
    case LexicalScopeKind::kForkJoin:
      return vpiJoin;
    case LexicalScopeKind::kForkJoinAny:
      return vpiJoinAny;
    case LexicalScopeKind::kForkJoinNone:
      return vpiJoinNone;
    case LexicalScopeKind::kBegin:
    case LexicalScopeKind::kFor:
    case LexicalScopeKind::kForeach:
    case LexicalScopeKind::kTask:
    case LexicalScopeKind::kFunction:
    case LexicalScopeKind::kClass:
    case LexicalScopeKind::kClockingBlock:
      return std::nullopt;
  }
  throw InternalError("JoinType: unknown kind");
}

auto VpiNetType(support::DefaultNetType type) -> PLI_INT64 {
  switch (type) {
    case support::DefaultNetType::kWire:
      return vpiWire;
    case support::DefaultNetType::kTri:
      return vpiTri;
    case support::DefaultNetType::kTri0:
      return vpiTri0;
    case support::DefaultNetType::kTri1:
      return vpiTri1;
    case support::DefaultNetType::kWand:
      return vpiWand;
    case support::DefaultNetType::kTriand:
      return vpiTriAnd;
    case support::DefaultNetType::kWor:
      return vpiWor;
    case support::DefaultNetType::kTrior:
      return vpiTriOr;
    case support::DefaultNetType::kTrireg:
      return vpiTriReg;
    case support::DefaultNetType::kUwire:
      return vpiUwire;
    case support::DefaultNetType::kNone:
      return vpiNone;
  }
  throw InternalError("VpiNetType: unknown kind");
}

auto VpiUnconnDrive(support::UnconnectedDrive drive) -> PLI_INT64 {
  switch (drive) {
    case support::UnconnectedDrive::kNone:
      return vpiHighZ;
    case support::UnconnectedDrive::kPull0:
      return vpiPull0;
    case support::UnconnectedDrive::kPull1:
      return vpiPull1;
  }
  throw InternalError("VpiUnconnDrive: unknown kind");
}

// What every instance and namespace answers of the definition it comes from
// (LRM 37.10). Lyra reads neither the delay mode directives nor the decay
// time one (LRM Annex E), and source protection is not Lyra's.
auto DefinitionIntegerProperty(
    PLI_INT32 property, const abi::DefinitionRecord& definition)
    -> std::optional<PLI_INT64> {
  switch (property) {
    case vpiCellInstance:
      return definition.celldefine;
    case vpiDefNetType:
      return VpiNetType(definition.DefaultNetType());
    case vpiUnconnDrive:
      return VpiUnconnDrive(definition.UnconnectedDrive());
    case vpiAutomatic:
      return definition.automatic;
    case vpiProtected:
    case vpiIsProtected:
      return 0;
    case vpiTimeUnit:
      return definition.time_unit_power;
    case vpiTimePrecision:
      return definition.time_precision_power;
    case vpiDefDelayMode:
      return vpiDelayModeNone;
    case vpiDefDecayTime:
      return vpiUndefined;
    default:
      return std::nullopt;
  }
}

// What a block a generate built answers of itself (LRM 37.85).
auto GenerateScopeProperty(PLI_INT32 property, const Scope& scope)
    -> std::optional<PLI_INT64> {
  switch (property) {
    case vpiArrayMember:
      return scope.Segment().Indices().empty() ? 0 : 1;
    case vpiImplicitDecl:
      return scope.Segment().Declaration().ImplicitlyNamed() ? 1 : 0;
    case vpiProtected:
    case vpiIsProtected:
      return 0;
    default:
      return std::nullopt;
  }
}

// What an instance answers of itself (LRM 37.10, 37.11).
auto InstanceProperty(PLI_INT32 property, const Scope& scope)
    -> std::optional<PLI_INT64> {
  switch (property) {
    case vpiTop:
      return IsTopLevel(scope) ? 1 : 0;
    case vpiTopModule:
      return IsTopLevel(scope) && ScopeType(scope) == vpiModule ? 1 : 0;
    case vpiArrayMember:
      return scope.Segment().Indices().empty() ? 0 : 1;
    default:
      return DefinitionIntegerProperty(property, scope.Definition());
  }
}

auto ScopeProperty(PLI_INT32 property, const Scope& scope)
    -> std::optional<PLI_INT64> {
  return IsGenerateScope(scope) ? GenerateScopeProperty(property, scope)
                                : InstanceProperty(property, scope);
}

// Whether `property` asks where an object was written (LRM 37.10), which no
// object answers yet: no constant of a unit says where its text lies.
auto AsksWhereWritten(PLI_INT32 property) -> bool {
  switch (property) {
    case vpiFile:
    case vpiLineNo:
    case vpiDefFile:
    case vpiDefLineNo:
      return true;
    default:
      return false;
  }
}

auto ObjectIntegerProperty(
    PLI_INT32 property, const VpiObject& object, VpiMode mode)
    -> VpiAnswer<PLI_INT64> {
  using Answer = VpiAnswer<PLI_INT64>;
  if (property == vpiType) return TypeOf(object, mode);
  if (AsksWhereWritten(property)) {
    return VpiNotYet("where an object was written");
  }
  const auto answered = [](std::optional<PLI_INT64> answer) -> Answer {
    if (!answer.has_value()) return VpiNotAProperty();
    return *answer;
  };
  const auto of_variable = [&]() {
    return VariablePartIntegerProperty(property, object, mode);
  };
  return std::visit(
      Overloaded{
          [&](Scope* scope) {
            return answered(ScopeProperty(property, *scope));
          },
          [&](const NamespaceObject& ns) -> Answer {
            const abi::DefinitionRecord& definition =
                *ns.scope_declarations->definition;
            if (property == vpiUnit) {
              return support::IsCompilationUnit(definition.Kind()) ? 1 : 0;
            }
            return answered(DefinitionIntegerProperty(property, definition));
          },
          [&](const ScopeArray& array) -> Answer {
            if (property != vpiSize) return VpiNotAProperty();
            return static_cast<PLI_INT64>(ElementsOf(array).size());
          },
          [&](const LexicalScope& scope) -> Answer {
            if (property != vpiJoinType) return VpiNotAProperty();
            return answered(JoinType(scope.declaration->Kind()));
          },
          [&](const VariableObject&) { return of_variable(); },
          [&](const TypespecObject&) { return of_variable(); },
          [&](const TypespecMemberObject&) { return of_variable(); },
          [&](const RangeObject&) { return of_variable(); },
          [&](const ConstantObject&) { return of_variable(); },
          [&](const EnumConstantObject&) { return of_variable(); }},
      object);
}

// What every instance and namespace answers of the definition it comes from
// as text (LRM 37.10).
auto DefinitionStringProperty(
    PLI_INT32 property, const abi::DefinitionRecord& definition)
    -> VpiAnswer<std::optional<std::string>> {
  switch (property) {
    case vpiDefName:
      return std::string(definition.name);
    // The cell bound to the instance, which a module declared inside another
    // shares with it (LRM 33.2.1, 33.7).
    case vpiCell:
      return std::string(definition.cell);
    case vpiLibrary:
      return std::string(definition.library);
    case vpiConfig:
      return std::optional<std::string>{};
    default:
      return VpiNotAProperty();
  }
}

auto ObjectStringProperty(
    PLI_INT32 property, const VpiObject& object, VpiMode mode)
    -> VpiAnswer<std::optional<std::string>> {
  if (property == vpiType) {
    const std::optional<std::string_view> name =
        TypeName(TypeOf(object, mode), mode);
    if (!name.has_value()) return VpiNotAProperty();
    return std::string(*name);
  }
  if (AsksWhereWritten(property)) {
    return VpiNotYet("where an object was written");
  }
  const auto of_variable = [&]() {
    return VariablePartStringProperty(property, object, mode);
  };
  return std::visit(
      Overloaded{
          [&](Scope* scope) -> VpiAnswer<std::optional<std::string>> {
            switch (property) {
              case vpiName:
                return scope->DisplaySegment();
              case vpiFullName:
                return std::string(scope->HierarchicalPath().View());
              default:
                if (IsGenerateScope(*scope)) return VpiNotAProperty();
                return DefinitionStringProperty(property, scope->Definition());
            }
          },
          [&](const NamespaceObject& ns)
              -> VpiAnswer<std::optional<std::string>> {
            switch (property) {
              case vpiName:
                return std::string(ns.scope_declarations->definition->name);
              case vpiFullName:
                return NamespaceFullName(ns);
              default:
                return DefinitionStringProperty(
                    property, *ns.scope_declarations->definition);
            }
          },
          [&](const ScopeArray& array)
              -> VpiAnswer<std::optional<std::string>> {
            switch (property) {
              case vpiName:
                return std::string(array.declaration->name);
              case vpiFullName:
                return HolderFullName(array.holder) + array.declaration->name;
              default:
                return VpiNotAProperty();
            }
          },
          [&](const LexicalScope& scope)
              -> VpiAnswer<std::optional<std::string>> {
            switch (property) {
              case vpiName:
                if (!scope.declaration->Named()) {
                  return std::optional<std::string>{};
                }
                return std::string(scope.declaration->name);
              case vpiFullName:
                return LexicalScopeFullName(scope);
              default:
                return VpiNotAProperty();
            }
          },
          [&](const VariableObject&) { return of_variable(); },
          [&](const TypespecObject&) { return of_variable(); },
          [&](const TypespecMemberObject&) { return of_variable(); },
          [&](const RangeObject&) { return of_variable(); },
          [&](const ConstantObject&) { return of_variable(); },
          [&](const EnumConstantObject&) { return of_variable(); }},
      object);
}

}  // namespace

auto DeclarationPrefix(
    const ScopeHolder& holder, const abi::LexicalScopeRecord* enclosing)
    -> std::string {
  std::string prefix = HolderFullName(holder);
  if (enclosing == nullptr) return prefix;
  for (const abi::LexicalScopeRecord* named : NamedEnclosingScopes(enclosing)) {
    prefix.append(named->name).append(".");
  }
  return prefix;
}

auto VpiObjectHandles::Issue(Target target) -> vpiHandle {
  PLI_UINT32 number = 0;
  if (released_.empty()) {
    number = static_cast<PLI_UINT32>(entries_.size());
    entries_.push_back(Entry{.number = number, .target = std::nullopt});
    issued_.insert(&entries_.back().number);
  } else {
    number = released_.back();
    released_.pop_back();
  }
  Entry& entry = entries_[number];
  entry.target = std::move(target);
  return &entry.number;
}

auto VpiObjectHandles::Find(const PLI_UINT32* handle) -> Target* {
  if (!issued_.contains(handle)) return nullptr;
  Entry& entry = entries_[*handle];
  if (!entry.target.has_value()) return nullptr;
  return &*entry.target;
}

auto VpiObjectHandles::Release(const PLI_UINT32* handle) -> bool {
  if (Find(handle) == nullptr) return false;
  entries_[*handle].target.reset();
  released_.push_back(*handle);
  return true;
}

auto TypeOf(const VpiObject& object, VpiMode mode) -> PLI_INT32 {
  const auto of_variable = [&]() { return TypeOfVariablePart(object, mode); };
  return std::visit(
      Overloaded{
          [](Scope* scope) { return ScopeType(*scope); },
          [](const NamespaceObject&) -> PLI_INT32 { return vpiPackage; },
          [](const ScopeArray& array) { return TypesOf(array).array; },
          [](const LexicalScope& scope) {
            return LexicalScopeType(*scope.declaration);
          },
          [&](const VariableObject&) { return of_variable(); },
          [&](const TypespecObject&) { return of_variable(); },
          [&](const TypespecMemberObject&) { return of_variable(); },
          [&](const RangeObject&) { return of_variable(); },
          [&](const ConstantObject&) { return of_variable(); },
          [&](const EnumConstantObject&) { return of_variable(); }},
      object);
}

auto TypeName(PLI_INT32 type, VpiMode mode) -> std::optional<std::string_view> {
  const bool verilog = SpellsVerilogNames(mode);
  switch (type) {
    case vpiLogicVar:
      return verilog ? "vpiReg" : "vpiLogicVar";
    case vpiArrayVar:
      return verilog ? "vpiRegArray" : "vpiArrayVar";
    case vpiVarBit:
      return verilog ? "vpiRegBit" : "vpiVarBit";
    case vpiMemory:
      return "vpiMemory";
    case vpiMemoryWord:
      return "vpiMemoryWord";
    case vpiBitVar:
      return "vpiBitVar";
    case vpiIntVar:
      return "vpiIntVar";
    case vpiShortIntVar:
      return "vpiShortIntVar";
    case vpiLongIntVar:
      return "vpiLongIntVar";
    case vpiByteVar:
      return "vpiByteVar";
    case vpiIntegerVar:
      return "vpiIntegerVar";
    case vpiTimeVar:
      return "vpiTimeVar";
    case vpiRealVar:
      return "vpiRealVar";
    case vpiShortRealVar:
      return "vpiShortRealVar";
    case vpiEnumVar:
      return "vpiEnumVar";
    case vpiStructVar:
      return "vpiStructVar";
    case vpiUnionVar:
      return "vpiUnionVar";
    case vpiPackedArrayVar:
      return "vpiPackedArrayVar";
    case vpiStringVar:
      return "vpiStringVar";
    case vpiChandleVar:
      return "vpiChandleVar";
    case vpiClassVar:
      return "vpiClassVar";
    case vpiVirtualInterfaceVar:
      return "vpiVirtualInterfaceVar";
    case vpiRefObj:
      return "vpiRefObj";
    case vpiBitTypespec:
      return "vpiBitTypespec";
    case vpiLogicTypespec:
      return "vpiLogicTypespec";
    case vpiIntTypespec:
      return "vpiIntTypespec";
    case vpiShortIntTypespec:
      return "vpiShortIntTypespec";
    case vpiLongIntTypespec:
      return "vpiLongIntTypespec";
    case vpiByteTypespec:
      return "vpiByteTypespec";
    case vpiIntegerTypespec:
      return "vpiIntegerTypespec";
    case vpiTimeTypespec:
      return "vpiTimeTypespec";
    case vpiRealTypespec:
      return "vpiRealTypespec";
    case vpiShortRealTypespec:
      return "vpiShortRealTypespec";
    case vpiEnumTypespec:
      return "vpiEnumTypespec";
    case vpiStructTypespec:
      return "vpiStructTypespec";
    case vpiUnionTypespec:
      return "vpiUnionTypespec";
    case vpiPackedArrayTypespec:
      return "vpiPackedArrayTypespec";
    case vpiEventTypespec:
      return "vpiEventTypespec";
    case vpiStringTypespec:
      return "vpiStringTypespec";
    case vpiChandleTypespec:
      return "vpiChandleTypespec";
    case vpiClassTypespec:
      return "vpiClassTypespec";
    case vpiInterfaceTypespec:
      return "vpiInterfaceTypespec";
    case vpiArrayTypespec:
      return "vpiArrayTypespec";
    case vpiTypespecMember:
      return "vpiTypespecMember";
    case vpiEnumConst:
      return "vpiEnumConst";
    case vpiRange:
      return "vpiRange";
    case vpiConstant:
      return "vpiConstant";
    case vpiModule:
      return "vpiModule";
    case vpiInterface:
      return "vpiInterface";
    case vpiProgram:
      return "vpiProgram";
    case vpiPackage:
      return "vpiPackage";
    case vpiModuleArray:
      return "vpiModuleArray";
    case vpiInterfaceArray:
      return "vpiInterfaceArray";
    case vpiProgramArray:
      return "vpiProgramArray";
    case vpiGenScope:
      return "vpiGenScope";
    case vpiGenScopeArray:
      return "vpiGenScopeArray";
    case vpiBegin:
      return "vpiBegin";
    case vpiNamedBegin:
      return "vpiNamedBegin";
    case vpiFork:
      return "vpiFork";
    case vpiNamedFork:
      return "vpiNamedFork";
    case vpiFor:
      return "vpiFor";
    case vpiForeachStmt:
      return "vpiForeachStmt";
    case vpiTask:
      return "vpiTask";
    case vpiFunction:
      return "vpiFunction";
    case vpiClassDefn:
      return "vpiClassDefn";
    case vpiClockingBlock:
      return "vpiClockingBlock";
    case vpiIterator:
      return "vpiIterator";
    default:
      return std::nullopt;
  }
}

auto Related(
    RuntimeEffects& run, PLI_INT32 relation, const Referent& from, VpiMode mode)
    -> VpiAnswer<std::vector<VpiObject>> {
  return std::visit(
      Overloaded{
          [&](const Simulation&) { return FromDesign(run, relation); },
          [&](const VpiObject& object) {
            return RelatedToObject(relation, object, mode);
          },
          [](const VpiIteration*) -> VpiAnswer<std::vector<VpiObject>> {
            return std::unexpected<std::string>(
                "an iterator is related to no objects");
          }},
      from);
}

auto RelatedOne(RuntimeEffects& run, PLI_INT32 relation, const Referent& from)
    -> VpiAnswer<std::optional<VpiObject>> {
  using Answer = VpiAnswer<std::optional<VpiObject>>;
  const auto* iteration = std::get_if<const VpiIteration*>(&from);
  if (iteration != nullptr) {
    if (relation != vpiUse) {
      return std::unexpected<std::string>(
          "an iterator is related only to its use");
    }
    return (*iteration)->use;
  }
  const auto* object = std::get_if<VpiObject>(&from);
  if (object == nullptr) return VpiNotYet("this relation from no object");
  const auto of_variable = [&]() {
    return OneRelatedToVariablePart(run, relation, *object);
  };
  switch (relation) {
    case vpiModule:
      return Answer{InstanceAroundObject(*object, IsModule)};
    case vpiInstance:
      return Answer{InstanceAroundObject(*object, IsInstance)};
    // An element of an array has the array as its parent (LRM 37.11, 37.85).
    case vpiParent:
      return std::visit(
          Overloaded{
              [](Scope* scope) -> Answer {
                if (scope->Segment().Indices().empty()) {
                  return VpiNotYet("the parent of a scope that is no element");
                }
                return VpiObject{ScopeArray{
                    .holder = scope->Parent(),
                    .declaration = &scope->Segment().Declaration()}};
              },
              [](const NamespaceObject&) -> Answer {
                return VpiNotYet("the parent of a package");
              },
              [](const ScopeArray&) -> Answer {
                return VpiNotYet("the parent of an array");
              },
              [](const LexicalScope&) -> Answer {
                return VpiNotYet(
                    "the parent of a block, a subroutine, a class or a "
                    "clocking block");
              },
              [&](const VariableObject&) { return of_variable(); },
              [&](const TypespecObject&) { return of_variable(); },
              [&](const TypespecMemberObject&) { return of_variable(); },
              [&](const RangeObject&) { return of_variable(); },
              [&](const ConstantObject&) { return of_variable(); },
              [&](const EnumConstantObject&) { return of_variable(); }},
          *object);
    default:
      if (IsVariablePart(*object)) return of_variable();
      return VpiNotYet("this relation");
  }
}

auto ObjectNamed(
    RuntimeEffects& run, std::string_view name, const Referent& within)
    -> VpiAnswer<std::optional<VpiObject>> {
  const std::optional<HierarchicalName> read = ReadHierarchicalName(name);
  return std::visit(
      Overloaded{
          [&](const Simulation&) -> VpiAnswer<std::optional<VpiObject>> {
            if (!read.has_value()) return std::optional<VpiObject>{};
            return NamedFromDesign(run, *read);
          },
          [&](const VpiObject& object) -> VpiAnswer<std::optional<VpiObject>> {
            if (!read.has_value()) return std::optional<VpiObject>{};
            return NamedWithin(object, *read);
          },
          [](const VpiIteration*) -> VpiAnswer<std::optional<VpiObject>> {
            return std::unexpected<std::string>("an iterator holds no names");
          }},
      within);
}

auto ElementAt(const VpiObject& of, std::span<const std::int64_t> indices)
    -> VpiAnswer<std::optional<VpiObject>> {
  if (const auto* variable = std::get_if<VariableObject>(&of)) {
    return VariableElementAt(*variable, indices);
  }
  const auto* array = std::get_if<ScopeArray>(&of);
  if (array == nullptr) return VpiNotYet("an element of this object");
  for (Scope* scope : ElementsOf(*array)) {
    if (std::ranges::equal(scope->Segment().Indices(), indices)) {
      return VpiObject{scope};
    }
  }
  return std::optional<VpiObject>{};
}

auto IntegerProperty(
    RuntimeEffects& run, PLI_INT32 property, const Referent& of, VpiMode mode)
    -> VpiAnswer<PLI_INT64> {
  return std::visit(
      Overloaded{
          [&](const Simulation&) -> VpiAnswer<PLI_INT64> {
            // Both are the simulation time unit (LRM 38.6), as DPI's are (LRM
            // H.13).
            const TimeResolution resolution = TimeResolutionOf(run, nullptr);
            switch (property) {
              case vpiTimeUnit:
                return resolution.unit_power;
              case vpiTimePrecision:
                return resolution.precision_power;
              default:
                return VpiNotAProperty();
            }
          },
          [&](const VpiObject& object) {
            return ObjectIntegerProperty(property, object, mode);
          },
          // An iterator answers its own type and the type it iterates (LRM
          // 37.84).
          [&](const VpiIteration* iteration) -> VpiAnswer<PLI_INT64> {
            switch (property) {
              case vpiType:
                return vpiIterator;
              case vpiIteratorType:
                return iteration->relation;
              default:
                return std::unexpected<std::string>(
                    "an iterator has no such property");
            }
          }},
      of);
}

auto StringProperty(PLI_INT32 property, const Referent& of, VpiMode mode)
    -> VpiAnswer<std::optional<std::string>> {
  return std::visit(
      Overloaded{
          [](const Simulation&) -> VpiAnswer<std::optional<std::string>> {
            return VpiNotAProperty();
          },
          [&](const VpiObject& object) {
            return ObjectStringProperty(property, object, mode);
          },
          [&](const VpiIteration*) -> VpiAnswer<std::optional<std::string>> {
            if (property != vpiType) {
              return std::unexpected<std::string>(
                  "an iterator has no such property");
            }
            return std::string(*TypeName(vpiIterator, mode));
          }},
      of);
}

auto TimeUnitOf(RuntimeEffects& run, const Referent& of)
    -> VpiAnswer<std::int8_t> {
  return std::visit(
      Overloaded{
          [&](const Simulation&) -> VpiAnswer<std::int8_t> {
            return TimeResolutionOf(run, nullptr).unit_power;
          },
          [&](const VpiObject& object) -> VpiAnswer<std::int8_t> {
            const std::optional<ScopeHolder> holder = HolderOf(object);
            return TimeResolutionOf(
                       run, holder.has_value()
                                ? ScopeDeclarationsOf(*holder).definition
                                : nullptr)
                .unit_power;
          },
          [](const VpiIteration*) -> VpiAnswer<std::int8_t> {
            return std::unexpected<std::string>("an iterator reads no time");
          }},
      of);
}

}  // namespace lyra::runtime

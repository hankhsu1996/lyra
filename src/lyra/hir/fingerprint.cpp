#include "lyra/hir/fingerprint.hpp"

#include <algorithm>
#include <array>
#include <bit>
#include <boost/pfr/core.hpp>
#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <ranges>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/class_decl.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/class_registry.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/external_class.hpp"
#include "lyra/hir/external_scope_class.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/unit_signature.hpp"
#include "lyra/hir/unit_signatures.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/stable_hash/fingerprint.hpp"
#include "lyra/stable_hash/stable_hasher.hpp"

namespace lyra::hir {

namespace {

using stable_hash::Fingerprint;
using stable_hash::StableHasher;

// What one unit publishes, with each other unit its signature names written as
// its position in `reaches` rather than as anything of that unit's own, so the
// fingerprint is finished only once those units' are known.
struct UnresolvedFingerprint {
  Fingerprint own;
  std::vector<std::string> reaches;
};

// A reference to the unit being fingerprinted, which is which application
// asked and so nothing of its own.
struct Itself {};

// A reference to another unit by the fingerprint of what it publishes.
struct PublishedBy {
  Fingerprint fingerprint;
};

// A reference to another unit by the position it was first reached at.
struct ReachedAt {
  std::size_t position = 0;
};

// A reference to another unit by the name it is compiled under, which is what
// a call into it names.
struct NamedAs {
  std::string unit;
};

// What one unit's reference to another is written as. The alternative is
// written before what it holds, so no two of them are read as one another.
using UnitReference = std::variant<Itself, PublishedBy, ReachedAt, NamedAs>;

void Write(StableHasher& hash, const UnitReference& reference) {
  hash.Number(reference.index());
  std::visit(
      Overloaded{
          [](const Itself&) {},
          [&](const PublishedBy& published) { hash.Of(published.fingerprint); },
          [&](const ReachedAt& reached) { hash.Number(reached.position); },
          [&](const NamedAs& named) { hash.Text(named.unit); }},
      reference);
}

// The units one unit reaches, in the order it first reached them.
class Reached {
 public:
  explicit Reached(std::string_view own) : own_(own) {
  }

  // Whether `unit` is the one whose references these are.
  [[nodiscard]] auto IsOwn(std::string_view unit) const -> bool {
    return unit == own_;
  }

  auto Reach(const std::string& unit) -> UnitReference {
    if (IsOwn(unit)) return Itself{};
    const auto [at, added] = positions_.try_emplace(unit, units_.size());
    if (added) units_.push_back(unit);
    return ReachedAt{.position = at->second};
  }

  auto Units() && -> std::vector<std::string> {
    return std::move(units_);
  }

 private:
  std::string_view own_;
  std::vector<std::string> units_;
  ByUnit<std::size_t> positions_;
};

// A signature names another unit only to say what it publishes is laid out
// against what that unit publishes, so every unit it names is one it reaches.
class SignatureReferences {
 public:
  explicit SignatureReferences(std::string_view own) : reached_(own) {
  }

  auto Read(const std::string& unit) -> UnitReference {
    return reached_.Reach(unit);
  }

  auto Call(const std::string& unit) -> UnitReference {
    return reached_.Reach(unit);
  }

  auto Reaches() && -> std::vector<std::string> {
    return std::move(reached_).Units();
  }

 private:
  Reached reached_;
};

// What a unit's compiled code holds of each other unit. A read of another
// unit's declarations is laid out against what that unit published, so it is
// written as what was published. A call into another unit names the code it
// runs by that unit's name and the callable's, never by the code itself, so a
// call is written as that name: what the callee's body does is no part of the
// caller, and changing it leaves the caller as it was. A place in the source
// is what a report the program makes prints of it.
class BodyReferences {
 public:
  BodyReferences(
      const CompilationUnit& unit, const ByUnit<Fingerprint>& published,
      const diag::SourceManager& sources)
      : unit_(&unit), published_(&published), sources_(&sources) {
  }

  auto Read(const std::string& unit) -> UnitReference {
    if (unit == unit_->name) return Itself{};
    const auto found = published_->find(unit);
    if (found == published_->end()) {
      throw InternalError(
          std::format(
              "hir::BodyFingerprintOf: unit '{}' reads unit '{}', which "
              "published nothing",
              unit_->name, unit));
    }
    return PublishedBy{.fingerprint = found->second};
  }

  auto Call(const std::string& unit) const -> UnitReference {
    if (unit == unit_->name) return Itself{};
    return NamedAs{.unit = unit};
  }

  auto CallScopeClass(ExternalScopeClassId scope_class) const -> UnitReference {
    return Call(unit_->external_scope_classes.Get(scope_class).unit_name);
  }

  void Place(StableHasher& hash, const diag::SourceSpan& span) const {
    hash.Text(diag::RuntimeOriginOf(span, *sources_));
  }

 private:
  const CompilationUnit* unit_;
  const ByUnit<Fingerprint>* published_;
  const diag::SourceManager* sources_;
};

template <typename T, template <typename...> class Of>
constexpr bool kIsA = false;

template <template <typename...> class Of, typename... Arguments>
constexpr bool kIsA<Of<Arguments...>, Of> = true;

// Which alternative of the variant `Variant` the type `Arm` is.
template <typename Arm, typename Variant>
struct AlternativeOf;

template <typename Arm, typename... Arms>
struct AlternativeOf<Arm, std::variant<Arms...>> {
  static constexpr std::size_t kIndex = [] {
    constexpr std::array kIsArm{std::is_same_v<Arm, Arms>...};
    return static_cast<std::size_t>(
        std::ranges::find(kIsArm, true) - kIsArm.begin());
  }();
};

// Writes everything a node's equality compares, which is every field it has:
// an aggregate's fields are walked without being named, so a field added to a
// node is fingerprinted without anyone remembering to. The nodes named below
// are the ones that hold a reference to a unit, which `References` writes, or
// whose content is not their fields.
template <typename References>
class Walker {
 public:
  Walker(StableHasher& hash, References& references)
      : hash_(&hash), references_(&references) {
  }

  template <typename T>
  void Add(const T& value) {
    if constexpr (std::is_same_v<T, bool>) {
      hash_->Number(value ? 1 : 0);
    } else if constexpr (std::is_enum_v<T>) {
      hash_->Number(static_cast<std::uint64_t>(std::to_underlying(value)));
    } else if constexpr (std::is_integral_v<T>) {
      hash_->Number(static_cast<std::uint64_t>(value));
    } else if constexpr (std::is_floating_point_v<T>) {
      static_assert(sizeof(T) <= sizeof(std::uint64_t));
      using Bits = std::conditional_t<
          sizeof(T) == sizeof(std::uint32_t), std::uint32_t, std::uint64_t>;
      hash_->Number(std::bit_cast<Bits>(value));
    } else if constexpr (std::is_same_v<T, std::string>) {
      hash_->Text(value);
    } else if constexpr (std::is_same_v<T, VariablePattern>) {
      // A variable pattern is its name and nothing else, and the name is one
      // the program never reads, so which pattern it is says all there is.
      static_assert(boost::pfr::tuple_size_v<VariablePattern> == 1);
    } else if constexpr (kIsA<T, std::optional>) {
      Add(value.has_value());
      if (value.has_value()) Add(*value);
    } else if constexpr (kIsA<T, std::variant>) {
      Add(value.index());
      std::visit([this](const auto& arm) { Add(arm); }, value);
    } else if constexpr (kIsA<T, std::pair>) {
      Add(value.first);
      Add(value.second);
    } else if constexpr (kIsA<T, base::Registry>) {
      Add(value.size());
      for (const auto id : value.Ids()) {
        const bool defined = value.IsDefined(id);
        Add(defined);
        if (defined) Add(value.Get(id));
      }
    } else if constexpr (std::ranges::sized_range<T>) {
      Add(std::ranges::size(value));
      for (const auto& element : value) {
        Add(element);
      }
    } else {
      boost::pfr::for_each_field(
          value, [this](const auto& field) { Add(field); });
    }
  }

  void Add(const Type& type) {
    type.Visit([this]<typename Arm>(const Arm& arm) {
      Add(AlternativeOf<Arm, Type::Data>::kIndex);
      Add(arm);
    });
  }

  void Add(const ClassRegistry& classes) {
    Add(classes.size());
    for (const ClassId id : classes.Ids()) {
      Add(classes.PathOf(id));
      const bool defined = classes.IsDefined(id);
      Add(defined);
      if (defined) Add(classes.Get(id));
    }
  }

  void Add(const diag::SourceSpan& span) {
    references_->Place(*hash_, span);
  }

  // The name the unit is compiled under is which application asked for it,
  // and the only part of it left out.
  void Add(const CompilationUnit& unit) {
    const auto& [name, source_name, library_binding, role, types, builtins, root_scope, classes, external_scope_classes, external_classes, foreign_imports] =
        unit;
    Add(source_name);
    Add(library_binding);
    Add(role);
    Add(types);
    Add(builtins);
    Add(root_scope);
    Add(classes);
    Add(external_scope_classes);
    Add(external_classes);
    Add(foreign_imports);
  }

  void Add(const UnitSignature& signature) {
    const auto& [unit_name, types, classes, unit] = signature;
    Add(types);
    Add(classes);
    Add(unit);
  }

  // The names below, and a variable pattern's, are read by no program compiled
  // from the unit: they reach the spelling of emitted text at most, never
  // anything a run prints, compares or links against. So they are left out,
  // and renaming one changes nothing the unit is. Where the lowering reads
  // whether there is a name at all, that much is kept.
  void Add(const ProceduralVarDecl& decl) {
    const auto& [name, type, lifetime, lifetime_extended, init] = decl;
    Add(name.has_value());
    Add(type);
    Add(lifetime);
    Add(lifetime_extended);
    Add(init);
  }

  void Add(const Stmt& stmt) {
    const auto& [label, data, span] = stmt;
    Add(label.has_value());
    Add(data);
    Add(span);
  }

  void Add(const ClassField& field) {
    const auto& [name, type, is_published] = field;
    Add(type);
    Add(is_published);
  }

  void Add(const WithClause& clause) {
    const auto& [id, element_name, expr] = clause;
    Add(id);
    Add(expr);
  }

  void Add(const InterfacePortDecl& port) {
    const auto& [name, array_dims] = port;
    Add(array_dims);
  }

  void Add(const ExternalClassRef& ref) {
    const auto& [unit_name, class_path] = ref;
    Write(*hash_, references_->Read(unit_name));
    Add(class_path);
  }

  void Add(const ExternalClassPropertyTarget& target) {
    const auto& [unit_name, class_path, property] = target;
    Write(*hash_, references_->Read(unit_name));
    Add(class_path);
    Add(property);
  }

  void Add(const ExternalStaticPropertyTarget& target) {
    const auto& [unit_name, class_path, property_name] = target;
    Write(*hash_, references_->Read(unit_name));
    Add(class_path);
    Add(property_name);
  }

  void Add(const ExternalClassMethodTarget& target) {
    const auto& [unit_name, class_path, method_name] = target;
    Write(*hash_, references_->Read(unit_name));
    Add(class_path);
    Add(method_name);
  }

  void Add(const ExternalDispatchSlot& slot) {
    const auto& [unit_name, class_path, behavior] = slot;
    Write(*hash_, references_->Read(unit_name));
    Add(class_path);
    Add(behavior);
  }

  void Add(const ExternalClass& record) {
    const auto& [unit_name, class_path, base, is_interface_class, implements, properties, local_property_types, static_properties, constructor, methods, overrides, takes_declaring_instance] =
        record;
    Write(*hash_, references_->Read(unit_name));
    Add(class_path);
    Add(base);
    Add(is_interface_class);
    Add(implements);
    Add(properties);
    Add(local_property_types);
    Add(static_properties);
    Add(constructor);
    Add(methods);
    Add(overrides);
    Add(takes_declaring_instance);
  }

  void Add(const ExternalScopeClass& record) {
    const auto& [unit_name, signature] = record;
    Write(*hash_, references_->Read(unit_name));
    Add(signature);
  }

  void Add(const ExternalUnitValueRef& ref) {
    const auto& [unit_name, variable_name, value_type] = ref;
    Write(*hash_, references_->Read(unit_name));
    Add(variable_name);
    Add(value_type);
  }

  void Add(const TypeDeclarationRef& ref) {
    const auto& [unit_name, path] = ref;
    Write(*hash_, references_->Read(unit_name));
    Add(path);
  }

  void Add(const UnitObjectType& type) {
    const auto& [unit_name, class_path] = type;
    Write(*hash_, references_->Read(unit_name));
    Add(class_path);
  }

  void Add(const VirtualInterfaceType& type) {
    const auto& [unit_name] = type;
    Write(*hash_, references_->Read(unit_name));
  }

  void Add(const ExternalUnitSubroutineRef& ref) {
    const auto& [unit_name, subroutine_name, interface] = ref;
    Write(*hash_, references_->Call(unit_name));
    Add(subroutine_name);
    Add(interface);
  }

  void Add(const ExternalUnitMethodRef& ref) {
    const auto& [receiver, scope_class, callable] = ref;
    Write(*hash_, references_->CallScopeClass(scope_class));
    Add(receiver);
    Add(scope_class);
    Add(callable);
  }

  void Add(const ExternalMethodCallee& callee) {
    const auto& [target, slot, interface] = callee;
    Write(*hash_, references_->Call(target.unit_name));
    Add(target);
    Add(slot);
    Add(interface);
  }

  void Add(const ClassNewExpr& expr) {
    const auto& [class_ref, arguments, declaring_instance] = expr;
    CallInto(class_ref);
    Add(class_ref);
    Add(arguments);
    Add(declaring_instance);
  }

  // A class extending another runs that class's construction and holds its
  // methods; one implementing an interface class holds none of its code,
  // since an interface class has none (LRM 8.26).
  void Add(const ClassDecl& decl) {
    const auto& [is_interface_class, takes_declaring_instance, base, implements, conforming, fields, static_properties, methods, constructor, base_call, field_inits, static_init, static_property_inits, procedural_scopes] =
        decl;
    if (base.has_value()) CallInto(*base);
    Add(is_interface_class);
    Add(takes_declaring_instance);
    Add(base);
    Add(implements);
    Add(conforming);
    Add(fields);
    Add(static_properties);
    Add(methods);
    Add(constructor);
    Add(base_call);
    Add(field_inits);
    Add(static_init);
    Add(static_property_inits);
    Add(procedural_scopes);
  }

 private:
  // The code of the class `ref` names runs here, where another unit declares
  // it.
  void CallInto(const ClassRef& ref) {
    std::visit(
        Overloaded{
            [](const LocalClassRef&) {},
            [this](const ExternalClassRef& external) {
              Write(*hash_, references_->Call(external.unit_name));
            }},
        ref);
  }

  StableHasher* hash_;
  References* references_;
};

// Resolves what every unit publishes, each unit its signature names written as
// that unit's own (the resolution of a content-addressed derivation in Nix,
// RFC 62), once the units it names are resolved -- the order the strongly
// connected components of the graph complete in (Tarjan): a component
// completes only after every component it reaches has. Units whose signatures
// name one another in a cycle are resolved as one set, each name inside it
// written as the named unit's place in the set.
class Resolver {
 public:
  explicit Resolver(const ByUnit<UnresolvedFingerprint>& units)
      : units_(&units) {
  }

  auto Run() && -> ByUnit<Fingerprint> {
    for (const auto& [name, unit] : *units_) {
      if (!visits_.contains(name)) Visit(name);
    }
    return std::move(resolved_);
  }

 private:
  struct Visiting {
    std::size_t index = 0;
    std::size_t low = 0;
    bool on_stack = false;
  };

  [[nodiscard]] auto UnitNamed(std::string_view name) const
      -> const UnresolvedFingerprint& {
    const auto found = units_->find(name);
    if (found == units_->end()) {
      throw InternalError(
          std::format(
              "hir::PublishedFingerprintsOf: a signature names unit '{}', "
              "which published nothing",
              name));
    }
    return found->second;
  }

  void Visit(const std::string& name) {
    const std::size_t index = visits_.size();
    visits_.emplace(
        name, Visiting{.index = index, .low = index, .on_stack = true});
    stack_.push_back(name);
    for (const std::string& reached : UnitNamed(name).reaches) {
      if (!visits_.contains(reached)) {
        Visit(reached);
        visits_.at(name).low =
            std::min(visits_.at(name).low, visits_.at(reached).low);
      } else if (visits_.at(reached).on_stack) {
        visits_.at(name).low =
            std::min(visits_.at(name).low, visits_.at(reached).index);
      }
    }
    if (visits_.at(name).low != visits_.at(name).index) return;
    std::vector<std::string> component;
    while (true) {
      std::string member = std::move(stack_.back());
      stack_.pop_back();
      visits_.at(member).on_stack = false;
      const bool last = member == name;
      component.push_back(std::move(member));
      if (last) break;
    }
    Resolve(std::move(component));
  }

  // A component of one unit is the case of a set of one. Its members are put
  // in an order their content decides, so where one unit lies in it says what
  // the unit is and not what it is called; the name settles only an order
  // their content leaves open, which costs units that would have been one
  // being kept apart and never the reverse.
  void Resolve(std::vector<std::string> members) {
    std::ranges::sort(members, [&](const std::string& a, const std::string& b) {
      const Fingerprint& own_a = UnitNamed(a).own;
      const Fingerprint& own_b = UnitNamed(b).own;
      return own_a != own_b ? own_a < own_b : a < b;
    });
    ByUnit<std::size_t> position;
    for (std::size_t i = 0; i < members.size(); ++i) {
      position.emplace(members[i], i);
    }
    StableHasher set;
    set.Number(members.size());
    for (const std::string& member : members) {
      const UnresolvedFingerprint& unit = UnitNamed(member);
      set.Of(unit.own);
      set.Number(unit.reaches.size());
      for (const std::string& reached : unit.reaches) {
        const auto inside = position.find(reached);
        set.Number(inside != position.end() ? 1 : 0);
        if (inside != position.end()) {
          set.Number(inside->second);
        } else {
          set.Of(resolved_.at(reached));
        }
      }
    }
    const Fingerprint whole = set.Finish();
    for (std::size_t i = 0; i < members.size(); ++i) {
      StableHasher one;
      one.Of(whole);
      one.Number(i);
      resolved_.emplace(members[i], one.Finish());
    }
  }

  const ByUnit<UnresolvedFingerprint>* units_;
  ByUnit<Visiting> visits_;
  std::vector<std::string> stack_;
  ByUnit<Fingerprint> resolved_;
};

}  // namespace

auto PublishedFingerprintsOf(const UnitSignatures& signatures)
    -> ByUnit<Fingerprint> {
  ByUnit<UnresolvedFingerprint> published;
  for (const auto& [name, signature] : signatures) {
    StableHasher hash;
    SignatureReferences references(signature.unit_name);
    Walker(hash, references).Add(signature);
    published.emplace(
        name,
        UnresolvedFingerprint{
            .own = hash.Finish(), .reaches = std::move(references).Reaches()});
  }
  return Resolver(published).Run();
}

auto BodyFingerprintOf(
    const CompilationUnit& unit, const ByUnit<Fingerprint>& published,
    const diag::SourceManager& sources) -> Fingerprint {
  StableHasher hash;
  BodyReferences references(unit, published, sources);
  Walker(hash, references).Add(unit);
  return hash.Finish();
}

}  // namespace lyra::hir

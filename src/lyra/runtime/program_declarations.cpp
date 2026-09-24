#include "lyra/runtime/program_declarations.hpp"

#include <memory>
#include <span>
#include <unordered_map>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/member_storage.hpp"
#include "lyra/support/member_storage_kind.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::runtime {

namespace {

// One body's description and the descriptors it points at, which travel
// together because the description names them rather than holding them.
struct DeclaredVariables {
  std::vector<MemberStorageDescriptor> descriptors;
  MemberStorageSchema schema;
};

// What the artifacts composed into this process have declared. Each entry keeps
// its address for as long as it stands, because generated code holds the
// address of what was declared rather than a copy of it.
auto DeclaredBodies() -> std::vector<std::unique_ptr<DeclaredVariables>>& {
  static std::vector<std::unique_ptr<DeclaredVariables>> bodies;
  return bodies;
}

auto DeclaredSharedStorage() -> std::vector<std::unique_ptr<MemberStorage>>& {
  static std::vector<std::unique_ptr<MemberStorage>> storage;
  return storage;
}

// One closure's definition and the descriptors its schema names, which travel
// together for the reason a body's do.
struct DeclaredClosure {
  std::vector<MemberStorageDescriptor> captures;
  ClosureDefinition definition;
};

auto DeclaredClosures() -> std::vector<std::unique_ptr<DeclaredClosure>>& {
  static std::vector<std::unique_ptr<DeclaredClosure>> closures;
  return closures;
}

// One behavior a class takes over, as its own artifact can state it: the class
// that introduced the behavior is named by the cell holding its definition,
// because that class may be stated by an artifact that has not spoken yet.
struct PendingTakeover {
  const ObjectDefinition* const* introduced_by = nullptr;
  std::uint32_t ordinal = 0;
  ErasedMethodEntry body = nullptr;
};

// One class a scope answers a name with, named the same way and for the same
// reason.
struct PendingClass {
  AbiStringRef name;
  const ObjectDefinition* const* definition = nullptr;
};

// What one class's own artifact stated it adds, kept until the lineage can be
// laid out. Every table the definition ends up naming points into this, so it
// outlives every value built from that definition. The tables a cell has to be
// read for are built where it is read, which is the one place the cells hold
// what they will hold.
struct DeclaredContribution {
  const ObjectDefinition* const* base = nullptr;
  std::vector<MemberStorageDescriptor> members;
  std::vector<ErasedMethodEntry> introductions;
  std::vector<DeclaredName> property_names;
  std::vector<DeclaredName> behavior_names;
  std::vector<DeclaredBody> body_names;
  std::vector<PendingTakeover> pending_takeovers;
  std::vector<DispatchTakeover> takeovers;
  std::vector<ScopeCallable> subroutines;
  std::vector<ScopeCallable> exports;
  std::vector<PendingClass> pending_classes;
  std::vector<ScopeClass> declared_classes;
  RealizedClass realization;
};

// One class a unit declared: the definition every value of it carries, and what
// the artifact stated about it. Which kind of definition it is, is settled by
// the artifact that declared it and never changes afterwards. The definition
// keeps its address for as long as it stands, because a cell of every artifact
// that names the class holds it.
struct DeclaredClass {
  std::variant<ObjectDefinition, ScopeDefinition> definition;
  DeclaredContribution adds;
  // A lineage never returns to a class it passed, since a class cannot extend
  // itself or anything extending it (LRM 8.13), so marking a class before its
  // base is laid out records that the pass has reached it and nothing more.
  bool reached = false;

  [[nodiscard]] auto Definition() -> ObjectDefinition* {
    return std::visit(
        [](auto& held) -> ObjectDefinition* { return &held; }, definition);
  }
};

auto DeclaredClasses() -> std::vector<std::unique_ptr<DeclaredClass>>& {
  static std::vector<std::unique_ptr<DeclaredClass>> classes;
  return classes;
}

// What a class's definition was declared as, reached by the definition itself,
// which is the only thing an artifact holds of a class after declaring it.
auto StatedClasses()
    -> std::unordered_map<const ObjectDefinition*, DeclaredClass*>& {
  static std::unordered_map<const ObjectDefinition*, DeclaredClass*> stated;
  return stated;
}

auto Stated(const ObjectDefinition* cls) -> DeclaredClass& {
  const auto found = StatedClasses().find(cls);
  if (found == StatedClasses().end()) {
    throw InternalError(
        "program declarations: an artifact states something about a class this "
        "runtime never declared");
  }
  return *found->second;
}

auto Declare(std::unique_ptr<DeclaredClass> declared) -> ObjectDefinition* {
  ObjectDefinition* definition = declared->Definition();
  StatedClasses().emplace(definition, declared.get());
  DeclaredClasses().push_back(std::move(declared));
  return definition;
}

}  // namespace

auto RealizeMemberStorage(support::DeclaredMemberStorage described)
    -> MemberStorageDescriptor {
  const support::ValueDomain domain = described.domain;
  switch (described.kind) {
    case support::MemberStorageKind::kObservableCell:
      return ObservableCellStorage{.domain = domain};
    case support::MemberStorageKind::kResolvedNet:
      return ResolvedNetStorage{.domain = domain};
    case support::MemberStorageKind::kSampledHistory:
      return SampledHistoryStorage{.domain = domain};
    case support::MemberStorageKind::kValueCell:
      return ValueCellStorage{.domain = domain};
    case support::MemberStorageKind::kInlineValue:
      return InlineValueStorage{.domain = domain};
    case support::MemberStorageKind::kBorrowedHandle:
      return BorrowedHandleStorage{};
    case support::MemberStorageKind::kPromotedScope:
      return PromotedScopeStorage{};
    case support::MemberStorageKind::kNamedEvent:
      return NamedEventStorage{};
    case support::MemberStorageKind::kCancellationTarget:
      return CancellationTargetStorage{};
    case support::MemberStorageKind::kChannelCancellation:
      return ChannelCancellationStorage{};
    case support::MemberStorageKind::kEvaluationAttempts:
      return EvaluationAttemptsStorage{};
  }
  throw InternalError(
      "program declarations: an artifact states a storage kind this runtime "
      "does not realize");
}

auto DeclareVariableSchema(
    std::span<const support::DeclaredMemberStorage> described)
    -> const MemberStorageSchema* {
  auto declared = std::make_unique<DeclaredVariables>();
  declared->descriptors.reserve(described.size());
  for (const support::DeclaredMemberStorage& member : described) {
    declared->descriptors.push_back(RealizeMemberStorage(member));
  }
  declared->schema = MemberStorageSchema{
      .data = declared->descriptors.data(),
      .size = static_cast<std::uint32_t>(declared->descriptors.size())};
  const MemberStorageSchema* schema = &declared->schema;
  DeclaredBodies().push_back(std::move(declared));
  return schema;
}

auto DeclareClosure(
    std::span<const support::DeclaredMemberStorage> captures, ClosureBody body)
    -> const ClosureDefinition* {
  auto declared = std::make_unique<DeclaredClosure>();
  declared->captures.reserve(captures.size());
  for (const support::DeclaredMemberStorage& capture : captures) {
    declared->captures.push_back(RealizeMemberStorage(capture));
  }
  declared->definition.body = body;
  declared->definition.captures = MemberStorageSchema{
      .data = declared->captures.data(),
      .size = static_cast<std::uint32_t>(declared->captures.size())};
  const ClosureDefinition* definition = &declared->definition;
  DeclaredClosures().push_back(std::move(declared));
  return definition;
}

auto DeclareSharedStorage(support::DeclaredMemberStorage described) -> void* {
  auto storage =
      std::make_unique<MemberStorage>(RealizeMemberStorage(described));
  void* address = storage->Address();
  DeclaredSharedStorage().push_back(std::move(storage));
  return address;
}

auto DeclareClass() -> ObjectDefinition* {
  auto declared = std::make_unique<DeclaredClass>();
  declared->definition.emplace<ObjectDefinition>();
  return Declare(std::move(declared));
}

auto DeclareScopeClass(
    std::int8_t time_unit_power, std::int8_t time_precision_power)
    -> ScopeDefinition* {
  auto declared = std::make_unique<DeclaredClass>();
  ScopeDefinition& scope = declared->definition.emplace<ScopeDefinition>();
  scope.program.metadata = ScopeMetadata{time_unit_power, time_precision_power};
  Declare(std::move(declared));
  return &scope;
}

void DeclareBase(ObjectDefinition* cls, const ObjectDefinition* const* base) {
  Stated(cls).adds.base = base;
}

void DeclareMembers(
    ObjectDefinition* cls,
    std::span<const support::DeclaredMemberStorage> members) {
  std::vector<MemberStorageDescriptor>& described = Stated(cls).adds.members;
  described.reserve(members.size());
  for (const support::DeclaredMemberStorage& member : members) {
    described.push_back(RealizeMemberStorage(member));
  }
}

void DeclareIntroduction(ObjectDefinition* cls, ErasedMethodEntry body) {
  Stated(cls).adds.introductions.push_back(body);
}

void DeclareTakeover(
    ObjectDefinition* cls, const ObjectDefinition* const* introduced_by,
    std::uint32_t ordinal, ErasedMethodEntry body) {
  Stated(cls).adds.pending_takeovers.push_back(
      PendingTakeover{
          .introduced_by = introduced_by, .ordinal = ordinal, .body = body});
}

void DeclarePropertyName(
    ObjectDefinition* cls, AbiStringRef name, std::uint32_t position) {
  Stated(cls).adds.property_names.push_back(
      DeclaredName{.name = name, .position = position});
}

void DeclareBehaviorName(
    ObjectDefinition* cls, AbiStringRef name, std::uint32_t position) {
  Stated(cls).adds.behavior_names.push_back(
      DeclaredName{.name = name, .position = position});
}

void DeclareBodyName(
    ObjectDefinition* cls, AbiStringRef name, ErasedMethodEntry body) {
  Stated(cls).adds.body_names.emplace_back(name, body);
}

void DeclareScopeProgram(
    ScopeDefinition* scope, ScopeEntry resolve_state,
    ScopeEntry initialize_state, ScopeEntry create_processes,
    ScopeConstructEntry construct) {
  scope->program.resolve_state = resolve_state;
  scope->program.initialize_state = initialize_state;
  scope->program.create_processes = create_processes;
  scope->construct = construct;
}

void DeclareSubroutineName(
    ScopeDefinition* scope, AbiStringRef name, ErasedScopeCallable entry) {
  Stated(scope).adds.subroutines.emplace_back(name, entry);
}

void DeclareExportName(
    ScopeDefinition* scope, AbiStringRef name, ErasedScopeCallable entry) {
  Stated(scope).adds.exports.emplace_back(name, entry);
}

void DeclareClassName(
    ScopeDefinition* scope, AbiStringRef name,
    const ObjectDefinition* const* declared) {
  Stated(scope).adds.pending_classes.push_back(
      PendingClass{.name = name, .definition = declared});
}

namespace {

void LayOut(DeclaredClass& declared) {
  if (declared.reached) {
    return;
  }
  declared.reached = true;
  DeclaredContribution& adds = declared.adds;
  const ObjectDefinition* base = nullptr;
  if (adds.base != nullptr) {
    base = *adds.base;
    if (base == nullptr) {
      throw InternalError(
          "program declarations: a class extends one no artifact of this "
          "program declared");
    }
    LayOut(Stated(base));
  }
  adds.takeovers.reserve(adds.pending_takeovers.size());
  for (const PendingTakeover& taken : adds.pending_takeovers) {
    const ObjectDefinition* introducer = *taken.introduced_by;
    if (introducer == nullptr) {
      throw InternalError(
          "program declarations: a class takes over a behavior of one no "
          "artifact of this program declared");
    }
    adds.takeovers.emplace_back(introducer, taken.ordinal, taken.body);
  }
  RealizeClass(
      ClassContribution{
          .base = base,
          .members = adds.members,
          .introductions = adds.introductions,
          .takeovers = adds.takeovers,
          .property_names = adds.property_names,
          .behavior_names = adds.behavior_names,
          .body_names = adds.body_names},
      adds.realization, *declared.Definition());
  auto* scope = std::get_if<ScopeDefinition>(&declared.definition);
  if (scope == nullptr) {
    return;
  }
  adds.declared_classes.reserve(adds.pending_classes.size());
  for (const PendingClass& answers : adds.pending_classes) {
    adds.declared_classes.emplace_back(answers.name, *answers.definition);
  }
  scope->program.subroutines = ScopeCallableTable{
      adds.subroutines.data(),
      static_cast<std::uint32_t>(adds.subroutines.size())};
  scope->program.exports = ScopeCallableTable{
      adds.exports.data(), static_cast<std::uint32_t>(adds.exports.size())};
  scope->program.classes = ScopeClassTable{
      adds.declared_classes.data(),
      static_cast<std::uint32_t>(adds.declared_classes.size())};
}

}  // namespace

void RealizeDeclarations() {
  for (const std::unique_ptr<DeclaredClass>& declared : DeclaredClasses()) {
    LayOut(*declared);
  }
}

}  // namespace lyra::runtime

#include "lyra/runtime/class_definition.hpp"

#include <gtest/gtest.h>
#include <memory>
#include <string>
#include <tuple>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/scope_info.hpp"
#include "lyra/value/object_ref.hpp"

namespace {

using lyra::InternalError;
using lyra::SimulationError;
using lyra::runtime::AdoptObject;
using lyra::runtime::EndObjectOf;
using lyra::runtime::GcObject;
using lyra::runtime::HierarchySegment;
using lyra::runtime::ObjectDefinition;
using lyra::runtime::ObjectOf;
using lyra::runtime::OwnedObject;
using lyra::runtime::RequireScopeClass;
using lyra::runtime::Scope;
using lyra::runtime::ScopeInfo;
using lyra::value::ObjectRef;

// A definition is a constant the unit declaring its class emits, so every one
// here is one too, naming what it reaches by address the way an emitted one
// does.

constexpr ScopeInfo kScopeInfo{
    .metadata = {.time_unit_power = -9, .time_precision_power = -9},
    .exports = {},
    .resolve = nullptr,
    .initialize = nullptr,
    .create_processes = nullptr,
    .describe = nullptr};

// The definition of a class of the design hierarchy extending `base`, whose
// instances end as the library's scope.
auto ScopeClass(const ObjectDefinition* base) -> ObjectDefinition {
  return ObjectDefinition{
      .base = base,
      .scope = &kScopeInfo,
      .class_object = nullptr,
      .end = &EndObjectOf<Scope>,
      .interfaces = {},
      .methods = nullptr};
}

const ObjectDefinition kRoot = ScopeClass(nullptr);
const ObjectDefinition kOuter = ScopeClass(nullptr);
const ObjectDefinition kMiddle = ScopeClass(nullptr);
const ObjectDefinition kInner = ScopeClass(nullptr);
const ObjectDefinition kOuterExtended = ScopeClass(&kOuter);
const ObjectDefinition kOther = ScopeClass(nullptr);

// A class no instance of the design hierarchy is built of.
constexpr ObjectDefinition kPlain{
    .base = nullptr,
    .scope = nullptr,
    .class_object = nullptr,
    .end = nullptr,
    .interfaces = {},
    .methods = nullptr};

auto Segment(std::string name) -> HierarchySegment {
  return HierarchySegment{std::move(name), {}};
}

// A scope created of `definition`'s class and then given its place under
// `parent`, as a scope's constructor gives it.
auto PlacedScope(
    Scope* parent, const HierarchySegment& segment,
    const ObjectDefinition* definition) -> OwnedObject<Scope> {
  auto scope = std::make_unique<Scope>();
  scope->definition = definition;
  scope->SetPlace(parent, segment);
  return OwnedObject<Scope>(scope.release());
}

// A scope is built of a class stating what only a scope class has. A class
// stating none is refused as the class of one.
TEST(ClassDefinitionTest, AScopeIsBuiltOfAScopeClass) {
  const auto built = PlacedScope(nullptr, Segment("u"), &kOuter);
  EXPECT_EQ(built->definition, &kOuter);
  EXPECT_EQ(&built->Info(), &kScopeInfo);
  EXPECT_EQ(built->DisplaySegment(), "u");

  EXPECT_THROW(std::ignore = RequireScopeClass(&kPlain), InternalError);
}

// An upward name starts at the nearest instance above the one naming it whose
// class is the one the name was resolved against, or extends it; past the
// topmost such ancestor, a top-level instance of that class is where it starts
// (LRM 23.8). Every instance a resolution covered has one, so a scope with
// none is a compiler fault.
TEST(ClassDefinitionTest, AnUpwardNameStartsAtTheNearestInstanceOfItsClass) {
  const auto root = PlacedScope(nullptr, Segment("$root"), &kRoot);
  Scope* outer =
      Scope::Attach(PlacedScope(root.get(), Segment("top"), &kOuterExtended));
  Scope* other =
      Scope::Attach(PlacedScope(root.get(), Segment("side"), &kOther));
  Scope* middle = Scope::Attach(PlacedScope(outer, Segment("m"), &kMiddle));
  Scope* inner = Scope::Attach(PlacedScope(middle, Segment("i"), &kInner));

  EXPECT_EQ(inner->EnclosingScope(&kMiddle), middle);
  EXPECT_EQ(inner->EnclosingScope(&kOuterExtended), outer);
  EXPECT_EQ(inner->EnclosingScope(&kOuter), outer);
  EXPECT_EQ(inner->EnclosingScope(&kOther), other);
  EXPECT_THROW(std::ignore = inner->EnclosingScope(&kInner), InternalError);
}

// Which phases ran, and on which object the last of them was entered.
struct PhasesRun {
  std::string order;
  const GcObject* entered = nullptr;
};

auto PhasesSoFar() -> PhasesRun& {
  static PhasesRun run;
  return run;
}

// The entries a class's definition holds for its phases, each handed the
// scope as the root every object is.
void Enter(GcObject* self, char phase) {
  PhasesSoFar().order += phase;
  PhasesSoFar().entered = self;
}
void Resolve(GcObject* self) {
  Enter(self, 'r');
}
void Initialize(GcObject* self) {
  Enter(self, 'i');
}
void CreateProcesses(GcObject* self) {
  Enter(self, 'c');
}

// A scope class a target builds as a C++ class of its own, holding something
// only that class's end lets go of.
class OwnClassScope : public Scope {
 public:
  explicit OwnClassScope(std::shared_ptr<int> held) : held_(std::move(held)) {
  }

 private:
  std::shared_ptr<int> held_;
};

const ScopeInfo kOwnClassScopeInfo{
    .metadata = {.time_unit_power = -9, .time_precision_power = -9},
    .exports = {},
    .resolve = &Resolve,
    .initialize = &Initialize,
    .create_processes = &CreateProcesses,
    .describe = nullptr};

const ObjectDefinition kOwnClassScope{
    .base = nullptr,
    .scope = &kOwnClassScopeInfo,
    .class_object = nullptr,
    .end = &EndObjectOf<OwnClassScope>,
    .interfaces = {},
    .methods = nullptr};

// An owner holds every scope as a scope, so driving one through its phases runs
// what its own class's definition states of each, and letting go of it ends
// the class it was built as, and not only the part every scope shares.
TEST(ClassDefinitionTest, AScopeIsDrivenAndEndedAsTheClassItWasBuiltAs) {
  const auto held = std::make_shared<int>(0);
  auto own = std::make_unique<OwnClassScope>(held);
  own->definition = &kOwnClassScope;
  own->SetPlace(nullptr, Segment("u"));
  OwnedObject<Scope> built(own.release());
  PhasesSoFar() = PhasesRun{};
  built->Resolve();
  built->Initialize();
  built->CreateProcesses();
  EXPECT_EQ(PhasesSoFar().order, "ric");
  EXPECT_EQ(PhasesSoFar().entered, built.get());
  EXPECT_EQ(held.use_count(), 2);
  built.reset();
  EXPECT_EQ(held.use_count(), 1);
}

// A class extending the part every object starts with, holding something only
// its own end lets go of.
class OwnObject : public GcObject {
 public:
  explicit OwnObject(std::shared_ptr<int> held) : held_(std::move(held)) {
  }

 private:
  std::shared_ptr<int> held_;
};

const ObjectDefinition kOwnObject{
    .base = nullptr,
    .scope = nullptr,
    .class_object = nullptr,
    .end = &EndObjectOf<OwnObject>,
    .interfaces = {},
    .methods = nullptr};

// A value the program built with `new` is handed, constructed, to the handle
// that owns it, which reaches it at the address it was made at and ends it as
// its class's definition says when the last reference goes. A handle naming no
// value reaches nothing, which is the design's failure (LRM 8.4).
TEST(ClassDefinitionTest, AnAdoptedObjectIsReachedAndEndedAsItWasBuilt) {
  const auto held = std::make_shared<int>(0);
  auto built = std::make_unique<OwnObject>(held);
  built->definition = &kOwnObject;
  void* const address = static_cast<GcObject*>(built.get());
  ObjectRef object = AdoptObject(built.release());
  EXPECT_EQ(ObjectOf(object), address);
  EXPECT_THROW(std::ignore = ObjectOf(ObjectRef{}), SimulationError);
  EXPECT_EQ(held.use_count(), 2);
  object = ObjectRef{};
  EXPECT_EQ(held.use_count(), 1);
}

}  // namespace

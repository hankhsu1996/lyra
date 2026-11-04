#include "lyra/runtime/class_definition.hpp"

#include <array>
#include <gtest/gtest.h>
#include <memory>
#include <string>
#include <tuple>
#include <utility>

#include "lyra/abi/records.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/base/source_location.hpp"
#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/value/constant_span.hpp"
#include "lyra/value/object_ref.hpp"
#include "tests/base/written_at.hpp"

namespace {

using lyra::test::kWrittenAt;

using lyra::InternalError;
using lyra::SimulationError;
using lyra::abi::InterfaceReach;
using lyra::abi::ObjectDefinition;
using lyra::abi::ScopeInfo;
using lyra::runtime::AdoptObject;
using lyra::runtime::EndObjectOf;
using lyra::runtime::GcObject;
using lyra::runtime::HierarchySegment;
using lyra::runtime::IsOfInterfaceOf;
using lyra::runtime::OwnedObject;
using lyra::runtime::RequireScopeClass;
using lyra::runtime::Scope;
using lyra::value::ConstantSpan;
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
      .methods = nullptr,
      .declaring_instance = nullptr,
      .interface_reaches = {}};
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
    .methods = nullptr,
    .declaring_instance = nullptr,
    .interface_reaches = {}};

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
    .methods = nullptr,
    .declaring_instance = nullptr,
    .interface_reaches = {}};

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
    .methods = nullptr,
    .declaring_instance = nullptr,
    .interface_reaches = {}};

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
  EXPECT_EQ(object.Object<GcObject>(kWrittenAt), address);
  EXPECT_THROW(
      std::ignore = ObjectRef{}.Object<GcObject>(kWrittenAt), SimulationError);
  EXPECT_EQ(held.use_count(), 2);
  object = ObjectRef{};
  EXPECT_EQ(held.use_count(), 1);
}

// Three instances of the design hierarchy, which the walk below only compares
// and never enters.
auto InstanceA() -> Scope* {
  static Scope instance;
  return &instance;
}
auto InstanceB() -> Scope* {
  static Scope instance;
  return &instance;
}
auto InstanceC() -> Scope* {
  static Scope instance;
  return &instance;
}

// The ways a name reaches an instance from the one its declaration belongs
// to: the same one, or a fixed other one, as a name through a port bound to
// it reaches it.
auto SameInstance(Scope* from) -> Scope* {
  return from;
}
auto ToB(Scope*) -> Scope* {
  return InstanceB();
}
auto ToC(Scope*) -> Scope* {
  return InstanceC();
}

// What an object of a class belonging to the instance A keeps for it.
auto KeptA(GcObject*) -> Scope* {
  return InstanceA();
}

constexpr auto InterfaceClass(ConstantSpan<InterfaceReach> extends)
    -> ObjectDefinition {
  return ObjectDefinition{
      .base = nullptr,
      .scope = nullptr,
      .class_object = nullptr,
      .end = nullptr,
      .interfaces = {},
      .methods = nullptr,
      .declaring_instance = nullptr,
      .interface_reaches = extends};
}

// `Root`, of an instance, extended by `Left` and `Right`, each of the instance
// it extends `Root` from; `Leaf` stands alone.
constexpr ObjectDefinition kRootIface = InterfaceClass({});
constexpr ObjectDefinition kLeafIface = InterfaceClass({});
constexpr std::array kToRootAtC{
    InterfaceReach{.of = &kRootIface, .reach = &ToC}};
constexpr ObjectDefinition kLeftIface =
    InterfaceClass(ConstantSpan<InterfaceReach>(&kToRootAtC));
constexpr ObjectDefinition kRightIface =
    InterfaceClass(ConstantSpan<InterfaceReach>(&kToRootAtC));

// A class of the instance A implementing `Leaf` twice, A's copy and B's, and
// `Root` along two paths to C's copy.
constexpr std::array kNamed{
    InterfaceReach{.of = &kLeafIface, .reach = &SameInstance},
    InterfaceReach{.of = &kLeafIface, .reach = &ToB},
    InterfaceReach{.of = &kLeftIface, .reach = &SameInstance},
    InterfaceReach{.of = &kRightIface, .reach = &ToB}};
const ObjectDefinition kNamingClass{
    .base = nullptr,
    .scope = nullptr,
    .class_object = nullptr,
    .end = &EndObjectOf<GcObject>,
    .interfaces = {},
    .methods = nullptr,
    .declaring_instance = &KeptA,
    .interface_reaches = ConstantSpan<InterfaceReach>(&kNamed)};
// A class of no instance extending it, whose objects are of what it names.
const ObjectDefinition kExtendingClass{
    .base = &kNamingClass,
    .scope = nullptr,
    .class_object = nullptr,
    .end = &EndObjectOf<GcObject>,
    .interfaces = {},
    .methods = nullptr,
    .declaring_instance = nullptr,
    .interface_reaches = {}};

auto ObjectOf(const ObjectDefinition* definition) -> ObjectRef {
  auto built = std::make_unique<GcObject>();
  built->definition = definition;
  return AdoptObject(built.release());
}

// An object is a value of each instance's copy of an interface class its
// lineage names (LRM 6.22, 8.26.5): two names of one interface class reach two
// copies, a copy named through what an interface class extends is reached from
// the instance that one is of, and two paths to one copy reach it once. It is
// a value of no other copy, and a handle naming no object is of none.
TEST(ClassDefinitionTest, AnObjectIsOfEachInstancesCopyItsLineageNames) {
  for (const ObjectDefinition* definition : {&kNamingClass, &kExtendingClass}) {
    const ObjectRef object = ObjectOf(definition);
    EXPECT_TRUE(IsOfInterfaceOf(object, &kLeafIface, InstanceA()));
    EXPECT_TRUE(IsOfInterfaceOf(object, &kLeafIface, InstanceB()));
    EXPECT_FALSE(IsOfInterfaceOf(object, &kLeafIface, InstanceC()));
    EXPECT_TRUE(IsOfInterfaceOf(object, &kLeftIface, InstanceA()));
    EXPECT_FALSE(IsOfInterfaceOf(object, &kLeftIface, InstanceB()));
    EXPECT_TRUE(IsOfInterfaceOf(object, &kRightIface, InstanceB()));
    EXPECT_TRUE(IsOfInterfaceOf(object, &kRootIface, InstanceC()));
    EXPECT_FALSE(IsOfInterfaceOf(object, &kRootIface, InstanceA()));
  }
  EXPECT_FALSE(IsOfInterfaceOf(ObjectRef{}, &kLeafIface, InstanceA()));
}

}  // namespace

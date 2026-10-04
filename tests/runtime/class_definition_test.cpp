#include "lyra/runtime/class_definition.hpp"

#include <array>
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
using lyra::runtime::DeclaredBody;
using lyra::runtime::ErasedEntry;
using lyra::runtime::FindBehaviorBody;
using lyra::runtime::FindProperty;
using lyra::runtime::GcObject;
using lyra::runtime::HierarchySegment;
using lyra::runtime::ObjectDefinition;
using lyra::runtime::PropertyAt;
using lyra::runtime::PropertyCoordinate;
using lyra::runtime::PropertySlotEntry;
using lyra::runtime::RequireScopeClass;
using lyra::runtime::ResolvedProperty;
using lyra::runtime::Scope;
using lyra::runtime::ScopeClass;
using lyra::runtime::ScopeInfo;
using lyra::runtime::ViewOf;
using lyra::value::ObjectRef;

// A definition is a constant the unit declaring its class emits, so every one
// here is one too, naming what it reaches by address the way an emitted one
// does.

void Introduced() {
}
void Kept() {
}
void TakenOver() {
}

auto FirstProperty(void* self) -> void* {
  return self;
}

extern const ObjectDefinition kBase;

constexpr std::array<PropertySlotEntry, 1> kBaseSlots{&FirstProperty};
constexpr std::array<ResolvedProperty, 1> kBaseProperties{ResolvedProperty{
    .name = "p", .at = PropertyCoordinate{.declared_by = &kBase, .slot = 0}}};
constexpr std::array<DeclaredBody, 2> kBaseBodies{
    DeclaredBody{.name = "taken", .body = &Introduced},
    DeclaredBody{.name = "kept", .body = &Kept}};
constexpr ObjectDefinition kBase{
    .base = nullptr,
    .property_names = {.data = &kBaseProperties, .size = 1},
    .body_names = {.data = &kBaseBodies, .size = 2},
    .property_slots = {.data = &kBaseSlots, .size = 1},
    .scope = nullptr};

constexpr std::array<DeclaredBody, 1> kDerivedBodies{
    DeclaredBody{.name = "taken", .body = &TakenOver}};
constexpr ObjectDefinition kDerived{
    .base = &kBase,
    .property_names = {},
    .body_names = {.data = &kDerivedBodies, .size = 1},
    .property_slots = {},
    .scope = nullptr};

// A name the base declared is answered on the class extending it -- a
// property through the entry the base declared for it, and a body unless the
// class extending it declares one under the same name, which then answers
// instead. A name no class of the lineage declares answers nothing, and asking
// for one is the run's failure rather than a quiet null.
TEST(ClassDefinitionTest, ALineageAnswersNamesItsClassesDeclared) {
  EXPECT_EQ(FindBehaviorBody(&kDerived, "taken"), ErasedEntry{&TakenOver});
  EXPECT_EQ(FindBehaviorBody(&kDerived, "kept"), ErasedEntry{&Kept});
  EXPECT_EQ(FindBehaviorBody(&kBase, "taken"), ErasedEntry{&Introduced});
  const PropertyCoordinate* found = FindProperty(&kDerived, "p");
  EXPECT_EQ(found->declared_by, &kBase);
  ASSERT_EQ(kBase.property_slots.size, 1U);
  int value = 0;
  EXPECT_EQ(
      kBase.property_slots.Entries()[found->slot](&value),
      static_cast<void*>(&value));

  EXPECT_THROW(std::ignore = FindProperty(&kDerived, "q"), SimulationError);
  EXPECT_THROW(
      std::ignore = FindBehaviorBody(&kDerived, "absent"), SimulationError);
}

constexpr ObjectDefinition kDeclared{};
constexpr std::array<ScopeClass, 1> kScopeClasses{
    ScopeClass{.name = "C", .definition = &kDeclared}};
constexpr ScopeInfo kScopeInfo{
    .metadata = {.time_unit_power = -9, .time_precision_power = -9},
    .exports = {},
    .subroutines = {},
    .classes = {.data = &kScopeClasses, .size = 1}};
constexpr ObjectDefinition kScope{
    .base = nullptr,
    .property_names = {},
    .body_names = {},
    .property_slots = {},
    .scope = &kScopeInfo};

// What a unit promises of its object: a class no instance is built of, so it
// states nothing about one.
constexpr ObjectDefinition kPromised{
    .base = nullptr,
    .property_names = {},
    .body_names = {},
    .property_slots = {},
    .scope = nullptr};

// A scope answers the classes its instance declares by name. A class no
// instance is built of is refused as the class of a scope.
TEST(ClassDefinitionTest, AScopeIsBuiltOfAScopeClassAndAnswersThroughIt) {
  const auto built = std::make_unique<Scope>(
      nullptr, HierarchySegment{std::string{"u"}, {}}, &kScope);
  EXPECT_EQ(built->Definition(), &kScope);
  ASSERT_EQ(built->Info().classes.size, 1U);
  EXPECT_EQ(built->Info().classes.Entries()[0].definition, &kDeclared);
  EXPECT_EQ(built->Parent(), nullptr);
  EXPECT_EQ(built->Name(), "u");

  EXPECT_THROW(std::ignore = RequireScopeClass(&kPromised), InternalError);
  EXPECT_THROW(std::ignore = RequireScopeClass(&kDerived), InternalError);
}

extern const ObjectDefinition kOwnClassScope;

// A scope class a target builds as a C++ class of its own, holding something
// only that class's destructor lets go of, and recording which of its phases
// ran.
class OwnClassScope : public Scope {
 public:
  OwnClassScope(const HierarchySegment& segment, std::shared_ptr<int> held)
      : Scope(nullptr, segment, &kOwnClassScope), held_(std::move(held)) {
  }

  [[nodiscard]] auto Phases() const -> const std::string& {
    return phases_;
  }

 private:
  void sv_resolve() override {
    phases_ += 'r';
  }
  void sv_initialize() override {
    phases_ += 'i';
  }
  void sv_create_processes() override {
    phases_ += 'c';
  }

  std::shared_ptr<int> held_;
  std::string phases_;
};

constexpr ScopeInfo kOwnClassInfo{
    .metadata = {.time_unit_power = -9, .time_precision_power = -9},
    .exports = {},
    .subroutines = {},
    .classes = {}};
constexpr ObjectDefinition kOwnClassScope{
    .base = nullptr,
    .property_names = {},
    .body_names = {},
    .property_slots = {},
    .scope = &kOwnClassInfo};

// An owner holds every scope as a scope, so driving one through its phases runs
// what its own class does in each, and letting go of it runs, through the
// virtual destructor, the destructor of the class it was built as, and not only
// the one of the part every scope shares.
TEST(ClassDefinitionTest, AScopeIsDrivenAndEndedAsTheClassItWasBuiltAs) {
  const HierarchySegment segment{std::string{"u"}, {}};
  const auto held = std::make_shared<int>(0);
  auto own = std::make_unique<OwnClassScope>(segment, held);
  const OwnClassScope& seen = *own;
  std::unique_ptr<Scope> built = std::move(own);
  built->Resolve();
  built->Initialize();
  built->CreateProcesses();
  EXPECT_EQ(seen.Phases(), "ric");
  EXPECT_EQ(held.use_count(), 2);
  built.reset();
  EXPECT_EQ(held.use_count(), 1);
}

extern const ObjectDefinition kOwnObjectClass;

constexpr std::array<PropertySlotEntry, 1> kOwnObjectSlots{&FirstProperty};
constexpr PropertyCoordinate kOwnObjectProperty{
    .declared_by = &kOwnObjectClass, .slot = 0};
constexpr ObjectDefinition kOwnObjectClass{
    .base = nullptr,
    .property_names = {},
    .body_names = {},
    .property_slots = {.data = &kOwnObjectSlots, .size = 1},
    .scope = nullptr};

// A class extending the part every object starts with, holding something only
// its own destructor lets go of.
class OwnObject : public GcObject {
 public:
  explicit OwnObject(std::shared_ptr<int> held) : held_(std::move(held)) {
  }

 private:
  std::shared_ptr<int> held_;
};

// A value the program built with `new` is handed, constructed, to the handle
// that owns it, which reaches it at the address it was made at -- both to enter
// a body on it and to apply a property's position to it -- and ends it as the
// class it was built as when the last reference goes. A handle naming no value
// reaches nothing, which is the design's failure (LRM 8.4).
TEST(ClassDefinitionTest, AnAdoptedObjectIsReachedAndEndedAsItWasBuilt) {
  const auto held = std::make_shared<int>(0);
  auto built = std::make_unique<OwnObject>(held);
  void* const address = static_cast<GcObject*>(built.get());
  ObjectRef object = AdoptObject(static_cast<GcObject*>(built.release()));
  EXPECT_EQ(ViewOf(object), address);
  EXPECT_EQ(PropertyAt(object, &kOwnObjectProperty), address);
  EXPECT_THROW(std::ignore = ViewOf(ObjectRef{}), SimulationError);
  EXPECT_THROW(
      std::ignore = PropertyAt(ObjectRef{}, &kOwnObjectProperty),
      SimulationError);
  EXPECT_EQ(held.use_count(), 2);
  object = ObjectRef{};
  EXPECT_EQ(held.use_count(), 1);
}

}  // namespace

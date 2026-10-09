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
using lyra::runtime::GcObject;
using lyra::runtime::HierarchySegment;
using lyra::runtime::ObjectDefinition;
using lyra::runtime::RequireScopeClass;
using lyra::runtime::Scope;
using lyra::runtime::ScopeInfo;
using lyra::runtime::ViewOf;
using lyra::value::ObjectRef;

// A definition is a constant the unit declaring its class emits, so every one
// here is one too, naming what it reaches by address the way an emitted one
// does.

constexpr ScopeInfo kScopeInfo{
    .metadata = {.time_unit_power = -9, .time_precision_power = -9},
    .exports = {}};
constexpr ObjectDefinition kRoot{.base = nullptr, .scope = &kScopeInfo};
constexpr ObjectDefinition kOuter{.base = nullptr, .scope = &kScopeInfo};
constexpr ObjectDefinition kMiddle{.base = nullptr, .scope = &kScopeInfo};
constexpr ObjectDefinition kInner{.base = nullptr, .scope = &kScopeInfo};
constexpr ObjectDefinition kOuterExtended{
    .base = &kOuter, .scope = &kScopeInfo};
constexpr ObjectDefinition kOther{.base = nullptr, .scope = &kScopeInfo};

// A class no instance of the design hierarchy is built of.
constexpr ObjectDefinition kPlain{.base = nullptr, .scope = nullptr};

auto Segment(std::string name) -> HierarchySegment {
  return HierarchySegment{std::move(name), {}};
}

// A scope is built of a class stating what only a scope class has. A class
// stating none is refused as the class of one.
TEST(ClassDefinitionTest, AScopeIsBuiltOfAScopeClass) {
  const auto built = std::make_unique<Scope>(nullptr, Segment("u"), &kOuter);
  EXPECT_EQ(built->Definition(), &kOuter);
  EXPECT_EQ(&built->Info(), &kScopeInfo);
  EXPECT_EQ(built->Parent(), nullptr);
  EXPECT_EQ(built->DisplaySegment(), "u");

  EXPECT_THROW(std::ignore = RequireScopeClass(&kPlain), InternalError);
}

// An upward name starts at the nearest instance above the one naming it whose
// class is the one the name was resolved against, or extends it; past the
// topmost such ancestor, a top-level instance of that class is where it starts
// (LRM 23.8). Every instance a resolution covered has one, so a scope with
// none is a compiler fault.
TEST(ClassDefinitionTest, AnUpwardNameStartsAtTheNearestInstanceOfItsClass) {
  const auto root = std::make_unique<Scope>(nullptr, Segment("$root"), &kRoot);
  Scope* outer = root->AddOwnedChild(
      std::make_unique<Scope>(nullptr, Segment("top"), &kOuterExtended));
  Scope* other = root->AddOwnedChild(
      std::make_unique<Scope>(nullptr, Segment("side"), &kOther));
  Scope* middle = outer->AddOwnedChild(
      std::make_unique<Scope>(nullptr, Segment("m"), &kMiddle));
  Scope* inner = middle->AddOwnedChild(
      std::make_unique<Scope>(nullptr, Segment("i"), &kInner));

  EXPECT_EQ(inner->EnclosingScope(&kMiddle), middle);
  EXPECT_EQ(inner->EnclosingScope(&kOuterExtended), outer);
  EXPECT_EQ(inner->EnclosingScope(&kOuter), outer);
  EXPECT_EQ(inner->EnclosingScope(&kOther), other);
  EXPECT_THROW(std::ignore = inner->EnclosingScope(&kInner), InternalError);
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

constexpr ObjectDefinition kOwnClassScope{
    .base = nullptr, .scope = &kScopeInfo};

// An owner holds every scope as a scope, so driving one through its phases runs
// what its own class does in each, and letting go of it runs, through the
// virtual destructor, the destructor of the class it was built as, and not only
// the one of the part every scope shares.
TEST(ClassDefinitionTest, AScopeIsDrivenAndEndedAsTheClassItWasBuiltAs) {
  const auto held = std::make_shared<int>(0);
  auto own = std::make_unique<OwnClassScope>(Segment("u"), held);
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
// that owns it, which reaches it at the address it was made at and ends it as
// the class it was built as when the last reference goes. A handle naming no
// value reaches nothing, which is the design's failure (LRM 8.4).
TEST(ClassDefinitionTest, AnAdoptedObjectIsReachedAndEndedAsItWasBuilt) {
  const auto held = std::make_shared<int>(0);
  auto built = std::make_unique<OwnObject>(held);
  void* const address = static_cast<GcObject*>(built.get());
  ObjectRef object = AdoptObject(static_cast<GcObject*>(built.release()));
  EXPECT_EQ(ViewOf(object), address);
  EXPECT_THROW(std::ignore = ViewOf(ObjectRef{}), SimulationError);
  EXPECT_EQ(held.use_count(), 2);
  object = ObjectRef{};
  EXPECT_EQ(held.use_count(), 1);
}

}  // namespace

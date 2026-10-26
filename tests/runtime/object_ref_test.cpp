#include "lyra/runtime/object_ref.hpp"

#include <array>
#include <gtest/gtest.h>
#include <memory>
#include <tuple>

#include "lyra/runtime/class_definition.hpp"
#include "lyra/runtime/scope_info.hpp"

namespace {

using lyra::runtime::AdoptObject;
using lyra::runtime::ConstantSpan;
using lyra::runtime::Dispatch;
using lyra::runtime::DynamicCast;
using lyra::runtime::EndObjectOf;
using lyra::runtime::GcObject;
using lyra::runtime::InterfaceDispatch;
using lyra::runtime::InterfaceRow;
using lyra::runtime::IsOfClass;
using lyra::runtime::ObjectDefinition;
using lyra::runtime::SelfHandle;
using lyra::value::ObjectRef;

// The definition of a class `T` extending `base`, whose objects end as `T`'s,
// implementing `interfaces` and dispatching through `methods`.
template <typename T>
constexpr auto DefinitionOf(
    const ObjectDefinition* base, ConstantSpan<InterfaceRow> interfaces = {},
    const void* methods = nullptr) -> ObjectDefinition {
  return ObjectDefinition{
      .base = base,
      .scope = nullptr,
      .class_object = nullptr,
      .end = &EndObjectOf<T>,
      .interfaces = interfaces,
      .methods = methods};
}

// Every object the runtime holds is one of a class extending the part every
// object starts with, which is what holds its creator's reference until a
// handle takes it over, and is ended as its class's definition says.
struct Counted : GcObject {
  static inline int alive = 0;
  Counted() {
    ++alive;
  }
  Counted(const Counted&) = delete;
  auto operator=(const Counted&) -> Counted& = delete;
  Counted(Counted&&) = delete;
  auto operator=(Counted&&) -> Counted& = delete;
  ~Counted() {
    --alive;
  }
};

const ObjectDefinition kCounted = DefinitionOf<Counted>(nullptr);

// A new object of `T`, of the class `definition` defines, held by the handle
// its creation answers.
template <typename T>
auto Create(const ObjectDefinition& definition) -> ObjectRef {
  auto object = std::make_unique<T>();
  object->definition = &definition;
  return AdoptObject(object.release());
}

class ObjectRefTest : public ::testing::Test {
 protected:
  void SetUp() override {
    Counted::alive = 0;
  }
};

TEST_F(ObjectRefTest, LastReferenceDropReleasesObject) {
  {
    auto a = Create<Counted>(kCounted);
    EXPECT_EQ(Counted::alive, 1);
  }
  EXPECT_EQ(Counted::alive, 0);
}

// An object's first word is its class's definition, which is where every
// backend reads it, whichever bases the library gives the root.
TEST_F(ObjectRefTest, AnObjectStartsWithItsDefinition) {
  const auto a = Create<Counted>(kCounted);
  const GcObject& object = a.Deref<GcObject>();
  EXPECT_EQ(
      static_cast<const void*>(&object.definition),
      static_cast<const void*>(&object));
  EXPECT_EQ(object.definition, &kCounted);
}

// An object refers to itself from its first moment, so a reference its
// constructor hands out shares ownership with the one `new` answers.
struct Registers : Counted {
  static inline ObjectRef last;
  Registers() {
    last = SelfHandle(this);
  }
};

const ObjectDefinition kRegisters = DefinitionOf<Registers>(&kCounted);

TEST_F(ObjectRefTest, ConstructorHandsOutAReferenceToItself) {
  {
    auto a = Create<Registers>(kRegisters);
    EXPECT_EQ(a, Registers::last);
    a = nullptr;
    EXPECT_EQ(Counted::alive, 1);
    Registers::last = nullptr;
  }
  EXPECT_EQ(Counted::alive, 0);
}

TEST_F(ObjectRefTest, ReferenceCopyRetainsObject) {
  {
    auto a = Create<Counted>(kCounted);
    {
      auto b = a;
      EXPECT_EQ(Counted::alive, 1);
    }
    EXPECT_EQ(Counted::alive, 1);
  }
  EXPECT_EQ(Counted::alive, 0);
}

TEST_F(ObjectRefTest, ReassignmentReleasesPrevious) {
  auto a = Create<Counted>(kCounted);
  a = Create<Counted>(kCounted);
  EXPECT_EQ(Counted::alive, 1);
}

TEST_F(ObjectRefTest, IdentityByObject) {
  auto a = Create<Counted>(kCounted);
  auto b = a;
  auto c = Create<Counted>(kCounted);
  EXPECT_EQ(a, b);
  EXPECT_NE(a, c);
}

TEST_F(ObjectRefTest, NullReferenceEquality) {
  ObjectRef a;
  ObjectRef b(nullptr);
  EXPECT_EQ(a, b);
  auto c = Create<Counted>(kCounted);
  EXPECT_NE(a, c);
  c = nullptr;
  EXPECT_EQ(c, a);
  EXPECT_EQ(Counted::alive, 0);
}

TEST_F(ObjectRefTest, DerefOnANullReferenceIsTheDesignsFailure) {
  ObjectRef a;
  EXPECT_THROW(std::ignore = a.Deref<Counted>(), lyra::SimulationError);
}

// Is a Counted so that reclaiming a node is observable; a node holding only a
// reference would be reclaimed or leaked with the same result.
struct Node : Counted {
  int value = 0;
  ObjectRef next;
};

const ObjectDefinition kNode = DefinitionOf<Node>(&kCounted);

TEST_F(ObjectRefTest, SelfReferentialAcyclicChainReclaims) {
  auto head = Create<Node>(kNode);
  auto tail = Create<Node>(kNode);
  head.Deref<Node>().value = 1;
  tail.Deref<Node>().value = 2;
  head.Deref<Node>().next = tail;
  EXPECT_EQ(head.Deref<Node>().next.Deref<Node>().value, 2);
  EXPECT_EQ(Counted::alive, 2);
  // Dropping the head releases the reference it holds, which is the only thing
  // keeping the tail alive besides the local one.
  head = nullptr;
  EXPECT_EQ(Counted::alive, 1);
  tail = nullptr;
  EXPECT_EQ(Counted::alive, 0);
}

// A lineage of two classes, a class outside it, and two interface classes,
// the second extending the first: `Base` implements `Named`, and `Heir` extends
// it and implements `Ranked`, which extends `Named`, so `Named` is reached
// along two paths and is one interface of `Heir` (LRM 8.26.6.3).
struct Base : GcObject {
  int tag = 1;
  [[nodiscard]] auto Myself() -> ObjectRef {
    return SelfHandle(this);
  }
};
struct Heir : Base {};
struct Unrelated : GcObject {};

// An interface class's definition is its identity; no object is of it alone.
constexpr ObjectDefinition kNamed{
    .base = nullptr,
    .scope = nullptr,
    .class_object = nullptr,
    .end = nullptr,
    .interfaces = {},
    .methods = nullptr};
constexpr ObjectDefinition kRanked{
    .base = nullptr,
    .scope = nullptr,
    .class_object = nullptr,
    .end = nullptr,
    .interfaces = {},
    .methods = nullptr};

// The table type the class introducing a behavior declares, and the one a
// class extending it declares over it, as generated code lays tables out: the
// bodies of what the class it extends introduced first.
struct BaseTable {
  int (*level)(GcObject*, int);
};
struct HeirTable : BaseTable {
  int (*depth)(GcObject*);
};
struct NamedTable {
  int (*name)(GcObject*);
};
struct RankedTable {
  int (*rank)(GcObject*);
};

auto BaseLevel(GcObject*, int bias) -> int {
  return 10 + bias;
}
auto HeirLevel(GcObject*, int bias) -> int {
  return 20 + bias;
}
auto HeirDepth(GcObject*) -> int {
  return 2;
}
auto BaseName(GcObject*) -> int {
  return 100;
}
auto HeirRank(GcObject*) -> int {
  return 300;
}

constexpr BaseTable kBaseMethods{.level = &BaseLevel};
constexpr HeirTable kHeirMethods{{.level = &HeirLevel}, &HeirDepth};
constexpr NamedTable kNamedMethods{.name = &BaseName};
constexpr RankedTable kRankedMethods{.rank = &HeirRank};

constexpr std::array kBaseRows{
    InterfaceRow{.of = &kNamed, .table = &kNamedMethods}};
constexpr std::array kHeirRows{
    InterfaceRow{.of = &kNamed, .table = &kNamedMethods},
    InterfaceRow{.of = &kRanked, .table = &kRankedMethods}};

const ObjectDefinition kBase = DefinitionOf<Base>(
    nullptr, {.data = kBaseRows.data(), .size = kBaseRows.size()},
    &kBaseMethods);
const ObjectDefinition kHeir = DefinitionOf<Heir>(
    &kBase, {.data = kHeirRows.data(), .size = kHeirRows.size()},
    &kHeirMethods);
const ObjectDefinition kUnrelated = DefinitionOf<Unrelated>(nullptr);

// An object is of its own class, of every class its class extends, and of
// every interface class its class implements, whichever class it extends
// implements it; of nothing else (LRM 8.16, 8.26.5).
TEST_F(ObjectRefTest, AnObjectIsOfItsLineageAndItsInterfaceClasses) {
  EXPECT_TRUE(IsOfClass(&kHeir, &kHeir));
  EXPECT_TRUE(IsOfClass(&kHeir, &kBase));
  EXPECT_TRUE(IsOfClass(&kHeir, &kNamed));
  EXPECT_TRUE(IsOfClass(&kHeir, &kRanked));
  EXPECT_FALSE(IsOfClass(&kBase, &kHeir));
  EXPECT_FALSE(IsOfClass(&kBase, &kRanked));
  EXPECT_FALSE(IsOfClass(&kUnrelated, &kBase));
}

// A `$cast` answers the handle it was handed where the object is of the class
// asked about, and one referring to no object otherwise; a handle referring to
// no object answers one referring to none (LRM 8.16).
TEST_F(ObjectRefTest, ADynamicCastAsksTheObject) {
  const ObjectRef heir = Create<Heir>(kHeir);
  EXPECT_EQ(DynamicCast(heir, &kBase), heir);
  EXPECT_EQ(DynamicCast(heir, &kRanked), heir);
  const ObjectRef base = Create<Base>(kBase);
  EXPECT_FALSE(static_cast<bool>(DynamicCast(base, &kHeir)));
  EXPECT_FALSE(static_cast<bool>(DynamicCast(base, &kRanked)));
  EXPECT_FALSE(static_cast<bool>(DynamicCast(ObjectRef{}, &kBase)));
}

// A call dispatching on a behavior reads the table of the object's class as
// the introducer's table type, which a table of a class extending the
// introducer begins with, so the object's own class's body runs (LRM 8.20).
TEST_F(ObjectRefTest, ADispatchRunsTheBodyOfTheObjectsClass) {
  const ObjectRef heir = Create<Heir>(kHeir);
  const ObjectRef base = Create<Base>(kBase);
  EXPECT_EQ(
      (Dispatch<BaseTable, &BaseTable::level>(heir.Deref<GcObject>(), 1)), 21);
  EXPECT_EQ(
      (Dispatch<BaseTable, &BaseTable::level>(base.Deref<GcObject>(), 1)), 11);
  EXPECT_EQ(
      (Dispatch<HeirTable, &HeirTable::depth>(heir.Deref<GcObject>())), 2);
}

// A call through an interface class's handle reads the table the object's
// class gives that interface class, found among those its definition lists,
// whichever class of its lineage implements it (LRM 8.26.5).
TEST_F(ObjectRefTest, AnInterfaceCallRunsTheTableTheObjectsClassGives) {
  const ObjectRef heir = Create<Heir>(kHeir);
  EXPECT_EQ(
      (InterfaceDispatch<NamedTable, &NamedTable::name, &kNamed>(
          heir.Deref<GcObject>())),
      100);
  EXPECT_EQ(
      (InterfaceDispatch<RankedTable, &RankedTable::rank, &kRanked>(
          heir.Deref<GcObject>())),
      300);
}

// `this` names the object its body runs on, the same object every handle to
// it names, whichever class of its lineage the body is of (LRM 8.11).
TEST_F(ObjectRefTest, SelfHandleNamesTheObjectItsBodyRunsOn) {
  const ObjectRef heir = Create<Heir>(kHeir);
  EXPECT_EQ(heir.Deref<Heir>().Myself(), heir);
  EXPECT_EQ(heir.Deref<Base>().Myself(), heir);
}

}  // namespace

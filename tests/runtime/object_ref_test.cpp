#include "lyra/runtime/object_ref.hpp"

#include <gtest/gtest.h>

namespace {

using lyra::runtime::GcNew;
using lyra::runtime::GcObject;
using lyra::runtime::SelfHandle;
using lyra::runtime::ViewAs;
using lyra::value::ObjectRef;

struct Counted {
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

class ObjectRefTest : public ::testing::Test {
 protected:
  void SetUp() override {
    Counted::alive = 0;
  }
};

TEST_F(ObjectRefTest, LastReferenceDropReleasesObject) {
  {
    auto a = GcNew<Counted>();
    EXPECT_EQ(Counted::alive, 1);
  }
  EXPECT_EQ(Counted::alive, 0);
}

TEST_F(ObjectRefTest, ReferenceCopyRetainsObject) {
  {
    auto a = GcNew<Counted>();
    {
      auto b = a;
      EXPECT_EQ(Counted::alive, 1);
    }
    EXPECT_EQ(Counted::alive, 1);
  }
  EXPECT_EQ(Counted::alive, 0);
}

TEST_F(ObjectRefTest, ReassignmentReleasesPrevious) {
  auto a = GcNew<Counted>();
  a = GcNew<Counted>();
  EXPECT_EQ(Counted::alive, 1);
}

TEST_F(ObjectRefTest, IdentityByObject) {
  auto a = GcNew<Counted>();
  auto b = a;
  auto c = GcNew<Counted>();
  EXPECT_EQ(a, b);
  EXPECT_NE(a, c);
}

TEST_F(ObjectRefTest, NullReferenceEquality) {
  ObjectRef a;
  ObjectRef b(nullptr);
  EXPECT_EQ(a, b);
  auto c = GcNew<Counted>();
  EXPECT_NE(a, c);
  c = nullptr;
  EXPECT_EQ(c, a);
  EXPECT_EQ(Counted::alive, 0);
}

TEST_F(ObjectRefTest, DerefOnANullReferenceIsTheDesignsFailure) {
  ObjectRef a;
  EXPECT_THROW((void)a.Deref<Counted>(), lyra::SimulationError);
}

// Carries a Counted so that reclaiming a node is observable; a node holding
// only a reference would be reclaimed or leaked with the same result.
struct Node {
  Counted marker;
  int value = 0;
  ObjectRef next;
};

TEST_F(ObjectRefTest, SelfReferentialAcyclicChainReclaims) {
  auto head = GcNew<Node>();
  auto tail = GcNew<Node>();
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

// The two axes a target language is free to lay out differently: whether the
// class conforms to a contract with its own dispatch, and whether the lineage
// declares a virtual method beside the destructor every class of the source
// declares virtual. Every combination must agree on which object a reference
// names, and on what `this` answers.
class Contract {
 public:
  Contract() = default;
  Contract(const Contract&) = delete;
  auto operator=(const Contract&) -> Contract& = delete;
  Contract(Contract&&) = delete;
  auto operator=(Contract&&) -> Contract& = delete;
  virtual ~Contract() = default;
  [[nodiscard]] virtual auto Level() const -> int = 0;
};

class PlainRoot : public GcObject {
 public:
  PlainRoot() = default;
  PlainRoot(const PlainRoot&) = delete;
  auto operator=(const PlainRoot&) -> PlainRoot& = delete;
  PlainRoot(PlainRoot&&) = delete;
  auto operator=(PlainRoot&&) -> PlainRoot& = delete;
  virtual ~PlainRoot() = default;
  int tag = 0;
  [[nodiscard]] auto Myself() -> ObjectRef {
    return SelfHandle(this);
  }
};

class PlainHeir : public PlainRoot {};

class VirtualRoot : public GcObject {
 public:
  VirtualRoot() = default;
  VirtualRoot(const VirtualRoot&) = delete;
  auto operator=(const VirtualRoot&) -> VirtualRoot& = delete;
  VirtualRoot(VirtualRoot&&) = delete;
  auto operator=(VirtualRoot&&) -> VirtualRoot& = delete;
  virtual ~VirtualRoot() = default;
  [[nodiscard]] virtual auto Tag() const -> int {
    return 0;
  }
  [[nodiscard]] auto Myself() -> ObjectRef {
    return SelfHandle(this);
  }
};

class Conforming : public VirtualRoot, public Contract {
 public:
  [[nodiscard]] auto Level() const -> int override {
    return 7;
  }
};

class PlainConforming : public PlainRoot, public Contract {
 public:
  [[nodiscard]] auto Level() const -> int override {
    return 9;
  }
};

TEST_F(ObjectRefTest, IdentitySurvivesABaseView) {
  auto heir = GcNew<PlainHeir>();
  EXPECT_EQ((ViewAs<PlainHeir, PlainRoot>(heir)), heir);

  auto conforming = GcNew<Conforming>();
  EXPECT_EQ((ViewAs<Conforming, VirtualRoot>(conforming)), conforming);
}

TEST_F(ObjectRefTest, IdentitySurvivesAContractView) {
  auto conforming = GcNew<Conforming>();
  EXPECT_EQ((ViewAs<Conforming, Contract>(conforming)), conforming);

  auto plain = GcNew<PlainConforming>();
  EXPECT_EQ((ViewAs<PlainConforming, Contract>(plain)), plain);
}

TEST_F(ObjectRefTest, AContractViewStillReachesTheContract) {
  auto conforming = GcNew<Conforming>();
  const ObjectRef as_contract = ViewAs<Conforming, Contract>(conforming);
  EXPECT_EQ(as_contract.Deref<Contract>().Level(), 7);

  auto plain = GcNew<PlainConforming>();
  const ObjectRef plain_contract = ViewAs<PlainConforming, Contract>(plain);
  EXPECT_EQ(plain_contract.Deref<Contract>().Level(), 9);
}

// A view toward a class the static types do not prove the object is -- a
// subclass, or an interface its class may or may not implement -- is the
// object's to answer, and an object that is no such thing answers with a
// reference to no object. A `$cast` asks by forming the view, so this is
// reached with objects of either answer, and the answer is whether the result
// refers to anything.
TEST_F(ObjectRefTest, AViewTheTypesDoNotProveIsTheObjectsToAnswer) {
  auto root = GcNew<PlainRoot>();
  const ObjectRef as_heir = ViewAs<PlainRoot, PlainHeir>(root);
  EXPECT_EQ(as_heir.View<PlainHeir>(), nullptr);
  EXPECT_FALSE(static_cast<bool>(as_heir));
  EXPECT_TRUE(static_cast<bool>(root));

  auto plain = GcNew<PlainConforming>();
  const ObjectRef as_root = ViewAs<PlainConforming, PlainRoot>(plain);
  const ObjectRef as_heir_of_conforming = ViewAs<PlainRoot, PlainHeir>(as_root);
  EXPECT_EQ(as_heir_of_conforming.View<PlainHeir>(), nullptr);

  const ObjectRef as_contract = ViewAs<PlainRoot, Contract>(as_root);
  EXPECT_EQ(as_contract.Deref<Contract>().Level(), 9);
  EXPECT_EQ(as_contract, plain);
  const ObjectRef back = ViewAs<Contract, PlainConforming>(as_contract);
  EXPECT_EQ(&back.Deref<PlainConforming>(), &plain.Deref<PlainConforming>());
}

TEST_F(ObjectRefTest, SelfHandleNamesTheObjectItsBodyRunsOn) {
  auto heir = GcNew<PlainHeir>();
  EXPECT_EQ(heir.Deref<PlainHeir>().Myself(), heir);

  auto conforming = GcNew<Conforming>();
  EXPECT_EQ(conforming.Deref<Conforming>().Myself(), conforming);

  // The one that fails if identity is taken from a base subobject: the object
  // conforms to a contract, so its common root is displaced from the object's
  // own address.
  auto plain = GcNew<PlainConforming>();
  EXPECT_EQ(plain.Deref<PlainConforming>().Myself(), plain);
}

TEST_F(ObjectRefTest, SelfHandleAgreesWithAViewOfTheSameObject) {
  auto plain = GcNew<PlainConforming>();
  const ObjectRef as_contract = ViewAs<PlainConforming, Contract>(plain);
  EXPECT_EQ(plain.Deref<PlainConforming>().Myself(), as_contract);
}

// An interface class is a virtual base, so one reached along two paths is one
// part of the object (LRM 8.26.6.3): a view of it formed along either path is
// the same part, and either names the same object.
class Refined : public virtual Contract {
 public:
  [[nodiscard]] virtual auto Depth() const -> int = 0;
};

class Diamond : public VirtualRoot,
                public virtual Contract,
                public virtual Refined {
 public:
  [[nodiscard]] auto Level() const -> int override {
    return 11;
  }
  [[nodiscard]] auto Depth() const -> int override {
    return 2;
  }
};

TEST_F(ObjectRefTest, AnInterfaceReachedAlongTwoPathsIsOnePart) {
  auto diamond = GcNew<Diamond>();
  const ObjectRef direct = ViewAs<Diamond, Contract>(diamond);
  const ObjectRef as_refined = ViewAs<Diamond, Refined>(diamond);
  const ObjectRef through = ViewAs<Refined, Contract>(as_refined);
  EXPECT_EQ(direct.View<Contract>(), through.View<Contract>());
  EXPECT_EQ(through, diamond);
  EXPECT_EQ(through.Deref<Contract>().Level(), 11);
  EXPECT_EQ(as_refined.Deref<Refined>().Depth(), 2);
  EXPECT_EQ(diamond.Deref<Diamond>().Myself(), through);
}

}  // namespace

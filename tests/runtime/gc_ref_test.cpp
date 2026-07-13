#include "lyra/runtime/gc_ref.hpp"

#include <gtest/gtest.h>

namespace {

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

class GcRefTest : public ::testing::Test {
 protected:
  void SetUp() override {
    Counted::alive = 0;
  }
};

TEST_F(GcRefTest, LastHandleDropReleasesObject) {
  {
    auto a = lyra::runtime::GcNew<Counted>();
    EXPECT_EQ(Counted::alive, 1);
  }
  EXPECT_EQ(Counted::alive, 0);
}

TEST_F(GcRefTest, HandleCopyRetainsObject) {
  {
    auto a = lyra::runtime::GcNew<Counted>();
    {
      auto b = a;
      EXPECT_EQ(Counted::alive, 1);
    }
    EXPECT_EQ(Counted::alive, 1);
  }
  EXPECT_EQ(Counted::alive, 0);
}

TEST_F(GcRefTest, HandleReassignmentReleasesPrevious) {
  auto a = lyra::runtime::GcNew<Counted>();
  a = lyra::runtime::GcNew<Counted>();
  EXPECT_EQ(Counted::alive, 1);
}

TEST_F(GcRefTest, IdentityByObjectAddress) {
  auto a = lyra::runtime::GcNew<Counted>();
  auto b = a;
  auto c = lyra::runtime::GcNew<Counted>();
  EXPECT_EQ(a, b);
  EXPECT_NE(a, c);
}

TEST_F(GcRefTest, NullHandleEquality) {
  lyra::runtime::GcRef<Counted> a;
  lyra::runtime::GcRef<Counted> b(nullptr);
  EXPECT_EQ(a, b);
  auto c = lyra::runtime::GcNew<Counted>();
  EXPECT_NE(a, c);
  c = nullptr;
  EXPECT_EQ(c, a);
  EXPECT_EQ(Counted::alive, 0);
}

struct Node {
  int value = 0;
  lyra::runtime::GcRef<Node> next;
};

TEST_F(GcRefTest, SelfReferentialAcyclicChainReclaims) {
  auto head = lyra::runtime::GcNew<Node>();
  auto tail = lyra::runtime::GcNew<Node>();
  head->value = 1;
  tail->value = 2;
  head->next = tail;
  EXPECT_EQ(head->next->value, 2);
  head = nullptr;
  tail = nullptr;
  auto probe = lyra::runtime::GcNew<Counted>();
  EXPECT_EQ(Counted::alive, 1);
}

}  // namespace

#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <functional>
#include <gtest/gtest.h>
#include <optional>
#include <random>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

#include "lyra/runtime/net.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/support/strength_level.hpp"
#include "lyra/support/takeover_level.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "tests/base/written_at.hpp"

// What a net shows is defined bit by bit (LRM 6.6.7: each element of a vector
// net is resolved and updated independently): at each position, the strongest
// level any contribution there drives decides, and the net type's table folds
// the contributions at that level (LRM 28.12.1, 6.6.1, 6.6.3). A net groups
// positions however it likes to compute that; this test holds every grouping
// to the definition, written here position by position, over drivers of
// random parts of either of two joined nets, at random strengths, writing
// random values, under each net type, with part of one net forced.
namespace {

using lyra::runtime::ResolvedNet;
using lyra::support::StrengthLevel;
using lyra::support::TakeoverLevel;
using lyra::value::FourStateBit;

constexpr std::uint32_t kWidth = 16;
using Bus = lyra::value::LogicVector<kWidth>;

// A second net of the same type, `kJoinedCount` of whose positions from
// `kJoinedThere` are one physical net with the bus's from `kJoinedHere` (LRM
// 23.3.3.7): what drives either name there drives both.
constexpr std::uint32_t kOtherWidth = 8;
using OtherBus = lyra::value::LogicVector<kOtherWidth>;
constexpr std::uint32_t kJoinedHere = 6;
constexpr std::uint32_t kJoinedThere = 2;
constexpr std::uint32_t kJoinedCount = 4;

// Every position of the two nets as one physical position: the bus's first,
// then the other net's that are not the bus's.
constexpr std::uint32_t kPhysicalCount = kWidth + kOtherWidth - kJoinedCount;

auto PhysicalOfBus(std::uint32_t position) -> std::uint32_t {
  return position;
}

auto PhysicalOfOther(std::uint32_t position) -> std::uint32_t {
  if (position >= kJoinedThere && position < kJoinedThere + kJoinedCount) {
    return kJoinedHere + (position - kJoinedThere);
  }
  return kWidth + position - (position < kJoinedThere ? 0 : kJoinedCount);
}

// The net types under test, as the table they fold under and what they
// contribute themselves.
enum class NetKind : std::uint8_t { kWire, kWand, kWor, kTri0, kTrireg };

auto Fold(NetKind kind, FourStateBit a, FourStateBit b) -> FourStateBit {
  if (a == FourStateBit::kHighImpedance) return b;
  if (b == FourStateBit::kHighImpedance) return a;
  switch (kind) {
    case NetKind::kWand:
      if (a == FourStateBit::kZero || b == FourStateBit::kZero) {
        return FourStateBit::kZero;
      }
      return a == b ? a : FourStateBit::kUnknown;
    case NetKind::kWor:
      if (a == FourStateBit::kOne || b == FourStateBit::kOne) {
        return FourStateBit::kOne;
      }
      return a == b ? a : FourStateBit::kUnknown;
    case NetKind::kWire:
    case NetKind::kTri0:
    case NetKind::kTrireg:
      return a == b ? a : FourStateBit::kUnknown;
  }
  return FourStateBit::kUnknown;
}

struct Contribution {
  FourStateBit bit;
  StrengthLevel strength;
};

// What one position resolves to from what reaches it (LRM 28.12.1): the
// strongest level whose contributions fold to anything but z decides.
auto Resolve(NetKind kind, const std::vector<Contribution>& reaching)
    -> FourStateBit {
  for (std::size_t level = lyra::support::kStrengthLevelCount; level-- > 0;) {
    FourStateBit folded = FourStateBit::kHighImpedance;
    for (const Contribution& c : reaching) {
      if (static_cast<std::size_t>(c.strength) == level) {
        folded = Fold(kind, folded, c.bit);
      }
    }
    if (folded != FourStateBit::kHighImpedance) return folded;
  }
  return FourStateBit::kHighImpedance;
}

template <std::size_t W>
auto ValueOf(const std::array<FourStateBit, W>& bits)
    -> lyra::value::LogicVector<W> {
  std::uint64_t value = 0;
  std::uint64_t unknown = 0;
  for (std::size_t i = 0; i < W; ++i) {
    const auto encoded = static_cast<std::uint64_t>(bits.at(i));
    value |= (encoded & 1U) << i;
    unknown |= ((encoded >> 1U) & 1U) << i;
  }
  return lyra::value::LogicVector<W>::FromWords({value}, {unknown});
}

template <std::size_t W>
auto BitOf(const lyra::value::LogicVector<W>& bus, std::uint32_t position)
    -> FourStateBit {
  return lyra::value::ScalarAt(bus.View(), position);
}

// One driver as the definition sees it: the physical positions its part
// lies at, how strongly it drives, and what it last gave at each.
struct ModelDriver {
  std::vector<std::uint32_t> physical;
  StrengthLevel strength;
  std::vector<FourStateBit> given;
};

auto RandomBit(std::mt19937& random) -> FourStateBit {
  return static_cast<FourStateBit>(random() % 4U);
}

constexpr std::array<StrengthLevel, 3> kStrengths{
    StrengthLevel::kStrong, StrengthLevel::kPull, StrengthLevel::kWeak};

// Two nets of one kind joined over some of their positions, the drivers
// attached to either, and what the definition says each shows. Each driver
// gives a value of one of a few widths, which is the part of its net it
// drives.
class Harness {
 public:
  Harness(NetKind kind, std::uint32_t seed) : kind_(kind), random_(seed) {
    own_.fill(
        Contribution{
            .bit = FourStateBit::kHighImpedance,
            .strength = StrengthLevel::kHighImpedance});
    Initialize(net_);
    Initialize(other_);
    switch (kind) {
      case NetKind::kWire:
      case NetKind::kWand:
      case NetKind::kWor:
        break;
      case NetKind::kTri0:
        own_.fill(
            Contribution{
                .bit = FourStateBit::kZero, .strength = StrengthLevel::kPull});
        break;
      case NetKind::kTrireg:
        own_.fill(
            Contribution{
                .bit = FourStateBit::kUnknown,
                .strength = StrengthLevel::kMedium});
        break;
    }
  }

  // Drivers of random parts of either net, the two nets joined once half of
  // them have attached, so the join meets physical nets that already carry
  // contributions and cuts them where its positions end.
  void AttachRandomDrivers(std::size_t count) {
    for (std::size_t i = 0; i < count; ++i) {
      if (i == count / 2) {
        net_.Join(
            &other_, kJoinedHere, kJoinedThere, kJoinedCount, kJoinedHere,
            kJoinedCount, kJoinedThere, kJoinedCount, lyra::test::kWrittenAt);
      }
      const bool on_other = random_() % 3U == 0;
      switch (random_() % 3U) {
        case 0:
          AttachTo<1>(on_other);
          break;
        case 1:
          AttachTo<4>(on_other);
          break;
        default:
          AttachTo<8>(on_other);
          break;
      }
    }
  }

  // Each driver in turn gives a random value, and after each the net shows
  // what the definition says.
  void DriveAndCheck(std::size_t writes) {
    for (std::size_t i = 0; i < writes; ++i) {
      const std::size_t which = random_() % writers_.size();
      writers_[which]();
      Check(std::format("after write {} by driver {}", i, which));
      if (i == writes / 3) Force();
      if (i == 2 * writes / 3) Release();
    }
  }

 private:
  static auto Fill(FourStateBit bit) -> std::int64_t {
    return static_cast<std::int64_t>(bit);
  }
  static auto Level(std::size_t level) -> std::int64_t {
    return static_cast<std::int64_t>(level);
  }

  template <typename Net>
  void Initialize(Net& net) {
    switch (kind_) {
      case NetKind::kWire:
        net.InitializeTriState(Fill(FourStateBit::kHighImpedance), Level(0));
        break;
      case NetKind::kWand:
        net.InitializeWiredAnd(Fill(FourStateBit::kHighImpedance), Level(0));
        break;
      case NetKind::kWor:
        net.InitializeWiredOr(Fill(FourStateBit::kHighImpedance), Level(0));
        break;
      case NetKind::kTri0:
        net.InitializeTriState(
            Fill(FourStateBit::kZero),
            static_cast<std::int64_t>(StrengthLevel::kPull));
        break;
      case NetKind::kTrireg:
        net.InitializeRetaining(
            Fill(FourStateBit::kUnknown),
            static_cast<std::int64_t>(StrengthLevel::kMedium));
        break;
    }
  }

  template <std::size_t W>
  void AttachTo(bool on_other) {
    if (on_other) {
      Attach<W>(other_, PhysicalOfOther);
    } else {
      Attach<W>(net_, PhysicalOfBus);
    }
  }

  template <std::size_t W, std::size_t N>
  void Attach(
      ResolvedNet<lyra::value::LogicVector<N>>& net,
      std::uint32_t (*physical_of)(std::uint32_t)) {
    const auto first = static_cast<std::uint32_t>(random_() % (N - W + 1));
    const StrengthLevel strength = kStrengths.at(random_() % kStrengths.size());
    std::vector<std::uint32_t> physical;
    for (std::uint32_t p = first; p < first + W; ++p) {
      physical.push_back(physical_of(p));
    }
    model_.push_back(
        ModelDriver{
            .physical = std::move(physical),
            .strength = strength,
            .given =
                std::vector<FourStateBit>(W, FourStateBit::kHighImpedance)});
    auto driver = net.template AttachDriver<lyra::value::LogicVector<W>>(
        static_cast<std::int64_t>(strength), first, W, first, W,
        lyra::test::kWrittenAt);
    const std::size_t index = model_.size() - 1;
    writers_.push_back([this, driver, index] {
      std::array<FourStateBit, W> bits{};
      for (FourStateBit& bit : bits) bit = RandomBit(random_);
      model_[index].given.assign(bits.begin(), bits.end());
      driver.Set(ValueOf(bits));
    });
  }

  void Force() {
    forced_first_ = static_cast<std::uint32_t>(random_() % (kWidth - 4));
    std::array<FourStateBit, kWidth> bits{};
    for (FourStateBit& bit : bits) bit = RandomBit(random_);
    forced_ = bits;
    const auto level = static_cast<std::int64_t>(TakeoverLevel::kForce);
    const std::int64_t generation = net_.BeginTakeover(
        level, forced_first_, kForcedCount, lyra::test::kWrittenAt);
    net_.DriveTakeover(
        level, generation, ValueOf(bits), lyra::test::kWrittenAt);
    Check("after a force");
  }

  void Release() {
    net_.EndTakeover(
        static_cast<std::int64_t>(TakeoverLevel::kForce), forced_first_,
        kForcedCount, lyra::test::kWrittenAt);
    forced_.reset();
    Check("after a release");
  }

  void Check(const std::string& when) {
    std::array<FourStateBit, kPhysicalCount> expected{};
    for (std::uint32_t p = 0; p < kPhysicalCount; ++p) {
      std::vector<Contribution> reaching{own_.at(p)};
      for (const ModelDriver& d : model_) {
        for (std::size_t i = 0; i < d.physical.size(); ++i) {
          if (d.physical[i] == p) {
            reaching.push_back(
                Contribution{.bit = d.given[i], .strength = d.strength});
          }
        }
      }
      expected.at(p) = Resolve(kind_, reaching);
    }
    // Charge takes what the drivers decided wherever one decided (LRM 6.6.4).
    if (kind_ == NetKind::kTrireg) {
      for (std::uint32_t p = 0; p < kPhysicalCount; ++p) {
        own_.at(p).bit = expected.at(p);
      }
    }
    // A force of the bus is driven with a value as wide as the bus and shows
    // it at the positions it holds (LRM 10.6.2), under every name reaching
    // them.
    if (forced_.has_value()) {
      for (std::uint32_t p = forced_first_; p < forced_first_ + kForcedCount;
           ++p) {
        expected.at(PhysicalOfBus(p)) = forced_->at(p);
      }
    }
    const Bus& shown = net_.Get();
    for (std::uint32_t p = 0; p < kWidth; ++p) {
      ASSERT_EQ(BitOf(shown, p), expected.at(PhysicalOfBus(p)))
          << "position " << p << " of the bus " << when;
    }
    const OtherBus& other_shown = other_.Get();
    for (std::uint32_t p = 0; p < kOtherWidth; ++p) {
      ASSERT_EQ(BitOf(other_shown, p), expected.at(PhysicalOfOther(p)))
          << "position " << p << " of the joined net " << when;
    }
  }

  static constexpr std::uint32_t kForcedCount = 4;

  NetKind kind_;
  std::mt19937 random_;
  lyra::runtime::Runtime run_;
  ResolvedNet<Bus> net_;
  ResolvedNet<OtherBus> other_;
  std::array<Contribution, kPhysicalCount> own_{};
  std::vector<ModelDriver> model_;
  std::vector<std::function<void()>> writers_;
  std::optional<std::array<FourStateBit, kWidth>> forced_;
  std::uint32_t forced_first_ = 0;
};

class NetResolution
    : public ::testing::TestWithParam<std::tuple<NetKind, std::uint32_t>> {};

TEST_P(NetResolution, EveryPositionIsWhatItsDriversResolveTo) {
  const auto [kind, seed] = GetParam();
  Harness harness(kind, seed);
  harness.AttachRandomDrivers(2 + (seed % 6));
  harness.DriveAndCheck(60);
}

INSTANTIATE_TEST_SUITE_P(
    EveryNetType, NetResolution,
    ::testing::Combine(
        ::testing::Values(
            NetKind::kWire, NetKind::kWand, NetKind::kWor, NetKind::kTri0,
            NetKind::kTrireg),
        ::testing::Range<std::uint32_t>(0, 12)));

// A net narrower than the word its value is kept in holds no bit above its
// positions, as every integral value holds none above its width, whichever
// scalar its net type contributes.
TEST(NetStorage, ANetNarrowerThanItsWordHoldsNothingAboveItsPositions) {
  constexpr std::uint32_t kCount = 5;
  for (const FourStateBit fill :
       {FourStateBit::kHighImpedance, FourStateBit::kUnknown,
        FourStateBit::kOne}) {
    lyra::runtime::Runtime run;
    ResolvedNet<lyra::value::LogicVector<8>> net(kCount);
    net.InitializeTriState(static_cast<std::int64_t>(fill), 0);
    const lyra::value::ConstIntegralView held = net.Get().AsWords().View();
    EXPECT_EQ(held.value[0] >> kCount, 0U) << static_cast<int>(fill);
    EXPECT_EQ(held.unknown[0] >> kCount, 0U) << static_cast<int>(fill);
  }
}

}  // namespace

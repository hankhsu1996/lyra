#include "lyra/runtime/net.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/base/source_location.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/takeover.hpp"
#include "lyra/support/strength_level.hpp"
#include "lyra/support/takeover_level.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

auto NetFillOf(std::int64_t fill) -> value::FourStateBit {
  return static_cast<value::FourStateBit>(fill);
}

NetPositions::NetPositions(std::uint32_t count, value::FourStateBit bit)
    : count_(count), words_(2 * value::WordCountForBits(count)) {
  value::FillScalar(MutableView(), bit);
}

NetPositions::NetPositions(const NetPositions&) = default;
NetPositions::NetPositions(NetPositions&&) noexcept = default;
auto NetPositions::operator=(const NetPositions&) -> NetPositions& = default;
auto NetPositions::operator=(NetPositions&&) noexcept
    -> NetPositions& = default;
NetPositions::~NetPositions() = default;

auto NetPositions::Count() const -> std::uint32_t {
  return count_;
}

auto NetPositions::View() const -> value::ConstIntegralView {
  const std::span<const std::uint64_t> all(words_.data(), words_.size());
  return value::ConstIntegralView{
      .value = all.first(all.size() / 2),
      .unknown = all.subspan(all.size() / 2),
      .width = count_,
      .signedness = value::Signedness::kUnsigned};
}

auto NetPositions::MutableView() -> value::IntegralView {
  const std::span<std::uint64_t> all(words_.data(), words_.size());
  return value::IntegralView{
      .value = all.first(all.size() / 2),
      .unknown = all.subspan(all.size() / 2),
      .width = count_};
}

auto LevelBit(support::StrengthLevel level) -> OccupiedLevels {
  if (level == support::StrengthLevel::kHighImpedance) {
    return 0U;
  }
  return OccupiedLevels{1} << static_cast<unsigned>(level);
}

auto NetPositions::Part(std::uint32_t from, std::uint32_t count) const
    -> NetPositions {
  NetPositions part(count, value::FourStateBit::kHighImpedance);
  value::Extract(part.MutableView(), View(), from);
  return part;
}

NetName::NetName() = default;
NetName::~NetName() = default;

CarriedObjectSink::CarriedObjectSink() = default;
CarriedObjectSink::~CarriedObjectSink() = default;

DrivenNet::DrivenNet() = default;
DrivenNet::~DrivenNet() = default;

Net::Net(CarriedObjectSink& carried) : carried_(&carried) {
}

Net::~Net() = default;

ConnectableNet::ConnectableNet(NetName& name, DrivenNet& driven)
    : Net(driven), name_(&name), driven_(&driven) {
}

namespace {

// Some adjacent positions, counted from `first`, which a select's position may
// put partly or wholly outside what they are taken within.
struct PositionRun {
  std::int64_t first = 0;
  std::int64_t count = 0;
};

// The part of `run` that lies within `within` (LRM 7.4.5, 11.5.1): none where
// the two do not meet, which is then at the edge of `within` nearest `run`.
auto Within(PositionRun run, PositionRun within) -> PositionRun {
  const std::int64_t end = within.first + within.count;
  const std::int64_t low =
      std::clamp(std::max(run.first, within.first), within.first, end);
  const std::int64_t high =
      std::clamp(std::min(run.first + run.count, end), low, end);
  return {.first = low, .count = high - low};
}

}  // namespace

auto ConnectableNet::AttachDriverSlot(
    std::int64_t strength, std::int64_t first, std::int64_t width,
    std::int64_t reached_first, std::int64_t reached_count,
    base::SourceLocation at) -> DriverSlot& {
  const base::SourceLocationScope bound(at);
  const PositionRun driven = Within(
      Within(
          {.first = first, .count = width},
          {.first = reached_first, .count = reached_count}),
      {.first = 0, .count = name_->Count()});
  const std::uint32_t contribution = name_->Attach(
      static_cast<support::StrengthLevel>(strength),
      static_cast<std::uint32_t>(driven.first),
      static_cast<std::uint32_t>(driven.count));
  return slots_.emplace_back(
      DriverSlot{
          .net = driven_,
          .contribution = contribution,
          .width = static_cast<std::uint32_t>(width),
          .skip = static_cast<std::uint32_t>(
              std::max<std::int64_t>(driven.first - first, 0))});
}

void ConnectableNet::Join(
    ConnectableNet* other, std::int64_t here, std::int64_t there,
    std::int64_t count, std::int64_t here_reached_first,
    std::int64_t here_reached_count, std::int64_t there_reached_first,
    std::int64_t there_reached_count, base::SourceLocation at) {
  const base::SourceLocationScope bound(at);
  // Each side's run, as offsets into the `count` positions the two lay over
  // each other, and the offsets both keep.
  const PositionRun here_kept = Within(
      {.first = here, .count = count},
      {.first = here_reached_first, .count = here_reached_count});
  const PositionRun there_kept = Within(
      {.first = there, .count = count},
      {.first = there_reached_first, .count = there_reached_count});
  const PositionRun shared = Within(
      {.first = here_kept.first - here, .count = here_kept.count},
      {.first = there_kept.first - there, .count = there_kept.count});
  if (shared.count == 0) {
    return;
  }
  Name().Join(
      other->Name(), static_cast<std::uint32_t>(here + shared.first),
      static_cast<std::uint32_t>(there + shared.first),
      static_cast<std::uint32_t>(shared.count));
}

ConnectableNet::~ConnectableNet() = default;

auto ConnectableNet::Name() -> NetName& {
  return *name_;
}

void NetHasNoResolution() {
  // LRM 6.7.1, 6.6.7.
  throw SimulationError(
      "a net whose data type no net type resolves -- a real, a string, a "
      "class handle, or an aggregate holding one -- has more than one "
      "driver");
}

void RequireUnsettled(const std::optional<NetDecision>& decision) {
  if (decision.has_value()) {
    throw InternalError(
        "Net: every driver attaches while the design resolves, which "
        "is before the net settles what decides it");
  }
}

void NetName::SettleReaches() {
  settled_ = true;
  for (const NetReach& reach : reaches_) {
    reach.physical->names_settled_ = true;
  }
}

auto NetName::Attach(
    support::StrengthLevel strength, std::uint32_t first, std::uint32_t count)
    -> std::uint32_t {
  if (settled_) {
    throw InternalError(
        "NetName: every driver attaches while the design resolves, which is "
        "before the net settles what decides it");
  }
  const auto index = static_cast<std::uint32_t>(contributions_.size());
  contributions_.push_back(
      NetContribution{
          .first = first,
          .strength = strength,
          .positions =
              NetPositions(count, value::FourStateBit::kHighImpedance)});
  occupied_ |= LevelBit(strength);
  std::uint32_t position = first;
  std::uint32_t remaining = count;
  while (remaining > 0) {
    NetReach& reach = reaches_[ReachAt(position, remaining)];
    reach.covering.push_back(index);
    position += reach.count;
    remaining -= reach.count;
  }
  return index;
}

auto NetName::PositionsOf(std::uint32_t index) -> NetPositions& {
  if (index >= contributions_.size()) {
    throw InternalError("NetName: a driver names no attached contribution");
  }
  return contributions_[index].positions;
}

auto NetName::FirstOf(std::uint32_t index) const -> std::uint32_t {
  if (index >= contributions_.size()) {
    throw InternalError("NetName: a driver names no attached contribution");
  }
  return contributions_[index].first;
}

auto NetName::ContributionCount() const -> std::size_t {
  return contributions_.size();
}

auto NetName::Count() const -> std::uint32_t {
  return reaches_.back().net_offset + reaches_.back().count;
}

auto NetName::DecidesAlone(std::uint32_t index) const -> bool {
  const NetContribution& contribution = contributions_.at(index);
  if (LevelBit(contribution.strength) == 0U) {
    return false;
  }
  const auto [begin, end] = ReachesWithin(
      {.lsb = contribution.first, .width = contribution.positions.Count()});
  for (std::size_t r = begin; r < end; ++r) {
    const NetReach& reach = reaches_[r];
    const PhysicalNet& physical = *reach.physical;
    if (reach.covering.size() != 1 || physical.placements_.size() != 1 ||
        physical.own_kind_ != OwnContribution::kFixed ||
        LevelBit(physical.own_.strength) != 0U) {
      return false;
    }
  }
  return true;
}

void NetName::Reresolve(RuntimeEffects& runtime, value::BitPositions within) {
  const auto [begin, end] = ReachesWithin(within);
  for (std::size_t r = begin; r < end; ++r) {
    reaches_[r].physical->Reresolve(runtime);
  }
}

auto NetName::Overridden() const -> bool {
  return OverriddenWithin({.lsb = 0, .width = Count()});
}

auto NetName::OverriddenWithin(value::BitPositions within) const -> bool {
  const auto [begin, end] = ReachesWithin(within);
  for (std::size_t r = begin; r < end; ++r) {
    const PhysicalNet& physical = *reaches_[r].physical;
    if (physical.takeovers_ != nullptr && physical.takeovers_->Shows()) {
      return true;
    }
  }
  return false;
}

void NetName::FoldContributions(
    support::StrengthLevel level, value::NetResolution fold,
    std::uint32_t offset, NetPositions& group) const {
  NetPositions contribution(group.Count(), value::FourStateBit::kHighImpedance);
  for (const std::uint32_t index : reaches_[ReachHolding(offset)].covering) {
    const NetContribution& driver = contributions_[index];
    if (driver.strength == level) {
      value::Extract(
          contribution.MutableView(), driver.positions.View(),
          offset - driver.first);
      value::Resolve(
          group.MutableView(), group.View(), contribution.View(), fold);
    }
  }
}

auto NetName::ReachHolding(std::uint32_t position) const -> std::size_t {
  const auto after = std::ranges::upper_bound(
      reaches_, position, std::less<>{}, &NetReach::net_offset);
  if (after == reaches_.begin()) {
    throw InternalError(
        "NetName: a net's own reaches cover every position it has, so a "
        "position of it lies in one");
  }
  return static_cast<std::size_t>(after - reaches_.begin()) - 1;
}

auto NetName::ReachesWithin(value::BitPositions within) const
    -> std::pair<std::size_t, std::size_t> {
  if (within.width == 0) {
    return {0, 0};
  }
  const auto first = static_cast<std::uint32_t>(within.lsb);
  const auto last = static_cast<std::uint32_t>(within.lsb + within.width - 1);
  return {ReachHolding(first), ReachHolding(last) + 1};
}

namespace {

// The positions of `within`, some of a name's own, that `reach` covers,
// counted among the physical net's own.
auto CoveredByReach(const NetReach& reach, value::BitPositions within)
    -> value::BitPositions {
  const std::uint64_t low =
      std::max<std::uint64_t>(within.lsb, reach.net_offset);
  const std::uint64_t high = std::min<std::uint64_t>(
      within.lsb + within.width, std::uint64_t{reach.net_offset} + reach.count);
  return {.lsb = low - reach.net_offset, .width = high - low};
}

}  // namespace

auto NetName::BeginTakeover(
    support::TakeoverLevel level, value::BitPositions held) -> std::uint32_t {
  NameTakeover takeover{
      .level = level, .generation = ++takeover_generation_, .parts = {}};
  const auto [begin, end] = ReachesWithin(held);
  for (std::size_t r = begin; r < end; ++r) {
    const NetReach& reach = reaches_[r];
    takeover.parts.push_back(
        TakeoverPart{
            .physical = reach.physical.get(),
            .net_offset = reach.net_offset,
            .generation = reach.physical->BeginTakeover(
                level, CoveredByReach(reach, held))});
  }
  ForgetEndedTakeovers(level);
  const std::uint32_t generation = takeover.generation;
  takeovers_.push_back(std::move(takeover));
  return generation;
}

auto NetName::DriveTakeover(
    support::TakeoverLevel level, std::uint32_t generation,
    const value::ConstIntegralView& forced)
    -> std::optional<value::BitPositions> {
  const auto found =
      std::ranges::find_if(takeovers_, [&](const NameTakeover& takeover) {
        return takeover.level == level && takeover.generation == generation;
      });
  if (found == takeovers_.end()) {
    return std::nullopt;
  }
  std::optional<std::uint64_t> low;
  std::uint64_t high = 0;
  for (const TakeoverPart& part : found->parts) {
    if (!part.physical->DriveTakeover(
            level, part.generation, forced, part.net_offset)) {
      continue;
    }
    low =
        std::min(low.value_or(part.net_offset), std::uint64_t{part.net_offset});
    high =
        std::max(high, std::uint64_t{part.net_offset} + part.physical->Count());
  }
  if (!low.has_value()) {
    takeovers_.erase(found);
    return std::nullopt;
  }
  return value::BitPositions{.lsb = *low, .width = high - *low};
}

auto NetName::EndTakeover(
    support::TakeoverLevel level, value::BitPositions released) -> bool {
  bool ended = false;
  const auto [begin, end] = ReachesWithin(released);
  for (std::size_t r = begin; r < end; ++r) {
    const NetReach& reach = reaches_[r];
    ended =
        reach.physical->EndTakeover(level, CoveredByReach(reach, released)) ||
        ended;
  }
  ForgetEndedTakeovers(level);
  return ended;
}

void NetName::ForgetEndedTakeovers(support::TakeoverLevel level) {
  std::erase_if(takeovers_, [&](const NameTakeover& takeover) {
    return takeover.level == level &&
           std::ranges::none_of(takeover.parts, [&](const TakeoverPart& part) {
             return part.physical->takeovers_ != nullptr &&
                    part.physical->takeovers_->Holds(level, part.generation);
           });
  });
}

auto PhysicalNet::Count() const -> std::uint32_t {
  return own_.positions.Count();
}

PhysicalNet::PhysicalNet(
    NetContribution own, value::FourStateBit own_fill,
    value::NetResolution fold, OwnContribution own_kind)
    : own_(std::move(own)),
      own_fill_(own_fill),
      fold_(fold),
      own_kind_(own_kind) {
}

PhysicalNet::~PhysicalNet() = default;

void PhysicalNet::Admit(const NetPlacement& placement) {
  if (names_settled_) {
    throw InternalError(
        "PhysicalNet: every connection is stated while the design resolves, "
        "which is before any net reaching these positions settles what "
        "decides it");
  }
  for (const NetPlacement& existing : placements_) {
    if (existing.net == placement.net &&
        existing.net_offset == placement.net_offset) {
      return;
    }
  }
  placements_.push_back(placement);
}

auto PhysicalNet::Resolve() const -> NetPositions {
  OccupiedLevels occupied = LevelBit(own_.strength);
  for (const NetPlacement& placement : placements_) {
    occupied |= placement.net->occupied_;
  }
  NetPositions resolved(Count(), value::FourStateBit::kHighImpedance);
  for (std::size_t level = support::kStrengthLevelCount; level-- > 0;) {
    const auto at = static_cast<support::StrengthLevel>(level);
    if ((occupied & LevelBit(at)) == 0U) {
      continue;
    }
    NetPositions group =
        own_.strength == at
            ? own_.positions
            : NetPositions(Count(), value::FourStateBit::kHighImpedance);
    for (const NetPlacement& placement : placements_) {
      placement.net->FoldContributions(at, fold_, placement.net_offset, group);
    }
    value::Dominate(resolved.MutableView(), resolved.View(), group.View());
  }
  return resolved;
}

void PhysicalNet::Reresolve(RuntimeEffects& runtime) {
  NetPositions next = Resolve();
  // A net type that stores a value holds what its drivers last decided, so its
  // own contribution takes the resolution it just took part in: the positions
  // something drove are what it now carries, and the positions nothing drove
  // are the ones it decided itself, which leaves them as they were (LRM 6.6.4).
  // A takeover displaces what the positions show and not what they hold, so
  // this happens before one is consulted (LRM 10.6.2).
  if (own_kind_ == OwnContribution::kRetained) {
    own_.positions = next;
  }
  if (takeovers_ != nullptr) {
    takeovers_->Overlay(next);
  }
  for (const NetPlacement& placement : placements_) {
    placement.net->ShowPositions(runtime, placement.net_offset, next);
  }
}

auto PhysicalNet::BeginTakeover(
    support::TakeoverLevel level, value::BitPositions held) -> std::uint32_t {
  if (takeovers_ == nullptr) {
    takeovers_ = std::make_unique<Takeovers<NetPositions>>();
  }
  return takeovers_->Begin(level, held);
}

auto PhysicalNet::DriveTakeover(
    support::TakeoverLevel level, std::uint32_t generation,
    const value::ConstIntegralView& forced, std::uint32_t from) -> bool {
  if (takeovers_ == nullptr) {
    return false;
  }
  NetPositions evaluated(Count(), value::FourStateBit::kHighImpedance);
  value::Extract(evaluated.MutableView(), forced, from);
  return takeovers_->Drive(level, generation, evaluated);
}

auto PhysicalNet::EndTakeover(
    support::TakeoverLevel level, value::BitPositions released) -> bool {
  if (takeovers_ == nullptr) {
    return false;
  }
  takeovers_->End(level, released);
  return true;
}

auto PhysicalNet::Split(std::uint32_t cut) -> std::shared_ptr<PhysicalNet> {
  if (cut == 0 || cut >= Count()) {
    throw InternalError(
        "PhysicalNet: a cut falls inside these positions, since a cut at "
        "either end asks for the positions that are already here");
  }
  if (takeovers_ != nullptr || names_settled_) {
    throw InternalError(
        "PhysicalNet: every connection is stated while the design "
        "resolves, which is before any procedural continuous assignment "
        "can have taken these positions over and before any net reaching "
        "them settles what decides it");
  }
  auto high = std::make_shared<PhysicalNet>(
      NetContribution{
          .first = 0,
          .strength = own_.strength,
          .positions = own_.positions.Part(cut, Count() - cut)},
      own_fill_, fold_, own_kind_);
  for (const NetPlacement& placement : placements_) {
    high->placements_.push_back(
        NetPlacement{
            .net = placement.net, .net_offset = placement.net_offset + cut});
  }
  own_.positions = own_.positions.Part(0, cut);
  return high;
}

auto NetName::Declare(
    support::StrengthLevel strength, value::FourStateBit own_fill,
    value::NetResolution fold, OwnContribution own_kind, std::uint32_t count)
    -> NetPositions {
  auto physical = std::make_shared<PhysicalNet>(
      NetContribution{
          .first = 0,
          .strength = strength,
          .positions = NetPositions(count, own_fill)},
      own_fill, fold, own_kind);
  physical->Admit(NetPlacement{.net = this, .net_offset = 0});
  reaches_.push_back(
      NetReach{
          .physical = physical,
          .net_offset = 0,
          .count = count,
          .covering = {}});
  return physical->Resolve();
}

void NetName::Join(
    NetName& other, std::uint32_t here, std::uint32_t there,
    std::uint32_t count) {
  std::uint32_t position_here = here;
  std::uint32_t position_there = there;
  std::uint32_t remaining = count;
  // The two sides reach physical nets whose reaches fall wherever earlier
  // connections left them, so the coupling is taken in the pieces both sides
  // have whole. Each piece cuts what is longer than it, which is what leaves
  // every physical net covered entirely by every name in it.
  while (remaining > 0) {
    // Each side is asked in turn and cuts as it answers, so asking the first
    // again after the second has answered is what settles the piece: a cut for
    // one side reaches every name in the physical net it cut, the other side
    // among them.
    std::uint32_t piece = reaches_[ReachAt(position_here, remaining)].count;
    piece = other.reaches_[other.ReachAt(position_there, piece)].count;
    const std::shared_ptr<PhysicalNet> mine =
        reaches_[ReachAt(position_here, piece)].physical;
    const std::shared_ptr<PhysicalNet> theirs =
        other.reaches_[other.ReachAt(position_there, piece)].physical;
    MergePhysical(mine, theirs);
    position_here += piece;
    position_there += piece;
    remaining -= piece;
  }
}

auto NetName::ReachAt(std::uint32_t position, std::uint32_t within)
    -> std::size_t {
  const std::size_t index = ReachHolding(position);
  const NetReach& reach = reaches_[index];
  if (position > reach.net_offset) {
    CutPhysical(reach.physical, position - reach.net_offset);
    return ReachAt(position, within);
  }
  if (reach.count > within) {
    CutPhysical(reach.physical, within);
  }
  return index;
}

void NetName::CutPhysical(
    std::shared_ptr<PhysicalNet> physical, std::uint32_t cut) {
  const std::shared_ptr<PhysicalNet> above = physical->Split(cut);
  for (const NetPlacement& placement : above->placements_) {
    std::vector<NetReach>& reaches = placement.net->reaches_;
    const std::size_t i =
        placement.net->ReachHolding(placement.net_offset - cut);
    NetReach tail{
        .physical = above,
        .net_offset = reaches[i].net_offset + cut,
        .count = reaches[i].count - cut,
        .covering = reaches[i].covering};
    reaches[i].count = cut;
    reaches.insert(
        reaches.begin() + static_cast<std::ptrdiff_t>(i) + 1, std::move(tail));
  }
}

void NetName::MergePhysical(
    std::shared_ptr<PhysicalNet> keep, std::shared_ptr<PhysicalNet> folded) {
  if (keep == folded) {
    return;
  }
  if (keep->fold_ != folded->fold_ || keep->own_kind_ != folded->own_kind_ ||
      keep->own_.strength != folded->own_.strength ||
      keep->own_fill_ != folded->own_fill_) {
    // LRM 23.3.3.7, 10.11.
    throw SimulationError(
        "a connection makes one physical net of positions whose nets state "
        "dissimilar net types; a resolution over net types that resolve "
        "differently is not yet supported");
  }
  // Both cover the same number of positions and the coupling puts their first
  // ones together, so each name keeps the offset it had among its own.
  for (const NetPlacement& placement : folded->placements_) {
    keep->Admit(placement);
    for (NetReach& reach : placement.net->reaches_) {
      if (reach.physical == folded) {
        reach.physical = keep;
      }
    }
  }
  keep->Reresolve(current_runtime());
}

}  // namespace lyra::runtime

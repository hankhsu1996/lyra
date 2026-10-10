#include "lyra/runtime/net.hpp"

#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
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
  value::FillScalar(Write(), count, bit);
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

auto NetPositions::Read() const -> value::ConstPlanes {
  const std::span<const std::uint64_t> all(words_.data(), words_.size());
  return value::ConstPlanes{
      .value = all.first(all.size() / 2),
      .unknown = all.subspan(all.size() / 2)};
}

auto NetPositions::Write() -> value::Planes {
  const std::span<std::uint64_t> all(words_.data(), words_.size());
  return value::Planes{
      .value = all.first(all.size() / 2),
      .unknown = all.subspan(all.size() / 2)};
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
  value::Extract(part.Write(), count, Read(), count_, from);
  return part;
}

NetName::NetName() = default;
NetName::~NetName() = default;

ConnectableNet::ConnectableNet(NetName& name) : name_(&name) {
}

ConnectableNet::~ConnectableNet() = default;

auto ConnectableNet::Name() const -> NetName& {
  return *name_;
}

auto NetName::Reaches() const -> const std::vector<NetReach>& {
  return reaches_;
}

void NetName::Occupy(support::StrengthLevel level) {
  occupied_ |= LevelBit(level);
}

auto PhysicalNet::Count() const -> std::uint32_t {
  return own_.value.Count();
}

PhysicalNet::PhysicalNet(
    DriveContribution<NetPositions> own, value::FourStateBit own_fill,
    value::NetResolution fold, OwnContribution own_kind)
    : own_(std::move(own)),
      own_fill_(own_fill),
      fold_(fold),
      own_kind_(own_kind) {
}

PhysicalNet::~PhysicalNet() = default;

void PhysicalNet::Admit(const NetPlacement& placement) {
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
            ? own_.value
            : NetPositions(Count(), value::FourStateBit::kHighImpedance);
    for (const NetPlacement& placement : placements_) {
      placement.net->FoldContributions(at, fold_, placement.net_offset, group);
    }
    value::Dominate(resolved.Write(), resolved.Read(), group.Read());
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
    own_.value = next;
  }
  const NetPositions* forced =
      takeovers_ == nullptr ? nullptr : takeovers_->Highest();
  if (forced != nullptr) {
    next = *forced;
  }
  for (const NetPlacement& placement : placements_) {
    placement.net->ShowPositions(runtime, placement.net_offset, next);
  }
}

auto PhysicalNet::BeginTakeover(support::TakeoverLevel level) -> std::uint32_t {
  if (takeovers_ == nullptr) {
    takeovers_ = std::make_unique<Takeovers<NetPositions>>();
  }
  return takeovers_->Begin(level);
}

auto PhysicalNet::DriveTakeover(
    support::TakeoverLevel level, std::uint32_t generation,
    value::ConstPlanes forced, std::uint64_t width) -> bool {
  return takeovers_ != nullptr &&
         takeovers_->Drive(
             level, generation, [&](std::optional<NetPositions>& held) {
               if (!held.has_value()) {
                 held.emplace(Count(), value::FourStateBit::kHighImpedance);
               }
               value::Extract(held->Write(), held->Count(), forced, width, 0);
             });
}

auto PhysicalNet::EndTakeover(support::TakeoverLevel level) -> bool {
  if (takeovers_ == nullptr) {
    return false;
  }
  takeovers_->End(level);
  return true;
}

auto PhysicalNet::Split(std::uint32_t at) -> std::shared_ptr<PhysicalNet> {
  if (at == 0 || at >= Count()) {
    throw InternalError(
        "PhysicalNet: a cut falls inside these positions, since a cut at "
        "either end asks for the positions that are already here");
  }
  if (takeovers_ != nullptr) {
    throw InternalError(
        "PhysicalNet: every connection is stated while the design "
        "resolves, which is before any procedural continuous assignment "
        "can have taken these positions over");
  }
  auto high = std::make_shared<PhysicalNet>(
      DriveContribution<NetPositions>{
          .value = own_.value.Part(at, Count() - at),
          .strength = own_.strength},
      own_fill_, fold_, own_kind_);
  for (const NetPlacement& placement : placements_) {
    high->placements_.push_back(
        NetPlacement{
            .net = placement.net, .net_offset = placement.net_offset + at});
  }
  own_.value = own_.value.Part(0, at);
  return high;
}

auto NetName::Declare(
    support::StrengthLevel strength, value::FourStateBit own_fill,
    value::NetResolution fold, OwnContribution own_kind, std::uint32_t count)
    -> NetPositions {
  auto physical = std::make_shared<PhysicalNet>(
      DriveContribution<NetPositions>{
          .value = NetPositions(count, own_fill), .strength = strength},
      own_fill, fold, own_kind);
  physical->Admit(NetPlacement{.net = this, .net_offset = 0});
  NetPositions resolved = physical->Resolve();
  reaches_.push_back(
      NetReach{
          .physical = std::move(physical), .net_offset = 0, .count = count});
  return resolved;
}

void NetName::Join(
    NetName& other, std::uint32_t here, std::uint32_t there,
    std::uint32_t count) {
  std::uint32_t at_here = here;
  std::uint32_t at_there = there;
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
    std::uint32_t piece = reaches_[ReachAt(at_here, remaining)].count;
    piece = other.reaches_[other.ReachAt(at_there, piece)].count;
    const std::shared_ptr<PhysicalNet> mine =
        reaches_[ReachAt(at_here, piece)].physical;
    const std::shared_ptr<PhysicalNet> theirs =
        other.reaches_[other.ReachAt(at_there, piece)].physical;
    MergePhysical(mine, theirs);
    at_here += piece;
    at_there += piece;
    remaining -= piece;
  }
}

auto NetName::ReachAt(std::uint32_t position, std::uint32_t within)
    -> std::size_t {
  for (std::size_t at = 0; at < reaches_.size(); ++at) {
    const NetReach& reach = reaches_[at];
    if (position < reach.net_offset ||
        position - reach.net_offset >= reach.count) {
      continue;
    }
    if (position > reach.net_offset) {
      CutPhysical(reach.physical, position - reach.net_offset);
      return ReachAt(position, within);
    }
    if (reach.count > within) {
      CutPhysical(reach.physical, within);
      return ReachAt(position, within);
    }
    return at;
  }
  throw InternalError(
      "ResolvedNet: a net's own reaches cover every position it has, so a "
      "connection naming one reaches a physical net that holds it");
}

void NetName::CutPhysical(
    std::shared_ptr<PhysicalNet> physical, std::uint32_t at) {
  const std::shared_ptr<PhysicalNet> above = physical->Split(at);
  for (const NetPlacement& placement : above->placements_) {
    std::vector<NetReach>& reaches = placement.net->reaches_;
    for (std::size_t i = 0; i < reaches.size(); ++i) {
      if (reaches[i].physical != physical) {
        continue;
      }
      const NetReach tail{
          .physical = above,
          .net_offset = reaches[i].net_offset + at,
          .count = reaches[i].count - at};
      reaches[i].count = at;
      reaches.insert(
          reaches.begin() + static_cast<std::ptrdiff_t>(i) + 1, tail);
      break;
    }
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
    throw SimulationError(
        "a connection makes one physical net of positions whose nets state "
        "dissimilar net types; a resolution over net types that resolve "
        "differently (LRM 23.3.3.7, 10.11) is not yet supported");
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

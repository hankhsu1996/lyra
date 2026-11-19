#pragma once

#include <array>
#include <cstddef>
#include <memory>
#include <optional>

#include "lyra/runtime/value_handle.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/kept_copy.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/wide.hpp"

namespace lyra::runtime {

class OpenWrite;

// A place designated within a write in progress, in a form a caller that names
// no C++ type can hold: the write, and where the place lies. It borrows the
// write, so it ends with nothing to do. The steps a write takes, and landing
// it, are handed one; each step answers with the next.
struct ErasedDesignation {
  OpenWrite* write;
  void* part;
};

// A write in progress into the storage a capability wrapper stands for (LRM
// 11.5.1), held by whoever writes for as long as the write lasts. It is the
// write bracket a wrapper opens, in a form a caller that names no C++ type can
// hold: the caller gives it storage, designates the whole of the wrapper's
// contents within it, takes steps into parts from there, lands it, writes
// whatever it writes there, and ends it once the write is over -- which is
// when the wrapper is told, once, whether the write changed it.
//
// Which wrapper it is, and the value the write lands on as it was before, are
// the bracket's own business and held inside it. So one object serves every
// wrapper and every value, and what it costs is what the bracket costs: what is
// kept of the part landed on, only where something reads the answer and no step
// has given it already.
class OpenWrite {
 public:
  template <MutationSink Sink>
  explicit OpenWrite(Sink sink) : bracket_of_(&kBracketOf<Sink>) {
    std::construct_at(Bracket<Sink>(bracket_.data()), sink);
  }

  OpenWrite(const OpenWrite&) = delete;
  auto operator=(const OpenWrite&) -> OpenWrite& = delete;
  OpenWrite(OpenWrite&&) = delete;
  auto operator=(OpenWrite&&) -> OpenWrite& = delete;

  ~OpenWrite() {
    if (landing_of_ != nullptr) {
      landing_of_->end(landing_.data(), *bracket_of_, bracket_.data());
    }
    bracket_of_->end(bracket_.data());
  }

  // The whole of the wrapper's contents, designated within this write.
  [[nodiscard]] auto Whole() -> ErasedDesignation {
    return ErasedDesignation{
        .write = this, .part = bracket_of_->storage(bracket_.data())};
  }

  void Formed(value::Formation formed) {
    bracket_of_->formed(bracket_.data(), formed);
  }

  // The write went on to a part of the one it had reached, by `step`.
  void Stepped(const PartPath& step) {
    bracket_of_->stepped(bracket_.data(), step);
  }

  // A write of the `count` elements from `start` moved at least one of them.
  void Moved(std::int64_t start, std::int64_t count) {
    bracket_of_->moved(bracket_.data(), start, count);
  }

  // Whether what a write lands on is worth keeping from before it; and the
  // bits a write moved, told by a write into some bits of a packed value, which
  // keeps and compares those bits itself.
  [[nodiscard]] auto Tells() -> bool {
    return bracket_of_->undecided(bracket_.data());
  }
  void Landed(const Change& change) {
    bracket_of_->landed(bracket_.data(), change);
  }

  // Whether a modification the write is still to make has to be announced,
  // which only a write a per-interval observer watches has to do.
  [[nodiscard]] auto Announces() -> bool {
    return bracket_of_->announces(bracket_.data());
  }

  // Announces that the part `further` leads to from the part the steps so far
  // reached, which `copy` copies, is about to be modified, so the open
  // interval keeps its original where a per-interval observer
  // watches the place (LRM 9.2.2.2). A write no such observer reads asks only
  // that.
  template <class Copy>
  void BeforeModify(const PartPath& further, Copy copy) {
    if (!bracket_of_->announces(bracket_.data())) {
      return;
    }
    bracket_of_->before_modify(bracket_.data(), further, CopyOriginalBy(copy));
  }
  // The same of a part of a type known here, of one laid out by `type`, and
  // of an integral value wider than a word.
  template <value::LyraValue Part>
  void BeforeModifyPart(Part& part) {
    BeforeModify(PartPath{}, [&](EffectStorage& into) {
      return NewOriginal<ValueOriginal<Part>>(into, part);
    });
  }
  void BeforeModifyLaidOut(
      const value::ValueType& type, void* part,
      const PartPath& further = PartPath{}) {
    BeforeModify(further, [&](EffectStorage& into) {
      return NewOriginal<LaidOutOriginal>(into, type, part);
    });
  }
  template <value::HeldAsWords Wide>
  void BeforeModifyWide(Wide part) {
    BeforeModify(PartPath{}, [&](EffectStorage& into) {
      return NewOriginal<WordsOriginal<Wide>>(into, part);
    });
  }
  // The same of the bits `bits` of the integral value the steps so far
  // reached, whose words `live` names: only the words they lie in.
  void BeforeModifyBits(value::IntegralView live, value::BitPositions bits) {
    if (!bracket_of_->announces(bracket_.data())) {
      return;
    }
    bracket_of_->before_modify_bits(bracket_.data(), live, bits);
  }

  // The write lands on `part`, whose value from before the write is kept where
  // the answer is still wanted.
  template <typename Part>
  void Land(Part& part) {
    if (!Tells()) {
      return;
    }
    BeforeModifyPart(part);
    std::construct_at(Landing<PartLanding<Part>>(landing_.data()), part);
    landing_of_ = &kLandingOf<Part>;
  }

  // The same, for a value laid out by its type where it lies, which is its
  // bytes and nothing that says what they are: `type` states it. Its value
  // from before the write is a value of its own.
  void LandLaidOut(const value::ValueType& type, void* part) {
    if (!Tells()) {
      return;
    }
    BeforeModifyLaidOut(type, part);
    std::construct_at(Landing<LaidOutLanding>(landing_.data()), type, part);
    landing_of_ = &kLaidOutLandingOf;
  }

  // The same, for an integral value wider than a word where it lies, which is
  // its bytes too: `part` names them with how wide the value is. What is kept
  // of it is the bits it held, so the write tells which moved.
  template <value::HeldAsWords Wide>
  void LandWide(Wide part) {
    if (!Tells()) {
      return;
    }
    BeforeModifyWide(part);
    std::construct_at(Landing<WideLanding<Wide>>(landing_.data()), part);
    landing_of_ = &kWideLandingOf<Wide>;
  }

 private:
  // Room for the largest bracket any wrapper opens: the wrapper, a reference
  // to its storage, what the write has learned, and a copy of the storage for
  // an overridden write to land in.
  static constexpr std::size_t kBracketCapacity = 256;
  // Room for what a write keeps of the largest part it lands on, beside where
  // the part lies.
  static constexpr std::size_t kLandingCapacity = 176;

  // What the room holds, asked of the bracket one wrapper opened there.
  struct BracketOf {
    void* (*storage)(void* room);
    bool (*undecided)(void* room);
    bool (*announces)(void* room);
    void (*formed)(void* room, value::Formation formed);
    void (*stepped)(void* room, const PartPath& step);
    void (*moved)(void* room, std::int64_t start, std::int64_t count);
    void (*landed)(void* room, const Change& change);
    void (*before_modify)(
        void* room, const PartPath& further, CopyOriginal copy);
    void (*before_modify_bits)(
        void* room, value::IntegralView live, value::BitPositions bits);
    void (*end)(void* room);
  };

  template <MutationSink Sink>
  static auto Bracket(void* room) -> WriteBracket<Sink>* {
    static_assert(sizeof(WriteBracket<Sink>) <= kBracketCapacity);
    static_assert(alignof(WriteBracket<Sink>) <= alignof(void*));
    return static_cast<WriteBracket<Sink>*>(room);
  }

  template <MutationSink Sink>
  static constexpr BracketOf kBracketOf{
      .storage = [](void* room) -> void* {
        return HandleTo(Bracket<Sink>(room)->Storage());
      },
      .undecided = [](void* room) -> bool {
        return Bracket<Sink>(room)->Tells();
      },
      .announces = [](void* room) -> bool {
        return Bracket<Sink>(room)->Announces();
      },
      .formed =
          [](void* room, value::Formation formed) {
            Bracket<Sink>(room)->Formed(formed);
          },
      .stepped =
          [](void* room, const PartPath& step) {
            Bracket<Sink>(room)->Stepped(step);
          },
      .moved =
          [](void* room, std::int64_t start, std::int64_t count) {
            Bracket<Sink>(room)->Moved(start, count);
          },
      .landed =
          [](void* room, const Change& change) {
            Bracket<Sink>(room)->Landed(change);
          },
      .before_modify =
          [](void* room, const PartPath& further, CopyOriginal copy) {
            Bracket<Sink>(room)->BeforeModify(further, copy);
          },
      .before_modify_bits =
          [](void* room, value::IntegralView live, value::BitPositions bits) {
            Bracket<Sink>(room)->BeforeModifyBits(live, bits);
          },
      .end =
          [](void* room) {
            WriteBracket<Sink>* bracket = Bracket<Sink>(room);
            bracket->End();
            std::destroy_at(bracket);
          }};

  // The part a write landed on, and what it keeps of it from before the write.
  template <typename Part>
  struct PartLanding {
    explicit PartLanding(Part& landed) : part(&landed), kept(landed) {
    }
    Part* part;
    WriteOriginal<Part> kept;
  };

  // A part whose type the library is handed keeps its value from before the
  // write aside, as the C++ backend's write keeps a part of a type it knows.
  struct LaidOutLanding {
    LaidOutLanding(const value::ValueType& type, void* landed)
        : part(landed), kept(type, landed) {
    }

    void* part;
    value::KeptCopy kept;
  };

  template <value::HeldAsWords Wide>
  struct WideLanding {
    explicit WideLanding(Wide landed) : part(landed), kept(landed) {
    }
    Wide part;
    WriteOriginal<Wide> kept;
  };

  // What the landing room holds, asked of the part one write landed on. Ending
  // it tells the bracket what the landing found.
  struct ResolveBitRange {
    void (*end)(void* room, const BracketOf& bracket_of, void* bracket);
  };

  template <typename Kept>
  static auto Landing(void* room) -> Kept* {
    static_assert(sizeof(Kept) <= kLandingCapacity);
    static_assert(alignof(Kept) <= alignof(void*));
    return static_cast<Kept*>(room);
  }

  template <typename Part>
  static constexpr ResolveBitRange kLandingOf{
      .end = [](void* room, const BracketOf& bracket_of, void* bracket) {
        auto* landing = Landing<PartLanding<Part>>(room);
        if (const std::optional<Change> change =
                landing->kept.ChangeTo(*landing->part)) {
          bracket_of.landed(bracket, *change);
        }
        std::destroy_at(landing);
      }};

  static constexpr ResolveBitRange kLaidOutLandingOf{
      .end = [](void* room, const BracketOf& bracket_of, void* bracket) {
        auto* landing = Landing<LaidOutLanding>(room);
        if (!landing->kept.IsBitIdenticalTo(landing->part)) {
          bracket_of.landed(bracket, Change::Whole());
        }
        std::destroy_at(landing);
      }};

  template <value::HeldAsWords Wide>
  static constexpr ResolveBitRange kWideLandingOf{
      .end = [](void* room, const BracketOf& bracket_of, void* bracket) {
        auto* landing = Landing<WideLanding<Wide>>(room);
        if (const std::optional<Change> change =
                landing->kept.ChangeTo(landing->part)) {
          bracket_of.landed(bracket, *change);
        }
        std::destroy_at(landing);
      }};

  alignas(void*) std::array<std::byte, kBracketCapacity> bracket_{};
  alignas(void*) std::array<std::byte, kLandingCapacity> landing_{};
  const BracketOf* bracket_of_;
  const ResolveBitRange* landing_of_ = nullptr;
};

}  // namespace lyra::runtime

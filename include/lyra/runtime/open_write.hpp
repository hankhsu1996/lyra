#pragma once

#include <array>
#include <cstddef>
#include <memory>
#include <optional>

#include "lyra/runtime/value_handle.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/formation.hpp"
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

  // A slice write moved at least one element.
  void Moved() {
    bracket_of_->moved(bracket_.data());
  }

  // Whether what a write lands on is worth keeping from before it; and the
  // bits a write moved, told by a write into some bits of a packed value, which
  // keeps and compares those bits itself.
  [[nodiscard]] auto Undecided() -> bool {
    return bracket_of_->undecided(bracket_.data());
  }
  void Landed(const Change& change) {
    bracket_of_->landed(bracket_.data(), change);
  }

  // The write lands on `part`, whose value from before the write is kept where
  // the answer is still wanted.
  template <typename Part>
  void Land(Part& part) {
    if (!Undecided()) {
      return;
    }
    std::construct_at(Landing<PartLanding<Part>>(landing_.data()), part);
    landing_of_ = &kLandingOf<Part>;
  }

  // The same, for a tuple where it lies. No object stands for one there -- a
  // component of a tuple is bytes of the tuple holding it -- so the part is the
  // tuple's bytes, and its value from before the write is a tuple of its own.
  void LandTuple(void* part) {
    if (!Undecided()) {
      return;
    }
    std::construct_at(Landing<TupleLanding>(landing_.data()), part);
    landing_of_ = &kTupleLandingOf;
  }

  // The same, for an integral value wider than a word where it lies, which is
  // its bytes too: `part` names them with how wide the value is. What is kept
  // of it is the bits it held, so the write tells which moved.
  template <value::HeldAsWords Wide>
  void LandWide(Wide part) {
    if (!Undecided()) {
      return;
    }
    std::construct_at(Landing<WideLanding<Wide>>(landing_.data()), part);
    landing_of_ = &kWideLandingOf<Wide>;
  }

 private:
  // Room for the largest bracket any wrapper opens: the wrapper, a reference
  // to its storage, what the write has learned, and a copy of the storage for
  // a write the wrapper turns away to land in.
  static constexpr std::size_t kBracketCapacity = 192;
  // Room for what a write keeps of the largest part it lands on, beside where
  // the part lies.
  static constexpr std::size_t kLandingCapacity = 176;

  // What the room holds, asked of the bracket one wrapper opened there.
  struct BracketOf {
    void* (*storage)(void* room);
    bool (*undecided)(void* room);
    void (*formed)(void* room, value::Formation formed);
    void (*moved)(void* room);
    void (*landed)(void* room, const Change& change);
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
        return Bracket<Sink>(room)->Undecided();
      },
      .formed =
          [](void* room, value::Formation formed) {
            Bracket<Sink>(room)->Formed(formed);
          },
      .moved = [](void* room) { Bracket<Sink>(room)->Moved(); },
      .landed =
          [](void* room, const Change& change) {
            Bracket<Sink>(room)->Landed(change);
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
    KeptPart<Part> kept;
  };

  struct TupleLanding {
    explicit TupleLanding(void* landed)
        : part(landed), before(value::RuntimeTuple::CopyOf(landed)) {
    }
    void* part;
    value::RuntimeTuple before;
  };

  template <value::HeldAsWords Wide>
  struct WideLanding {
    explicit WideLanding(Wide landed) : part(landed), kept(landed) {
    }
    Wide part;
    KeptPart<Wide> kept;
  };

  // What the landing room holds, asked of the part one write landed on. Ending
  // it tells the bracket what the landing found.
  struct LandingOf {
    void (*end)(void* room, const BracketOf& bracket_of, void* bracket);
  };

  template <typename Kept>
  static auto Landing(void* room) -> Kept* {
    static_assert(sizeof(Kept) <= kLandingCapacity);
    static_assert(alignof(Kept) <= alignof(void*));
    return static_cast<Kept*>(room);
  }

  template <typename Part>
  static constexpr LandingOf kLandingOf{
      .end = [](void* room, const BracketOf& bracket_of, void* bracket) {
        auto* landing = Landing<PartLanding<Part>>(room);
        if (const std::optional<Change> change =
                landing->kept.ChangeTo(*landing->part)) {
          bracket_of.landed(bracket, *change);
        }
        std::destroy_at(landing);
      }};

  static constexpr LandingOf kTupleLandingOf{
      .end = [](void* room, const BracketOf& bracket_of, void* bracket) {
        auto* landing = Landing<TupleLanding>(room);
        if (!value::RuntimeTuple::BitIdentical(
                landing->before.Bytes(), landing->part)) {
          bracket_of.landed(bracket, Change::Whole());
        }
        std::destroy_at(landing);
      }};

  template <value::HeldAsWords Wide>
  static constexpr LandingOf kWideLandingOf{
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
  const LandingOf* landing_of_ = nullptr;
};

}  // namespace lyra::runtime

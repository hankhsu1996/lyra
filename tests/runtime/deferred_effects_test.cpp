#include "lyra/runtime/deferred_effects.hpp"

#include <cstddef>
#include <gtest/gtest.h>
#include <memory>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/closure.hpp"
#include "lyra/runtime/effect_storage.hpp"

namespace {

using lyra::runtime::ClosureValue;
using lyra::runtime::DeferredEffects;
using lyra::runtime::EffectStorage;

using Log = std::vector<int>;

// A closure laid out as the program lays out one a region keeps: made in the
// storage the effects hand out, by a placement new-expression naming them, and
// then kept there. What it captures is shared with the test, so the test sees
// whether it was ended by whether the share was let go of.
class ProgramEffect final : public ClosureValue {
 public:
  ProgramEffect(std::shared_ptr<Log> log, int id);

  void operator()() {
    log_->push_back(id_);
  }

 private:
  std::shared_ptr<Log> log_;
  int id_;
};

constexpr lyra::runtime::ClosureDefinition<void()> kProgramEffectDefinition =
    lyra::runtime::DefinitionOf<ProgramEffect>();

ProgramEffect::ProgramEffect(std::shared_ptr<Log> log, int id)
    : ClosureValue(&kProgramEffectDefinition), log_(std::move(log)), id_(id) {
}

// What a placement new-expression naming the effects does, then the commit:
// storage from them, the closure made there, and then kept by its address.
auto MakeProgramEffect(
    DeferredEffects& effects, const std::shared_ptr<Log>& log, int id)
    -> void* {
  return std::construct_at(
      static_cast<ProgramEffect*>(operator new(
          sizeof(ProgramEffect), &effects)),
      log, id);
}

void KeepProgramEffect(
    DeferredEffects& effects, const std::shared_ptr<Log>& log, int id) {
  effects.Commit(MakeProgramEffect(effects, log, id));
}

void RunAll(DeferredEffects& effects) {
  effects.Drain([](const auto& effect) { effect(); });
}

// LRM 4.6: the effects run in the order they were kept, whether the program
// made them or the library did, and however many blocks of storage they took
// (five thousand fill several blocks); each is ended once it has run.
TEST(DeferredEffectsTest, RunsInTheOrderKeptAndEndsEachOnce) {
  auto log = std::make_shared<Log>();
  EffectStorage storage;
  DeferredEffects effects(storage);
  constexpr int kCount = 5000;
  for (int id = 0; id < kCount; ++id) {
    if (id % 2 == 0) {
      KeepProgramEffect(effects, log, id);
    } else {
      effects.Emplace([log, id] { log->push_back(id); });
    }
  }
  RunAll(effects);
  ASSERT_EQ(log->size(), static_cast<std::size_t>(kCount));
  for (int id = 0; id < kCount; ++id) {
    EXPECT_EQ(log->at(static_cast<std::size_t>(id)), id);
  }
  EXPECT_EQ(log.use_count(), 1);
  EXPECT_TRUE(effects.Empty());

  KeepProgramEffect(effects, log, kCount);
  RunAll(effects);
  EXPECT_EQ(log->back(), kCount);
  EXPECT_EQ(log.use_count(), 1);
}

// An effect never run -- the run ended first -- is ended all the same, when
// the effects holding it end.
TEST(DeferredEffectsTest, EndsWhatWasNeverRun) {
  auto log = std::make_shared<Log>();
  EffectStorage storage;
  {
    DeferredEffects effects(storage);
    KeepProgramEffect(effects, log, 1);
    effects.Emplace([log] { log->push_back(2); });
    EXPECT_EQ(log.use_count(), 3);
  }
  EXPECT_TRUE(log->empty());
  EXPECT_EQ(log.use_count(), 1);
}

// An effect a run leaves by raising is ended on the way out, and what was not
// reached yet is ended with the effects.
TEST(DeferredEffectsTest, EndsAnEffectARunLeavesByRaising) {
  auto log = std::make_shared<Log>();
  EffectStorage storage;
  {
    DeferredEffects effects(storage);
    KeepProgramEffect(effects, log, 1);
    effects.Emplace([] { throw lyra::InternalError("leaves"); });
    KeepProgramEffect(effects, log, 3);
    EXPECT_THROW(RunAll(effects), lyra::InternalError);
    EXPECT_EQ(*log, Log{1});
    EXPECT_EQ(log.use_count(), 2);
  }
  EXPECT_EQ(log.use_count(), 1);
}

// A later slot's effects appended to the current one's come after what the
// current one already holds, each where it was made, and each is ended once.
TEST(DeferredEffectsTest, AppendsALaterSlotsEffectsBehindItsOwn) {
  auto log = std::make_shared<Log>();
  EffectStorage storage;
  DeferredEffects current(storage);
  DeferredEffects later(storage);
  KeepProgramEffect(current, log, 1);
  for (int id = 2; id < 2000; ++id) {
    KeepProgramEffect(later, log, id);
  }
  current.Append(later);
  EXPECT_TRUE(later.Empty());
  KeepProgramEffect(current, log, 2000);
  RunAll(current);
  ASSERT_EQ(log->size(), std::size_t{2000});
  for (std::size_t at = 0; at < log->size(); ++at) {
    EXPECT_EQ(log->at(at), static_cast<int>(at) + 1);
  }
  EXPECT_EQ(log.use_count(), 1);
}

// An effect's bytes go back once it has ended, so the next effect of its size
// is made where it lay, whichever effects keep it: what is held follows how
// many are pending at once, never how many times they are due at.
TEST(DeferredEffectsTest, MakesTheNextEffectWhereAnEndedOneLay) {
  auto log = std::make_shared<Log>();
  EffectStorage storage;
  DeferredEffects first(storage);
  DeferredEffects second(storage);
  void* const made = MakeProgramEffect(first, log, 1);
  first.Commit(made);
  RunAll(first);
  void* const again = MakeProgramEffect(second, log, 2);
  second.Commit(again);
  EXPECT_EQ(again, made);
  RunAll(second);
  EXPECT_EQ(*log, (Log{1, 2}));
  EXPECT_EQ(log.use_count(), 1);
}

// An effect taken rather than committed is held by whoever takes it, which
// runs it as it chooses and ends it once it ends itself, whether or not it ran.
TEST(DeferredEffectsTest, EndsATakenEffectWithItsHolder) {
  auto log = std::make_shared<Log>();
  EffectStorage storage;
  DeferredEffects effects(storage);
  {
    auto ran = effects.Take(MakeProgramEffect(effects, log, 1));
    ran.Call();
    auto unrun = effects.Take(MakeProgramEffect(effects, log, 2));
    EXPECT_EQ(log.use_count(), 3);
  }
  EXPECT_TRUE(effects.Empty());
  EXPECT_EQ(*log, Log{1});
  EXPECT_EQ(log.use_count(), 1);
}

}  // namespace

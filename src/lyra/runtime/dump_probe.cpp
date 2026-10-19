#include "lyra/runtime/dump_probe.hpp"

#include <memory>

#include "lyra/runtime/named_event.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/value_dump.hpp"
#include "lyra/runtime/value_dumps.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/real.hpp"

namespace lyra::runtime {

namespace {

// A named event, recorded in a time step it was triggered in (LRM 21.7.2.2).
class EventProbe final : public DumpProbe {
 public:
  explicit EventProbe(NamedEvent& event) : event_(&event) {
  }

  void Read(DumpCapture& into, DumpCode code) const override {
    switch (event_->Triggered(current_runtime()).Truth()) {
      case value::Truthiness::kKnownNonzero:
        into.Triggered(code);
        return;
      case value::Truthiness::kKnownZero:
      case value::Truthiness::kUnknown:
        return;
    }
  }

  [[nodiscard]] auto Watched() const -> Trigger override {
    Trigger watched;
    watched.observable = event_;
    return watched;
  }

  [[nodiscard]] auto Identity() const -> const void* override {
    return event_;
  }

  [[nodiscard]] auto Member() const -> const void* override {
    return event_;
  }

 private:
  NamedEvent* event_;
};

}  // namespace

auto Describing(RuntimeEffects& runtime) -> Describer& {
  return runtime.Dumps().Describing();
}

void DescribeTo(
    NamedEvent& event, RuntimeEffects& runtime, const DumpEntry* entry) {
  Describing(runtime).Var(*entry, std::make_unique<EventProbe>(event));
}

void BeginDescribedBlock(RuntimeEffects& runtime, const DumpBlock* block) {
  Describing(runtime).BeginBlock(*block);
}

void EndDescribedBlock(RuntimeEffects& runtime) {
  Describing(runtime).EndBlock();
}

void DescribeTo(
    Var<value::Real>& cell, RuntimeEffects& runtime, const DumpEntry* entry) {
  Describing(runtime).Var(
      *entry,
      std::make_unique<dump_detail::VarProbe<value::Real>>(cell, entry->width));
}

void DescribeTo(
    Var<value::ShortReal>& cell, RuntimeEffects& runtime,
    const DumpEntry* entry) {
  Describing(runtime).Var(
      *entry, std::make_unique<dump_detail::VarProbe<value::ShortReal>>(
                  cell, entry->width));
}

void DescribeTo(
    VariableRef<value::Real>& port, RuntimeEffects& runtime,
    const DumpEntry* entry) {
  DescribeReference<value::Real>(port.Erased(), runtime, entry);
}

void DescribeTo(
    VariableRef<value::ShortReal>& port, RuntimeEffects& runtime,
    const DumpEntry* entry) {
  DescribeReference<value::ShortReal>(port.Erased(), runtime, entry);
}

}  // namespace lyra::runtime

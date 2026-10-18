#include "lyra/runtime/closure.hpp"

#include "lyra/support/callable_bounds.hpp"

namespace lyra::runtime {

void EndClosure::operator()(ClosureValue* closure) const noexcept {
  closure->End();
  ::operator delete(closure);
}

ClosureValue::ClosureValue(const ClosureCommonEntries* definition)
    : definition_(definition) {
}

void ClosureValue::End() {
  definition_->end(this);
}

template ClosureValue::ClosureValue(
    const ClosureDefinition<support::EffectSignature>* definition);
template ClosureValue::ClosureValue(
    const ClosureDefinition<support::ConditionSignature>* definition);
template ClosureValue::ClosureValue(
    const ClosureDefinition<support::WatchSignature>* definition);
template ClosureValue::ClosureValue(
    const ClosureDefinition<support::OrderSignature>* definition);
template class OwnedClosure<support::EffectSignature>;
template class OwnedClosure<support::ConditionSignature>;
template class OwnedClosure<support::WatchSignature>;

}  // namespace lyra::runtime

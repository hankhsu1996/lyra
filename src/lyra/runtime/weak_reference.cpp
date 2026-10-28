#include "lyra/runtime/weak_reference.hpp"

#include <bit>
#include <cstdint>
#include <memory>
#include <utility>

#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/read_report.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/managed_ref.hpp"

namespace lyra::runtime {

auto Downgrade(const value::ObjectRef& strong) -> WeakObjectRef {
  return WeakObjectRef(strong.Handle().Share());
}

auto Upgrade(const WeakObjectRef& weak, ReadReport* report)
    -> value::ObjectRef {
  std::shared_ptr<void> share = weak.Share().lock();
  if (share == nullptr) return value::ObjectRef{};
  if (report != nullptr) {
    report->AddExpiry(static_cast<GcObject*>(share.get())->Expiry(), share);
  }
  return value::ObjectRef(value::ManagedRef(std::move(share)));
}

auto WeakRefersTo(const WeakObjectRef& weak, const value::ObjectRef& strong)
    -> bool {
  const std::shared_ptr<void>& share = strong.Handle().Share();
  return !weak.Share().owner_before(share) && !share.owner_before(weak.Share());
}

// The address the handle shares is the object's own, which its equality
// compares (LRM 11.4.5), so two objects that exist at once never share it.
auto ObjectIdentity(const value::ObjectRef& object) -> value::LongInt {
  return value::LongInt::FromInt(
      std::bit_cast<std::int64_t>(object.Handle().Share().get()));
}

}  // namespace lyra::runtime

#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <new>
#include <span>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/class_definition.hpp"
#include "lyra/runtime/member_storage.hpp"
#include "lyra/runtime/object_ref.hpp"

namespace lyra::runtime {

// A value of a class whose storage the runtime owns: which class it is of, and
// one slot per property its lineage carries.
//
// The slots follow the value in its own allocation, in lineage order and all
// one size, so a property sits at a fixed distance from the value's address and
// generated code reaches it there without asking. That distance depends on the
// size of the kind of value ahead of the slots -- an object or a scope -- which
// is why each kind hands its own size to this base when it is built.
//
// A target that lays its own values out states where its properties sit through
// the entries its class carries, so a class it builds has no slots and the
// value takes no room after itself.
class ClassValue : public GcObject {
 public:
  ClassValue(const ClassValue&) = delete;
  auto operator=(const ClassValue&) -> ClassValue& = delete;
  ClassValue(ClassValue&&) = delete;
  auto operator=(ClassValue&&) -> ClassValue& = delete;
  // Defined in this class's own source file, because a class whose virtual
  // functions are all written in a header is emitted into every translation
  // unit that builds one.
  ~ClassValue() override;

  // A value of kind `Holder`, allocated with room for its class's slots right
  // after it.
  template <typename Holder, typename... Args>
  [[nodiscard]] static auto Make(
      const ObjectDefinition* definition, Args&&... args)
      -> std::unique_ptr<Holder> {
    void* storage =
        ::operator new(MembersAt(sizeof(Holder)) + MemberBytes(definition));
    std::unique_ptr<Holder> made(::new (storage)
                                     Holder(std::forward<Args>(args)...));
    // The slots are placed from this base's address and generated code
    // reaches them from the value's, so the two have to be one address.
    if (static_cast<void*>(static_cast<ClassValue*>(made.get())) != storage) {
      throw InternalError("ClassValue: a kind of value does not begin with it");
    }
    return made;
  }

  // Where the slots of a value of a kind `size` bytes long begin: at the first
  // slot boundary after it.
  static constexpr auto MembersAt(std::size_t size) -> std::size_t {
    constexpr std::size_t kAlign = alignof(MemberStorage);
    return (size + kAlign - 1) / kAlign * kAlign;
  }

  // Ending a value returns the whole allocation it was made in, whose size is
  // not the value's own, so the release takes the address alone.
  static void operator delete(void* address);

  // Where the property `declared_by` gave `slot` to lives on this value. The
  // pair is the whole coordinate: a class extending another carries its base's
  // properties as well as its own and may declare one of the same name, so
  // which storage is meant is fixed by the class the access names (LRM 8.14)
  // rather than by what this value is.
  [[nodiscard]] auto Member(
      const ObjectDefinition* declared_by, std::uint32_t slot) -> void*;

  // The body this value's class answers `introduced_by`'s `ordinal`-th behavior
  // with (LRM 8.20). What class it is, is this side's to answer; entering the
  // body is the asking code's own, which is why this answers with the address.
  [[nodiscard]] auto Method(
      const ObjectDefinition* introduced_by, std::uint32_t ordinal) const
      -> ErasedMethodEntry {
    return BodyOf(Class(), introduced_by, ordinal);
  }

 protected:
  // `holder_size` is the size of the kind of value this is; its slots follow.
  ClassValue(const ObjectDefinition* definition, std::size_t holder_size);

 private:
  static auto MemberBytes(const ObjectDefinition* definition) -> std::size_t;

  std::span<MemberStorage> members_;
};

}  // namespace lyra::runtime

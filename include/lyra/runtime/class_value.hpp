#pragma once

#include <cstdint>

#include "lyra/runtime/class_definition.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/storage_block.hpp"

namespace lyra::runtime {

// A value of a class whose storage the runtime owns: one storage object per
// property, laid out from the schema the class was realized with. Those two --
// which class it is of, and where that class's properties live on it -- are the
// whole of what an operation performed through the class needs, so an operation
// asked of the class is asked the same way whatever kind of value holds it, and
// one entry serves them all.
//
// Which kind it is is deliberately not here. A value the program built with
// `new` and an instance standing in the design hierarchy differ in the
// lifecycle they join and the reference that reaches them, and those sit on the
// kinds themselves; neither is a fact an operation through the class consults.
//
// A target that lays its own values out is not one of these: it states where
// its properties sit through the entries its class carries, and holds no
// storage the runtime can address. So the schema here is empty for such a
// class, exactly as the class's own flat schema is.
class ClassValue : public GcObject {
 public:
  explicit ClassValue(const ObjectDefinition* definition)
      : members_(RequireDefinition(definition)->members) {
    AdoptClass(definition);
    // A value the runtime lays out is at the address of the base every entry
    // reaches it through, which is what lets one be recovered from that base
    // whether or not an allocation produced it -- an instance standing in the
    // design hierarchy is asked the same questions and no allocation made it.
    AdoptIdentity(this);
  }

  ClassValue(const ClassValue&) = delete;
  auto operator=(const ClassValue&) -> ClassValue& = delete;
  ClassValue(ClassValue&&) = delete;
  auto operator=(ClassValue&&) -> ClassValue& = delete;
  // Defined in this class's own source file, because a class whose virtual
  // functions are all written in a header is emitted into every translation
  // unit that builds one.
  ~ClassValue() override;

  // Where the property `declared_by` gave `slot` to lives on this value, which
  // is what a place naming it resolves to. The pair is the whole coordinate: a
  // class extending another carries its base's properties as well as its own
  // and may declare one of the same name, so which storage is meant is fixed by
  // the class the access names (LRM 8.14) rather than by what this value is.
  [[nodiscard]] auto Member(
      const ObjectDefinition* declared_by, std::uint32_t slot) -> void* {
    return members_.Address(
        RequireDefinition(declared_by)->first_member + slot);
  }

  // The body this value's class answers `introduced_by`'s `ordinal`-th behavior
  // with (LRM 8.20). What class it is, is this side's to answer; entering the
  // body is the asking code's own, which is why this answers with the address.
  [[nodiscard]] auto Method(
      const ObjectDefinition* introduced_by, std::uint32_t ordinal) const
      -> ErasedMethodEntry {
    return BodyOf(Class(), introduced_by, ordinal);
  }

 private:
  StorageBlock members_;
};

}  // namespace lyra::runtime

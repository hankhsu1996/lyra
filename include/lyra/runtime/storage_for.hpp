#pragma once

#include "lyra/runtime/laid_out_storage.hpp"
#include "lyra/runtime/value_storage_core.hpp"
#include "lyra/runtime/words_storage.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/wide.hpp"

namespace lyra::runtime {

// The storage a holder keeps a value of `T` in: the value itself, for a type
// known where it is compiled, and at the holder's tail the words of an
// integral value wider than a word, or a tuple or a fixed-size unpacked array
// laid out by its type, whose width or type the library is told where it
// builds the holder.
template <class T>
struct StorageFor {
  using Type = ValueStorageCore<T>;
};
template <value::HeldAsWords T>
struct StorageFor<T> {
  using Type = WordsStorage<T::kDomain>;
};
template <value::LaidOutByType T>
struct StorageFor<T> {
  using Type = LaidOutStorage<T>;
};

// A value whose size is known only from what the holder is told of it -- its
// width, or its type -- as a dynamically sized type's is from its metadata,
// and which a holder therefore keeps at its tail.
template <class T>
concept DynamicallySized = value::HeldAsWords<T> || value::LaidOutByType<T>;

}  // namespace lyra::runtime

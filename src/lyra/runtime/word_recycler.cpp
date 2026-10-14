#include "lyra/runtime/word_recycler.hpp"

#include <array>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <memory>
#include <type_traits>

namespace lyra::runtime {

namespace {

// How many freed buffers of one length a bucket keeps, and how many buckets
// there are. An expression holds a few values of one type at once, so a few
// buffers serve every value it makes. A length has the one bucket its word
// count names, so finding it costs the same however many lengths a design has;
// two lengths naming one bucket share it by turns, the bucket going to the
// other whenever it is empty. A buffer freed past either bound goes back to
// the system, so what is kept beyond what is in use is bounded by these two
// numbers and the design's own widths.
constexpr std::size_t kMaxFreePerBucket = 8;
constexpr std::size_t kBucketCount = 61;

// The buffers freed on this thread and not yet handed out again, as free lists
// by length. A kept buffer holds the address of the next one of its length in
// its first word, so keeping one allocates nothing. It ends without giving its
// buffers back: a holder that lives as long as the program frees its buffer
// after every object of the thread has ended, and has to find this still
// standing.
class WordRecycler {
 public:
  [[nodiscard]] auto Pop(std::size_t count) noexcept -> std::uint64_t* {
    Bucket& bucket = BucketFor(count);
    if (bucket.count != count || bucket.first == nullptr) {
      return nullptr;
    }
    std::uint64_t* words = bucket.first;
    bucket.first = std::bit_cast<std::uint64_t*>(*words);
    --bucket.free;
    return words;
  }

  // Answers whether the buffer was kept.
  [[nodiscard]] auto Push(std::uint64_t* words, std::size_t count) noexcept
      -> bool {
    Bucket& bucket = BucketFor(count);
    if (bucket.free == 0) {
      bucket.count = count;
    }
    if (bucket.count != count || bucket.free >= kMaxFreePerBucket) {
      return false;
    }
    *words = std::bit_cast<std::uint64_t>(bucket.first);
    bucket.first = words;
    ++bucket.free;
    return true;
  }

 private:
  struct Bucket {
    std::size_t count = 0;
    std::uint64_t* first = nullptr;
    std::size_t free = 0;
  };

  [[nodiscard]] auto BucketFor(std::size_t count) noexcept -> Bucket& {
    return *std::next(
        buckets_.begin(), static_cast<std::ptrdiff_t>(count % kBucketCount));
  }

  std::array<Bucket, kBucketCount> buckets_{};
};

static_assert(std::is_trivially_destructible_v<WordRecycler>);

auto ThreadRecycler() noexcept -> WordRecycler& {
  thread_local WordRecycler kept;
  return kept;
}

}  // namespace

auto AllocateWords(std::size_t count) -> std::uint64_t* {
  if (std::uint64_t* kept = ThreadRecycler().Pop(count)) {
    return kept;
  }
  return std::allocator<std::uint64_t>{}.allocate(count);
}

void DeallocateWords(std::uint64_t* words, std::size_t count) noexcept {
  if (words == nullptr || ThreadRecycler().Push(words, count)) {
    return;
  }
  std::allocator<std::uint64_t>{}.deallocate(words, count);
}

}  // namespace lyra::runtime

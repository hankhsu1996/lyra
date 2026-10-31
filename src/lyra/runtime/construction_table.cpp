#include "lyra/runtime/construction_table.hpp"

#include <cstddef>
#include <cstdint>

namespace lyra::runtime {

namespace {

auto BlockAt(
    const abi::ConstructionTable* table, std::int64_t generate,
    std::int64_t place) -> const abi::Instantiation& {
  const auto places =
      table->generates.Entries()[static_cast<std::size_t>(generate)]
          .places.Entries();
  return places[places.size() == 1 ? 0 : static_cast<std::size_t>(place)];
}

}  // namespace

auto InstantiationEntry(const abi::ConstructionTable* table, std::int64_t at)
    -> abi::ErasedEntry {
  return table->instantiations.Entries()[static_cast<std::size_t>(at)].entry;
}

auto InstantiationTable(const abi::ConstructionTable* table, std::int64_t at)
    -> const abi::ConstructionTable* {
  return table->instantiations.Entries()[static_cast<std::size_t>(at)].table;
}

auto BlockEntry(
    const abi::ConstructionTable* table, std::int64_t generate,
    std::int64_t place) -> abi::ErasedEntry {
  return BlockAt(table, generate, place).entry;
}

auto BlockTable(
    const abi::ConstructionTable* table, std::int64_t generate,
    std::int64_t place) -> const abi::ConstructionTable* {
  return BlockAt(table, generate, place).table;
}

}  // namespace lyra::runtime

#pragma once

#include <cstdint>

#include "lyra/abi/records.hpp"

namespace lyra::runtime {

// The entry and the table of the object at `at` among the instantiations of
// `table`.
[[nodiscard]] auto InstantiationEntry(
    const abi::ConstructionTable* table, std::int64_t at) -> abi::ErasedEntry;
[[nodiscard]] auto InstantiationTable(
    const abi::ConstructionTable* table, std::int64_t at)
    -> const abi::ConstructionTable*;

// The entry and the table of the block at place `place` of the generate at
// `generate` in `table`.
[[nodiscard]] auto BlockEntry(
    const abi::ConstructionTable* table, std::int64_t generate,
    std::int64_t place) -> abi::ErasedEntry;
[[nodiscard]] auto BlockTable(
    const abi::ConstructionTable* table, std::int64_t generate,
    std::int64_t place) -> const abi::ConstructionTable*;

}  // namespace lyra::runtime

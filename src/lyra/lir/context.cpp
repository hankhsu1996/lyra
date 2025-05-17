#include "lyra/lir/context.hpp"

namespace lyra::lir {

auto LirContext::AllocateTemp(std::string name, common::Type type) -> TempRef {
  temp_storage_.push_back(TempSymbol{.name = std::move(name), .type = type});
  return TempRef{.ptr = &temp_storage_.back()};
}

auto LirContext::InternLabel(std::string_view name) -> LabelRef {
  auto [it, _] = label_pool_.emplace(name);
  return LabelRef{.ptr = &*it};
}

auto LirContext::InternLiteral(const common::Literal& literal) -> LiteralRef {
  literal_storage_.push_back(literal);
  const auto* ptr = &literal_storage_.back();

  auto [it, inserted] = literal_set_.emplace(ptr);
  if (!inserted) {
    literal_storage_.pop_back();
    ptr = *it;
  }

  return LiteralRef{.ptr = ptr};
}

auto LirContext::LiteralPtrHash::operator()(const common::Literal* ptr) const
    -> std::size_t {
  return ptr->Hash();
}

auto LirContext::LiteralPtrEqual::operator()(
    const common::Literal* a, const common::Literal* b) const -> bool {
  return *a == *b;
}

}  // namespace lyra::lir

#include "lyra/lir/context.hpp"

#include <cstddef>
#include <string>
#include <string_view>
#include <utility>

#include "lyra/common/constant.hpp"
#include "lyra/common/type.hpp"

namespace lyra::lir {

auto LirContext::AllocateTempWithId(
    TempId id, std::string name, common::Type type) -> TempRef {
  // Store metadata in temp_storage_ (for TempRef.ptr), but use provided ID
  temp_storage_.push_back(TempSymbol{.name = std::move(name), .type = type});
  return TempRef{.id = id, .ptr = &temp_storage_.back()};
}

auto LirContext::InternLabel(std::string_view name) -> LabelRef {
  auto it = label_index_.find(std::string(name));
  if (it != label_index_.end()) {
    return LabelRef{.id = it->second, .ptr = &label_storage_[it->second]};
  }
  auto id = static_cast<LabelId>(label_storage_.size());
  label_storage_.emplace_back(name);
  label_index_.emplace(label_storage_.back(), id);
  return LabelRef{.id = id, .ptr = &label_storage_.back()};
}

auto LirContext::InternConstant(const common::Constant& constant)
    -> ConstantRef {
  constant_storage_.push_back(constant);
  const auto* ptr = &constant_storage_.back();

  auto [it, inserted] = constant_set_.emplace(ptr);
  if (!inserted) {
    constant_storage_.pop_back();
    ptr = *it;
  }

  return ConstantRef{.ptr = ptr};
}

auto LirContext::InternType(const common::Type& type) -> const common::Type* {
  type_storage_.push_back(type);
  return &type_storage_.back();
}

auto LirContext::ConstantPtrHash::operator()(const common::Constant* ptr) const
    -> std::size_t {
  return ptr->Hash();
}

auto LirContext::ConstantPtrEqual::operator()(
    const common::Constant* a, const common::Constant* b) const -> bool {
  return *a == *b;
}

}  // namespace lyra::lir

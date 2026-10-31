#include <cstdint>
#include <fstapi.h>
#include <limits>
#include <memory>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/dump_writer.hpp"
#include "lyra/support/dump_kind.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

namespace {

auto FstVarType(support::DumpVarKind kind) -> fstVarType {
  switch (kind) {
    case support::DumpVarKind::kEvent:
      return FST_VT_VCD_EVENT;
    case support::DumpVarKind::kInteger:
      return FST_VT_VCD_INTEGER;
    case support::DumpVarKind::kReal:
      return FST_VT_VCD_REAL;
    case support::DumpVarKind::kRealtime:
      return FST_VT_VCD_REALTIME;
    case support::DumpVarKind::kReg:
      return FST_VT_VCD_REG;
    case support::DumpVarKind::kSupply0:
      return FST_VT_VCD_SUPPLY0;
    case support::DumpVarKind::kSupply1:
      return FST_VT_VCD_SUPPLY1;
    case support::DumpVarKind::kTime:
      return FST_VT_VCD_TIME;
    case support::DumpVarKind::kTri:
      return FST_VT_VCD_TRI;
    case support::DumpVarKind::kTriand:
      return FST_VT_VCD_TRIAND;
    case support::DumpVarKind::kTrior:
      return FST_VT_VCD_TRIOR;
    case support::DumpVarKind::kTrireg:
      return FST_VT_VCD_TRIREG;
    case support::DumpVarKind::kTri0:
      return FST_VT_VCD_TRI0;
    case support::DumpVarKind::kTri1:
      return FST_VT_VCD_TRI1;
    case support::DumpVarKind::kWand:
      return FST_VT_VCD_WAND;
    case support::DumpVarKind::kWire:
      return FST_VT_VCD_WIRE;
    case support::DumpVarKind::kWor:
      return FST_VT_VCD_WOR;
  }
  throw InternalError("FstVarType: unknown kind");
}

auto FstScopeType(support::DumpScopeKind kind) -> fstScopeType {
  switch (kind) {
    case support::DumpScopeKind::kModule:
      return FST_ST_VCD_MODULE;
    case support::DumpScopeKind::kTask:
      return FST_ST_VCD_TASK;
    case support::DumpScopeKind::kFunction:
      return FST_ST_VCD_FUNCTION;
    case support::DumpScopeKind::kBegin:
      return FST_ST_VCD_BEGIN;
    case support::DumpScopeKind::kFork:
      return FST_ST_VCD_FORK;
  }
  throw InternalError("FstScopeType: unknown kind");
}

// GTKWave's FST format: each thing's changes kept as a chain of its own,
// compressed a block at a time, the dump's off periods kept as intervals.
class FstWriter final : public DumpWriter {
 public:
  explicit FstWriter(fstWriterContext* context) : context_(context) {
    fstWriterSetPackType(context_, FST_WR_PT_LZ4);
    fstWriterSetParallelMode(context_, 1);
    fstWriterSetVersion(context_, "Lyra");
  }
  FstWriter(const FstWriter&) = delete;
  auto operator=(const FstWriter&) -> FstWriter& = delete;
  FstWriter(FstWriter&&) = delete;
  auto operator=(FstWriter&&) -> FstWriter& = delete;
  ~FstWriter() override {
    if (context_ != nullptr) {
      fstWriterClose(context_);
    }
  }

  void Timescale(std::int8_t precision_power) override {
    fstWriterSetTimescale(context_, precision_power);
  }

  void Scope(support::DumpScopeKind kind, std::string_view name) override {
    const std::string owned(name);
    fstWriterSetScope(context_, FstScopeType(kind), owned.c_str(), nullptr);
  }

  void Upscope() override {
    fstWriterSetUpscope(context_);
  }

  void Var(DumpCode code, const abi::DumpEntry& entry) override {
    if (things_.size() <= code.value) {
      things_.resize(code.value + 1);
    }
    things_[code.value] = {
        .handle = Create(entry, 0),
        .width = entry.width,
        .form = support::DumpValueFormOf(entry.Kind())};
  }

  void Alias(DumpCode code, const abi::DumpEntry& entry) override {
    Create(entry, things_.at(code.value).handle);
  }

  void EndDefinitions() override {
  }

  void Time(SimTime time) override {
    fstWriterEmitTimeChange(context_, time);
  }

  void Change(DumpCode code, const DumpValue& value) override {
    const Thing& thing = things_.at(code.value);
    std::visit(
        Overloaded{
            [&](const value::ConstIntegralView& bits) {
              EmitBits(thing.handle, bits);
            },
            [&](double real) {
              fstWriterEmitValueChange(context_, thing.handle, &real);
            },
            [&](const DumpTriggered&) {
              fstWriterEmitValueChange(context_, thing.handle, "1");
            },
            [&](const DumpUnknown&) { EmitUnknown(thing); }},
        value);
  }

  // FST keeps a dump's off periods as intervals of their own, which a reader
  // shows blank, beside the unknown values the standard has the dump record.
  void BeginCheckpoint(DumpCheckpoint checkpoint) override {
    switch (checkpoint) {
      case DumpCheckpoint::kDumpvars:
      case DumpCheckpoint::kDumpall:
        return;
      case DumpCheckpoint::kDumpon:
        fstWriterEmitDumpActive(context_, 1);
        return;
      case DumpCheckpoint::kDumpoff:
        fstWriterEmitDumpActive(context_, 0);
        return;
    }
    throw InternalError("FstWriter: unknown checkpoint");
  }

  void EndCheckpoint() override {
  }

  void Comment(std::string_view text) override {
    const std::string owned(text);
    fstWriterSetComment(context_, owned.c_str());
  }

  void Flush() override {
    fstWriterFlushContext(context_);
  }

  void Close() override {
    fstWriterClose(context_);
    context_ = nullptr;
  }

  void LimitTo(std::uint64_t bytes) override {
    fstWriterSetDumpSizeLimit(context_, bytes);
  }

  [[nodiscard]] auto ReachedLimit() const -> bool override {
    return fstWriterGetDumpSizeLimitReached(context_) != 0;
  }

 private:
  // The name the FST hierarchy records is the reference a VCD `$var` writes.
  auto Create(const abi::DumpEntry& entry, fstHandle alias) -> fstHandle {
    const std::string name = DumpReference(entry);
    return fstWriterCreateVar(
        context_, FstVarType(entry.Kind()), FST_VD_IMPLICIT, entry.width,
        name.c_str(), alias);
  }

  // A dumped thing as FST takes its values: a real as the double itself, and
  // anything else as one character a position.
  struct Thing {
    fstHandle handle = 0;
    std::uint32_t width = 0;
    support::DumpValueForm form = support::DumpValueForm::kBits;
  };

  // A double has no unknown value, so a real is recorded as not a number,
  // which a reader shows as no value at all.
  void EmitUnknown(const Thing& thing) {
    switch (thing.form) {
      case support::DumpValueForm::kReal: {
        const double unknown = std::numeric_limits<double>::quiet_NaN();
        fstWriterEmitValueChange(context_, thing.handle, &unknown);
        return;
      }
      case support::DumpValueForm::kBits:
      case support::DumpValueForm::kTriggered:
        spelled_.assign(thing.width, 'x');
        fstWriterEmitValueChange(context_, thing.handle, spelled_.data());
        return;
    }
    throw InternalError("FstWriter: unknown value");
  }

  // FST takes a vector as its positions from the most significant, one
  // character each, which it packs eight to a byte where none is x or z.
  void EmitBits(fstHandle handle, const value::ConstIntegralView& bits) {
    spelled_.clear();
    for (std::uint64_t position = bits.width; position-- > 0;) {
      spelled_.push_back(DumpBitCharacter(value::ScalarAt(bits, position)));
    }
    fstWriterEmitValueChange(context_, handle, spelled_.data());
  }

  fstWriterContext* context_;
  std::vector<Thing> things_;
  std::string spelled_;
};

}  // namespace

auto OpenFstWriter(const std::string& path) -> std::unique_ptr<DumpWriter> {
  fstWriterContext* context = fstWriterCreate(path.c_str(), 1);
  if (context == nullptr) {
    throw SimulationError("the dump file '{}' could not be opened", path);
  }
  return std::make_unique<FstWriter>(context);
}

}  // namespace lyra::runtime

#include <cstdint>
#include <fstapi.h>
#include <limits>
#include <memory>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/time.hpp"
#include "lyra/base/version.hpp"
#include "lyra/runtime/dump_kind.hpp"
#include "lyra/runtime/dump_writer.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

namespace {

auto FstVarType(DumpVarKind kind) -> fstVarType {
  switch (kind) {
    case DumpVarKind::kEvent:
      return FST_VT_VCD_EVENT;
    case DumpVarKind::kInteger:
      return FST_VT_VCD_INTEGER;
    case DumpVarKind::kReal:
      return FST_VT_VCD_REAL;
    case DumpVarKind::kRealtime:
      return FST_VT_VCD_REALTIME;
    case DumpVarKind::kReg:
      return FST_VT_VCD_REG;
    case DumpVarKind::kSupply0:
      return FST_VT_VCD_SUPPLY0;
    case DumpVarKind::kSupply1:
      return FST_VT_VCD_SUPPLY1;
    case DumpVarKind::kTime:
      return FST_VT_VCD_TIME;
    case DumpVarKind::kTri:
      return FST_VT_VCD_TRI;
    case DumpVarKind::kTriand:
      return FST_VT_VCD_TRIAND;
    case DumpVarKind::kTrior:
      return FST_VT_VCD_TRIOR;
    case DumpVarKind::kTrireg:
      return FST_VT_VCD_TRIREG;
    case DumpVarKind::kTri0:
      return FST_VT_VCD_TRI0;
    case DumpVarKind::kTri1:
      return FST_VT_VCD_TRI1;
    case DumpVarKind::kWand:
      return FST_VT_VCD_WAND;
    case DumpVarKind::kWire:
      return FST_VT_VCD_WIRE;
    case DumpVarKind::kWor:
      return FST_VT_VCD_WOR;
  }
  throw InternalError("FstVarType: unknown kind");
}

auto FstScopeType(DumpScopeKind kind) -> fstScopeType {
  switch (kind) {
    case DumpScopeKind::kModule:
      return FST_ST_VCD_MODULE;
    case DumpScopeKind::kTask:
      return FST_ST_VCD_TASK;
    case DumpScopeKind::kFunction:
      return FST_ST_VCD_FUNCTION;
    case DumpScopeKind::kBegin:
      return FST_ST_VCD_BEGIN;
    case DumpScopeKind::kFork:
      return FST_ST_VCD_FORK;
    case DumpScopeKind::kPackage:
      return FST_ST_VCD_PACKAGE;
  }
  throw InternalError("FstScopeType: unknown kind");
}

// GTKWave's FST format: each thing's changes kept as a chain of its own,
// compressed a block at a time, the dump's off periods kept as intervals.
class FstWriter final : public DumpWriter {
 public:
  explicit FstWriter(fstWriterContext* context) : context_(context) {
    fstWriterSetPackType(context_, FST_WR_PT_LZ4);
    const std::string version(base::kProductName);
    fstWriterSetVersion(context_, version.c_str());
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

  void Scope(DumpScopeKind kind, std::string_view name) override {
    const std::string owned(name);
    fstWriterSetScope(context_, FstScopeType(kind), owned.c_str(), nullptr);
  }

  void Upscope() override {
    fstWriterSetUpscope(context_);
  }

  void Var(DumpCode code, const DumpVarDeclaration& variable) override {
    if (things_.size() <= code.value) {
      things_.resize(code.value + 1);
    }
    things_[code.value] = {
        .handle = Create(variable),
        .width = variable.width,
        .form = DumpValueFormOf(variable.kind)};
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

  // The file's ending writes most of what makes it readable, so a refusal is
  // often first met here, and the writer is gone once it is.
  void Close() override {
    closed_with_ = RefusalOf(fstWriterClose2(std::exchange(context_, nullptr)));
  }

  void LimitTo(std::uint64_t bytes) override {
    fstWriterSetDumpSizeLimit(context_, bytes);
  }

  [[nodiscard]] auto ReachedLimit() const -> bool override {
    return fstWriterGetDumpSizeLimitReached(context_) != 0;
  }

  [[nodiscard]] auto Refused() const -> std::error_code override {
    return context_ != nullptr ? RefusalOf(fstWriterGetWriteError(context_))
                               : closed_with_;
  }

 private:
  // libfst answers a refusal as the system's error number, and none as 0.
  static auto RefusalOf(int error_number) -> std::error_code {
    if (error_number == 0) {
      return {};
    }
    return {error_number, std::system_category()};
  }

  // The name the FST hierarchy records is the reference a VCD `$var` writes.
  // Each thing is a variable of its own, aliasing none.
  auto Create(const DumpVarDeclaration& variable) -> fstHandle {
    const std::string name = DumpReference(variable);
    return fstWriterCreateVar(
        context_, FstVarType(variable.kind), FST_VD_IMPLICIT, variable.width,
        name.c_str(), 0);
  }

  // A dumped thing as FST takes its values: a real as the double itself, and
  // anything else as one character a position.
  struct Thing {
    fstHandle handle = 0;
    std::uint32_t width = 0;
    DumpValueForm form = DumpValueForm::kBits;
  };

  // A double has no unknown value, so a real is recorded as not a number,
  // which a reader shows as no value at all.
  void EmitUnknown(const Thing& thing) {
    switch (thing.form) {
      case DumpValueForm::kReal: {
        const double unknown = std::numeric_limits<double>::quiet_NaN();
        fstWriterEmitValueChange(context_, thing.handle, &unknown);
        return;
      }
      case DumpValueForm::kBits:
      case DumpValueForm::kTriggered:
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
  std::error_code closed_with_;
  std::vector<Thing> things_;
  std::string spelled_;
};

}  // namespace

auto OpenFstWriter(const std::string& path) -> std::unique_ptr<DumpWriter> {
  fstWriterContext* context = fstWriterCreate(path.c_str(), 1);
  if (context == nullptr) {
    return nullptr;
  }
  return std::make_unique<FstWriter>(context);
}

}  // namespace lyra::runtime

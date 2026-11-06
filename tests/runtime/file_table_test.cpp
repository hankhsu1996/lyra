#include "lyra/runtime/file_table.hpp"

#include <algorithm>
#include <cerrno>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <gtest/gtest.h>
#include <string>
#include <string_view>
#include <sys/stat.h>
#include <system_error>
#include <unistd.h>
#include <vector>

#include "lyra/runtime/file.hpp"
#include "lyra/value/string.hpp"

namespace {

using lyra::runtime::File;
using lyra::runtime::FileTable;
using lyra::value::String;

// What `$ferror` answers for `fd`: the code and its description.
struct Answer {
  std::int64_t code;
  std::string text;
};

auto ErrorOf(FileTable& files, std::int64_t fd) -> Answer {
  const auto answer = files.Error(fd);
  return Answer{
      .code = answer.Component<0>().ToInt64(),
      .text = std::string(answer.Component<1>().View())};
}

auto Opened(FileTable& files, std::string_view name, std::string_view mode)
    -> std::int64_t {
  return files
      .OpenWithMode(String{std::string(name)}, String{std::string(mode)})
      .ToInt64();
}

// More than a file holds before it hands what it was written to its
// descriptor.
auto MoreThanABuffer() -> String {
  return String{std::string(std::size_t{1} << 17, 'a')};
}

// A device that refuses every write with ENOSPC stands for any descriptor the
// system refuses, so each way a write reaches it is one an operation reports.
TEST(FileTableTest, AWriteTheSystemRefusesIsReportedByTheOperationThatMetIt) {
  FileTable files;
  const std::int64_t fd = Opened(files, "/dev/full", "w");
  ASSERT_NE(fd, 0);

  files.Writeln(fd, String{"held, not yet handed over"});
  EXPECT_EQ(ErrorOf(files, fd).code, 0);

  files.Flush(fd);
  const Answer flushed = ErrorOf(files, fd);
  EXPECT_EQ(flushed.code, ENOSPC);
  EXPECT_FALSE(flushed.text.empty());
  // Asking is no operation, so asking again answers the same.
  EXPECT_EQ(ErrorOf(files, fd).code, ENOSPC);

  files.Write(fd, MoreThanABuffer());
  EXPECT_EQ(ErrorOf(files, fd).code, ENOSPC);

  files.Write(fd, String{"x"});
  files.FlushAll();
  EXPECT_EQ(ErrorOf(files, fd).code, ENOSPC);

  files.Write(fd, String{"x"});
  files.Close(fd);
  EXPECT_EQ(ErrorOf(files, fd).code, ENOSPC);

  // The next file opened takes the closed one's place, and its opening met
  // nothing.
  const std::string path = ::testing::TempDir() + "/file_table_test.txt";
  EXPECT_EQ(Opened(files, path, "w"), fd);
  EXPECT_EQ(ErrorOf(files, fd).code, 0);
  EXPECT_TRUE(ErrorOf(files, fd).text.empty());
}

// The most recent operation is what is answered: a failure is not kept past an
// operation that met none, and an operation on another descriptor changes
// nothing.
TEST(FileTableTest, TheMostRecentOperationOnADescriptorIsAnswered) {
  FileTable files;
  const std::string path = ::testing::TempDir() + "/file_table_recent.txt";
  const std::int64_t writer = Opened(files, path, "w");
  ASSERT_NE(writer, 0);
  files.Write(writer, String{"abc"});
  files.Close(writer);

  const std::int64_t reader = Opened(files, path, "r");
  ASSERT_NE(reader, 0);
  files.Write(reader, String{"not writable"});
  EXPECT_EQ(ErrorOf(files, reader).code, EBADF);

  const std::int64_t other = Opened(files, path, "r");
  ASSERT_NE(other, 0);
  EXPECT_EQ(files.Getc(other).ToInt64(), 'a');
  EXPECT_EQ(ErrorOf(files, reader).code, EBADF);

  EXPECT_EQ(files.Getc(reader).ToInt64(), 'a');
  EXPECT_EQ(ErrorOf(files, reader).code, 0);

  // A descriptor naming no open file is an operation that met EBADF.
  files.Close(reader);
  EXPECT_EQ(files.Getc(reader).ToInt64(), -1);
  EXPECT_EQ(ErrorOf(files, reader).code, EBADF);
}

// A read the system refuses is no end of the file: the read answers -1, the
// file is not at its end, and the refusal is what the operation met.
TEST(FileTableTest, AReadTheSystemRefusesIsNoEndOfFile) {
  FileTable files;
  const std::int64_t fd = Opened(files, ::testing::TempDir(), "r");
  ASSERT_NE(fd, 0);
  EXPECT_EQ(files.Getc(fd).ToInt64(), -1);
  EXPECT_EQ(files.Eof(fd).ToInt64(), 0);
  EXPECT_EQ(ErrorOf(files, fd).code, EISDIR);
}

// An opening that fails answers descriptor 0, and what it met is answered
// there.
TEST(FileTableTest, AFailedOpeningIsAnsweredUnderTheDescriptorItAnswers) {
  FileTable files;
  EXPECT_EQ(Opened(files, "no_such_directory/file.txt", "w"), 0);
  EXPECT_EQ(ErrorOf(files, 0).code, ENOENT);

  EXPECT_EQ(Opened(files, "no_such_directory/file.txt", "q"), 0);
  EXPECT_EQ(ErrorOf(files, 0).code, EINVAL);

  EXPECT_EQ(files.Open(String{"no_such_directory/file.txt"}).ToInt64(), 0);
  EXPECT_EQ(ErrorOf(files, 0).code, ENOENT);
}

// A hand-over no operation on the file caused -- before a report on the error
// stream, before foreign code runs -- keeps what it was refused until an
// operation takes it.
TEST(FileTableTest, AFileKeepsARefusalUntilAnOperationTakesIt) {
  const lyra::runtime::OpenedStream full(
      std::fopen("/dev/full", "w"), &std::fclose);
  ASSERT_NE(full, nullptr);
  File file(::fileno(full.get()), false, true, 64, nullptr);
  file.Write("refused when handed over");
  file.BeforeForeignCodeRuns();
  const File::Refusal refused = file.TakeRefusal();
  EXPECT_EQ(refused.code, std::error_code(ENOSPC, std::system_category()));
  EXPECT_TRUE(refused.lost_output);
  EXPECT_FALSE(file.TakeRefusal().code);
}

// What the run's end answers: each file whose written output was lost while
// the design was never told, once, open or already closed. A design is told
// when `$ferror` answers it an error; a later operation that met none, which
// `$ferror` then answers 0 for, tells it nothing.
TEST(FileTableTest, TheEndAnswersOutputLostThatTheDesignWasNotToldOf) {
  FileTable files;
  const std::error_code full(ENOSPC, std::system_category());

  // Each file is the same device spelled its own way, so what the end answers
  // says which file it is about.

  // Refused only as the end hands it over.
  const std::int64_t held = Opened(files, "/dev/full", "w");
  files.Write(held, String{"held"});

  // Refused during the run, twice, and never asked about.
  const std::int64_t ignored = Opened(files, "/dev//full", "w");
  files.Write(ignored, MoreThanABuffer());
  files.Write(ignored, MoreThanABuffer());

  // Refused, and the design asked and was answered.
  const std::int64_t asked = Opened(files, "/./dev/full", "w");
  files.Write(asked, MoreThanABuffer());
  EXPECT_EQ(ErrorOf(files, asked).code, ENOSPC);

  // Refused, then a flush of nothing, which met nothing, so the answer was 0.
  const std::int64_t overtaken = Opened(files, "/dev/./full", "w");
  files.Write(overtaken, MoreThanABuffer());
  files.Flush(overtaken);
  EXPECT_EQ(ErrorOf(files, overtaken).code, 0);

  // Refused, then an error of another kind, which tells the design nothing
  // of the loss.
  const std::int64_t other_error = Opened(files, "/./dev/./full", "w");
  files.Write(other_error, MoreThanABuffer());
  files.Flush(other_error);
  EXPECT_EQ(files.Getc(other_error).ToInt64(), -1);
  EXPECT_EQ(ErrorOf(files, other_error).code, EBADF);

  // Refused as it is closed; and the same, asked after the close, where the
  // second takes the first one's descriptor.
  const std::int64_t closed = Opened(files, "//dev/full", "w");
  files.Write(closed, String{"held"});
  files.Close(closed);
  const std::int64_t closed_and_asked = Opened(files, "/dev/../dev/full", "w");
  EXPECT_EQ(closed_and_asked, closed);
  files.Write(closed_and_asked, String{"held"});
  files.Close(closed_and_asked);
  EXPECT_EQ(ErrorOf(files, closed_and_asked).code, ENOSPC);

  std::vector<std::string> destinations;
  for (const lyra::runtime::LostOutput& one : files.Finish()) {
    EXPECT_EQ(one.refused, full) << one.destination;
    destinations.push_back(one.destination);
  }
  std::ranges::sort(destinations);
  EXPECT_EQ(
      destinations, (std::vector<std::string>{
                        "'/./dev/./full'", "'//dev/full'", "'/dev/./full'",
                        "'/dev//full'", "'/dev/full'"}));
  EXPECT_TRUE(files.Finish().empty());
}

// A descriptor that cannot be moved, a pipe, cannot be put back over what was
// read ahead of the design, and a write after the read is still taken, as C's
// stdio takes it.
TEST(FileTableTest, AWriteAfterAReadOnAPipeMeetsNothing) {
  const std::string path = ::testing::TempDir() + "/file_table_pipe";
  ASSERT_EQ(::mkfifo(path.c_str(), 0600), 0);
  FileTable files;
  const std::int64_t fd = Opened(files, path, "r+");
  ASSERT_NE(fd, 0);
  files.Write(fd, String{"ab"});
  EXPECT_EQ(files.Getc(fd).ToInt64(), 'a');
  files.Write(fd, String{"c"});
  EXPECT_EQ(ErrorOf(files, fd).code, 0);
  files.Close(fd);
  ::unlink(path.c_str());
}

}  // namespace

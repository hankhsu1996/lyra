/* Puts a pipe holding `text` on descriptor 0, so standard input is a stream
   that cannot seek, as a terminal or a shell pipe is. The text is short enough
   that the pipe holds all of it before anything reads. */
#include <stdio.h>
#include <string.h>
#include <svdpi.h>
#include <unistd.h>

int32_t feed_standard_input(const char* text) {
  int ends[2];
  if (pipe(ends) != 0) {
    return 0;
  }
  const size_t length = strlen(text);
  const ssize_t written = write(ends[1], text, length);
  close(ends[1]);
  if (written != (ssize_t)length || dup2(ends[0], 0) != 0) {
    close(ends[0]);
    return 0;
  }
  close(ends[0]);
  clearerr(stdin);
  return 1;
}

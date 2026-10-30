/* Each string handed back is read after a second call of the same export,
   which makes another string of the same length, so text released when the
   first call's entry returned would read as the second call's or as garbage
   rather than as itself. */
#include <stdlib.h>
#include <string.h>
#include <svdpi.h>

/* The exported subroutines called from here, declared as the design
   exports them (LRM 35.5.6, Annex H). */
const char* made_text(int32_t);
void fill_text(int32_t, const char**);
void grow_text(const char**);
int32_t measure_text(const char**);
int32_t fill_text_later(int32_t, const char**);

static int matches(const char* got, const char* want) {
  return got != NULL && strcmp(got, want) == 0;
}

int32_t drive(void) {
  int32_t verdict = 0;

  const char* first = made_text(1);
  const char* second = made_text(2);
  if (matches(
          first, "a returned string longer than a small buffer, number 1") &&
      matches(
          second, "a returned string longer than a small buffer, number 2")) {
    verdict |= 1;
  }

  const char* out_first = NULL;
  const char* out_second = NULL;
  fill_text(1, &out_first);
  fill_text(2, &out_second);
  if (matches(
          out_first, "an output string longer than a small buffer, number 1") &&
      matches(
          out_second,
          "an output string longer than a small buffer, number 2")) {
    verdict |= 2;
  }

  const char* grown = "a seed from C";
  grow_text(&grown);
  const char* grown_again = "another seed from C";
  grow_text(&grown_again);
  if (matches(grown, "a seed from C, and a tail longer than a small buffer")) {
    verdict |= 4;
  }
  if (matches(
          grown_again,
          "another seed from C, and a tail longer than a small buffer")) {
    verdict |= 8;
  }

  /* An inout the export leaves alone keeps this side's own pointer, which this
     side then frees as the storage it allocated. */
  char* mine = malloc(64);
  strcpy(mine, "a buffer this side allocated and owns");
  const char* kept = mine;
  if (measure_text(&kept) == 37 && kept == mine) {
    verdict |= 16;
  }
  free(mine);

  return verdict;
}

int drive_task(int32_t* verdict) {
  *verdict = 0;
  const char* first = NULL;
  const char* second = NULL;
  fill_text_later(1, &first);
  fill_text_later(2, &second);
  if (matches(first, "an output string of a task, after a delay, number 1")) {
    *verdict |= 1;
  }
  if (matches(second, "an output string of a task, after a delay, number 2")) {
    *verdict |= 2;
  }
  return 0;
}

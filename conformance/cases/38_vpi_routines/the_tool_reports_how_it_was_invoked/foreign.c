/* Reads how the tool was invoked through VPI (LRM 38.17). The structure starts
   holding what no answer leaves, so a routine that writes nothing is told apart
   from one that answers. An option file the tool read is an entry that is no
   string (38.17), and only plusargs are given here, so each entry is compared
   only where it begins with `+`. */
#include <stddef.h>
#include <string.h>
#include <vpi_user.h>

static int Info(s_vpi_vlog_info* info) {
  info->argc = -1;
  info->argv = NULL;
  info->product = NULL;
  info->version = NULL;
  return vpi_get_vlog_info(info);
}

int count_of_options(void) {
  s_vpi_vlog_info info;
  return Info(&info) == 1 ? info.argc : -1;
}

int option_is_given(const char* option) {
  s_vpi_vlog_info info;
  if (Info(&info) != 1) return -1;
  for (int i = 1; i < info.argc; ++i) {
    const char* given = info.argv[i];
    if (given != NULL && given[0] == '+' && strcmp(given, option) == 0) {
      return 1;
    }
  }
  return 0;
}

int first_option_is_named(void) {
  s_vpi_vlog_info info;
  if (Info(&info) != 1 || info.argc < 1) return -1;
  return info.argv[0] != NULL && info.argv[0][0] != '\0';
}

int tool_is_named(void) {
  s_vpi_vlog_info info;
  if (Info(&info) != 1) return -1;
  return info.product != NULL && info.product[0] != '\0' &&
         info.version != NULL;
}

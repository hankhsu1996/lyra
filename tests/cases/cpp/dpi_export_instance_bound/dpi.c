/* Declared here rather than via svdpi.h so the link input needs no extra include
   path; the runtime provides the definitions. */
typedef void* svScope;
extern svScope svGetScopeFromName(const char* name);
extern svScope svSetScope(svScope scope);

/* The exported SV function wrapper. A single symbol reaches whichever instance is
   the current DPI scope. */
extern int read_id(void);

/* Redirect the call-chain context to the named instance, then call the export so
   it runs against that instance (LRM 35.5.3). */
int read_at(const char* path) {
  svSetScope(svGetScopeFromName(path));
  return read_id();
}

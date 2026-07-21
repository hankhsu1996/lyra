// Single-unit compilation (`--single-unit`): every file joins one
// compilation-unit input, so there is one `$unit` scope (LRM 3.12.1) shared
// across the files. This file's `$unit`-scope `shared` is visible to a design
// element in the other file, reached there by a bare name.
int shared = 42;

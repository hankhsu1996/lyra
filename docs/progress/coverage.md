# Functional coverage

Covergroups (LRM 19): coverage models a design declares, sampled into bins and answering coverage
for an instance, a type and a run, with the database the run leaves. The model is a class the
compiler writes over a coverage library
([a-covergroup-is-a-class-over-a-coverage-library](../decisions/a-covergroup-is-a-class-over-a-coverage-library.md)).

Done when every construct of LRM 19 behaves as the standard says on both backends.

- [x] A covergroup declared in a module, a package, the compilation-unit scope or a class, made by
      `new` with input arguments and sampled by calling `sample` (LRM 19.3, 19.8), the arguments of
      an overriding `sample` among what its coverpoints name (LRM 19.8.1); reached from another
      module by its type or by a hierarchical name (LRM 23.6).
- [x] Value bins: single, arrays of one bin per value and of a fixed number, from ranges with `$`,
      from a queue or an array the design builds (LRM 19.5.1.2), filtered by `with` (LRM 19.5.1.1);
      wildcard, ignore, illegal and default bins (LRM 19.5.4 -- 19.5.6); automatic bins, an
      enumeration's by its values (LRM 19.5.3).
- [x] Transition bins with every repetition form and `default sequence` (LRM 19.5.2).
- [x] Coverpoints of real expressions: values, intervals divided by `real_interval`, tolerance
      ranges and `$` bounds (LRM 19.5.1, 11.4.13).
- [x] Crosses: the product of their coverpoints' bins, `binsof` with `intersect`, negation, `&&` and
      `||`, a cross named as a whole, `with` filters with `matches`, ignore and illegal bins,
      retaining automatic bins (LRM 19.6).
- [ ] A cross bin given by a set expression of the cross's `CrossQueueType` (LRM 19.6.1.4). The
      front end builds the cross's `CrossValType` without filling its field list, so the structure
      has no fields for anything reading it; the fix belongs in the front end.
- [x] `option` and `type_option` of a covergroup, a coverpoint and a cross, set in the declaration
      and read and written by the program (LRM 19.7, 19.10); `start`, `stop`, `set_inst_name`.
- [x] Instance, item, type and run coverage with weights, `at_least`, `per_instance`,
      `get_inst_coverage` and `merge_instances` (LRM 19.8, 19.11), and `$get_coverage` (LRM 19.9).
- [x] A clocking event sampling at each occurrence, as if the triggering process called `sample`,
      and under `strobe` once a time slot in its Postponed region (LRM 19.3, 19.7).
- [x] Block events sampling at the beginning and end of a named block, task, function or method of
      the same instance, or a method of the class embedding the covergroup (LRM 19.3). A block of
      another instance or class is refused.
- [x] `ref` arguments read at every sample and watched by the clocking event (LRM 19.3).
- [x] Covergroups a class embeds, reading the object's members (LRM 19.4).
- [x] A covergroup extending one a base class embeds, including one of another unit's class (LRM
      19.4.1): its instance replaces the base's, so the base's `new` makes it and the name reaches
      it from either class; a coverpoint it declares again stops the base's from counting, one it
      adds counts beside them, an option it sets overrides the base's, and the two are separate
      types.
- [x] The coverage database: written at the end of a run that made an instance, named by
      `$set_coverage_db_name`, read by `$load_coverage_db` (LRM 19.9).

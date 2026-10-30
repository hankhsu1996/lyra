// An associative array literal lists index:value pairs and may name a default.
// Assigning one replaces the whole contents of the array, so entries it does
// not list are gone. The default is not an entry: it does not count towards
// num(), and reading an index that has none yields it in place of the value
// Table 7-1 would otherwise give. Allocating an entry for such an index, which
// a read-modify-write does before it operates, starts it from that default.
// Assigning one associative array to another clears the target of its entries
// and then copies the source's (LRM 7.9.11, 7.8.7, 7.9.9).
module Top;
  int declared [string] = '{"k": 5, default: 77};
  int declared_present;
  int declared_missing;
  int declared_count;

  int replaced [string];
  int replaced_a;
  int replaced_b;
  int replaced_missing;
  int replaced_count;
  int exists_before_replacement;
  int exists_after_replacement;

  int counters [int];
  int counter_after_increment;
  int counter_missing;
  int counter_count;

  int source [string];
  int target [string];
  int target_copied;
  int target_count;
  int exists_overwritten;

  initial begin
    declared_present = declared["k"];
    declared_missing = declared["zzz"];
    declared_count = declared.num();

    replaced["stale"] = 1;
    exists_before_replacement = replaced.exists("stale");
    replaced = '{"a": 1, "b": 2, default: 99};
    exists_after_replacement = replaced.exists("stale");
    replaced_a = replaced["a"];
    replaced_b = replaced["b"];
    replaced_missing = replaced["xyz"];
    replaced_count = replaced.num();

    counters = '{default: 1};
    counters[5]++;
    counter_after_increment = counters[5];
    counter_missing = counters[9];
    counter_count = counters.num();

    source["p"] = 7;
    target["old"] = 1;
    target = source;
    exists_overwritten = target.exists("old");
    target_copied = target["p"];
    target_count = target.num();
  end

  final begin
    if (declared_present !== 5)
      $fatal(1, "declared_present was %0d, expected 5", declared_present);
    if (declared_missing !== 77)
      $fatal(1, "declared_missing was %0d, expected 77", declared_missing);
    if (declared_count !== 1)
      $fatal(1, "declared_count was %0d, expected 1", declared_count);

    if (exists_before_replacement !== 1)
      $fatal(1, "exists_before_replacement was %0d, expected 1",
             exists_before_replacement);
    if (exists_after_replacement !== 0)
      $fatal(1, "exists_after_replacement was %0d, expected 0",
             exists_after_replacement);
    if (replaced_a !== 1)
      $fatal(1, "replaced_a was %0d, expected 1", replaced_a);
    if (replaced_b !== 2)
      $fatal(1, "replaced_b was %0d, expected 2", replaced_b);
    if (replaced_missing !== 99)
      $fatal(1, "replaced_missing was %0d, expected 99", replaced_missing);
    if (replaced_count !== 2)
      $fatal(1, "replaced_count was %0d, expected 2", replaced_count);

    if (counter_after_increment !== 2)
      $fatal(1, "counter_after_increment was %0d, expected 2",
             counter_after_increment);
    if (counter_missing !== 1)
      $fatal(1, "counter_missing was %0d, expected 1", counter_missing);
    if (counter_count !== 1)
      $fatal(1, "counter_count was %0d, expected 1", counter_count);

    if (exists_overwritten !== 0)
      $fatal(1, "exists_overwritten was %0d, expected 0", exists_overwritten);
    if (target_copied !== 7)
      $fatal(1, "target_copied was %0d, expected 7", target_copied);
    if (target_count !== 1)
      $fatal(1, "target_count was %0d, expected 1", target_count);
    $display("All checks passed");
  end
endmodule

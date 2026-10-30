// An associative array allocates an entry when an index is used as the target
// of an assignment, and reading that index afterwards yields what was written.
// The num() method reports how many entries the array holds, exists() reports
// whether one index has an entry, and delete() given an index removes that one
// entry. A read-modify-write of an index that has no entry allocates one
// holding the element type's default value before the operation is applied. An
// element that is itself an associative array is reached by indexing the outer
// array, and a method called on that element acts on the array stored there
// (LRM 7.8, 7.8.7, 7.9.1, 7.9.2, 7.9.3).
module Top;
  int by_name [string];
  int by_number [int];
  int nested [string][int];

  int exists_before_write;
  int value_a;
  int value_b;
  int exists_written;
  int exists_never_written;
  int count_after_writes;

  int compound_result;
  int count_after_compound;

  int count_after_delete;
  int exists_deleted;

  int value_ten;
  int value_three;
  int count_by_number;

  int nested_exists;
  int nested_count_after_delete;

  initial begin
    exists_before_write = by_name.exists("a");

    by_name["a"] = 1;
    by_name["b"] = 2;
    value_a = by_name["a"];
    value_b = by_name["b"];
    exists_written = by_name.exists("a");
    exists_never_written = by_name.exists("z");
    count_after_writes = by_name.num();

    by_name["c"] += 5;
    compound_result = by_name["c"];
    count_after_compound = by_name.num();

    by_name.delete("b");
    count_after_delete = by_name.num();
    exists_deleted = by_name.exists("b");

    by_number[10] = 100;
    by_number[3] = 30;
    value_ten = by_number[10];
    value_three = by_number[3];
    count_by_number = by_number.num();

    nested["x"][1] = 10;
    nested["x"][2] = 20;
    nested_exists = nested["x"].exists(2);
    nested["x"].delete(1);
    nested_count_after_delete = nested["x"].num();
  end

  final begin
    if (exists_before_write !== 0)
      $fatal(1, "exists_before_write was %0d, expected 0",
             exists_before_write);
    if (value_a !== 1) $fatal(1, "value_a was %0d, expected 1", value_a);
    if (value_b !== 2) $fatal(1, "value_b was %0d, expected 2", value_b);
    if (exists_written !== 1)
      $fatal(1, "exists_written was %0d, expected 1", exists_written);
    if (exists_never_written !== 0)
      $fatal(1, "exists_never_written was %0d, expected 0",
             exists_never_written);
    if (count_after_writes !== 2)
      $fatal(1, "count_after_writes was %0d, expected 2", count_after_writes);
    if (compound_result !== 5)
      $fatal(1, "compound_result was %0d, expected 5", compound_result);
    if (count_after_compound !== 3)
      $fatal(1, "count_after_compound was %0d, expected 3",
             count_after_compound);
    if (count_after_delete !== 2)
      $fatal(1, "count_after_delete was %0d, expected 2", count_after_delete);
    if (exists_deleted !== 0)
      $fatal(1, "exists_deleted was %0d, expected 0", exists_deleted);
    if (value_ten !== 100)
      $fatal(1, "value_ten was %0d, expected 100", value_ten);
    if (value_three !== 30)
      $fatal(1, "value_three was %0d, expected 30", value_three);
    if (count_by_number !== 2)
      $fatal(1, "count_by_number was %0d, expected 2", count_by_number);
    if (nested_exists !== 1)
      $fatal(1, "nested_exists was %0d, expected 1", nested_exists);
    if (nested_count_after_delete !== 1)
      $fatal(1, "nested_count_after_delete was %0d, expected 1",
             nested_count_after_delete);
    $display("All checks passed");
  end
endmodule

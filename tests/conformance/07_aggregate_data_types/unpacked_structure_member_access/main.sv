// A structure collects data of several types under member names, and each
// member is referenced through the structure by its name. A member is a
// variable of its own declared type and nothing less: it is an operand
// wherever a value of that type is one, it is the target of an assignment
// including a read-modify-write, a bit-select or part-select reaches into it
// when it is a vector, and when it is itself a structure or an array the next
// select applies to it in turn. That holds for a structure reached through an
// array element as much as for one named directly (LRM 7.2, 7.4.6).
module Top;
  typedef struct {
    int count;
    logic [7:0] flags;
  } entry_t;

  typedef struct {
    entry_t entry;
    string label;
    int trace [3];
  } record_t;

  record_t record;
  entry_t entries [2];

  int read_count;
  logic [7:0] read_flags;
  logic high_flag;
  logic [3:0] low_nibble;
  string read_label;
  int read_trace;
  int count_after_increase;
  int count_in_expression;
  int first_entry_count;
  int second_entry_count;

  initial begin
    record.entry.count = 42;
    record.entry.flags = 8'hA5;
    record.label = "header";
    record.trace[1] = 7;

    read_count = record.entry.count;
    read_flags = record.entry.flags;
    high_flag = record.entry.flags[7];
    low_nibble = record.entry.flags[3:0];
    read_label = record.label;
    read_trace = record.trace[1];

    record.entry.count += 8;
    count_after_increase = record.entry.count;
    count_in_expression = record.entry.count * 2 + 1;

    entries[0].count = 11;
    entries[1].count = 22;
    first_entry_count = entries[0].count;
    second_entry_count = entries[1].count;
  end

  final begin
    if (read_count !== 42)
      $fatal(1, "read_count was %0d, expected 42", read_count);
    if (read_flags !== 8'hA5)
      $fatal(1, "read_flags was %0h, expected a5", read_flags);
    if (high_flag !== 1'b1)
      $fatal(1, "high_flag was %0h, expected 1", high_flag);
    if (low_nibble !== 4'h5)
      $fatal(1, "low_nibble was %0h, expected 5", low_nibble);
    if (read_label != "header")
      $fatal(1, "read_label was '%s', expected 'header'", read_label);
    if (read_trace !== 7)
      $fatal(1, "read_trace was %0d, expected 7", read_trace);
    if (record.trace[0] !== 0)
      $fatal(1, "record.trace[0] was %0d, expected 0", record.trace[0]);

    if (count_after_increase !== 50)
      $fatal(1, "count_after_increase was %0d, expected 50",
             count_after_increase);
    if (count_in_expression !== 101)
      $fatal(1, "count_in_expression was %0d, expected 101",
             count_in_expression);

    if (first_entry_count !== 11)
      $fatal(1, "first_entry_count was %0d, expected 11", first_entry_count);
    if (second_entry_count !== 22)
      $fatal(1, "second_entry_count was %0d, expected 22",
             second_entry_count);
    $display("All checks passed");
  end
endmodule

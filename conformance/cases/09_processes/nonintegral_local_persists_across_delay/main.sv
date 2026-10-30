// A variable declared automatic inside a procedural block has the lifetime of
// that block and is initialized on entry to it (LRM 6.21), and a delay control
// delays only the statement following it with respect to the one preceding it
// (LRM 9.4.1) -- control never leaves the block, so what is read after the
// delay is the same variable, holding what the last write before the delay put
// there. None of that turns on the data type: a real, an unpacked structure,
// and a dynamic array whose size and elements were both changed all read back
// across a suspension as they were left (LRM 7.5.1).
module Top;
  timeunit 1ns;
  timeprecision 1ns;

  typedef struct {
    int count;
    real weight;
    string label;
  } sample_t;

  real real_after_delay = -1.0;
  int count_after_delay = -1;
  real weight_after_delay = -1.0;
  string label_after_delay = "unset";
  int size_after_delay = -1;
  int first_after_delay = -1;
  int middle_after_delay = -1;
  int last_after_delay = -1;
  time wake_time = 99;

  initial begin
    automatic real level;
    automatic sample_t sample;
    automatic int values[];

    level = 1.5;
    sample.count = 1;
    sample.weight = 0.25;
    sample.label = "first";
    values = new [2];
    values[0] = 10;
    values[1] = 20;

    level = 6.25;
    sample.count = 9;
    sample.weight = 2.75;
    sample.label = "second";
    values = new [3] (values);
    values[0] = 11;
    values[2] = 30;

    #5;

    real_after_delay = level;
    count_after_delay = sample.count;
    weight_after_delay = sample.weight;
    label_after_delay = sample.label;
    size_after_delay = values.size();
    first_after_delay = values[0];
    middle_after_delay = values[1];
    last_after_delay = values[2];
    wake_time = $time;
  end

  final begin
    if (real_after_delay != 6.25)
      $fatal(1, "the real read back as %f, expected 6.25", real_after_delay);
    if (count_after_delay !== 9)
      $fatal(1, "the structure's count read back as %0d, expected 9",
             count_after_delay);
    if (weight_after_delay != 2.75)
      $fatal(1, "the structure's weight read back as %f, expected 2.75",
             weight_after_delay);
    if (label_after_delay != "second")
      $fatal(1, "the structure's label read back as %s, expected second",
             label_after_delay);
    if (size_after_delay !== 3)
      $fatal(1, "the dynamic array's size read back as %0d, expected 3",
             size_after_delay);
    if (first_after_delay !== 11)
      $fatal(1, "the array's first element read back as %0d, expected 11",
             first_after_delay);
    if (middle_after_delay !== 20)
      $fatal(1, "the array's middle element read back as %0d, expected 20",
             middle_after_delay);
    if (last_after_delay !== 30)
      $fatal(1, "the array's last element read back as %0d, expected 30",
             last_after_delay);
    if (wake_time !== 5)
      $fatal(1, "the process resumed at %0d, expected 5", wake_time);
    $display("All checks passed");
  end
endmodule

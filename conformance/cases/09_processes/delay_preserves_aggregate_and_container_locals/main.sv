// A delay control delays the statement following it with respect to the one
// preceding it (LRM 9.4.1) and changes nothing about the variables the
// suspended process declared: an automatic variable is initialized on entry to
// the block that declares it and holds what was last written to it until that
// block is left (LRM 6.21), and suspending inside the block is not leaving it.
// That holds whatever the variable's type is, so a queue, a dynamic array, an
// associative array, a fixed-size unpacked array, an unpacked structure, a
// string and a real each read back after a delay exactly what was written
// before it -- including a write made after the delay to storage the earlier
// half of the process filled.
module Top;
  timeunit 1ns;
  timeprecision 1ns;

  typedef struct {
    int tag;
    int weight;
  } entry_t;

  int queue_sum;
  int dyn_sum;
  int assoc_sum;
  int fixed_sum;
  int struct_sum;
  string text;
  real ratio;

  initial begin
    automatic int q[$];
    automatic int dyn[];
    automatic int assoc[string];
    automatic int fixed[3];
    automatic entry_t record;
    automatic string label;
    automatic real fraction;

    q.push_back(3);
    dyn = new [2];
    dyn[0] = 5;
    assoc["a"] = 7;
    fixed[0] = 11;
    record.tag = 13;
    label = "be";
    fraction = 0.5;

    #10;

    q.push_back(4);
    dyn[1] = 6;
    assoc["b"] = 8;
    fixed[1] = 12;
    record.weight = 14;
    label = {label, "fore"};
    fraction = fraction + 0.25;

    queue_sum = q[0] + q[1];
    dyn_sum = dyn[0] + dyn[1];
    assoc_sum = assoc["a"] + assoc["b"];
    fixed_sum = fixed[0] + fixed[1];
    struct_sum = record.tag + record.weight;
    text = label;
    ratio = fraction;
  end

  final begin
    if (queue_sum !== 7)
      $fatal(1, "queue_sum was %0d, expected 7", queue_sum);
    if (dyn_sum !== 11)
      $fatal(1, "dyn_sum was %0d, expected 11", dyn_sum);
    if (assoc_sum !== 15)
      $fatal(1, "assoc_sum was %0d, expected 15", assoc_sum);
    if (fixed_sum !== 23)
      $fatal(1, "fixed_sum was %0d, expected 23", fixed_sum);
    if (struct_sum !== 27)
      $fatal(1, "struct_sum was %0d, expected 27", struct_sum);
    if (text != "before")
      $fatal(1, "text was %s, expected before", text);
    if (ratio != 0.75)
      $fatal(1, "ratio was %f, expected 0.75", ratio);
    $display("All checks passed");
  end
endmodule

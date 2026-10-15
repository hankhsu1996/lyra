// A concurrent assertion reads the sampled value of what its property names
// (LRM 16.5.1), and a member of a tagged union is one of those like any other:
// read through the tag the sampled union holds, and checked against it (LRM
// 11.9). Here every attempt reads members the tags name, so the property holds
// at each tick.
module Top;
  typedef union tagged {
    int a;
    int b;
  } choice_t;

  logic clk = 0;
  choice_t held[2];
  int passed = 0;

  always #5 clk = ~clk;

  a_members: assert property (@(posedge clk) held[0].a == 1 && held[1].b == 2)
      passed = passed + 1;
    else
      $fatal(1, "the property failed at time %0t", $time);

  initial begin
    held[0] = tagged a 1;
    held[1] = tagged b 2;
    #20 $finish;
  end

  final begin
    if (passed !== 2)
      $fatal(1, "the property held at %0d ticks, expected 2", passed);
    $display("All checks passed");
  end
endmodule

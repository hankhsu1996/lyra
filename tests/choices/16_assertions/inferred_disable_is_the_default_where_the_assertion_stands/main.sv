// `$inferred_disable` is the disable condition of the `default disable iff`
// declaration whose scope includes the call, and is replaced as determined
// where the property is instantiated (LRM 16.14.7). For a property declared in
// one scope and asserted, through another property, in a block with a default
// of its own, the two sentences give different answers, and the standard does
// not settle which. Lyra takes the default in force where the assertion
// stands, however deep the instance whose formal it defaults. This case holds
// Lyra to that.
module Top;
  logic clk = 0;
  // Held through the ticks at 5, 15, 25 and 35.
  logic rst = 1;
  // Held through the ticks at 5 up to 65.
  logic block_rst = 1;

  default disable iff rst;

  property never_inferred(r = $inferred_disable);
    @(posedge clk) disable iff (r) 1'b0;
  endproperty

  property never_nested;
    never_inferred;
  endproperty

  always #5 clk = ~clk;
  initial #40 rst = 0;
  initial #70 block_rst = 0;

  if (1) begin : overrides
    default disable iff block_rst;
    int nested_fails = 0;
    assert property (never_nested) else nested_fails++;
  end

  initial #100 $finish;

  final begin
    // The clock rises at 5, 15, ..., 95, the last three ticks after
    // `block_rst` falls.
    if (overrides.nested_fails !== 3)
      $fatal(1, "'$inferred_disable' in a nested instance left %0d ticks, expected 3",
             overrides.nested_fails);
    $display("All checks passed");
  end
endmodule

// A continuous assignment may be placed on a net in the same statement that
// declares the net, which drives it exactly as a separate continuous
// assignment statement on that net would (LRM 10.3.1). The net is never
// written: its value is the value of its driver, and it follows a change in
// the operands of the driving expression.
module Top;
  logic [7:0] source;

  wire [7:0] declared = source + 8'd1;
  wire [7:0] assigned;

  assign assigned = source + 8'd1;

  logic [7:0] declared_first;
  logic [7:0] assigned_first;
  logic [7:0] declared_second;
  logic [7:0] assigned_second;

  initial begin
    source = 8'd10;
    #1;
    declared_first = declared;
    assigned_first = assigned;
    source = 8'd20;
    #1;
    declared_second = declared;
    assigned_second = assigned;
  end

  final begin
    if (declared_first !== 8'd11)
      $fatal(1, "declared_first was %0d, expected 11", declared_first);
    if (assigned_first !== 8'd11)
      $fatal(1, "assigned_first was %0d, expected 11", assigned_first);
    if (declared_second !== 8'd21)
      $fatal(1, "declared_second was %0d, expected 21", declared_second);
    if (assigned_second !== 8'd21)
      $fatal(1, "assigned_second was %0d, expected 21", assigned_second);
    $display("All checks passed");
  end
endmodule

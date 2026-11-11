// @rejected:
//
// A procedural concurrent assertion saves the automatic variables of its
// procedure where its statement is reached, and they behave as inputs to its
// action block that shall not be modified, so a conforming tool rejects this
// program, whose action block writes the saved loop variable (LRM 16.14.6.1).
module Top;
  logic clk = 0;
  int seen[$];

  always #5 clk = ~clk;

  always @(posedge clk) begin
    for (int k = 0; k < 2; k++)
      a_w: assert property (@(posedge clk) 0) else begin
        k = 7;
        seen.push_back(k);
      end
  end

  initial #12 $finish;

  final $display("the program the standard rejects ran with %0d", seen.size());
endmodule

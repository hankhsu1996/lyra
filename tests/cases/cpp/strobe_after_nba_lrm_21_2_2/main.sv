module Top;
  // LRM 21.2.2 canonical reproducer: at the same posedge, $display sees the
  // pre-NBA value and $strobe sees the post-NBA value, proving that
  // $strobe's argument list is re-evaluated in the postponed region after
  // NBAs commit.
  logic clk;
  int   cnt;
  initial begin
    clk = 0;
    cnt = 0;
    #1 clk = 1;
  end
  always @(posedge clk) begin
    cnt <= cnt + 1;
    $display("display cnt=%0d", cnt);
    $strobe ("strobe  cnt=%0d", cnt);
  end
endmodule

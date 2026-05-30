module Top;
  logic [3:0] narrow;
  int eq_wider;
  int eq_wider_diff;

  initial begin
    narrow = 4'd5;
    eq_wider = (narrow === 32'd5) ? 1 : 0;
    eq_wider_diff = (narrow === 32'd6) ? 1 : 0;
  end
endmodule

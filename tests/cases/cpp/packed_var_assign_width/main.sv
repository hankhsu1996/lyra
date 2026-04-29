module Top;
  logic [3:0] sml;
  logic [7:0] wide;
  logic [3:0] narrow;

  initial begin
    sml = 4'b10xz;
    wide = sml;
    narrow = wide;
    $display("%b", wide);
    $display("%b", narrow);
  end
endmodule

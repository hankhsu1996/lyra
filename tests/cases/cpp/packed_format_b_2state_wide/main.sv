module Top;
  bit [127:0] a;

  initial begin
    a = 128'h0123_4567_89ab_cdef_0011_2233_4455_6677;
    $display("%b", a);
  end
endmodule

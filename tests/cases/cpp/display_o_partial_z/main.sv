module Top;
  logic [5:0] v;
  initial begin
    v = 6'b001_zzz;
    $display("%o", v);
  end
endmodule

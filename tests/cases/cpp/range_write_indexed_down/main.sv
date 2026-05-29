module Top;
  bit [31:0] out_const;
  bit [31:0] out_var;
  initial begin
    int idx;
    out_const = 32'h00000000;
    out_const[15-:8] = 8'hAB;
    idx = 23;
    out_var = 32'h00000000;
    out_var[idx-:8] = 8'hCD;
  end
endmodule

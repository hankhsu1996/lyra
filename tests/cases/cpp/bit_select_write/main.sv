module Top;
  bit [7:0] out_const;
  bit [7:0] out_var;
  bit [7:0] cleared;
  initial begin
    int idx;
    out_const = 8'h00;
    out_const[3] = 1'b1;
    idx = 5;
    out_var = 8'h00;
    out_var[idx] = 1'b1;
    cleared = 8'hFF;
    cleared[0] = 1'b0;
  end
endmodule

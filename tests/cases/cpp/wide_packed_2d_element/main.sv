module Top;
  bit [1:0][127:0] m;
  bit [127:0] elem_rt;
  bit [11:0] slice_rt;
  bit lsb_rt;

  initial begin
    m[0] = 128'hF0F0F0F0F0F0F0F00F0F0F0F0F0F0F0F;
    m[1] = 128'h00000000000000ABC000000000000000;
    elem_rt = m[1];
    slice_rt = m[1][71:60];
    lsb_rt = m[0][0];
  end
endmodule

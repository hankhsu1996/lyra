module Top;
  logic [7:0] mem [0:3];
  initial $readmemh("mem.hex", mem);
endmodule

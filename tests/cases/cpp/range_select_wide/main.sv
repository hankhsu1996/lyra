module Top;
  bit [31:0] low32;
  bit [31:0] high32;
  bit [63:0] middle64;
  initial begin
    bit [127:0] w;
    w = 128'h11223344_55667788_99AABBCC_DDEEFF00;
    low32 = w[31:0];
    high32 = w[127:96];
    middle64 = w[95:32];
  end
endmodule

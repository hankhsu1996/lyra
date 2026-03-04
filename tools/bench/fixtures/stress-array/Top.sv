module Top;
  logic [31:0] data [0:4095];
  int sum;
  initial begin
    for (int i = 0; i < 4096; i++) data[i] = i * 3 + 1;
    sum = 0;
    for (int i = 0; i < 4096; i++) sum = sum + int'(data[i]);
    $display("stress sum = %0d", sum);
    $finish;
  end
endmodule

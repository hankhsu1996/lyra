module Top;
  int a;
  initial begin
    if ($sscanf("1", "%d", a)) begin
      a = a + 1;
    end
  end
endmodule

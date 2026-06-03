module Top;
  int code;
  int a, b;
  initial begin
    code = $sscanf("1 2 3", "%d %*d %d", a, b);
  end
endmodule

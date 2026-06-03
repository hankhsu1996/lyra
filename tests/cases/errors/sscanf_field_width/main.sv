module Top;
  int code;
  int x;
  initial begin
    code = $sscanf("12345", "%5d", x);
  end
endmodule

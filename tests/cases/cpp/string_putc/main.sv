module Top;
  string s;

  initial begin
    s = "Hello";
    s.putc(1, 8'h78);
  end
endmodule

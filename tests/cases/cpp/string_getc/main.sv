module Top;
  string s;
  byte c0;
  byte c4;

  initial begin
    s = "Hello";
    c0 = s.getc(0);
    c4 = s.getc(4);
  end
endmodule

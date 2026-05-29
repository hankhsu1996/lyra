module Top;
  string a;
  string b;
  bit r;

  initial begin
    a = "";
    b = "x";
    r = (a < b);
  end
endmodule

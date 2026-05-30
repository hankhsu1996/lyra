module Top;
  string a;
  string b;
  integer ten;
  integer fifteen;

  initial begin
    a = "1010";
    b = "1111";
    ten = a.atobin();
    fifteen = b.atobin();
  end
endmodule

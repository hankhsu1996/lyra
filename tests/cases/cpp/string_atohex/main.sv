module Top;
  string a;
  string b;
  integer upper;
  integer mixed;

  initial begin
    a = "FF";
    b = "deadBEEF";
    upper = a.atohex();
    mixed = b.atohex();
  end
endmodule

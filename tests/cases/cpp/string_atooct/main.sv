module Top;
  string a;
  string b;
  integer seventy_seven;
  integer one_two_three;

  initial begin
    a = "77";
    b = "123";
    seventy_seven = a.atooct();
    one_two_three = b.atooct();
  end
endmodule

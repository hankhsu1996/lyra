module Top;
  int add_result;
  int sub_result;
  int mul_result;
  int div_result;
  int mod_result;

  initial begin
    add_result = 10;
    add_result += 3;

    sub_result = 10;
    sub_result -= 3;

    mul_result = 10;
    mul_result *= 3;

    div_result = 10;
    div_result /= 3;

    mod_result = 10;
    mod_result %= 3;
  end
endmodule

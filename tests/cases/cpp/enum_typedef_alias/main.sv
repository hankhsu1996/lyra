module Top;
  typedef enum {A, B, C} primary_t;
  typedef primary_t alias_t;
  primary_t v1;
  alias_t v2;
  initial begin
    v1 = A;
    v2 = B;
    $display("[%s][%s]", v1.name(), v2.name());
  end
endmodule

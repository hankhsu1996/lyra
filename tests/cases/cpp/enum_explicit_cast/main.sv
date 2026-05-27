module Top;
  typedef enum {A, B, C} val_t;
  val_t v;
  int result;
  initial begin
    v = val_t'(2);
    result = v;
  end
endmodule

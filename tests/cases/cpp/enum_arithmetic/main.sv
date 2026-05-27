module Top;
  typedef enum {A = 10, B = 20} val_t;
  val_t v;
  int result;
  initial begin
    v = A;
    result = v + 5;
  end
endmodule

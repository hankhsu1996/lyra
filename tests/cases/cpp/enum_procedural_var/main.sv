module Top;
  typedef enum {A, B, C} state_t;
  initial begin
    state_t local_s;
    local_s = B;
    $display("%s", local_s.name());
  end
endmodule

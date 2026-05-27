module Top;
  typedef enum {IDLE, RUN, STOP} state_t;
  state_t state;
  int result;
  initial begin
    state = IDLE;
    result = state;
  end
endmodule

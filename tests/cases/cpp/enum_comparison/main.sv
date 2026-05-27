module Top;
  typedef enum {IDLE, RUN, STOP} state_t;
  state_t state;
  int r0, r1, r2;
  initial begin
    state = RUN;
    r0 = (state == RUN) ? 1 : 0;
    r1 = (state == IDLE) ? 1 : 0;
    r2 = (state != STOP) ? 1 : 0;
  end
endmodule

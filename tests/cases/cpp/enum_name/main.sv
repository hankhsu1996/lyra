module Top;
  typedef enum {IDLE, RUN, STOP} state_t;
  state_t s;
  string name;
  initial begin
    s = RUN;
    name = s.name();
    $display("%s", name);
  end
endmodule

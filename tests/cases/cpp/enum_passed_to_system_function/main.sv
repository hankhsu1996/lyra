module Top;
  typedef enum {OFF, ON, STANDBY} state_t;
  state_t s;
  initial begin
    s = ON;
    $display(s);
  end
endmodule

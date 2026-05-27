module Top;
  typedef enum {OFF, ON} switch_t;
  switch_t sw;
  initial begin
    sw = OFF;
    $display("%0d", sw);
    sw = ON;
    $display("%0d", sw);
  end
endmodule

module Top;
  typedef enum logic [2:0] {RED, GREEN, BLUE} color_t;
  color_t color;
  int result;
  initial begin
    color = BLUE;
    result = color;
  end
endmodule

module Top;
  typedef struct packed {
    logic [7:0] high;
    logic [7:0] low;
  } pair_t;
  typedef union packed {
    pair_t       pair;
    logic [15:0] word;
  } combo_t;
  combo_t u;
  int result_word;
  int result_high;
  int result_low;
  initial begin
    u.word = 16'hABCD;
    result_word = u.word;
    result_high = u.pair.high;
    result_low = u.pair.low;
  end
endmodule

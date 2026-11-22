// @measure: run
// @work: call
//
// One call moves every bit of a 5x5 box of 64-bit lanes into a flat vector,
// one bit at a time, the way a Keccak state is converted between its two forms.
// Each position steps by one with the innermost loop, so the loop moves a run
// of a lane's bits; what differs from a slice write is that the source wrote
// it bit by bit.
module Top;
  localparam int W = 64;
  typedef logic [4:0][4:0][W-1:0] box_t;

  function automatic logic [25*W-1:0] box_to_bitarray(box_t state);
    automatic logic [25*W-1:0] bitarray;
    for (int y = 0; y < 5; y++)
      for (int x = 0; x < 5; x++)
        for (int z = 0; z < W; z++)
          bitarray[W*(5*y+x)+z] = state[x][y][z];
    return bitarray;
  endfunction

  box_t state;
  logic [25*W-1:0] flat;
  int sum;

  initial begin
    int num_calls;

    if (!$value$plusargs("work=%d", num_calls)) num_calls = 10;

    for (int i = 0; i < 25; i++) state[i % 5][i / 5] = 64'(i) * 64'h9e3779b97f4a7c15;
    sum = 0;
    for (int call = 0; call < num_calls; call++) begin
      state[call % 5][(call / 5) % 5][call % W] = ~state[call % 5][(call / 5) % 5][call % W];
      flat = box_to_bitarray(state);
      sum = sum ^ int'(flat[31:0]) ^ int'(flat[25*W-1 -: 32]);
    end

    $display("packed-bit-transpose done: sum=%0d", sum);
    $finish;
  end
endmodule

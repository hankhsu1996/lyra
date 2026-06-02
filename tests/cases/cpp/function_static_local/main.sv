module Top;
  int r_id1;
  int r_id2;
  int r_id3;
  int r_mix1;
  int r_mix2;

  function int next_id();
    int counter;
    counter = counter + 1;
    return counter;
  endfunction

  function automatic int mix();
    int a;
    static int hits;
    hits = hits + 1;
    a = hits * 2;
    return a;
  endfunction

  initial begin
    r_id1 = next_id();
    r_id2 = next_id();
    r_id3 = next_id();
    r_mix1 = mix();
    r_mix2 = mix();
  end
endmodule

// A loop around two loops, each counting out as many blocks as the outer index
// says (LRM 27.4): one from zero up to a bound the index gives, the other from
// past that bound to the end, which every fourth outer block counts out none
// of. Every outer block states the same two loops. N is how many blocks the
// outer loop counts out.
module Top #(parameter int N = 4);
  int sink [N][4];
  for (genvar g = 0; g < N; g += 1) begin : gg
    for (genvar i = 0; i <= g % 4; i += 1) begin : below
      initial sink[g][i] = 1;
    end
    for (genvar i = g % 4 + 1; i < 4; i += 1) begin : above
      initial sink[g][i] = 2;
    end
  end
endmodule

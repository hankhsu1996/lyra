// Every process has a generator of its own for the randomization calls made in
// it (LRM 18.14.1), an always_comb procedure and a continuous assignment among
// them (Annex P), and seeding a process through its handle sets that generator
// (LRM 18.13.3), so the value drawn next is the one any generator seeded the
// same way draws first. The generator is kept from one evaluation to the next,
// so two draws without a seed between them come out as one generator's next
// two values.
module Top;
  logic [3:0] a;
  int unsigned comb_draw;
  process comb;
  int unsigned seeded_first, seeded_second;
  int unsigned reference_first, reference_second;

  always_comb begin
    comb = process::self();
    comb_draw = $urandom + a;
  end

  initial begin
    process reference;
    a = 4'd0;
    #1;
    comb.srandom(7);
    a = 4'd2;
    #1 seeded_first = comb_draw - 2;
    a = 4'd3;
    #1 seeded_second = comb_draw - 3;
    fork
      begin
        reference = process::self();
        reference.srandom(7);
        reference_first = $urandom;
        reference_second = $urandom;
      end
    join
  end

  final begin
    if (seeded_first !== reference_first)
      $fatal(1, "seeded_first was %0d, expected %0d", seeded_first,
             reference_first);
    if (seeded_second !== reference_second)
      $fatal(1, "seeded_second was %0d, expected %0d", seeded_second,
             reference_second);
    $display("All checks passed");
  end
endmodule

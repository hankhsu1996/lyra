module Top;
  // Nested generate-for whose INNER stop bound references the OUTER genvar
  // `stg` (LRM 27.4): stage stg runs (4 - stg) segments. Stage 0 runs four
  // segments and drives every bit of mask[0] / emask[0], so each is fully
  // driven and fully defined (4'hF) in both 2-state and 4-state. Resolving
  // `stg` in the inner bound as the inner loop's own `seg` would change the
  // iteration count and the result.
  //
  // Both genvar-indexed write forms are exercised: a range select on `mask`
  // (whose bound must stay runtime, not fold to one elaboration constant) and a
  // bare element select on `emask` (whose nested place must lower without the
  // owned-child construction reading a stale type reference).
  logic [3:0] mask [3];
  logic [3:0] emask [3];
  for (genvar stg = 0; stg < 3; stg++) begin : gen_stage
    for (genvar seg = 0; seg < 4 - stg; seg++) begin : gen_seg
      assign mask[stg][seg +: 1] = 1'b1;
      assign emask[stg][seg] = 1'b1;
    end
  end

  int m0;
  int e0;
  assign m0 = mask[0];
  assign e0 = emask[0];
endmodule

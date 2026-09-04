// A module header may leave an interface reference unspecified, as a
// placeholder for an interface selected when the module itself is instantiated
// (LRM 25.3.3). The generic reference is reached by a named port connection,
// and what it names is a whole interface instance, so one module written
// against it serves two unrelated interfaces and each instantiation reaches the
// members of the interface its own connection named. A generic reference may
// carry a range as any interface reference may (LRM 25.3), and one that does
// selects its interface at the instantiation the same way, so the two forms are
// checked together. The two interfaces below declare their shared names in
// different orders, so a module that reached one of them the way it reaches the
// other would read the wrong member.
interface Alpha;
  logic [7:0] addr;
  int hits;
endinterface

interface Beta;
  logic extra;
  logic [7:0] addr;
  int hits;
endinterface

module Marker #(
    parameter logic [7:0] Mark = 8'h00
) (
    interface a
);
  logic [7:0] seen = 8'h3c;

  initial #2 begin
    seen   = a.addr;
    a.addr = Mark;
    a.hits = a.hits + 1;
  end
endmodule

module Pair (
    interface a[2]
);
  logic [7:0] seen_low = 8'h3c;
  logic [7:0] seen_high = 8'h3c;

  initial #2 begin
    seen_low  = a[0].addr;
    seen_high = a[1].addr;
  end
endmodule

module Top;
  Alpha alpha ();
  Beta beta ();

  Alpha alphas[2] ();
  Beta betas[2] ();

  Marker #(8'ha5) on_alpha (.a(alpha));
  Marker #(8'h5a) on_beta (.a(beta));

  Pair on_alphas (.a(alphas));
  Pair on_betas (.a(betas));

  initial #1 begin
    alpha.addr = 8'h11;
    beta.addr  = 8'h22;

    alphas[0].addr = 8'h31;
    alphas[1].addr = 8'h32;
    betas[0].addr  = 8'h41;
    betas[1].addr  = 8'h42;
  end

  final begin
    if (on_alpha.seen !== 8'h11)
      $fatal(1, "on_alpha.seen was %h, expected 11", on_alpha.seen);
    if (on_beta.seen !== 8'h22)
      $fatal(1, "on_beta.seen was %h, expected 22", on_beta.seen);
    if (alpha.addr !== 8'ha5)
      $fatal(1, "alpha.addr was %h, expected a5", alpha.addr);
    if (beta.addr !== 8'h5a)
      $fatal(1, "beta.addr was %h, expected 5a", beta.addr);
    if (alpha.hits !== 1)
      $fatal(1, "alpha.hits was %0d, expected 1", alpha.hits);
    if (beta.hits !== 1)
      $fatal(1, "beta.hits was %0d, expected 1", beta.hits);
    if (on_alphas.seen_low !== 8'h31)
      $fatal(1, "on_alphas.seen_low was %h, expected 31", on_alphas.seen_low);
    if (on_alphas.seen_high !== 8'h32)
      $fatal(1, "on_alphas.seen_high was %h, expected 32", on_alphas.seen_high);
    if (on_betas.seen_low !== 8'h41)
      $fatal(1, "on_betas.seen_low was %h, expected 41", on_betas.seen_low);
    if (on_betas.seen_high !== 8'h42)
      $fatal(1, "on_betas.seen_high was %h, expected 42", on_betas.seen_high);
    $display("All checks passed");
  end
endmodule

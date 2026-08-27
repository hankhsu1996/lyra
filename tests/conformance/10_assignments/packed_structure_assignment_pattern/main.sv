// A packed structure takes an assignment pattern the way any structure does:
// positionally in declaration order, by member name in whatever order the
// names are written, through the default key for the members no name reached,
// or from a replication supplying an exact number of members. What the
// pattern builds is a vector, in which the first member declared is the most
// significant and the rest follow in decreasing significance
// (LRM 10.9.2, 7.2.1).
module Top;
  typedef struct packed {
    logic [3:0] high;
    logic [3:0] middle;
    logic [3:0] low;
  } trio_t;

  typedef struct packed {
    logic [3:0] a;
    logic [3:0] b;
    logic [3:0] c;
    logic [3:0] d;
  } quad_t;

  trio_t positional = 12'h999;
  trio_t by_name = 12'h999;
  trio_t named_and_default = 12'h999;
  quad_t replicated = 16'h9999;

  initial begin
    positional = '{4'hA, 4'h5, 4'h3};
    by_name = '{low: 4'hC, high: 4'h3, middle: 4'h7};
    named_and_default = '{high: 4'h1, default: 4'hE};
    replicated = '{2{4'h1, 4'h2}};
  end

  final begin
    if (positional.high !== 4'hA)
      $fatal(1, "positional.high was %0h, expected a", positional.high);
    if (positional.low !== 4'h3)
      $fatal(1, "positional.low was %0h, expected 3", positional.low);
    if (positional !== 12'hA53)
      $fatal(1, "positional was %0h, expected a53", positional);

    if (by_name !== 12'h37C)
      $fatal(1, "by_name was %0h, expected 37c", by_name);

    if (named_and_default.high !== 4'h1)
      $fatal(1, "named_and_default.high was %0h, expected 1",
             named_and_default.high);
    if (named_and_default !== 12'h1EE)
      $fatal(1, "named_and_default was %0h, expected 1ee", named_and_default);

    if (replicated !== 16'h1212)
      $fatal(1, "replicated was %0h, expected 1212", replicated);
    $display("All checks passed");
  end
endmodule

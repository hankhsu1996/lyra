// An element select of an associative array with a constant index extends a
// static prefix (LRM 11.5.3), so an always_comb that writes one entry and
// another process that writes a different entry assign parts whose longest
// static prefixes do not overlap, which LRM 9.2.2.2 allows. The entry the
// block writes is the only thing left out of its implicit sensitivity list
// (LRM 9.2.2.2.1), so a change to the entry it reads still runs it.
module Top;
  typedef struct {
    int tag;
    int by_name [string];
  } bag_t;
  bag_t bag;
  int bag_seen = -1;
  always_comb begin
    bag.by_name["out"] = bag.by_name["in"] + 1;
    bag_seen = bag.by_name["in"];
  end

  initial begin
    #1;
    bag.by_name["in"] = 5;
    #1;
  end

  final begin
    if (bag_seen !== 5 || bag.by_name["out"] !== 6)
      $fatal(1, "the entry read was %0d and the one written %0d, expected 5, 6",
             bag_seen, bag.by_name["out"]);
    $display("All checks passed");
  end
endmodule

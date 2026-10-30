// A packed tagged union's tag is as many bits as it takes to code for every
// member name (LRM 7.3.2), so a union of one member carries a tag of no bits
// and always holds that member. It is declared, built with a tagged union
// expression, read and written through its member, matched by a pattern and
// printed like any other (LRM 11.9, 12.6, 21.2.1.6).
module Top;
  typedef union tagged packed {
    bit [7:0] only;
  } single_t;

  single_t value;
  bit [7:0] read_back;
  int matched;
  string printed;

  initial begin
    value = tagged only 8'h12;
    value.only[0] = 1'b1;
    read_back = value.only;
    matched = 0;
    if (value matches tagged only .x) matched = x;
    printed = $sformatf("%p", value);
  end

  final begin
    if (read_back !== 8'h13)
      $fatal(1, "read_back was %h, expected 13", read_back);
    if (matched !== 8'h13)
      $fatal(1, "matched was %0d, expected 19", matched);
    if (printed != "'{only:19}")
      $fatal(1, "printed '%s'", printed);
    $display("All checks passed");
  end
endmodule

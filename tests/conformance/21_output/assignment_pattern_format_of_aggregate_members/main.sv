// %p prints a structure as an assignment pattern whose elements carry the names
// the type declares for them, for an unpacked structure and for a packed one
// alike, and prints a union as only its first declared element (LRM 21.2.1.6).
// The names are what separates this from the shorter %0p form, which the same
// clause leaves to the tool.
module Top;
  typedef struct {
    int count;
    string label;
  } entry_t;

  typedef struct packed {
    bit [3:0] high;
    bit [3:0] low;
  } halves_t;

  typedef union {
    int whole;
    byte first;
  } view_t;

  entry_t entry;
  halves_t halves;
  view_t view;

  string entry_text;
  string halves_text;
  string view_text;

  initial begin
    entry = '{7, "seven"};
    halves = '{4'd1, 4'd2};
    view.whole = 9;

    entry_text = $sformatf("%p", entry);
    halves_text = $sformatf("%p", halves);
    view_text = $sformatf("%p", view);
  end

  final begin
    if (entry_text != "'{count:7, label:\"seven\"}")
      $fatal(1, "an unpacked structure printed as '%s'", entry_text);
    if (halves_text != "'{high:1, low:2}")
      $fatal(1, "a packed structure printed as '%s'", halves_text);
    if (view_text != "'{whole:9}")
      $fatal(1, "a union printed as '%s'", view_text);
    $display("All checks passed");
  end
endmodule

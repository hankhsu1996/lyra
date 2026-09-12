// %p prints a structure as an assignment pattern whose elements carry the names
// the type declares for them, for an unpacked structure and for a packed one
// alike; prints a union as only its first declared element; and prints a tagged
// union as the element its tag names (LRM 21.2.1.6). An aggregate a container
// holds is traversed down to, so the names reach an element no source
// expression names.
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

  typedef union packed {
    bit [7:0] whole;
    halves_t split;
  } packed_view_t;

  typedef union tagged {
    int Num;
    string Text;
  } choice_t;

  typedef union tagged packed {
    bit [7:0] Num;
    bit [7:0] Bits;
  } packed_choice_t;

  entry_t entry;
  halves_t halves;
  view_t view;
  packed_view_t packed_view;
  choice_t choice;
  packed_choice_t packed_choice;
  entry_t log [2];
  entry_t by_index [int];
  entry_t pending [$];
  entry_t absent [];

  string entry_text = "unset";
  string halves_text = "unset";
  string view_text = "unset";
  string packed_view_text = "unset";
  string choice_text = "unset";
  string packed_choice_text = "unset";
  string log_text = "unset";
  string by_index_text = "unset";
  string pending_text = "unset";
  string absent_text = "unset";

  initial begin
    entry = '{7, "seven"};
    halves = '{4'd1, 4'd2};
    view.whole = 9;
    packed_view.whole = 8'hC3;
    choice = tagged Text "named";
    packed_choice = tagged Bits 8'd7;
    log[0] = '{1, "one"};
    log[1] = '{2, "two"};
    by_index[10] = '{3, "three"};
    pending.push_back('{4, "four"});

    entry_text = $sformatf("%p", entry);
    halves_text = $sformatf("%p", halves);
    view_text = $sformatf("%p", view);
    packed_view_text = $sformatf("%p", packed_view);
    choice_text = $sformatf("%p", choice);
    packed_choice_text = $sformatf("%p", packed_choice);
    log_text = $sformatf("%p", log);
    by_index_text = $sformatf("%p", by_index);
    pending_text = $sformatf("%p", pending);
    absent_text = $sformatf("%p", absent);
  end

  final begin
    if (entry_text != "'{count:7, label:\"seven\"}")
      $fatal(1, "an unpacked structure printed as '%s'", entry_text);
    if (halves_text != "'{high:1, low:2}")
      $fatal(1, "a packed structure printed as '%s'", halves_text);
    if (view_text != "'{whole:9}")
      $fatal(1, "a union printed as '%s'", view_text);
    if (packed_view_text != "'{whole:195}")
      $fatal(1, "a packed union printed as '%s'", packed_view_text);
    if (choice_text != "'{Text:\"named\"}")
      $fatal(1, "a tagged union printed as '%s'", choice_text);
    if (packed_choice_text != "'{Bits:7}")
      $fatal(1, "a packed tagged union printed as '%s'", packed_choice_text);
    if (log_text != "'{'{count:1, label:\"one\"}, '{count:2, label:\"two\"}}")
      $fatal(1, "structures held by a fixed array printed as '%s'", log_text);
    if (by_index_text != "'{10:'{count:3, label:\"three\"}}")
      $fatal(1, "structures held by an associative array printed as '%s'",
             by_index_text);
    if (pending_text != "'{'{count:4, label:\"four\"}}")
      $fatal(1, "structures held by a queue printed as '%s'", pending_text);
    if (absent_text != "'{}")
      $fatal(1, "an empty container printed as '%s'", absent_text);
    $display("All checks passed");
  end
endmodule

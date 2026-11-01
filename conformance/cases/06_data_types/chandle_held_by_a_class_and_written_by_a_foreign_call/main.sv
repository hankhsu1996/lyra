// A chandle may be used within a class and passed to and from subroutines (LRM
// 6.14), a foreign one among them, where an output or inout chandle crosses
// as a pointer to the C side's `void*` (LRM 35.5.6, Annex H.7.4). A chandle a
// foreign call writes is the pointer that call stored, so it reaches the
// storage the call made; one held by a class property is the chandle it was
// assigned, compares equal to it and keeps it after the variable it came from
// is cleared; and an inout chandle the call sets to NULL comes back null.
module Top;
  import "DPI-C" function void make_cell_into(
      output chandle out, input int seed);
  import "DPI-C" function void add_to_cell(inout chandle through, input int amount);
  import "DPI-C" function void clear_handle(inout chandle through);
  import "DPI-C" function int read_cell(input chandle handle);
  import "DPI-C" function void release_cell(input chandle handle);

  class Holder;
    chandle held;

    function new(chandle given);
      held = given;
    endfunction

    function bit Holds(chandle other);
      return held == other;
    endfunction
  endclass

  chandle made;
  chandle through;
  chandle cleared;
  Holder holder;

  bit made_is_null;
  bit holder_holds_made;
  bit holder_holds_null;
  bit holder_property_is_made;
  bit property_kept_after_clear;
  bit cleared_is_null;
  int read_after_make;
  int read_through_holder;
  int read_after_add;

  initial begin
    made_is_null = 1'b1;
    holder_holds_made = 1'b0;
    holder_holds_null = 1'b1;
    holder_property_is_made = 1'b0;
    property_kept_after_clear = 1'b0;
    cleared_is_null = 1'b0;
    read_after_make = -1;
    read_through_holder = -1;
    read_after_add = -1;

    make_cell_into(made, 5);
    made_is_null = (made == null);
    read_after_make = read_cell(made);

    holder = new(made);
    holder_holds_made = holder.Holds(made);
    holder_holds_null = holder.Holds(null);
    holder_property_is_made = (holder.held === made);
    read_through_holder = read_cell(holder.held);

    through = made;
    add_to_cell(through, 3);
    read_after_add = read_cell(made);

    cleared = made;
    clear_handle(cleared);
    cleared_is_null = (cleared == null);

    made = null;
    property_kept_after_clear = (holder.held != null);

    release_cell(holder.held);
  end

  final begin
    if (made_is_null !== 1'b0)
      $fatal(1, "a chandle an output argument wrote compared to null as %b",
             made_is_null);
    if (read_after_make !== 5)
      $fatal(1, "the written chandle reached a cell holding %0d, expected 5",
             read_after_make);
    if (holder_holds_made !== 1'b1)
      $fatal(1, "a class property compared to its source as %b, expected 1",
             holder_holds_made);
    if (holder_holds_null !== 1'b0)
      $fatal(1, "a class property holding a pointer compared to null as %b",
             holder_holds_null);
    if (holder_property_is_made !== 1'b1)
      $fatal(1, "the property was %b against its source, expected 1",
             holder_property_is_made);
    if (read_through_holder !== 5)
      $fatal(1, "the property reached a cell holding %0d, expected 5",
             read_through_holder);
    if (read_after_add !== 8)
      $fatal(1, "an inout chandle reached a cell holding %0d, expected 8",
             read_after_add);
    if (cleared_is_null !== 1'b1)
      $fatal(1, "an inout chandle the call set to NULL compared to null as %b",
             cleared_is_null);
    if (property_kept_after_clear !== 1'b1)
      $fatal(1, "the property was lost when its source was cleared");
    $display("All checks passed");
  end
endmodule

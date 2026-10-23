// A class handle in a stream contributes the data members of the object it
// refers to, not the handle: in declaration order, those of a base class
// before those its extension adds, and those of the object the handle refers
// to whatever class the handle was declared with. A null handle contributes
// nothing, wherever it stands, and so does one held inside a structure or an
// array; each stream here is left-aligned in a target wide enough for any of
// them (LRM 11.4.14, 11.4.14.1).
module Top;
  class Base;
    byte first = 8'h11;
  endclass

  class Extended extends Base;
    byte second = 8'h22;
  endclass

  typedef struct {
    byte lead;
    Base held;
  } Holder;

  bit [31:0] from_extended;
  bit [31:0] through_base_handle;
  bit [31:0] null_is_skipped;
  bit [31:0] from_structure;
  bit [31:0] from_queue;

  initial begin
    Extended extended;
    Base base;
    Base none;
    Holder holder;
    Base handles[$];
    Base made;

    from_extended = '1;
    through_base_handle = '1;
    null_is_skipped = '1;
    from_structure = '1;
    from_queue = '1;

    extended = new;
    from_extended = {>>{extended}};
    base = extended;
    through_base_handle = {>>{base}};
    none = null;
    null_is_skipped = {>>{none, extended}};
    holder.lead = 8'hAA;
    holder.held = new;
    from_structure = {>>{holder}};
    made = new;
    handles = {null, made};
    from_queue = {>>{handles}};
  end

  final begin
    if (from_extended !== 32'h1122_0000)
      $fatal(1, "from_extended was %h, expected 11220000", from_extended);
    if (through_base_handle !== 32'h1122_0000)
      $fatal(1, "through_base_handle was %h, expected 11220000",
             through_base_handle);
    if (null_is_skipped !== 32'h1122_0000)
      $fatal(1, "null_is_skipped was %h, expected 11220000", null_is_skipped);
    if (from_structure !== 32'hAA11_0000)
      $fatal(1, "from_structure was %h, expected aa110000", from_structure);
    if (from_queue !== 32'h1100_0000)
      $fatal(1, "from_queue was %h, expected 11000000", from_queue);
    $display("All checks passed");
  end
endmodule

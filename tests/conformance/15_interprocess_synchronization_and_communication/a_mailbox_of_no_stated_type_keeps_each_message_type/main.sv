// @reports-none: error
//
// A mailbox declared with no type carries messages of any type, each keeping
// the type it was put with (LRM 15.4.5, 15.4.9). A message is a singular
// expression (LRM 15.4.3), and it is taken or copied only into a variable of
// an equivalent type (LRM 6.22.2): an integral of the same width, signedness
// and states however its range or members are written, and an enumeration or
// a class only into itself.
// Where the variable's type is not equivalent, try_get and try_peek answer
// below zero and the message stays. A class may extend it (LRM 15.2).
class Packet;
  int id;
  function new(int id);
    this.id = id;
  endfunction
endclass

class Counted extends mailbox;
  int puts;
  task put_counted(int value);
    puts++;
    put(value);
  endtask
endclass

module Top;
  typedef struct packed { bit [3:0] hi; bit [3:0] lo; } nibbles_t;
  typedef enum { RED, GREEN } color_t;

  mailbox box;
  Counted counted;

  bit [7:0] byte_in = 8'hA5;
  logic [7:0] logic_in = 8'hA5;
  string text_in = "text";
  Packet packet_in;
  bit [8:1] byte_out = 0;
  nibbles_t nibbles_out = '{4'h0, 4'h0};
  color_t color_out = RED;
  real real_out = 0.0;
  string text_out = "";
  Packet packet_out = null;
  int int_out = 0;
  logic [31:0] logic_out = 0;
  int wrong_int = 99;
  int mismatch_get = 9;
  int mismatch_peek = 9;
  int waited = 0;
  time waited_at = 0;
  int counted_out = 0;

  initial begin
    box = new;
    box.put(byte_in);
    box.put(byte_in);
    box.put(GREEN);
    box.put(2.5);
    box.put(text_in);
    packet_in = new(11);
    box.put(packet_in);
    box.put(12);
    box.put(logic_in + 1);

    box.get(byte_out);
    box.get(nibbles_out);
    box.get(color_out);
    box.get(real_out);
    // A string is not equivalent to an int, so the message stays.
    mismatch_peek = box.try_peek(wrong_int);
    mismatch_get = box.try_get(wrong_int);
    box.get(text_out);
    box.get(packet_out);
    // An int and a 32-bit logic are not equivalent: two states against four.
    if (box.try_get(logic_out) >= 0)
      $fatal(1, "an int message was taken into a 4-state variable");
    box.get(int_out);
    // Self-determined, `logic_in + 1` is 32 unsigned bits of four states.
    box.get(logic_out);
    if (box.num() !== 0) $fatal(1, "%0d messages were left", box.num());

    fork
      begin
        box.get(waited);
        waited_at = $time;
      end
      #3 box.put(21);
    join

    counted = new;
    counted.put_counted(31);
    counted.get(counted_out);
  end

  final begin
    if (byte_out !== 8'hA5) $fatal(1, "bit [8:1] got %h", byte_out);
    if (nibbles_out !== 8'hA5) $fatal(1, "a packed structure got %h", nibbles_out);
    if (color_out !== GREEN) $fatal(1, "an enumeration got %s", color_out.name());
    if (real_out != 2.5) $fatal(1, "a real got %f", real_out);
    if (mismatch_peek >= 0 || mismatch_get >= 0)
      $fatal(1, "try_peek answered %0d and try_get %0d on a mismatch",
             mismatch_peek, mismatch_get);
    if (wrong_int !== 99) $fatal(1, "a mismatched try_get wrote %0d", wrong_int);
    if (text_out != "text") $fatal(1, "a string got '%s'", text_out);
    if (packet_out == null || packet_out.id !== 11)
      $fatal(1, "a class handle did not arrive");
    if (int_out !== 12) $fatal(1, "an int got %0d", int_out);
    if (logic_out !== 32'hA6) $fatal(1, "a 4-state message got %h", logic_out);
    if (waited !== 21 || waited_at !== 3)
      $fatal(1, "a waiting get got %0d at %0t", waited, waited_at);
    if (counted.puts !== 1 || counted_out !== 31)
      $fatal(1, "a class extending mailbox got %0d", counted_out);
    $display("All checks passed");
  end
endmodule

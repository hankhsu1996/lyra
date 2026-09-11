// A hierarchical name reaches a variable holding a handle (LRM 23.8), and a
// property reached through that handle is the one the class the access names
// declares, never the one the object turns out to be (LRM 8.14) -- a derived
// class declaring a property of the base's name hides it from a base handle,
// so a reader that cannot name either class still has to get the base's.
//
// Writing is the same rule in the other direction, and it is what shows the
// access landed on storage rather than on a copy: the declaring scope reads
// its own object back and sees what the outside reader put there.
module Child;
  class Packet;
    int tag = 1;
  endclass

  class Framed extends Packet;
    int tag = 2;
  endclass

  Packet lone;

  int own_read_back = 0;
  int up_tag = 0;

  initial begin
    Framed f = new();
    lone = f;
  end

  initial begin
    #2;
    up_tag = Top.lone.tag;
    Top.lone.tag = 77;
  end

  initial begin
    #4;
    own_read_back = lone.tag;
  end
endmodule

module Top;
  class Record;
    int tag = 1;
  endclass

  class Stamped extends Record;
    int tag = 2;
  endclass

  Record lone;

  int own_read_back = 0;
  int down_tag = 0;

  Child kid ();

  initial begin
    Stamped s = new();
    lone = s;
  end

  initial begin
    #2;
    down_tag = kid.lone.tag;
    kid.lone.tag = 55;
  end

  initial begin
    #4;
    own_read_back = lone.tag;
  end

  final begin
    if (down_tag !== 1)
      $fatal(1, "a downward read of a hidden property saw %0d, expected 1", down_tag);
    if (kid.own_read_back !== 55)
      $fatal(1, "a downward write left the object at %0d, expected 55", kid.own_read_back);
    if (kid.up_tag !== 1)
      $fatal(1, "an upward read of a hidden property saw %0d, expected 1", kid.up_tag);
    if (own_read_back !== 77)
      $fatal(1, "an upward write left the object at %0d, expected 77", own_read_back);
    $display("All checks passed");
  end
endmodule

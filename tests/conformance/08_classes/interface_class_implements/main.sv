// An interface class states a set of behaviors as pure virtual method
// prototypes and holds no data. A class commits to one with the implements
// keyword, which requires it to supply a virtual method implementation for
// every prototype -- an implementation inherited from its base class counts,
// and one implementation may satisfy same-named prototypes of several
// interface classes at once -- while nothing at all is inherited through
// implements. An interface class may extend other interface classes,
// gathering their prototypes. A variable of an interface class type may
// hold any object whose class implements that interface class, and a call
// through such a variable runs the implementation belonging to the object's
// own class (LRM 8.26, 8.26.2, 8.26.5, 8.26.6.1).
package pkg;
  interface class Putter #(type T = int);
    pure virtual function void put(T a);
  endclass

  interface class Getter #(type T = int);
    pure virtual function T get();
  endclass

  interface class PutGet #(type T = int) extends Putter #(T), Getter #(T);
  endclass

  interface class Named;
    pure virtual function int tag();
  endclass

  interface class Tagged;
    pure virtual function int tag();
  endclass

  class Base;
    virtual function int tag();
      return 100;
    endfunction
  endclass
endpackage

module Top;
  class Cell implements pkg::PutGet #(int), pkg::Named;
    int value = 0;

    virtual function void put(int a);
      value = a;
    endfunction

    virtual function int get();
      return value + 1;
    endfunction

    virtual function int tag();
      return 7;
    endfunction
  endclass

  class ByteCell implements pkg::Putter #(byte), pkg::Getter #(byte);
    byte payload = 0;

    virtual function void put(byte a);
      payload = a;
    endfunction

    virtual function byte get();
      return payload;
    endfunction
  endclass

  class Derived extends pkg::Base implements pkg::Named, pkg::Tagged;
  endclass

  class MoreDerived extends Derived;
    virtual function int tag();
      return 200;
    endfunction
  endclass

  int cell_direct_get;
  int cell_via_putter;
  int cell_via_getter;
  int cell_via_putget;
  int cell_tag_direct;
  int cell_tag_via_named;
  byte byte_get_value;
  int derived_tag_direct;
  int derived_tag_via_named;
  int derived_tag_via_tagged;
  int more_tag_direct;
  int more_tag_via_named;
  int more_tag_via_tagged;

  initial begin
    Cell c;
    ByteCell bc;
    Derived d;
    MoreDerived md;
    pkg::Putter #(int) put_ref;
    pkg::Getter #(int) get_ref;
    pkg::PutGet #(int) putget_ref;
    pkg::Named named_ref;
    pkg::Tagged tagged_ref;
    pkg::Getter #(byte) byte_get_ref;

    c = new;
    c.put(41);
    cell_direct_get = c.get();

    put_ref = c;
    put_ref.put(50);
    cell_via_putter = c.get();

    get_ref = c;
    cell_via_getter = get_ref.get();

    putget_ref = c;
    putget_ref.put(200);
    cell_via_putget = putget_ref.get();

    named_ref = c;
    cell_tag_direct = c.tag();
    cell_tag_via_named = named_ref.tag();

    bc = new;
    bc.put(8'sd12);
    byte_get_ref = bc;
    byte_get_value = byte_get_ref.get();

    d = new;
    derived_tag_direct = d.tag();
    named_ref = d;
    derived_tag_via_named = named_ref.tag();
    tagged_ref = d;
    derived_tag_via_tagged = tagged_ref.tag();

    md = new;
    more_tag_direct = md.tag();
    named_ref = md;
    more_tag_via_named = named_ref.tag();
    tagged_ref = md;
    more_tag_via_tagged = tagged_ref.tag();
  end

  final begin
    if (cell_direct_get !== 42)
      $fatal(1, "cell_direct_get was %0d, expected 42", cell_direct_get);
    if (cell_via_putter !== 51)
      $fatal(1, "cell_via_putter was %0d, expected 51", cell_via_putter);
    if (cell_via_getter !== 51)
      $fatal(1, "cell_via_getter was %0d, expected 51", cell_via_getter);
    if (cell_via_putget !== 201)
      $fatal(1, "cell_via_putget was %0d, expected 201", cell_via_putget);
    if (cell_tag_direct !== 7)
      $fatal(1, "cell_tag_direct was %0d, expected 7", cell_tag_direct);
    if (cell_tag_via_named !== 7)
      $fatal(1, "cell_tag_via_named was %0d, expected 7",
             cell_tag_via_named);
    if (byte_get_value !== 12)
      $fatal(1, "byte_get_value was %0d, expected 12", byte_get_value);
    if (derived_tag_direct !== 100)
      $fatal(1, "derived_tag_direct was %0d, expected 100",
             derived_tag_direct);
    if (derived_tag_via_named !== 100)
      $fatal(1, "derived_tag_via_named was %0d, expected 100",
             derived_tag_via_named);
    if (derived_tag_via_tagged !== 100)
      $fatal(1, "derived_tag_via_tagged was %0d, expected 100",
             derived_tag_via_tagged);
    if (more_tag_direct !== 200)
      $fatal(1, "more_tag_direct was %0d, expected 200", more_tag_direct);
    if (more_tag_via_named !== 200)
      $fatal(1, "more_tag_via_named was %0d, expected 200",
             more_tag_via_named);
    if (more_tag_via_tagged !== 200)
      $fatal(1, "more_tag_via_tagged was %0d, expected 200",
             more_tag_via_tagged);
    $display("All checks passed");
  end
endmodule

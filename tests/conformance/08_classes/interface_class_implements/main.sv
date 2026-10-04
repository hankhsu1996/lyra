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
// own class. A subclass implicitly implements every interface class its
// superclass implements, and that holds wherever each class is declared: an
// interface class of one package may extend one of another, a class of a third
// may implement it, and a class extending that one elsewhere is a value of all
// of them, reached through a handle of any of them or of its superclass. A
// handle of one interface class is cast to another the object implements
// (LRM 8.26, 8.26.2, 8.26.5, 8.26.6.1, 26.3).
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

package wide_pkg;
  interface class Sized extends pkg::Named;
    pure virtual function int size();
  endclass
endpackage

package stock_pkg;
  class Stock implements wide_pkg::Sized;
    virtual function int tag();
      return 30;
    endfunction

    virtual function int size();
      return 3;
    endfunction

    virtual function int scaled(int by);
      return 3 * by;
    endfunction
  endclass
endpackage

module Top;
  interface class Scalable;
    pure virtual function int scaled(int by);
  endclass

  // Everything but `size` is answered by what it inherits, and two of the
  // interface classes it is a value of it never names.
  class Crate extends stock_pkg::Stock implements pkg::Tagged, Scalable;
    virtual function int size();
      return 9;
    endfunction
  endclass

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
  int crate_tag_via_named;
  int crate_tag_via_sized;
  int crate_size_via_sized;
  int crate_tag_via_tagged;
  int crate_scaled_via_scalable;
  int crate_size_through_stock;
  int crate_tag_after_cast;
  bit crate_cast_succeeded;

  initial begin
    Crate crate;
    stock_pkg::Stock as_stock;
    wide_pkg::Sized sized_ref;
    wide_pkg::Sized sized_through_stock;
    pkg::Named crate_named_ref;
    pkg::Named named_after_cast;
    pkg::Tagged crate_tagged_ref;
    Scalable scalable_ref;

    crate = new;
    crate_named_ref = crate;
    crate_tag_via_named = crate_named_ref.tag();
    sized_ref = crate;
    crate_tag_via_sized = sized_ref.tag();
    crate_size_via_sized = sized_ref.size();
    crate_tagged_ref = crate;
    crate_tag_via_tagged = crate_tagged_ref.tag();
    scalable_ref = crate;
    crate_scaled_via_scalable = scalable_ref.scaled(4);
    as_stock = crate;
    sized_through_stock = as_stock;
    crate_size_through_stock = sized_through_stock.size();
    crate_cast_succeeded = $cast(named_after_cast, crate_tagged_ref);
    crate_tag_after_cast = named_after_cast.tag();
  end

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
    if (crate_tag_via_named !== 30)
      $fatal(1, "crate_tag_via_named was %0d, expected 30",
             crate_tag_via_named);
    if (crate_tag_via_sized !== 30)
      $fatal(1, "crate_tag_via_sized was %0d, expected 30",
             crate_tag_via_sized);
    if (crate_size_via_sized !== 9)
      $fatal(1, "crate_size_via_sized was %0d, expected 9",
             crate_size_via_sized);
    if (crate_tag_via_tagged !== 30)
      $fatal(1, "crate_tag_via_tagged was %0d, expected 30",
             crate_tag_via_tagged);
    if (crate_scaled_via_scalable !== 12)
      $fatal(1, "crate_scaled_via_scalable was %0d, expected 12",
             crate_scaled_via_scalable);
    // The conversion is written against the superclass and made on an object
    // of the class extending it.
    if (crate_size_through_stock !== 9)
      $fatal(1, "crate_size_through_stock was %0d, expected 9",
             crate_size_through_stock);
    if (crate_cast_succeeded !== 1)
      $fatal(1, "crate_cast_succeeded was %0d, expected 1",
             crate_cast_succeeded);
    if (crate_tag_after_cast !== 30)
      $fatal(1, "crate_tag_after_cast was %0d, expected 30",
             crate_tag_after_cast);
    $display("All checks passed");
  end
endmodule

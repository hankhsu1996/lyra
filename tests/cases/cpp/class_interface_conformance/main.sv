// LRM 8.26 interface classes. A set of related classes shares a common set
// of behaviors through interface class contracts: only pure virtual method
// prototypes and type-level declarations, no instance storage, not itself
// constructible. Non-interface classes commit to satisfying an interface
// through `implements`; an interface class extends other interface classes
// through `extends`. Both keywords name the same relation at the object
// model layer (aggregate these contracts) and land in one HIR / MIR field.
// Covers:
//   - Interface class in a package, reached from a module class (cross-unit).
//   - Regular class implementing multiple interface classes.
//   - Interface class extending multiple interface classes (LRM 8.26.2).
//   - Parameterized interface class specialization (LRM 8.25 + 8.26).
//   - Handle upcast to an interface class handle (LRM 8.26.5).
//   - Virtual dispatch through an interface class handle (LRM 8.20 + 8.26).
//   - One method satisfying multiple same-name pure virtual slots (LRM
//     8.26.6.1 name conflict resolution).
//   - Inherited satisfaction: the concrete base provides the implementation
//     (LRM 8.26.2 -- `virtual` on the base method carries through).
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

  // Same-signature contract as Named: one inherited implementation satisfies
  // both (LRM 8.26.6.1), exercising adapter deduplication by implementation.
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
    byte b = 0;
    virtual function void put(byte a);
      b = a;
    endfunction
    virtual function byte get();
      return b;
    endfunction
  endclass

  // Inherited satisfaction (LRM 8.26.2): Base has `virtual function int
  // tag()`, and `pkg::Named` / `pkg::Tagged` each require `pure virtual
  // function int tag()`. The implementation is inherited from Base; Derived
  // does not redefine it, and one inherited method satisfies both same-name
  // contracts (LRM 8.26.6.1).
  class Derived extends pkg::Base implements pkg::Named, pkg::Tagged;
  endclass

  // A further-derived class overrides the inherited-satisfied method. Dispatch
  // through every handle -- the concrete base, each interface, and the
  // subclass -- must reach the most-derived override.
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

  int byte_get_value;
  int derived_tag_direct;
  int derived_tag_via_named;
  int derived_tag_via_tagged;
  int mderived_tag_direct;
  int mderived_tag_via_named;
  int mderived_tag_via_tagged;

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

    // A further-derived override dispatches to the most-derived method through
    // every handle -- direct and each interface (LRM 8.26.2: a subclass of an
    // implementing class also implements the interface).
    md = new;
    mderived_tag_direct = md.tag();
    named_ref = md;
    mderived_tag_via_named = named_ref.tag();
    tagged_ref = md;
    mderived_tag_via_tagged = tagged_ref.tag();
  end
endmodule

// LRM 8.9 static class properties + LRM 8.10 static class methods.
// Registry declares a static counter (initialized once at design init, LRM
// 10.5, before any initial procedure) and a static factory that reads and
// writes it. Derived inherits Registry unchanged; every reference form
// visits the same cell -- `Registry::counter` (type-qualified),
// `Derived::counter` (owner-qualified, still the base's cell),
// `h.counter` (through a handle whose value is unused), and inside a
// static method's body as a bare identifier. The `initial` block runs
// after the static initializer, so `counter_at_start` observes the
// source-declared 0 rather than any later mutation.
module Top;
  class Registry;
    static int counter = 0;
    static function int next_id();
      counter = counter + 1;
      return counter;
    endfunction
  endclass

  class Derived extends Registry;
  endclass

  int counter_at_start;
  int first_id;
  int second_id_via_qualified;
  int third_id_via_handle;
  int fourth_id_via_derived;
  int counter_after_all;
  int counter_via_derived;

  initial begin
    Registry r;
    Derived d;
    counter_at_start = Registry::counter;
    first_id = Registry::next_id();
    second_id_via_qualified = Registry::next_id();
    r = new;
    third_id_via_handle = r.next_id();
    d = new;
    fourth_id_via_derived = Derived::next_id();
    counter_after_all = Registry::counter;
    counter_via_derived = Derived::counter;
  end
endmodule

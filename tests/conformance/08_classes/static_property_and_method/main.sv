// A property declared static is one variable owned by the class rather than
// one per object, created and initialized once, and every way of naming it
// reaches that one variable: qualified by the class name, qualified by the
// name of a subclass that inherits it, reached through a handle, or written
// bare inside a static method. A property not declared static is one
// variable per object, so writes through two handles do not meet. A static
// method needs no object and may likewise be called on the class, on a
// subclass, or through a handle. Because a static property needs no object
// it is an ordinary operand of a continuous assignment, which re-evaluates
// whenever the operand changes (LRM 8.9, 8.10, 8.23, 10.3.2).
module Top;
  class Registry;
    static int counter = 5;
    int instance_count = 0;

    static function int next_id();
      counter = counter + 1;
      return counter;
    endfunction

    function int bump();
      instance_count = instance_count + 1;
      return instance_count;
    endfunction
  endclass

  class Derived extends Registry;
  endclass

  int counter_at_start;
  int first_id;
  int second_id;
  int id_through_handle;
  int id_through_subclass;
  int counter_after_all;
  int counter_via_subclass;
  int r_first;
  int r_second;
  int r2_first;

  int counter_continuous;
  assign counter_continuous = Registry::counter;

  initial begin
    Registry r;
    Registry r2;

    counter_at_start = Registry::counter;
    first_id = Registry::next_id();
    second_id = Registry::next_id();

    r = new;
    id_through_handle = r.next_id();
    id_through_subclass = Derived::next_id();

    counter_after_all = Registry::counter;
    counter_via_subclass = Derived::counter;

    r_first = r.bump();
    r_second = r.bump();
    r2 = new;
    r2_first = r2.bump();
  end

  final begin
    if (counter_at_start !== 5)
      $fatal(1, "counter_at_start was %0d, expected 5", counter_at_start);
    if (first_id !== 6) $fatal(1, "first_id was %0d, expected 6", first_id);
    if (second_id !== 7)
      $fatal(1, "second_id was %0d, expected 7", second_id);
    if (id_through_handle !== 8)
      $fatal(1, "id_through_handle was %0d, expected 8", id_through_handle);
    if (id_through_subclass !== 9)
      $fatal(1, "id_through_subclass was %0d, expected 9",
             id_through_subclass);
    if (counter_after_all !== 9)
      $fatal(1, "counter_after_all was %0d, expected 9", counter_after_all);
    if (counter_via_subclass !== 9)
      $fatal(1, "counter_via_subclass was %0d, expected 9",
             counter_via_subclass);
    if (r_first !== 1) $fatal(1, "r_first was %0d, expected 1", r_first);
    if (r_second !== 2) $fatal(1, "r_second was %0d, expected 2", r_second);
    if (r2_first !== 1) $fatal(1, "r2_first was %0d, expected 1", r2_first);
    if (counter_continuous !== 9)
      $fatal(1, "counter_continuous was %0d, expected 9",
             counter_continuous);
    $display("All checks passed");
  end
endmodule

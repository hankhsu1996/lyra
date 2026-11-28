// A method is called on whatever an expression yields (LRM 7.12, Syntax 7-5),
// and no clause asks that the object of a method changing it be a variable
// (LRM 6.16, 7.5.3, 7.9, 7.10.2, 7.12.2). Where the object is a value -- what a
// function, a class method or a virtual method returns, a member or an element
// of such a value, a cast, a conditional -- the method changes a copy of that
// value made once for the call, which nothing reads afterwards: a popped
// element is still answered, the value is computed once, and the variable it
// was copied from is not changed. Where the object names storage, as a property
// of the object a returned handle reaches does, the change lands there.
module Top;
  typedef int queue_t[$];
  typedef int dynamic_t[];
  typedef int fixed_t[3];
  typedef int associative_t[int];
  typedef queue_t queues_t[$];

  typedef struct {
    queue_t q;
    int tag;
  } holder_t;

  int calls;

  class Maker;
    queue_t kept;

    function queue_t made();
      calls = calls + 1;
      return kept;
    endfunction

    virtual function queue_t remade();
      return '{0};
    endfunction
  endclass

  class Remaker extends Maker;
    virtual function queue_t remade();
      calls = calls + 1;
      return kept;
    endfunction
  endclass

  class Owner;
    queue_t q;
  endclass

  queue_t source_q;
  queue_t other_q;
  dynamic_t source_d;
  fixed_t source_f;
  associative_t source_a;
  string source_s;
  Owner owner;
  Maker maker;
  Remaker remaker;
  bit pick_first;

  function automatic queue_t gq();
    calls = calls + 1;
    return source_q;
  endfunction

  function automatic dynamic_t gd();
    calls = calls + 1;
    return source_d;
  endfunction

  function automatic fixed_t gf();
    calls = calls + 1;
    return source_f;
  endfunction

  function automatic associative_t ga();
    calls = calls + 1;
    return source_a;
  endfunction

  function automatic holder_t gs();
    holder_t h;
    calls = calls + 1;
    h.q = source_q;
    h.tag = 1;
    return h;
  endfunction

  function automatic queues_t gqq();
    queues_t r;
    calls = calls + 1;
    r.push_back(source_q);
    return r;
  endfunction

  function automatic string gstr();
    calls = calls + 1;
    return source_s;
  endfunction

  function automatic Owner go();
    calls = calls + 1;
    return owner;
  endfunction

  int popped_front;
  int popped_back;
  int on_pop_front;
  int on_pop_back;
  int on_push_back;
  int on_push_front;
  int on_insert;
  int on_delete_at;
  int on_delete_all;
  int on_dynamic_delete;
  int on_associative_delete;
  int on_string;
  int on_queue_sort;
  int on_queue_rsort;
  int on_queue_reverse;
  int on_queue_sort_with;
  int on_dynamic_sort;
  int on_dynamic_rsort;
  int on_dynamic_reverse;
  int on_fixed_sort;
  int on_fixed_rsort;
  int on_fixed_reverse;
  int from_member;
  int on_member;
  int from_element;
  int on_element;
  int from_class_method;
  int on_class_method;
  int kept_after_class_method;
  int from_virtual;
  int on_virtual;
  int from_cast;
  int from_conditional;
  int on_handle_property;

  initial begin
    // Every target starts at a value its own check rejects, so a check that
    // never ran cannot pass as one that answered correctly.
    popped_front = -1;
    popped_back = -1;
    from_member = -1;
    from_element = -1;
    on_element = -1;
    from_class_method = -1;
    from_virtual = -1;
    from_cast = -1;
    from_conditional = -1;
    on_pop_front = -1;
    on_pop_back = -1;
    on_push_back = -1;
    on_push_front = -1;
    on_insert = -1;
    on_delete_at = -1;
    on_delete_all = -1;
    on_dynamic_delete = -1;
    on_associative_delete = -1;
    on_string = -1;
    on_queue_sort = -1;
    on_queue_rsort = -1;
    on_queue_reverse = -1;
    on_queue_sort_with = -1;
    on_dynamic_sort = -1;
    on_dynamic_rsort = -1;
    on_dynamic_reverse = -1;
    on_fixed_sort = -1;
    on_fixed_rsort = -1;
    on_fixed_reverse = -1;
    on_member = -1;
    on_class_method = -1;
    kept_after_class_method = -1;
    on_virtual = -1;
    on_handle_property = -1;

    source_q = '{3, 1, 2};
    source_d = '{3, 1, 2};
    source_f = '{3, 1, 2};
    source_a[5] = 50;
    source_a[7] = 70;
    source_s = "abc";

    calls = 0;
    popped_front = gq().pop_front();
    on_pop_front = calls;
    calls = 0;
    popped_back = gq().pop_back();
    on_pop_back = calls;

    calls = 0;
    gq().push_back(9);
    on_push_back = calls;
    calls = 0;
    gq().push_front(9);
    on_push_front = calls;
    calls = 0;
    gq().insert(1, 9);
    on_insert = calls;
    calls = 0;
    gq().delete(0);
    on_delete_at = calls;
    calls = 0;
    gq().delete();
    on_delete_all = calls;
    calls = 0;
    gd().delete();
    on_dynamic_delete = calls;
    calls = 0;
    ga().delete(5);
    on_associative_delete = calls;

    calls = 0;
    gstr().putc(0, "z");
    gstr().itoa(5);
    gstr().hextoa(255);
    gstr().octtoa(8);
    gstr().bintoa(3);
    gstr().realtoa(1.5);
    on_string = calls;

    calls = 0;
    gq().sort();
    on_queue_sort = calls;
    calls = 0;
    gq().rsort();
    on_queue_rsort = calls;
    calls = 0;
    gq().reverse();
    on_queue_reverse = calls;
    calls = 0;
    gq().sort() with (-item);
    on_queue_sort_with = calls;
    calls = 0;
    gd().sort();
    on_dynamic_sort = calls;
    calls = 0;
    gd().rsort();
    on_dynamic_rsort = calls;
    calls = 0;
    gd().reverse();
    on_dynamic_reverse = calls;
    calls = 0;
    gf().sort();
    on_fixed_sort = calls;
    calls = 0;
    gf().rsort();
    on_fixed_rsort = calls;
    calls = 0;
    gf().reverse();
    on_fixed_reverse = calls;

    calls = 0;
    from_member = gs().q.pop_back();
    on_member = calls;
    calls = 0;
    from_element = gqq()[0].pop_front();
    on_element = calls;

    maker = new;
    maker.kept = '{4, 5};
    calls = 0;
    from_class_method = maker.made().pop_front();
    on_class_method = calls;
    kept_after_class_method = maker.kept.size();

    remaker = new;
    remaker.kept = '{6, 7};
    maker = remaker;
    calls = 0;
    from_virtual = maker.remade().pop_back();
    on_virtual = calls;

    from_cast = queue_t'(source_q).pop_back();
    pick_first = 1;
    from_conditional = (pick_first ? source_q : other_q).pop_front();

    // A property of the object a returned handle reaches is storage, so the
    // change lands in the object.
    owner = new;
    owner.q = '{8};
    calls = 0;
    go().q.push_back(9);
    on_handle_property = calls;
  end

  final begin
    if (popped_front !== 3)
      $fatal(1, "gq().pop_front() was %0d, expected 3", popped_front);
    if (popped_back !== 2)
      $fatal(1, "gq().pop_back() was %0d, expected 2", popped_back);
    if (on_pop_front !== 1 || on_pop_back !== 1)
      $fatal(1, "pop_front ran its object %0d times and pop_back %0d, expected 1",
             on_pop_front, on_pop_back);
    if (on_push_back !== 1 || on_push_front !== 1 || on_insert !== 1)
      $fatal(1, "push_back, push_front, insert ran their object %0d %0d %0d times",
             on_push_back, on_push_front, on_insert);
    if (on_delete_at !== 1 || on_delete_all !== 1 || on_dynamic_delete !== 1 ||
        on_associative_delete !== 1)
      $fatal(1, "the deletes ran their object %0d %0d %0d %0d times",
             on_delete_at, on_delete_all, on_dynamic_delete,
             on_associative_delete);
    if (on_queue_sort !== 1 || on_queue_rsort !== 1 ||
        on_queue_reverse !== 1 || on_queue_sort_with !== 1)
      $fatal(1, "a queue's orderings ran their object %0d %0d %0d %0d times",
             on_queue_sort, on_queue_rsort, on_queue_reverse,
             on_queue_sort_with);
    if (on_dynamic_sort !== 1 || on_dynamic_rsort !== 1 ||
        on_dynamic_reverse !== 1)
      $fatal(1, "a dynamic array's orderings ran their object %0d %0d %0d times",
             on_dynamic_sort, on_dynamic_rsort, on_dynamic_reverse);
    if (on_fixed_sort !== 1 || on_fixed_rsort !== 1 || on_fixed_reverse !== 1)
      $fatal(1, "a fixed-size array's orderings ran their object %0d %0d %0d times",
             on_fixed_sort, on_fixed_rsort, on_fixed_reverse);

    if (source_q.size() !== 3 || source_q[0] !== 3 || source_q[1] !== 1 ||
        source_q[2] !== 2)
      $fatal(1, "source_q was %p, expected '{3, 1, 2} unchanged", source_q);
    if (source_d.size() !== 3 || source_d[0] !== 3 || source_d[1] !== 1 ||
        source_d[2] !== 2)
      $fatal(1, "source_d was %p, expected '{3, 1, 2} unchanged", source_d);
    if (source_f[0] !== 3 || source_f[1] !== 1 || source_f[2] !== 2)
      $fatal(1, "source_f was %p, expected '{3, 1, 2} unchanged", source_f);
    if (source_a.num() !== 2 || !source_a.exists(5))
      $fatal(1, "source_a was %p, expected both entries kept", source_a);
    if (on_string !== 6)
      $fatal(1, "a string's six changing methods ran their object %0d times, expected 6",
             on_string);
    if (source_s != "abc")
      $fatal(1, "source_s was %s, expected abc unchanged", source_s);

    if (from_member !== 2)
      $fatal(1, "gs().q.pop_back() was %0d, expected 2", from_member);
    if (on_member !== 1)
      $fatal(1, "a member's object ran %0d times, expected 1", on_member);
    if (from_element !== 3)
      $fatal(1, "gqq()[0].pop_front() was %0d, expected 3", from_element);
    if (on_element !== 1)
      $fatal(1, "an element's object ran %0d times, expected 1", on_element);
    if (from_class_method !== 4)
      $fatal(1, "maker.made().pop_front() was %0d, expected 4",
             from_class_method);
    if (on_class_method !== 1)
      $fatal(1, "a class method's object ran %0d times, expected 1",
             on_class_method);
    if (kept_after_class_method !== 2)
      $fatal(1, "maker.kept had %0d elements, expected 2 unchanged",
             kept_after_class_method);
    if (remaker.kept.size() !== 2)
      $fatal(1, "remaker.kept was %p, expected '{6, 7} unchanged",
             remaker.kept);
    if (from_virtual !== 7)
      $fatal(1, "maker.remade().pop_back() was %0d, expected 7", from_virtual);
    if (on_virtual !== 1)
      $fatal(1, "a virtual method's object ran %0d times, expected 1",
             on_virtual);
    if (from_cast !== 2)
      $fatal(1, "a cast's pop_back() was %0d, expected 2", from_cast);
    if (from_conditional !== 3)
      $fatal(1, "a conditional's pop_front() was %0d, expected 3",
             from_conditional);

    if (on_handle_property !== 1)
      $fatal(1, "a handle's property ran its handle %0d times, expected 1",
             on_handle_property);
    if (owner.q.size() !== 2 || owner.q[0] !== 8 || owner.q[1] !== 9)
      $fatal(1, "owner.q was %p, expected '{8, 9}", owner.q);

    $display("All checks passed");
  end
endmodule

// @reports: warning count=2
//
// An associative array may be indexed by any type for which equality is
// defined, the dynamically sized types included: an unpacked structure, one
// holding a string and a queue, and a dynamic array (LRM 7.8.5). Two indices
// name the same entry exactly when they are equal, so an index built apart
// from the one that made an entry reaches it. Where the index type has no
// relational operator the order of the entries is left to the tool, but each
// traversal visits every entry once. An index holding x or z in any of its
// elements is invalid: a write through it is ignored and a read returns the
// array's default, each with a warning (LRM 7.8.5, 7.8.6). An empty string, an
// empty queue or a null handle in an index leaves it valid.
module Top;
  class Tag;
  endclass

  typedef struct {
    logic [3:0] code;
    string name;
    int parts [$];
    Tag tag;
  } key_t;

  typedef int list_t [];

  int by_key [key_t];
  string by_list [list_t];

  key_t first_key, second_key, rebuilt_key, empty_key, unknown_key, walked;
  list_t first_list, second_list, rebuilt_list, walked_list;
  Tag shared;
  int visits;
  int total;
  int unknown_read;

  initial begin
    shared = new;
    first_key = '{code: 4'd1, name: "a name long enough to own its storage", parts: '{1, 2},
                  tag: shared};
    second_key = '{code: 4'd1, name: "a name long enough to own its storage", parts: '{1, 3},
                   tag: shared};
    empty_key = '{code: 4'd0, name: "", parts: '{}, tag: null};

    by_key[first_key] = 10;
    by_key[second_key] = 20;
    by_key[empty_key] = 30;
    if (by_key.num() !== 3) $fatal(1, "three distinct indices made %0d entries", by_key.num());

    rebuilt_key.code = 4'd1;
    rebuilt_key.name = {"a name long enough", " to own its storage"};
    rebuilt_key.parts.push_back(1);
    rebuilt_key.parts.push_back(2);
    rebuilt_key.tag = shared;
    if (!by_key.exists(rebuilt_key)) $fatal(1, "an equal index built apart did not exist");
    if (by_key[rebuilt_key] !== 10)
      $fatal(1, "an equal index built apart read %0d", by_key[rebuilt_key]);
    by_key[rebuilt_key] += 5;
    if (by_key.num() !== 3) $fatal(1, "a write through an equal index made a new entry");
    if (by_key[first_key] !== 15) $fatal(1, "the entry an equal index wrote read %0d",
                                         by_key[first_key]);

    rebuilt_key.tag = new;
    if (by_key.exists(rebuilt_key))
      $fatal(1, "an index differing in its handle named an existing entry");

    if (by_key[empty_key] !== 30)
      $fatal(1, "an index of empty members read %0d", by_key[empty_key]);

    visits = 0;
    total = 0;
    if (by_key.first(walked)) begin
      do begin
        visits++;
        total += by_key[walked];
      end while (by_key.next(walked));
    end
    if (visits !== 3 || total !== 65)
      $fatal(1, "a traversal made %0d visits summing %0d", visits, total);

    visits = 0;
    foreach (by_key[k]) visits++;
    if (visits !== 3) $fatal(1, "foreach made %0d visits", visits);

    by_key.delete(second_key);
    if (by_key.num() !== 2 || by_key.exists(second_key))
      $fatal(1, "deleting an entry left %0d entries", by_key.num());

    first_list = '{4, 5, 6};
    second_list = '{4, 5};
    by_list[first_list] = "three elements";
    by_list[second_list] = "two elements";
    rebuilt_list = new[3];
    rebuilt_list[0] = 4;
    rebuilt_list[1] = 5;
    rebuilt_list[2] = 6;
    if (by_list.num() !== 2) $fatal(1, "two dynamic array indices made %0d entries", by_list.num());
    if (by_list[rebuilt_list] != "three elements")
      $fatal(1, "an equal dynamic array index read '%s'", by_list[rebuilt_list]);
    visits = 0;
    if (by_list.first(walked_list)) begin
      do visits++; while (by_list.next(walked_list));
    end
    if (visits !== 2) $fatal(1, "a traversal of dynamic array indices made %0d visits", visits);

    unknown_key = first_key;
    unknown_key.code = 4'b1x01;
    by_key[unknown_key] = 77;
    if (by_key.num() !== 2) $fatal(1, "a write through an unknown index made an entry");
    unknown_read = 5;
    unknown_read = by_key[unknown_key];
    if (unknown_read !== 0) $fatal(1, "a read through an unknown index gave %0d", unknown_read);

    $display("All checks passed");
  end
endmodule

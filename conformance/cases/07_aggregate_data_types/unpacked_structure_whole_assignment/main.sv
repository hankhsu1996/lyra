// A structure can be assigned as a whole, and passed to and returned from a
// subroutine as a whole. Either way the value carries every member, however
// that member is stored -- a nested structure, a string, an unpacked array --
// and it carries them by value, so writing to one of the two structures
// afterwards leaves the other as it was. A structure argument passed by value
// is a copy in the same sense, so what the subroutine writes to its own
// argument never reaches the caller's variable (LRM 7.2.2, 7.7, 11.2.2,
// 13.5.1).
module Top;
  typedef struct {
    string first;
    string last;
  } name_t;

  typedef struct {
    name_t name;
    int id;
    int history [2];
  } record_t;

  function automatic record_t promoted(record_t original);
    record_t updated;
    updated = original;
    updated.id = original.id + 100;
    original.id = -1;
    original.name.first = "clobbered";
    return updated;
  endfunction

  record_t source;
  record_t copy;
  record_t returned;

  name_t names [2];

  initial begin
    source.name.first = "ann";
    source.name.last = "lee";
    source.id = 3;
    source.history[0] = 7;
    source.history[1] = 8;

    copy = source;
    source.name.first = "bob";
    source.id = 9;
    source.history[0] = 99;

    returned = promoted(source);

    names[0].first = "cat";
    names[0].last = "dog";
    names[1] = names[0];
    names[0].last = "fox";
  end

  final begin
    if (copy.name.first != "ann")
      $fatal(1, "copy.name.first was '%s', expected 'ann'", copy.name.first);
    if (copy.name.last != "lee")
      $fatal(1, "copy.name.last was '%s', expected 'lee'", copy.name.last);
    if (copy.id !== 3)
      $fatal(1, "copy.id was %0d, expected 3", copy.id);
    if (copy.history[0] !== 7)
      $fatal(1, "copy.history[0] was %0d, expected 7", copy.history[0]);
    if (copy.history[1] !== 8)
      $fatal(1, "copy.history[1] was %0d, expected 8", copy.history[1]);

    if (returned.id !== 109)
      $fatal(1, "returned.id was %0d, expected 109", returned.id);
    if (returned.name.first != "bob")
      $fatal(1, "returned.name.first was '%s', expected 'bob'",
             returned.name.first);
    if (returned.history[1] !== 8)
      $fatal(1, "returned.history[1] was %0d, expected 8",
             returned.history[1]);

    if (source.id !== 9)
      $fatal(1, "source.id was %0d, expected 9", source.id);
    if (source.name.first != "bob")
      $fatal(1, "source.name.first was '%s', expected 'bob'",
             source.name.first);

    if (names[1].first != "cat")
      $fatal(1, "names[1].first was '%s', expected 'cat'", names[1].first);
    if (names[1].last != "dog")
      $fatal(1, "names[1].last was '%s', expected 'dog'", names[1].last);
    $display("All checks passed");
  end
endmodule

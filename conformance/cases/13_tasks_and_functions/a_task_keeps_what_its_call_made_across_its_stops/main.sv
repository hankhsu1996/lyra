// What a task call's arguments evaluate to is copied in when the call is made
// (LRM 13.5.1), so a value made only for the call -- a string, a class handle, a
// structure holding a queue -- is what the task reads after it has stopped and
// resumed. A task disabled while stopped ends there (LRM 9.6.2), and what its
// outputs then hold is not specified, so this reads neither.
class Box;
  int value;
  function new(int v);
    value = v;
  endfunction
endclass

typedef struct {
  string name;
  int items[$];
  Box box;
} record_t;

function automatic string text(int i);
  return $sformatf("a string long enough to own storage %0d", i);
endfunction

function automatic Box box_of(int v);
  Box b = new(v);
  return b;
endfunction

function automatic record_t record_of(int v);
  record_t r;
  r.name = text(v);
  r.items.push_back(v);
  r.box = box_of(v);
  return r;
endfunction

task automatic reads_after_stopping(
    string s, Box b, record_t r, output string joined, output int total);
  #2;
  joined = {s, r.name};
  total = b.value + r.box.value + r.items[0];
endtask

module Top;
  string joined;
  int total;
  string never;
  int never_total;

  initial begin
    reads_after_stopping({text(1), "!"}, box_of(10), record_of(20), joined,
                         total);
    fork : parked
      reads_after_stopping({text(2), "!"}, box_of(30), record_of(40), never,
                           never_total);
    join_none
    #1 disable parked;
  end

  final begin
    if (joined != {text(1), "!", text(20)})
      $fatal(1, "joined was %s", joined);
    if (total !== 50) $fatal(1, "total was %0d, expected 50", total);
    $display("All checks passed");
  end
endmodule

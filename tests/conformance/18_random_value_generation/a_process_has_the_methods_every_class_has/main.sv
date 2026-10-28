// process is a class (LRM 9.7), and every class has randomize(),
// pre_randomize() and post_randomize() (LRM 18.6.1, 18.6.2) and the mode
// methods (LRM 18.8, 18.9). A process declares no random variable and no
// constraint and cannot be extended, so its hooks do nothing and randomize()
// solves nothing: it answers 1, and with an inline constraint on a state
// variable it answers whether that constraint holds, leaving the variable as it
// was (LRM 18.7). A name in the inline constraint is looked up in the class
// first, so status() there is the process's own. Turning the object's
// constraints off leaves an inline one in force, since constraint_mode() acts
// on the blocks a class declares. Its srandom() seeds the generator the
// process draws from (LRM 18.13.3).
//
// A process that has finished is still an object while a handle holds it (LRM
// 8.30, 9.7), so it still has every one of these methods, and a weak reference
// to it still answers it.
//
// A handle to a process may be a random object of another class (LRM 18.4),
// randomized with that class's object and answering for none of its variables;
// one holding null is not randomized. And $cast between two process handles is
// the assignment of one class handle to another of the same class (LRM 8.16).
class Holder;
  rand bit [3:0] v;
  rand process first;
  rand process second;
  int pre = 0;
  int post = 0;

  function void pre_randomize();
    pre++;
  endfunction

  function void post_randomize();
    post++;
  endfunction
endclass

module Top;
  int x = 0;
  int plain = -1;
  int holds = -1;
  int fails = -1;
  int after_modes = -1;
  int x_after = -1;
  int running = -1;
  int not_finished = -1;
  int unsigned drawn_first = 0;
  int unsigned drawn_again = 1;
  bit same_self = 0;
  int finished_status = -1;
  int finished_randomize = -1;
  bit weak_answers = 0;
  int with_null = -1;
  int with_processes = -1;
  int hooks_ran = -1;
  int cast_onto = -1;
  bit cast_names = 0;
  int cast_null = -1;
  bit cast_null_names = 0;
  int cast_self = -1;
  int calls = 0;
  bit cast_task_names = 0;
  process ended;
  process kept;

  function automatic process counted(process p);
    calls++;
    return p;
  endfunction

  initial begin
    process p;
    process q;
    process r;
    Holder h;
    automatic std::weak_reference #(process) refers;

    p = process::self();
    q = process::self();
    same_self = (p == q);

    plain = p.randomize();
    holds = p.randomize() with { x == 0; };
    fails = p.randomize() with { x == 1; };
    x_after = x;
    running = p.randomize() with { status() == process::RUNNING; };
    not_finished = p.randomize() with { status() == process::FINISHED; };

    p.pre_randomize();
    p.post_randomize();
    p.rand_mode(0);
    p.constraint_mode(0);
    after_modes = p.randomize() with { x == 1; };

    p.srandom(11);
    drawn_first = $urandom;
    p.srandom(11);
    drawn_again = $urandom;

    fork
      ended = process::self();
    join_none
    #1;
    kept = ended;
    refers = new(ended);
    ended = null;
    #1;
    finished_status = kept.status();
    finished_randomize = kept.randomize();
    weak_answers = (refers.get() == kept);

    h = new;
    with_null = h.randomize();
    h.first = p;
    h.second = p;
    with_processes = h.randomize();
    hooks_ran = h.pre * 10 + h.post;

    cast_onto = $cast(q, kept);
    cast_names = (q == kept);
    r = null;
    cast_null = $cast(q, r);
    cast_null_names = (q == null);
    cast_self = $cast(p, p);
    $cast(q, counted(kept));
    cast_task_names = (q == kept);
  end

  final begin
    if (!same_self)
      $fatal(1, "process::self() answered two different processes");
    if (plain !== 1)
      $fatal(1, "a process's randomize() answered %0d", plain);
    if (holds !== 1)
      $fatal(1, "a process's randomize() with a holding constraint answered %0d",
             holds);
    if (fails !== 0)
      $fatal(1, "a process's randomize() with a failing constraint answered %0d",
             fails);
    if (x_after !== 0)
      $fatal(1, "a process's randomize() changed the state variable to %0d",
             x_after);
    if (running !== 1 || not_finished !== 0)
      $fatal(1, "status() in a process's inline constraint answered %0d, %0d",
             running, not_finished);
    if (after_modes !== 0)
      $fatal(1, "constraint_mode(0) on a process turned off an inline constraint");
    if (drawn_first !== drawn_again)
      $fatal(1, "a reseeded process drew %0h then %0h", drawn_first, drawn_again);
    if (finished_status !== process::FINISHED)
      $fatal(1, "a finished process's status() answered %0d", finished_status);
    if (finished_randomize !== 1)
      $fatal(1, "a finished process's randomize() answered %0d",
             finished_randomize);
    if (!weak_answers)
      $fatal(1, "a weak reference to a finished process held by a handle lost it");
    if (with_null !== 1 || with_processes !== 1)
      $fatal(1, "randomizing an object holding random processes answered %0d, %0d",
             with_null, with_processes);
    if (hooks_ran !== 22)
      $fatal(1, "the hooks of an object holding random processes ran %0d", hooks_ran);
    if (cast_onto !== 1 || !cast_names)
      $fatal(1, "$cast between process handles answered %0d", cast_onto);
    if (cast_null !== 1 || !cast_null_names)
      $fatal(1, "$cast of a null process handle answered %0d", cast_null);
    if (cast_self !== 1)
      $fatal(1, "$cast of a process handle onto itself answered %0d", cast_self);
    if (calls !== 1 || !cast_task_names)
      $fatal(1, "$cast as a task evaluated its source %0d times", calls);
    $display("All checks passed");
  end
endmodule

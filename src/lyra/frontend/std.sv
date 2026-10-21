// What the classes of the built-in package `std` do (LRM 15.3, 15.4). The
// front end declares `std` itself, with every method and no body, and the
// package can not be declared again in source; so the definitions are this
// file's compilation-unit scope, which Lyra compiles in the name of `std`. It is
// no design element, so it carries no time scale of its own (LRM 3.14.2.3).
// Each class of `std` here has the name, the parameters and the methods, with
// their arguments and defaults, that the front end declares for it, since every
// call and every class extending one is checked against that declaration and
// reaches this definition.

// The processes waiting for one kind of thing, in the order they arrived: each
// waits until it is first and what it asks for is there, so they are served in
// that order (LRM 15.3.3, 15.4.5). Each waiter is a process forked for the wait
// alone, so a caller that stops waiting by any route -- killed (LRM 9.7), or
// leaving a block disabled around the call (LRM 9.6.2) -- ends it as well.
// Nothing in a process runs as it ends, so the list drops an ended waiter
// wherever it is next asked who is first, and a waiter behind the first
// watches it end.
class wait_list;
  local process waiting[$];
  local int changes;

  // Something a waiter asks about may have changed.
  function void changed();
    changes++;
  endfunction

  local function void drop_ended();
    while (waiting.size() > 0 &&
           (waiting[0].status() == process::FINISHED ||
            waiting[0].status() == process::KILLED)) begin
      void'(waiting.pop_front());
      changes++;
    end
  endfunction

  function bit is_empty();
    drop_ended();
    return waiting.size() == 0;
  endfunction

  function void arrive();
    waiting.push_back(process::self());
  endfunction

  function bit is_first();
    drop_ended();
    return waiting[0] == process::self();
  endfunction

  // Waits until something changed or the first waiter ended. The waiter has
  // no other process of its own, so `disable fork` ends only what this starts.
  task wait_for_change();
    int seen = changes;
    process first = waiting[0];
    if (first == process::self()) begin
      wait (changes != seen);
    end else begin
      fork
        wait (changes != seen);
        first.await();
      join_any
      disable fork;
    end
  endtask

  function void leave();
    void'(waiting.pop_front());
    changes++;
  endfunction
endclass

// A `try_` form succeeds only where its waiting form would not wait.
class semaphore;
  local int keys;
  local wait_list getting = new;

  function new(int keyCount = 0);
    keys = keyCount;
  endfunction

  function void put(int keyCount = 1);
    if (keyCount < 0) begin
      $error("a 'semaphore' cannot be given back a negative number of keys");
      return;
    end
    keys += keyCount;
    getting.changed();
  endfunction

  task get(int keyCount = 1);
    if (keyCount < 0) begin
      $error("a 'semaphore' cannot be asked for a negative number of keys");
      return;
    end
    if (try_get(keyCount)) return;
    fork
      begin
        getting.arrive();
        while (!(getting.is_first() && keys >= keyCount)) begin
          getting.wait_for_change();
        end
        keys -= keyCount;
        getting.leave();
      end
    join
  endtask

  function int try_get(int keyCount = 1);
    if (keyCount < 0) begin
      $error("a 'semaphore' cannot be asked for a negative number of keys");
      return 0;
    end
    if (!getting.is_empty() || keys < keyCount) return 0;
    keys -= keyCount;
    return 1;
  endfunction
endclass

// One message a mailbox holds. A mailbox of no stated type holds messages of
// a type no array may be of (LRM 15.4.9), so each message is an object of its
// own, and a mailbox of a stated type holds its messages the same way.
class mailbox_message #(type T);
  T value;
  function new(T value);
    this.value = value;
  endfunction
endclass

class mailbox #(type T);
  // Zero holds any number of messages (LRM 15.4.1).
  local int bound;
  local mailbox_message #(T) messages[$];
  local wait_list putting = new;
  local wait_list getting = new;

  function new(int bound = 0);
    if (bound < 0) begin
      $warning("a 'mailbox' bound cannot be negative; the mailbox holds any number of messages");
      bound = 0;
    end
    this.bound = bound;
  endfunction

  function int num();
    return messages.size();
  endfunction

  local function bit has_room();
    return bound == 0 || messages.size() < bound;
  endfunction

  local function void add(T message);
    mailbox_message #(T) held = new(message);
    messages.push_back(held);
    getting.changed();
  endfunction

  // Copies the first message into `message`, which only a variable of a type
  // equivalent to the message's receives (LRM 15.4.5, 15.4.9); answers whether
  // it did. Where the mailbox states a type the two are one and it always does.
  local function bit copy_first(ref T message);
    return $cast(message, messages[0].value);
  endfunction

  local function void remove_first();
    void'(messages.pop_front());
    putting.changed();
  endfunction

  local function void report_mismatch();
    $error("a 'mailbox' message is of a type not equivalent to the variable asking for it, so it stays in the mailbox");
  endfunction

  task put(T message);
    if (try_put(message)) return;
    fork
      begin
        putting.arrive();
        while (!(putting.is_first() && has_room())) begin
          putting.wait_for_change();
        end
        add(message);
        putting.leave();
      end
    join
  endtask

  function int try_put(T message);
    if (!putting.is_empty() || !has_room()) return 0;
    add(message);
    return 1;
  endfunction

  task get(ref T message);
    int taken = try_get(message);
    if (taken < 0) report_mismatch();
    if (taken != 0) return;
    fork
      begin
        getting.arrive();
        while (!(getting.is_first() && messages.size() > 0)) begin
          getting.wait_for_change();
        end
        if (copy_first(message)) remove_first();
        else report_mismatch();
        getting.leave();
      end
    join
  endtask

  // A message of another type than the variable's stays, answered below zero
  // (LRM 15.4.6).
  function int try_get(ref T message);
    if (!getting.is_empty() || messages.size() == 0) return 0;
    if (!copy_first(message)) return -1;
    remove_first();
    return 1;
  endfunction

  // Every waiting peek is let go while a message remains (LRM 15.4.7).
  task peek(ref T message);
    wait (messages.size() > 0);
    if (!copy_first(message)) report_mismatch();
  endtask

  function int try_peek(ref T message);
    if (messages.size() == 0) return 0;
    return copy_first(message) ? 1 : -1;
  endfunction
endclass

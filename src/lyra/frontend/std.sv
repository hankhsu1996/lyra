// What the classes of the built-in package `std` do (LRM 15.3, 15.4), and the
// library a covergroup's class is written over (LRM 19). The front end declares
// `std` itself, with every method and no body, and the package can not be
// declared again in source; so the definitions are this file's
// compilation-unit scope, which Lyra compiles in the name of `std`. It is no
// design element, so it carries no time scale of its own (LRM 3.14.2.3). Each
// class of `std` here has the name, the parameters and the methods, with their
// arguments and defaults, that the front end declares for it, since every call
// and every class extending one is checked against that declaration and
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

// What a covergroup counts and computes (LRM 19). A covergroup the source
// declares is a class the compiler writes from the declaration; what does not
// depend on the declaration -- distributing values into bins, matching a value
// or a sequence of values, the cross product, the arithmetic of LRM 19.11,
// naming bins, and keeping what an instance counted after the instance ends --
// is here.

// The `option` and `type_option` members of a covergroup, a coverpoint and a
// cross (LRM 19.10), in the order the standard declares their fields.
typedef struct {
  string name;
  int weight;
  int goal;
  string comment;
  int at_least;
  int auto_bin_max;
  int cross_num_print_missing;
  bit cross_retain_auto_bins;
  bit detect_overlap;
  bit per_instance;
  bit get_inst_coverage;
} covergroup_option;

typedef struct {
  int weight;
  int goal;
  string comment;
  int at_least;
  int auto_bin_max;
  bit detect_overlap;
} coverpoint_option;

typedef struct {
  int weight;
  int goal;
  string comment;
  int at_least;
  int cross_num_print_missing;
  bit cross_retain_auto_bins;
} cross_option;

typedef struct {
  int weight;
  int goal;
  string comment;
  bit strobe;
  bit merge_instances;
  bit distribute_first;
  real real_interval;
} covergroup_type_option;

typedef struct {
  int weight;
  int goal;
  string comment;
  real real_interval;
} coverpoint_type_option;

typedef struct {
  int weight;
  int goal;
  string comment;
} cross_type_option;

// The numbers of a coverpoint's counting bins, as a cross selects them.
typedef int coverage_bin_numbers[$];

// Tuples of a cross's bins, by their numbers.
typedef longint coverage_tuples[$];

typedef class covergroup_instance;
typedef class covergroup_type;
typedef class coverage_registry;

// What a bin counts toward (LRM 19.5.5, 19.5.6), or that it catches what no
// other bin does (LRM 19.5).
typedef enum int {
  COVERAGE_BINS,
  COVERAGE_IGNORE_BINS,
  COVERAGE_ILLEGAL_BINS,
  COVERAGE_DEFAULT_BINS
} coverage_bin_kind;

// How a transition step repeats (LRM 19.5.2).
typedef enum int {
  COVERAGE_ONCE,
  COVERAGE_CONSECUTIVE,
  COVERAGE_GOTO,
  COVERAGE_NONCONSECUTIVE
} coverage_repetition;

// How many bins one bin construct makes (LRM 19.5.1): one, one per value
// (`b[]`), or the fixed number its brackets give (`b[N]`).
typedef enum int {
  COVERAGE_ONE_BIN,
  COVERAGE_BIN_PER_VALUE,
  COVERAGE_FIXED_BINS
} coverage_bin_array;

// A coverpoint or a cross: what an instance's coverage and its type's are
// computed over (LRM 19.11).
virtual class coverage_item;
  string name;
  // Whether `start` or `stop` was called last (LRM 19.8).
  bit collecting = 1;
  // The type of the covergroup whose instance holds it, which keeps its type
  // options (LRM 19.7).
  protected covergroup_type owner_type;

  // `counted` is clear for an item a covergroup extending another declares
  // again: built and sampled still, for the crosses over it, but no part of
  // what the instance covers (LRM 19.4.1).
  function new(covergroup_instance group, string name, bit counted);
    this.name = name;
    owner_type = group.owner;
    if (counted) group.add(this);
  endfunction

  pure virtual function int weight();
  pure virtual function int goal();
  pure virtual function int at_least();
  pure virtual function int type_weight();

  // The bins that count toward coverage and how many of them were hit at
  // least `at_least` times; a bin with no value and no sequence is no such bin
  // (LRM 19.11.1).
  pure virtual function void parts(output int covered, output int total);

  // Every bin that counts toward coverage, by its name, with its count, which
  // is how instances of one type are merged (LRM 19.11.3).
  pure virtual function void counts_by_name(ref longint counts[string]);

  function void start();
    collecting = 1;
  endfunction

  function void stop();
    collecting = 0;
  endfunction

  // A coverage number in [0, 100] from what was covered of how many, with the
  // answer the standard gives where there is nothing to cover (LRM 19.11).
  static function real ratio(int covered, int total, int weight);
    if (total == 0) return weight == 0 ? 100.0 : 0.0;
    return 100.0 * covered / total;
  endfunction

  // `get_inst_coverage` of a coverpoint or a cross (LRM 19.8), with and
  // without the counts.
  function real get_inst_coverage(ref int covered, ref int total);
    parts(covered, total);
    return ratio(covered, total, weight());
  endfunction

  function real inst_coverage();
    int covered;
    int total;
    return get_inst_coverage(covered, total);
  endfunction

  virtual function bit is_cross();
    return 0;
  endfunction

  // This item in the coverage database (LRM 19.9): its counting bins by name,
  // as a coverpoint of the UCIS interchange format, which holds one with no
  // bin at all as nothing.
  virtual function void write_database(int fd);
    longint counts[string];
    counts_by_name(counts);
    if (counts.size() == 0) return;
    $fdisplay(fd, "        <coverpoint name=\"%s\" key=\"0\">",
              coverage_registry::escaped(name));
    $fdisplay(fd, "          <options weight=\"%0d\" goal=\"%0d\" at_least=\"%0d\"/>",
              weight(), goal(), at_least());
    foreach (counts[b]) begin
      $fdisplay(fd, "          <coverpointBin name=\"%s\" type=\"bins\" key=\"0\">",
                coverage_registry::escaped(b));
      $fdisplay(fd, "            <range from=\"-1\" to=\"-1\">");
      $fdisplay(fd, "              <contents coverageCount=\"%0d\"/>", counts[b]);
      $fdisplay(fd, "            </range>");
      $fdisplay(fd, "          </coverpointBin>");
    end
    $fdisplay(fd, "        </coverpoint>");
  endfunction
endclass

// A coverpoint as a cross sees it (LRM 19.6): its bins that count, numbered,
// and which of them the last sample hit.
virtual class coverage_coverpoint extends coverage_item;
  // The numbers of the counting bins the last sample hit, empty where the
  // sample did not count.
  int hits[$];

  function new(covergroup_instance group, string name, bit counted);
    super.new(group, name, counted);
  endfunction

  pure virtual function int normal_bin_count();
  pure virtual function string normal_bin_name(int index);
  // The numbers of the counting bins named `name`: one bin, or every bin of an
  // array of bins.
  pure virtual function void bins_named(string bin_name, ref int indices[$]);
  // How many values one counting bin holds, or -1 where it holds more than
  // can be counted.
  pure virtual function longint bin_size(int index);

  // What `binsof` names (LRM 19.6.1): every counting bin, or those of one
  // name.
  function coverage_bin_numbers all_numbers();
    coverage_bin_numbers numbers;
    for (int i = 0; i < normal_bin_count(); i++) numbers.push_back(i);
    return numbers;
  endfunction

  function coverage_bin_numbers numbers_named(string bin_name);
    coverage_bin_numbers numbers;
    bins_named(bin_name, numbers);
    return numbers;
  endfunction

  // Whatever the last sample hit no longer counts toward a cross, the
  // coverpoint's guard having been false (LRM 19.5).
  function void skip();
    hits.delete();
  endfunction
endclass

// A coverpoint of an integral expression (LRM 19.5), whose values are compared
// at the coverpoint's type (LRM 19.5.7).
class integral_coverpoint #(type T = int) extends coverage_coverpoint;
  typedef bit [$bits(T)-1:0] key_t;
  typedef logic [$bits(T)-1:0] pattern_t;
  typedef bit [$bits(T)+31:0] size_t;
  // A run of values, as keys: a value's key orders it as its type does.
  typedef struct {
    key_t lo;
    key_t hi;
  } span_t;
  // One element of a bin's range list: a range, or a value whose `care` bits
  // alone are compared (a wildcard bin, LRM 19.5.4).
  typedef struct {
    T lo;
    T hi;
    key_t care;
  } element_t;

  // A bin of values (LRM 19.5.1), or an array of them, one per value (`b[]`).
  // `construct` is the number of the source construct that made it, which its
  // `iff` names; an automatic bin has none.
  typedef struct {
    string name;
    coverage_bin_kind kind;
    int construct;
    bit per_value;
    bit gated;
    span_t spans[$];
    longint count;
    int first_index;
  } value_bin_t;

  // One step of a transition (LRM 19.5.2).
  typedef struct {
    span_t spans[$];
    coverage_repetition repetition;
    int least;
    int most;
  } step_t;

  typedef step_t steps_t[$];
  typedef T values_t[$];

  // Where one attempt at a sequence stands: the step it is in and how many
  // times that step has matched.
  typedef struct {
    int step;
    int matched;
  } attempt_t;

  // A transition bin: one name for every sequence of `sequences`.
  typedef struct {
    string name;
    coverage_bin_kind kind;
    int construct;
    bit gated;
    longint count;
    int first_index;
  } transition_bin_t;

  coverpoint_option option;
  local key_t sign_flip;
  local value_bin_t value_bins[$];
  local longint per_value_counts[int][key_t];
  local transition_bin_t transition_bins[$];
  local step_t sequences[int][$][$];
  local attempt_t attempts[int][$][$];
  local bit default_sequence_present;
  local int default_sequence_construct;
  local bit default_sequence_gated = 1;
  local string default_sequence_name;
  local longint default_sequence_count;
  local bit has_sample;
  local bit is_auto;
  local int counting;
  local string enum_names[key_t];
  // The bins being given: their name, what they count toward, whether they
  // are one bin (0), one per value (1) or a fixed number (2), how many, and
  // the range list so far.
  local string pending_name;
  local int pending_construct;
  local coverage_bin_kind pending_kind;
  local coverage_bin_array pending_array;
  local int pending_count;
  local element_t pending[$];
  local bit filtering;
  local bit kept_values[key_t];
  local step_t pending_steps[$];
  local step_t pending_sequences[$][$];

  function new(covergroup_instance group, string name, bit counted);
    super.new(group, name, counted);
    option.weight = 1;
    option.goal = 100;
    option.comment = "";
    option.at_least = group.option.at_least;
    option.auto_bin_max = group.option.auto_bin_max;
    option.detect_overlap = group.option.detect_overlap;
    sign_flip = (T'(-1) < T'(0)) ? key_t'(1) << ($bits(T) - 1) : '0;
  endfunction

  virtual function int weight();
    return option.weight;
  endfunction

  virtual function int goal();
    return option.goal;
  endfunction

  virtual function int at_least();
    return option.at_least;
  endfunction

  virtual function int type_weight();
    return owner_type.coverpoint_type_options[name].weight;
  endfunction

  local function key_t key_of(T value);
    return key_t'(value) ^ sign_flip;
  endfunction

  local function T value_of(key_t key);
    return T'(key ^ sign_flip);
  endfunction

  local function string spell(key_t key);
    if (enum_names.exists(key)) return enum_names[key];
    return $sformatf("%0d", value_of(key));
  endfunction

  // The values an element stands for, as runs of keys. A wildcard value is
  // one run per assignment of its compared bits above the lowest one that is
  // not compared.
  local function void spans_of(element_t element, ref span_t spans[$]);
    key_t free;
    int low_free;
    key_t lo_key;
    key_t hi_key;
    if (element.care == '1) begin
      lo_key = key_of(element.lo);
      hi_key = key_of(element.hi);
      if (lo_key <= hi_key) spans.push_back('{lo_key, hi_key});
      return;
    end
    free = ~element.care;
    low_free = 0;
    while (low_free < $bits(T) && free[low_free]) low_free++;
    begin
      key_t high_free = free & ~((key_t'(1) << low_free) - 1);
      key_t base = key_t'(element.lo) & element.care;
      key_t subset = '0;
      forever begin
        key_t value = base | subset;
        key_t lo_value = value;
        key_t hi_value = value | ((key_t'(1) << low_free) - 1);
        lo_key = key_of(T'(lo_value));
        hi_key = key_of(T'(hi_value));
        spans.push_back('{lo_key, hi_key});
        if (subset == high_free) break;
        subset = (subset - high_free) & high_free;
      end
    end
  endfunction

  // Sorts runs and joins those that touch or overlap.
  local function void normalize(ref span_t spans[$]);
    span_t sorted[$];
    sorted = spans;
    sorted.sort(s) with (s.lo);
    spans.delete();
    foreach (sorted[i]) begin
      if (spans.size() > 0 && (spans[$].hi == '1 || sorted[i].lo <= spans[$].hi + 1)) begin
        if (sorted[i].hi > spans[$].hi) spans[$].hi = sorted[i].hi;
      end else begin
        spans.push_back(sorted[i]);
      end
    end
  endfunction

  local function void subtract(ref span_t spans[$], input span_t removed[$]);
    span_t remaining[$];
    foreach (removed[r]) begin
      remaining.delete();
      foreach (spans[s]) begin
        if (removed[r].hi < spans[s].lo || removed[r].lo > spans[s].hi) begin
          remaining.push_back(spans[s]);
          continue;
        end
        if (removed[r].lo > spans[s].lo) remaining.push_back('{spans[s].lo, removed[r].lo - 1});
        if (removed[r].hi < spans[s].hi) remaining.push_back('{removed[r].hi + 1, spans[s].hi});
      end
      spans = remaining;
    end
  endfunction

  local function bit contains(input span_t spans[$], key_t key);
    foreach (spans[i]) if (key >= spans[i].lo && key <= spans[i].hi) return 1;
    return 0;
  endfunction

  local function size_t size_of(input span_t spans[$]);
    size_t total = 0;
    foreach (spans[i]) total += size_t'(spans[i].hi) - size_t'(spans[i].lo) + 1;
    return total;
  endfunction

  local function int rank_of(input span_t spans[$], key_t key);
    size_t preceding = 0;
    foreach (spans[i]) begin
      if (key >= spans[i].lo && key <= spans[i].hi) return int'(preceding + (key - spans[i].lo));
      preceding +=size_t'(spans[i].hi) - size_t'(spans[i].lo) + 1;
    end
    return -1;
  endfunction

  local function key_t key_at(input span_t spans[$], size_t position);
    size_t rank = position;
    foreach (spans[i]) begin
      size_t length = size_t'(spans[i].hi) - size_t'(spans[i].lo) + 1;
      if (rank < length) return spans[i].lo + key_t'(rank);
      rank -= length;
    end
    return '0;
  endfunction

  // The values a bin covers before anything is ignored, in the order the
  // range list gives them (LRM 19.5.1), each element's own values ascending.
  local function void ordered_spans(element_t elements[$], ref span_t spans[$]);
    foreach (elements[i]) begin
      span_t own[$];
      spans_of(elements[i], own);
      normalize(own);
      foreach (own[j]) spans.push_back(own[j]);
    end
  endfunction

  // Opens the bins one `bins`, `ignore_bins` or `illegal_bins` construct
  // makes (LRM 19.5.1), `count` of them where the construct gives a number.
  // Its range list is then given one element at a time, each value already
  // cast to the coverpoint's type (LRM 19.5.7). Each construct of the
  // coverpoint is numbered by its place among them in the source.
  function void begin_bins(int construct, string bin_name, coverage_bin_kind kind,
                           coverage_bin_array array_kind, int count);
    pending_construct = construct;
    pending_name = bin_name;
    pending_kind = kind;
    pending_array = array_kind;
    pending_count = count;
    pending.delete();
  endfunction

  function void add_value(T value);
    pending.push_back('{value, value, '1});
  endfunction

  function void add_range(T lo, T hi);
    pending.push_back('{lo, hi, '1});
  endfunction

  // A range one of whose ends is `$`: the least or the greatest value of the
  // coverpoint's type (LRM 19.5.1).
  function void add_range_from_least(T hi);
    pending.push_back('{value_of('0), hi, '1});
  endfunction

  function void add_range_to_greatest(T lo);
    pending.push_back('{lo, value_of('1), '1});
  endfunction

  // A value with a tolerance (LRM 11.4.13): `+/-` an absolute one, `+%-` one
  // in percent of the value.
  function void add_tolerance(T center, T tolerance, bit in_percent);
    T spread = in_percent ? T'((center < 0 ? -center : center) * tolerance / 100) : tolerance;
    pending.push_back('{T'(center - spread), T'(center + spread), '1});
  endfunction

  // A value of a `wildcard` bin, whose x, z and ? bits match either value
  // (LRM 19.5.4).
  function void add_wildcard(pattern_t value);
    key_t care;
    for (int i = 0; i < $bits(T); i++) care[i] = !$isunknown(value[i]);
    pending.push_back('{T'(value), T'(value), care});
  endfunction

  // Every value of the coverpoint, for a bin naming the coverpoint itself
  // (LRM 19.5.1.1).
  function void add_every_value();
    pending.push_back(every_value());
  endfunction

  // The values of the range list just given, in its order, for a `with`
  // filter (LRM 19.5.1.1) to be evaluated over, each one's answer handed back
  // by `keep_value`.
  function values_t pending_values();
    values_t listed;
    span_t ordered[$];
    ordered_spans(pending, ordered);
    foreach (ordered[i]) begin
      key_t key = ordered[i].lo;
      forever begin
        listed.push_back(value_of(key));
        if (key == ordered[i].hi) break;
        key++;
      end
    end
    return listed;
  endfunction

  function void keep_value(T value, bit kept);
    filtering = 1;
    if (kept) kept_values[key_of(value)] = 1;
  endfunction

  // The bins the construct makes. A `with` filter drops what it does not keep
  // before the values are distributed among the bins, or after where the
  // type's `distribute_first` says so (LRM 19.5.1.1, 19.7.1).
  function void end_bins();
    if (!filtering) begin
      make_value_bins(pending_name, pending_kind, pending_array, pending_count, pending);
    end else if (owner_type.type_option.distribute_first) begin
      span_t kept[$];
      int first = value_bins.size();
      make_value_bins(pending_name, pending_kind, pending_array, pending_count, pending);
      foreach (kept_values[key]) kept.push_back('{key, key});
      normalize(kept);
      for (int i = first; i < value_bins.size(); i++) begin
        span_t complement[$];
        complement.push_back('{'0, '1});
        subtract(complement, kept);
        subtract(value_bins[i].spans, complement);
      end
    end else begin
      element_t listed[$];
      values_t candidates = pending_values();
      foreach (candidates[i])
        if (kept_values.exists(key_of(candidates[i])))
          listed.push_back('{candidates[i], candidates[i], '1});
      make_value_bins(pending_name, pending_kind, pending_array, pending_count, listed);
    end
    pending.delete();
    kept_values.delete();
    filtering = 0;
  endfunction

  // A bin of the values `elements` lists (LRM 19.5.1). An array of a fixed
  // number of bins takes the values in order, B to a bin and the rest to the
  // last; an open array is a bin per distinct value.
  local function void make_value_bins(string bin_name, coverage_bin_kind kind,
                                      coverage_bin_array array_kind, int count,
                                      element_t elements[$]);
    span_t ordered[$];
    ordered_spans(elements, ordered);
    if (array_kind == COVERAGE_FIXED_BINS && kind == COVERAGE_BINS) begin
      size_t total = 0;
      size_t each;
      foreach (ordered[i]) total += size_t'(ordered[i].hi) - size_t'(ordered[i].lo) + 1;
      each = total / count;
      if (each == 0) each = 1;
      for (int b = 0; b < count; b++) begin
        value_bin_t bin;
        size_t first = size_t'(b) * each;
        size_t last = (b == count - 1) ? total : first + each;
        if (last > total) last = total;
        bin.name = $sformatf("%s[%0d]", bin_name, b);
        bin.kind = kind;
        bin.construct = pending_construct;
        bin.gated = 1;
        if (first < total) take_positions(ordered, first, last, bin.spans);
        normalize(bin.spans);
        value_bins.push_back(bin);
      end
      return;
    end
    begin
      value_bin_t bin;
      bin.name = bin_name;
      bin.kind = kind;
      bin.construct = pending_construct;
      bin.gated = 1;
      bin.per_value = array_kind != COVERAGE_ONE_BIN && kind == COVERAGE_BINS;
      bin.spans = ordered;
      normalize(bin.spans);
      value_bins.push_back(bin);
    end
  endfunction

  // The values at positions [first, last) of a sequence of runs.
  local function void take_positions(input span_t ordered[$], size_t first, size_t last,
                                     ref span_t taken[$]);
    size_t position = 0;
    foreach (ordered[i]) begin
      size_t length = size_t'(ordered[i].hi) - size_t'(ordered[i].lo) + 1;
      size_t from = position;
      size_t to = position + length;
      if (to > first && from < last) begin
        size_t lo = (first > from) ? first - from : 0;
        size_t hi = ((last < to) ? last : to) - from - 1;
        taken.push_back('{ordered[i].lo + key_t'(lo), ordered[i].lo + key_t'(hi)});
      end
      position = to;
    end
  endfunction

  // The bin that catches every value no other bin of the coverpoint has
  // (LRM 19.5); it counts toward nothing.
  function void add_default_bin(int construct, string bin_name, bit per_value);
    value_bin_t bin;
    bin.name = bin_name;
    bin.kind = COVERAGE_DEFAULT_BINS;
    bin.construct = construct;
    bin.per_value = per_value;
    bin.gated = 1;
    value_bins.push_back(bin);
  endfunction

  // A transition bin of its sequences (LRM 19.5.2), each a list of steps.
  local function void add_transition_bin(string bin_name, coverage_bin_kind kind,
                                         step_t bin_sequences[$][$]);
    transition_bin_t bin;
    bin.name = bin_name;
    bin.kind = kind;
    bin.construct = pending_construct;
    bin.gated = 1;
    sequences[transition_bins.size()] = bin_sequences;
    attempts[transition_bins.size()] = {};
    foreach (bin_sequences[s]) attempts[transition_bins.size()].push_back({});
    transition_bins.push_back(bin);
  endfunction

  // Opens the bins one transition construct makes (LRM 19.5.2): one bin for
  // every sequence of its list, or with `per_sequence` (`b[]`), one bin for
  // each sequence of values the list stands for. Each sequence is then given
  // step by step, each step's values as a range list.
  function void begin_transition_bins(int construct, string bin_name, coverage_bin_kind kind,
                                     bit per_sequence);
    pending_construct = construct;
    pending_name = bin_name;
    pending_kind = kind;
    pending_array = per_sequence ? COVERAGE_BIN_PER_VALUE : COVERAGE_ONE_BIN;
    pending_sequences.delete();
  endfunction

  function void begin_sequence();
    pending_steps.delete();
  endfunction

  function void begin_step();
    pending.delete();
  endfunction

  function void end_step(coverage_repetition repetition, int least, int most);
    step_t made;
    foreach (pending[i]) spans_of(pending[i], made.spans);
    normalize(made.spans);
    made.repetition = repetition;
    made.least = least;
    made.most = most;
    pending_steps.push_back(made);
    pending.delete();
  endfunction

  function void end_sequence();
    pending_sequences.push_back(pending_steps);
  endfunction

  function void end_transition_bins();
    if (pending_array == COVERAGE_BIN_PER_VALUE) begin
      key_t concrete[$][$];
      foreach (pending_sequences[s]) expand(pending_sequences[s], 0, {}, concrete);
      foreach (concrete[c])
        add_transition_bin($sformatf("%s[%s]", pending_name, spell_sequence(concrete[c])),
                           pending_kind, '{steps_of(concrete[c])});
    end else begin
      add_transition_bin(pending_name, pending_kind, pending_sequences);
    end
    pending_sequences.delete();
  endfunction

  // Every sequence of values a sequence of steps of bounded length stands for:
  // each step one of its values, repeated as many times as it repeats.
  local function void expand(step_t steps[$], int at, key_t prefix[$], ref key_t out[$][$]);
    if (at == steps.size()) begin
      out.push_back(prefix);
      return;
    end
    foreach (steps[at].spans[i]) begin
      key_t key = steps[at].spans[i].lo;
      forever begin
        for (int times = steps[at].least; times <= steps[at].most; times++) begin
          key_t longer[$] = prefix;
          for (int k = 0; k < times; k++) longer.push_back(key);
          expand(steps, at + 1, longer, out);
        end
        if (key == steps[at].spans[i].hi) break;
        key++;
      end
    end
  endfunction

  local function bit bounded(step_t steps[$]);
    foreach (steps[i])
      if (steps[i].repetition == COVERAGE_GOTO || steps[i].repetition == COVERAGE_NONCONSECUTIVE)
        return 0;
    return 1;
  endfunction

  local function steps_t steps_of(key_t values[$]);
    foreach (values[i]) begin
      step_t one;
      one.spans.push_back('{values[i], values[i]});
      one.repetition = COVERAGE_ONCE;
      one.least = 1;
      one.most = 1;
      steps_of.push_back(one);
    end
  endfunction

  local function string spell_sequence(key_t values[$]);
    string spelled;
    foreach (values[i]) spelled = {spelled, i > 0 ? "=>" : "", spell(values[i])};
    return spelled;
  endfunction

  // Whether `inner` occurs in `outer` as a run of consecutive values.
  local static function bit occurs_in(key_t inner[$], key_t outer[$]);
    for (int start = 0; start + inner.size() <= outer.size(); start++) begin
      bit all = 1;
      foreach (inner[i]) if (outer[start + i] != inner[i]) all = 0;
      if (all) return 1;
    end
    return 0;
  endfunction

  // Removes from every counting transition bin of bounded sequences each
  // sequence that cannot be matched without matching an ignored or illegal
  // one (LRM 19.5.5, 19.5.6).
  local function void remove_excluded_sequences();
    key_t excluded[$][$];
    foreach (transition_bins[t]) begin
      if (transition_bins[t].kind == COVERAGE_BINS) continue;
      foreach (sequences[t][s])
        if (bounded(sequences[t][s])) expand(sequences[t][s], 0, {}, excluded);
    end
    if (excluded.size() == 0) return;
    foreach (transition_bins[t]) begin
      step_t kept[$][$];
      bit any_bounded = 0;
      if (transition_bins[t].kind != COVERAGE_BINS) continue;
      foreach (sequences[t][s]) begin
        key_t concrete[$][$];
        if (!bounded(sequences[t][s])) begin
          kept.push_back(sequences[t][s]);
          continue;
        end
        expand(sequences[t][s], 0, {}, concrete);
        foreach (concrete[c]) begin
          bit dropped = 0;
          foreach (excluded[e]) if (occurs_in(excluded[e], concrete[c])) dropped = 1;
          if (!dropped) kept.push_back(steps_of(concrete[c]));
        end
      end
      sequences[t] = kept;
      attempts[t] = {};
      foreach (kept[s]) attempts[t].push_back({});
    end
  endfunction

  function void add_default_sequence_bin(int construct, string bin_name);
    default_sequence_present = 1;
    default_sequence_construct = construct;
    default_sequence_name = bin_name;
  endfunction

  // A bin per value of the coverpoint's enumeration type (LRM 19.5.3), named
  // by the value's name; handed in place of the bins the type's width gives.
  function void name_enumeration_value(T value, string value_name);
    enum_names[key_of(value)] = value_name;
  endfunction

  // Removes what is ignored or illegal from every bin that counts (LRM 19.5.5,
  // 19.5.6), after values were distributed; makes the bins the type gives where
  // the source wrote none that count (LRM 19.5.3); and numbers the bins that
  // count, which a cross reads.
  function void finish();
    span_t excluded[$];
    bit any_counting = 0;
    foreach (value_bins[i]) begin
      if (value_bins[i].kind == COVERAGE_IGNORE_BINS || value_bins[i].kind == COVERAGE_ILLEGAL_BINS)
        foreach (value_bins[i].spans[j]) excluded.push_back(value_bins[i].spans[j]);
      if (value_bins[i].kind == COVERAGE_BINS) any_counting = 1;
    end
    foreach (transition_bins[i]) if (transition_bins[i].kind == COVERAGE_BINS) any_counting = 1;
    normalize(excluded);
    if (!any_counting) add_automatic_bins();
    foreach (value_bins[i]) if (value_bins[i].kind == COVERAGE_BINS) subtract(value_bins[i].spans, excluded);
    remove_excluded_sequences();
    if (option.detect_overlap) report_overlaps();
    counting = 0;
    foreach (value_bins[i]) begin
      if (value_bins[i].kind != COVERAGE_BINS) continue;
      value_bins[i].first_index = counting;
      counting += value_bins[i].per_value ? int'(size_of(value_bins[i].spans)) : 1;
    end
    foreach (transition_bins[i]) begin
      if (transition_bins[i].kind != COVERAGE_BINS) continue;
      transition_bins[i].first_index = counting;
      counting++;
    end
  endfunction

  local function void add_automatic_bins();
    is_auto = 1;
    if (enum_names.size() > 0) begin
      foreach (enum_names[key]) begin
        value_bin_t bin;
        bin.name = $sformatf("auto[%s]", enum_names[key]);
        bin.kind = COVERAGE_BINS;
        bin.construct = -1;
        bin.gated = 1;
        bin.spans.push_back('{key, key});
        value_bins.push_back(bin);
      end
      return;
    end
    begin
      size_t values = size_t'(1) << $bits(T);
      size_t made = (option.auto_bin_max > 0 && size_t'(option.auto_bin_max) < values) ?
          size_t'(option.auto_bin_max) : values;
      size_t each = values / made;
      for (size_t b = 0; b < made; b++) begin
        value_bin_t bin;
        key_t lo = key_t'(b * each);
        key_t hi = (b == made - 1) ? '1 : key_t'((b + 1) * each - 1);
        bin.name = (lo == hi) ? $sformatf("auto[%s]", spell(lo)) :
            $sformatf("auto[%s:%s]", spell(lo), spell(hi));
        bin.kind = COVERAGE_BINS;
        bin.construct = -1;
        bin.gated = 1;
        bin.spans.push_back('{lo, hi});
        value_bins.push_back(bin);
      end
    end
  endfunction

  local function void report_overlaps();
    for (int a = 0; a < value_bins.size(); a++) begin
      if (value_bins[a].kind != COVERAGE_BINS) continue;
      for (int b = a + 1; b < value_bins.size(); b++) begin
        span_t common[$];
        if (value_bins[b].kind != COVERAGE_BINS) continue;
        common = value_bins[a].spans;
        begin
          span_t complement[$];
          complement.push_back('{'0, '1});
          subtract(complement, value_bins[b].spans);
          subtract(common, complement);
        end
        if (common.size() > 0)
          $warning("bins '%s' and '%s' of coverpoint '%s' cover a value in common", value_bins[a].name, value_bins[b].name, name);
      end
    end
  endfunction

  // Whether the `iff` of the construct numbered `construct` held at this sample
  // (LRM 19.5.1); it guards every bin the construct made.
  function void gate(int construct, bit open);
    foreach (value_bins[i]) if (value_bins[i].construct == construct) value_bins[i].gated = open;
    foreach (transition_bins[i])
      if (transition_bins[i].construct == construct) transition_bins[i].gated = open;
    if (default_sequence_present && default_sequence_construct == construct)
      default_sequence_gated = open;
  endfunction

  // Counts one sampled value (LRM 19.5).
  function void sample(T value);
    bit in_any = 0;
    key_t key;
    hits.delete();
    if (!collecting) return;
    if ($isunknown(value)) begin
      foreach (value_bins[i])
        if (value_bins[i].kind == COVERAGE_DEFAULT_BINS && !value_bins[i].per_value &&
            value_bins[i].gated) value_bins[i].count++;
      return;
    end
    key = key_of(value);
    foreach (value_bins[i]) begin
      if (value_bins[i].kind == COVERAGE_ILLEGAL_BINS && contains(value_bins[i].spans, key)) begin
        $error("the value %s of coverpoint '%s' is in its illegal bin '%s'", spell(key), name, value_bins[i].name);
        return;
      end
    end
    foreach (value_bins[i]) begin
      if (value_bins[i].kind == COVERAGE_DEFAULT_BINS) continue;
      if (!contains(value_bins[i].spans, key)) continue;
      in_any = 1;
      if (value_bins[i].kind != COVERAGE_BINS || !value_bins[i].gated) continue;
      if (value_bins[i].per_value) begin
        per_value_counts[i][key]++;
        hits.push_back(value_bins[i].first_index + rank_of(value_bins[i].spans, key));
      end else begin
        value_bins[i].count++;
        hits.push_back(value_bins[i].first_index);
      end
    end
    sample_transitions(key);
    if (!in_any) begin
      foreach (value_bins[i]) begin
        if (value_bins[i].kind != COVERAGE_DEFAULT_BINS || !value_bins[i].gated) continue;
        if (value_bins[i].per_value) per_value_counts[i][key]++;
        else value_bins[i].count++;
      end
    end
  endfunction

  // Advances every attempt at every sequence by one sampled value, starts a
  // new one, and counts each bin a sequence of which completed (at most once a
  // sample, LRM 19.5.2).
  local function void sample_transitions(key_t key);
    bit any_completed = 0;
    bit pending_before = 0;
    bit pending_after = 0;
    foreach (transition_bins[t]) begin
      bit completed = 0;
      foreach (sequences[t][s]) begin
        attempt_t next[$];
        bit done = 0;
        if (attempts[t][s].size() > 0) pending_before = 1;
        attempts[t][s].push_back('{0, 0});
        foreach (attempts[t][s][a]) advance(sequences[t][s], attempts[t][s][a], key, next, done);
        attempts[t][s] = next;
        if (attempts[t][s].size() > 0) pending_after = 1;
        if (done) completed = 1;
      end
      if (!completed || !transition_bins[t].gated || !collecting) continue;
      case (transition_bins[t].kind)
        COVERAGE_ILLEGAL_BINS:
          $error("a sequence of values of coverpoint '%s' is in its illegal bin '%s'", name, transition_bins[t].name);
        COVERAGE_BINS: begin
          transition_bins[t].count++;
          any_completed = 1;
          hits.push_back(transition_bins[t].first_index);
        end
        default: ;
      endcase
    end
    if (default_sequence_present && default_sequence_gated && has_sample && !any_completed &&
        !(pending_before && pending_after))
      default_sequence_count++;
    has_sample = 1;
  endfunction

  // Where one attempt can go on taking `key`: as another match of its step,
  // or, once the step matched often enough, as the first match of the next.
  local function void advance(input step_t steps[$], attempt_t at, key_t key,
                              ref attempt_t next[$], ref bit done);
    step_t now = steps[at.step];
    bit fits = contains(now.spans, key);
    if (fits && at.matched < now.most) arrive(steps, attempt_t'{at.step, at.matched + 1}, next, done);
    else if (!fits && at.matched < now.most && now.repetition != COVERAGE_CONSECUTIVE &&
             now.repetition != COVERAGE_ONCE)
      keep(next, at);
    else if (!fits && at.matched >= now.least && now.repetition == COVERAGE_NONCONSECUTIVE)
      keep(next, at);
    if (at.matched >= now.least && at.step + 1 < steps.size()) begin
      step_t after = steps[at.step + 1];
      if (contains(after.spans, key)) arrive(steps, attempt_t'{at.step + 1, 1}, next, done);
      else if (after.repetition == COVERAGE_GOTO || after.repetition == COVERAGE_NONCONSECUTIVE)
        if (at.step > 0 || at.matched > 0) keep(next, attempt_t'{at.step + 1, 0});
    end
  endfunction

  local function void arrive(input step_t steps[$], attempt_t at, ref attempt_t next[$],
                             ref bit done);
    step_t now = steps[at.step];
    if (at.step == steps.size() - 1 && at.matched >= now.least) done = 1;
    if (at.matched < now.most || at.step + 1 < steps.size()) keep(next, at);
  endfunction

  local function void keep(ref attempt_t next[$], input attempt_t at);
    foreach (next[i]) if (next[i] == at) return;
    next.push_back(at);
  endfunction

  virtual function int normal_bin_count();
    return counting;
  endfunction

  virtual function string normal_bin_name(int index);
    foreach (value_bins[i]) begin
      int width;
      if (value_bins[i].kind != COVERAGE_BINS) continue;
      width = value_bins[i].per_value ? int'(size_of(value_bins[i].spans)) : 1;
      if (index < value_bins[i].first_index + width && index >= value_bins[i].first_index) begin
        if (!value_bins[i].per_value) return value_bins[i].name;
        return $sformatf("%s[%s]", value_bins[i].name,
                         spell(key_at(value_bins[i].spans, index - value_bins[i].first_index)));
      end
    end
    foreach (transition_bins[i])
      if (transition_bins[i].kind == COVERAGE_BINS && transition_bins[i].first_index == index)
        return transition_bins[i].name;
    return "";
  endfunction

  virtual function void bins_named(string bin_name, ref int indices[$]);
    foreach (value_bins[i]) begin
      int width;
      if (value_bins[i].kind != COVERAGE_BINS) continue;
      if (value_bins[i].name != bin_name &&
          !(value_bins[i].name.len() > bin_name.len() &&
            value_bins[i].name.substr(0, bin_name.len()) == {bin_name, "["}))
        continue;
      width = value_bins[i].per_value ? int'(size_of(value_bins[i].spans)) : 1;
      for (int w = 0; w < width; w++) indices.push_back(value_bins[i].first_index + w);
    end
    foreach (transition_bins[i])
      if (transition_bins[i].kind == COVERAGE_BINS && transition_bins[i].name == bin_name)
        indices.push_back(transition_bins[i].first_index);
  endfunction

  // Opens a range list a cross's select expression intersects this
  // coverpoint's bins with (LRM 19.6.1), given as a bin's is.
  function void begin_selection();
    pending.delete();
  endfunction

  // The counting bins with a value in the range list just given.
  function coverage_bin_numbers selected_bins();
    coverage_bin_numbers indices;
    span_t wanted[$];
    foreach (pending[i]) spans_of(pending[i], wanted);
    pending.delete();
    normalize(wanted);
    for (int index = 0; index < counting; index++) begin
      span_t own[$];
      span_t common[$];
      span_t complement[$];
      values_of_bin(index, own);
      common = own;
      complement.push_back('{'0, '1});
      subtract(complement, wanted);
      subtract(common, complement);
      if (common.size() > 0) indices.push_back(index);
    end
    return indices;
  endfunction

  // The values of one counting bin, as runs; a transition bin's are the
  // values its sequences end on (LRM 19.6.1).
  local function void values_of_bin(int index, ref span_t spans[$]);
    foreach (value_bins[i]) begin
      int width;
      if (value_bins[i].kind != COVERAGE_BINS) continue;
      width = value_bins[i].per_value ? int'(size_of(value_bins[i].spans)) : 1;
      if (index >= value_bins[i].first_index && index < value_bins[i].first_index + width) begin
        if (value_bins[i].per_value) begin
          key_t key = key_at(value_bins[i].spans, index - value_bins[i].first_index);
          spans.push_back('{key, key});
        end else begin
          spans = value_bins[i].spans;
        end
        return;
      end
    end
    foreach (transition_bins[t]) begin
      if (transition_bins[t].kind != COVERAGE_BINS || transition_bins[t].first_index != index)
        continue;
      foreach (sequences[t][s]) foreach (sequences[t][s][$].spans[k])
        spans.push_back(sequences[t][s][$].spans[k]);
      normalize(spans);
      return;
    end
  endfunction

  // The values of one counting bin, in order, for a cross's `with` filter
  // evaluated over them (LRM 19.6.1.2).
  function values_t values_in_bin(int index);
    values_t values;
    span_t spans[$];
    values_of_bin(index, spans);
    foreach (spans[i]) begin
      key_t key = spans[i].lo;
      forever begin
        values.push_back(value_of(key));
        if (key == spans[i].hi) break;
        key++;
      end
    end
    return values;
  endfunction

  virtual function longint bin_size(int index);
    span_t spans[$];
    size_t size;
    values_of_bin(index, spans);
    size = size_of(spans);
    return size > size_t'(64'h7FFF_FFFF_FFFF_FFFF) ? -1 : longint'(size);
  endfunction

  // The counting bins holding a value, and how the value is spelled, for a
  // cross's set expression (LRM 19.6.1.4).
  function coverage_bin_numbers numbers_containing(T value);
    coverage_bin_numbers numbers;
    key_t key = key_of(value);
    for (int index = 0; index < counting; index++) begin
      span_t spans[$];
      values_of_bin(index, spans);
      if (contains(spans, key)) numbers.push_back(index);
    end
    return numbers;
  endfunction

  function string text_of(T value);
    return spell(key_of(value));
  endfunction

  // Every value of the coverpoint's type, for a bin that names the
  // coverpoint itself (LRM 19.5.1.1).
  function element_t every_value();
    element_t all;
    all.lo = value_of('0);
    all.hi = value_of('1);
    all.care = '1;
    return all;
  endfunction

  virtual function void parts(output int covered, output int total);
    covered = 0;
    total = 0;
    foreach (value_bins[i]) begin
      if (value_bins[i].kind != COVERAGE_BINS) continue;
      if (value_bins[i].per_value) begin
        size_t width = size_of(value_bins[i].spans);
        total += int'(width);
        foreach (per_value_counts[i][key])
          if (per_value_counts[i][key] >= option.at_least) covered++;
      end else if (value_bins[i].spans.size() > 0) begin
        total++;
        if (value_bins[i].count >= option.at_least) covered++;
      end
    end
    foreach (transition_bins[i]) begin
      if (transition_bins[i].kind != COVERAGE_BINS || sequences[i].size() == 0) continue;
      total++;
      if (transition_bins[i].count >= option.at_least) covered++;
    end
  endfunction

  virtual function void counts_by_name(ref longint counts[string]);
    foreach (value_bins[i]) begin
      if (value_bins[i].kind != COVERAGE_BINS) continue;
      if (value_bins[i].per_value) begin
        size_t width = size_of(value_bins[i].spans);
        for (size_t w = 0; w < width; w++) begin
          key_t key = key_at(value_bins[i].spans, w);
          string bin_name = $sformatf("%s[%s]", value_bins[i].name, spell(key));
          longint count = per_value_counts[i].exists(key) ? per_value_counts[i][key] : 0;
          counts[bin_name] = counts.exists(bin_name) ? counts[bin_name] + count : count;
        end
      end else if (value_bins[i].spans.size() > 0) begin
        counts[value_bins[i].name] = counts.exists(value_bins[i].name) ?
            counts[value_bins[i].name] + value_bins[i].count : value_bins[i].count;
      end
    end
    foreach (transition_bins[i]) begin
      if (transition_bins[i].kind != COVERAGE_BINS || sequences[i].size() == 0) continue;
      counts[transition_bins[i].name] = counts.exists(transition_bins[i].name) ?
          counts[transition_bins[i].name] + transition_bins[i].count : transition_bins[i].count;
    end
  endfunction
endclass

// A coverpoint of a real expression (LRM 19.5.1): its bins hold values and
// intervals, never transitions, and none are made for it automatically.
class real_coverpoint extends coverage_coverpoint;
  // A value or an interval; an end is in it unless that end is open.
  typedef struct {
    real lo;
    real hi;
    bit hi_open;
    string label;
  } interval_t;

  typedef struct {
    string name;
    coverage_bin_kind kind;
    int construct;
    bit gated;
    interval_t parts[$];
    longint count;
  } real_bin_t;

  coverpoint_option option;
  local real_bin_t real_bins[$];
  local string pending_name;
  local int pending_construct;
  local coverage_bin_kind pending_kind;
  local coverage_bin_array pending_array;
  local int pending_count;
  local interval_t pending[$];
  local int counting;

  function new(covergroup_instance group, string name, bit counted);
    super.new(group, name, counted);
    option.weight = 1;
    option.goal = 100;
    option.comment = "";
    option.at_least = group.option.at_least;
    option.auto_bin_max = group.option.auto_bin_max;
    option.detect_overlap = group.option.detect_overlap;
  endfunction

  virtual function int weight();
    return option.weight;
  endfunction

  virtual function int goal();
    return option.goal;
  endfunction

  virtual function int at_least();
    return option.at_least;
  endfunction

  virtual function int type_weight();
    return owner_type.coverpoint_type_options[name].weight;
  endfunction

  static function string spell(real value);
    if (value == $floor(value) && value < 1.0e15 && value > -1.0e15) return $sformatf("%0.1f", value);
    return $sformatf("%0g", value);
  endfunction

  local function real interval_size();
    return owner_type.coverpoint_type_options[name].real_interval;
  endfunction

  // The bins being given: their name, what they count toward, how many bins
  // the construct makes -- one per value and interval for `b[]` -- and how
  // many it gives; and the number of the construct giving them, by its place
  // among the coverpoint's constructs in the source.
  function void begin_bins(int construct, string bin_name, coverage_bin_kind kind,
                           coverage_bin_array array_kind, int count);
    pending_construct = construct;
    pending_name = bin_name;
    pending_kind = kind;
    pending_array = array_kind;
    pending_count = count;
  endfunction

  // An element of the range list being given: a single value, or a range,
  // which is divided into intervals of `real_interval` unless an end of it is
  // given as `$` (LRM 19.5.1).
  function void add_value(real value);
    pending.push_back('{value, value, 0, spell(value)});
  endfunction

  function void add_range(real lo, real hi);
    real size = interval_size();
    if (lo > hi) return;
    if (size <= 0.0 || hi - lo <= size) begin
      pending.push_back('{lo, hi, 0, $sformatf("%s:%s]", spell(lo), spell(hi))});
      return;
    end
    for (real at = lo; at < hi; at += size) begin
      if (at + size >= hi) pending.push_back('{at, hi, 0, $sformatf("%s:%s]", spell(at), spell(hi))});
      else pending.push_back('{at, at + size, 1, $sformatf("%s:%s)", spell(at), spell(at + size))});
    end
  endfunction

  function void add_range_from_least(real hi);
    pending.push_back('{-infinity(), hi, 0, $sformatf("$:%s]", spell(hi))});
  endfunction

  function void add_range_to_greatest(real lo);
    pending.push_back('{lo, infinity(), 0, $sformatf("%s:$]", spell(lo))});
  endfunction

  function void add_every_value();
    pending.push_back('{-infinity(), infinity(), 0, "$:$]"});
  endfunction

  // `[center +/- tolerance]`, or `[center +%- tolerance]` where the
  // tolerance is a percentage of the center (LRM 11.4.13), a range like any
  // other.
  function void add_tolerance(real center, real tolerance, bit in_percent);
    real reach = in_percent ? center * tolerance / 100.0 : tolerance;
    if (reach < 0.0) reach = -reach;
    add_range(center - reach, center + reach);
  endfunction

  local static function real infinity();
    return $bitstoreal(64'h7FF0_0000_0000_0000);
  endfunction

  // The bins the pending range list makes (LRM 19.5.1): one, one per value
  // and interval with identical intervals merged, or a fixed number sharing
  // the items in order.
  function void end_bins();
    if (pending_array == COVERAGE_BIN_PER_VALUE && pending_kind == COVERAGE_BINS) begin
      string seen[string];
      foreach (pending[i]) begin
        real_bin_t bin;
        bin.name = (pending[i].lo == pending[i].hi) ? $sformatf("%s[%s]", pending_name, pending[i].label) :
            $sformatf("%s[%s", pending_name, pending[i].label);
        if (seen.exists(bin.name)) continue;
        seen[bin.name] = "";
        bin.kind = pending_kind;
        bin.construct = pending_construct;
        bin.gated = 1;
        bin.parts.push_back(pending[i]);
        real_bins.push_back(bin);
      end
    end else if (pending_array == COVERAGE_FIXED_BINS && pending_kind == COVERAGE_BINS) begin
      int each = pending.size() / pending_count;
      if (each == 0) each = 1;
      for (int b = 0; b < pending_count; b++) begin
        real_bin_t bin;
        int first = b * each;
        int last = (b == pending_count - 1) ? pending.size() : first + each;
        if (last > pending.size()) last = pending.size();
        bin.name = $sformatf("%s[%0d]", pending_name, b);
        bin.kind = pending_kind;
        bin.construct = pending_construct;
        bin.gated = 1;
        for (int i = first; i < last; i++) bin.parts.push_back(pending[i]);
        real_bins.push_back(bin);
      end
    end else begin
      real_bin_t bin;
      bin.name = pending_name;
      bin.kind = pending_kind;
      bin.construct = pending_construct;
      bin.gated = 1;
      bin.parts = pending;
      real_bins.push_back(bin);
    end
    pending.delete();
  endfunction

  // A default bin of a real coverpoint is never an array of bins (LRM
  // 19.5.1), which the front end enforces.
  function void add_default_bin(int construct, string bin_name, bit per_value);
    real_bin_t bin;
    bin.name = bin_name;
    bin.kind = COVERAGE_DEFAULT_BINS;
    bin.construct = construct;
    bin.gated = 1;
    real_bins.push_back(bin);
  endfunction

  function void finish();
    counting = 0;
    foreach (real_bins[i]) if (real_bins[i].kind == COVERAGE_BINS) counting++;
  endfunction

  // Whether the `iff` of the construct numbered `construct` held at this sample
  // (LRM 19.5.1); it guards every bin the construct made.
  function void gate(int construct, bit open);
    foreach (real_bins[i]) if (real_bins[i].construct == construct) real_bins[i].gated = open;
  endfunction

  local static function bit holds(input interval_t parts[$], real value);
    foreach (parts[i])
      if (value >= parts[i].lo && (parts[i].hi_open ? value < parts[i].hi : value <= parts[i].hi))
        return 1;
    return 0;
  endfunction

  local function bit excluded(real value);
    foreach (real_bins[i])
      if ((real_bins[i].kind == COVERAGE_IGNORE_BINS || real_bins[i].kind == COVERAGE_ILLEGAL_BINS) &&
          holds(real_bins[i].parts, value))
        return 1;
    return 0;
  endfunction

  function void sample(real value);
    bit in_any = 0;
    int index = 0;
    hits.delete();
    if (!collecting) return;
    foreach (real_bins[i]) begin
      if (real_bins[i].kind == COVERAGE_ILLEGAL_BINS && holds(real_bins[i].parts, value)) begin
        $error("the value %s of coverpoint '%s' is in its illegal bin '%s'", spell(value), name, real_bins[i].name);
        return;
      end
    end
    foreach (real_bins[i]) begin
      if (real_bins[i].kind == COVERAGE_DEFAULT_BINS) continue;
      if (holds(real_bins[i].parts, value)) begin
        in_any = 1;
        if (real_bins[i].kind == COVERAGE_BINS && real_bins[i].gated && !excluded(value)) begin
          real_bins[i].count++;
          hits.push_back(index);
        end
      end
      if (real_bins[i].kind == COVERAGE_BINS) index++;
    end
    if (!in_any)
      foreach (real_bins[i])
        if (real_bins[i].kind == COVERAGE_DEFAULT_BINS && real_bins[i].gated) real_bins[i].count++;
  endfunction

  virtual function int normal_bin_count();
    return counting;
  endfunction

  virtual function string normal_bin_name(int index);
    int at = 0;
    foreach (real_bins[i]) begin
      if (real_bins[i].kind != COVERAGE_BINS) continue;
      if (at == index) return real_bins[i].name;
      at++;
    end
    return "";
  endfunction

  virtual function void bins_named(string bin_name, ref int indices[$]);
    int at = 0;
    foreach (real_bins[i]) begin
      if (real_bins[i].kind != COVERAGE_BINS) continue;
      if (real_bins[i].name == bin_name ||
          (real_bins[i].name.len() > bin_name.len() &&
           real_bins[i].name.substr(0, bin_name.len()) == {bin_name, "["}))
        indices.push_back(at);
      at++;
    end
  endfunction

  // Opens a range list a cross's select expression intersects this
  // coverpoint's bins with (LRM 19.6.1), given as a bin's is.
  function void begin_selection();
    pending.delete();
  endfunction

  // The counting bins with a value in the range list just given.
  function coverage_bin_numbers selected_bins();
    coverage_bin_numbers indices;
    int at = 0;
    foreach (real_bins[i]) begin
      bit meets = 0;
      if (real_bins[i].kind != COVERAGE_BINS) continue;
      foreach (real_bins[i].parts[p]) foreach (pending[q])
        if (real_bins[i].parts[p].lo <= pending[q].hi && pending[q].lo <= real_bins[i].parts[p].hi) meets = 1;
      if (meets) indices.push_back(at);
      at++;
    end
    pending.delete();
    return indices;
  endfunction

  // A bin holds as many values as it lists single values, and more than can
  // be counted where it holds an interval.
  virtual function longint bin_size(int index);
    int at = 0;
    foreach (real_bins[i]) begin
      longint size = 0;
      if (real_bins[i].kind != COVERAGE_BINS) continue;
      if (at++ != index) continue;
      foreach (real_bins[i].parts[p]) begin
        if (real_bins[i].parts[p].lo != real_bins[i].parts[p].hi) return -1;
        size++;
      end
      return size;
    end
    return 0;
  endfunction

  function coverage_bin_numbers numbers_containing(real value);
    coverage_bin_numbers numbers;
    int at = 0;
    foreach (real_bins[i]) begin
      if (real_bins[i].kind != COVERAGE_BINS) continue;
      if (holds(real_bins[i].parts, value)) numbers.push_back(at);
      at++;
    end
    return numbers;
  endfunction

  function string text_of(real value);
    return spell(value);
  endfunction

  virtual function void parts(output int covered, output int total);
    covered = 0;
    total = 0;
    foreach (real_bins[i]) begin
      if (real_bins[i].kind != COVERAGE_BINS || real_bins[i].parts.size() == 0) continue;
      total++;
      if (real_bins[i].count >= option.at_least) covered++;
    end
  endfunction

  virtual function void counts_by_name(ref longint counts[string]);
    foreach (real_bins[i]) begin
      if (real_bins[i].kind != COVERAGE_BINS || real_bins[i].parts.size() == 0) continue;
      counts[real_bins[i].name] = counts.exists(real_bins[i].name) ?
          counts[real_bins[i].name] + real_bins[i].count : real_bins[i].count;
    end
  endfunction
endclass

// A set of bin tuples of a cross (LRM 19.6.1), each tuple numbered by the
// cross.
class coverage_selection;
  bit tuples[longint];

  function coverage_selection both(coverage_selection other);
    coverage_selection made = new;
    foreach (tuples[t]) if (other.tuples.exists(t)) made.tuples[t] = 1;
    return made;
  endfunction

  function coverage_selection either(coverage_selection other);
    coverage_selection made = new;
    made.tuples = tuples;
    foreach (other.tuples[t]) made.tuples[t] = 1;
    return made;
  endfunction
endclass

// A cross of coverpoints (LRM 19.6): its bins are the tuples of the crossed
// coverpoints' counting bins, the user's bins each a set of them, and the
// tuples no user bin holds, unless the option says otherwise.
class coverage_cross extends coverage_item;
  typedef struct {
    string name;
    coverage_bin_kind kind;
    bit gated;
    coverage_selection selection;
    longint count;
  } user_bin_t;

  cross_option option;
  local coverage_coverpoint dimensions[$];
  local user_bin_t user_bins[$];
  local bit held_by_user_bin[longint];
  local longint automatic_counts[longint];
  local longint product;
  local int retained;
  local longint satisfied[longint];
  local longint offered[longint];
  local bit listed[longint][string];
  local coverage_bin_numbers pending_components[$];
  local string pending_text;
  local coverage_selection filtering;

  function new(covergroup_instance group, string name, bit counted);
    super.new(group, name, counted);
    option.weight = 1;
    option.goal = 100;
    option.comment = "";
    option.at_least = group.option.at_least;
    option.cross_num_print_missing = group.option.cross_num_print_missing;
    option.cross_retain_auto_bins = group.option.cross_retain_auto_bins;
  endfunction

  virtual function int weight();
    return option.weight;
  endfunction

  virtual function int goal();
    return option.goal;
  endfunction

  virtual function int at_least();
    return option.at_least;
  endfunction

  virtual function int type_weight();
    return owner_type.cross_type_options[name].weight;
  endfunction

  // A crossed coverpoint, in the order the cross lists them; every one is
  // given before any bin is.
  function void cross_coverpoint(coverage_coverpoint crossed);
    dimensions.push_back(crossed);
    product = 1;
    foreach (dimensions[d]) product *= dimensions[d].normal_bin_count();
  endfunction

  function int component(longint tuple, int dimension);
    for (int d = dimensions.size() - 1; d > dimension; d--)
      tuple /= dimensions[d].normal_bin_count();
    return int'(tuple % dimensions[dimension].normal_bin_count());
  endfunction

  // Every tuple whose component in one dimension is one of `numbers` (a
  // select condition, LRM 19.6.1).
  function coverage_selection select_numbers(int dimension, coverage_bin_numbers numbers);
    coverage_selection made = new;
    bit wanted[int];
    foreach (numbers[i]) wanted[numbers[i]] = 1;
    for (longint tuple = 0; tuple < product; tuple++)
      if (wanted.exists(component(tuple, dimension))) made.tuples[tuple] = 1;
    return made;
  endfunction

  // Every tuple `selected` does not hold (`!`, LRM 19.6.1).
  function coverage_selection select_outside(coverage_selection selected);
    coverage_selection made = new;
    for (longint tuple = 0; tuple < product; tuple++)
      if (!selected.tuples.exists(tuple)) made.tuples[tuple] = 1;
    return made;
  endfunction

  // The numbers in both lists.
  static function coverage_bin_numbers both_numbers(coverage_bin_numbers first,
                                                    coverage_bin_numbers second);
    coverage_bin_numbers common;
    bit listed[int];
    foreach (second[i]) listed[second[i]] = 1;
    foreach (first[i]) if (listed.exists(first[i])) common.push_back(first[i]);
    return common;
  endfunction

  // Every tuple (the cross named as a select expression, LRM 19.6.1.2).
  function coverage_selection select_all();
    coverage_selection made = new;
    for (longint tuple = 0; tuple < product; tuple++) made.tuples[tuple] = 1;
    return made;
  endfunction

  // A select expression's `with` filter or a set expression (LRM 19.6.1.2,
  // 19.6.1.4) keeps, of the tuples `candidates` holds, those with at least
  // `least` value tuples satisfying it, or with every one where `least` is
  // negative (`matches $`).
  function void begin_filter(coverage_selection candidates);
    filtering = candidates;
    satisfied.delete();
    offered.delete();
    listed.delete();
    pending_components.delete();
    pending_text = "";
  endfunction

  function coverage_tuples candidate_tuples();
    coverage_tuples tuples;
    foreach (filtering.tuples[t]) tuples.push_back(t);
    return tuples;
  endfunction

  // One value tuple of a candidate, and whether it satisfies a `with` filter.
  function void offer(longint tuple, bit holds);
    offered[tuple] = offered.exists(tuple) ? offered[tuple] + 1 : 1;
    if (holds) satisfied[tuple] = satisfied.exists(tuple) ? satisfied[tuple] + 1 : 1;
  endfunction

  // One component of a value tuple a set expression lists: the bins of its
  // dimension holding it, and how it is spelled, so that a value tuple listed
  // twice counts once.
  function void value_component(coverage_bin_numbers numbers, string text);
    pending_components.push_back(numbers);
    pending_text = {pending_text, text, ","};
  endfunction

  // The value tuple whose components were just given, counted toward every
  // candidate tuple whose bins hold it.
  function void end_value_tuple();
    longint tuples[$];
    tuples.push_back(0);
    foreach (pending_components[d]) begin
      longint grown[$];
      foreach (tuples[t])
        foreach (pending_components[d][n])
          grown.push_back(tuples[t] * dimensions[d].normal_bin_count() + pending_components[d][n]);
      tuples = grown;
    end
    foreach (tuples[t]) begin
      if (!filtering.tuples.exists(tuples[t])) continue;
      if (listed.exists(tuples[t]) && listed[tuples[t]].exists(pending_text)) continue;
      listed[tuples[t]][pending_text] = 1;
      satisfied[tuples[t]] = satisfied.exists(tuples[t]) ? satisfied[tuples[t]] + 1 : 1;
    end
    pending_components.delete();
    pending_text = "";
  endfunction

  // How many value tuples a tuple's bins hold, or -1 where one of them holds
  // more than can be counted.
  local function longint value_tuple_count(longint tuple);
    longint count = 1;
    foreach (dimensions[d]) begin
      longint size = dimensions[d].bin_size(component(tuple, d));
      if (size < 0) return -1;
      count *= size;
    end
    return count;
  endfunction

  function coverage_selection end_filter(longint least);
    coverage_selection made = new;
    foreach (satisfied[t]) begin
      longint needed = least >= 0 ? least : offered.exists(t) ? offered[t] : value_tuple_count(t);
      if (needed >= 0 && satisfied[t] >= needed) made.tuples[t] = 1;
    end
    return made;
  endfunction

  function void add_bin(string bin_name, coverage_bin_kind kind, coverage_selection selection);
    user_bin_t bin;
    bin.name = bin_name;
    bin.kind = kind;
    bin.gated = 1;
    bin.selection = selection;
    user_bins.push_back(bin);
  endfunction

  function void finish();
    held_by_user_bin.delete();
    foreach (user_bins[b]) foreach (user_bins[b].selection.tuples[t]) held_by_user_bin[t] = 1;
    retained = 0;
    if (option.cross_retain_auto_bins || user_bins.size() == 0)
      retained = int'(product - held_by_user_bin.size());
  endfunction

  // Whether the `iff` of the construct numbered `construct` held at this sample
  // (LRM 19.6.1). Each construct of a cross makes one bin, in the order the
  // source declares them, so the number is the bin's place.
  function void gate(int construct, bit open);
    user_bins[construct].gated = open;
  endfunction

  local function bit retains(longint tuple);
    return (option.cross_retain_auto_bins || user_bins.size() == 0) &&
        !held_by_user_bin.exists(tuple);
  endfunction

  // Counts the tuples the crossed coverpoints' last samples make (LRM 19.6).
  function void sample();
    longint tuples[$];
    if (!collecting) return;
    tuples.push_back(0);
    foreach (dimensions[d]) begin
      longint grown[$];
      foreach (tuples[t])
        foreach (dimensions[d].hits[h])
          grown.push_back(tuples[t] * dimensions[d].normal_bin_count() + dimensions[d].hits[h]);
      tuples = grown;
    end
    foreach (tuples[t]) begin
      bit illegal = 0;
      bit ignored = 0;
      foreach (user_bins[b]) begin
        if (!user_bins[b].selection.tuples.exists(tuples[t])) continue;
        if (user_bins[b].kind == COVERAGE_ILLEGAL_BINS) illegal = 1;
        if (user_bins[b].kind == COVERAGE_IGNORE_BINS) ignored = 1;
      end
      if (illegal) begin
        $error("a combination of values crossed by '%s' is in its illegal bins", name);
        continue;
      end
      if (ignored) continue;
      foreach (user_bins[b])
        if (user_bins[b].kind == COVERAGE_BINS && user_bins[b].gated &&
            user_bins[b].selection.tuples.exists(tuples[t]))
          user_bins[b].count++;
      if (retains(tuples[t]))
        automatic_counts[tuples[t]] = automatic_counts.exists(tuples[t]) ?
            automatic_counts[tuples[t]] + 1 : 1;
    end
  endfunction

  local function string tuple_name(longint tuple);
    string spelled = "<";
    foreach (dimensions[d]) begin
      if (d > 0) spelled = {spelled, ","};
      spelled = {spelled, dimensions[d].normal_bin_name(component(tuple, d))};
    end
    return {spelled, ">"};
  endfunction

  // Coverage over the retained tuples and the user's counting bins (LRM
  // 19.11.2).
  virtual function void parts(output int covered, output int total);
    covered = 0;
    total = retained;
    foreach (automatic_counts[t]) if (automatic_counts[t] >= option.at_least) covered++;
    foreach (user_bins[b]) begin
      if (user_bins[b].kind != COVERAGE_BINS) continue;
      total++;
      if (user_bins[b].count >= option.at_least) covered++;
    end
  endfunction

  virtual function void counts_by_name(ref longint counts[string]);
    for (longint tuple = 0; tuple < product; tuple++) begin
      string bin_name;
      longint count;
      if (!retains(tuple)) continue;
      bin_name = tuple_name(tuple);
      count = automatic_counts.exists(tuple) ? automatic_counts[tuple] : 0;
      counts[bin_name] = counts.exists(bin_name) ? counts[bin_name] + count : count;
    end
    foreach (user_bins[b]) begin
      if (user_bins[b].kind != COVERAGE_BINS) continue;
      counts[user_bins[b].name] = counts.exists(user_bins[b].name) ?
          counts[user_bins[b].name] + user_bins[b].count : user_bins[b].count;
    end
  endfunction

  virtual function bit is_cross();
    return 1;
  endfunction

  virtual function void write_database(int fd);
    longint counts[string];
    counts_by_name(counts);
    $fdisplay(fd, "        <cross name=\"%s\" key=\"0\">", coverage_registry::escaped(name));
    $fdisplay(fd, "          <options weight=\"%0d\" goal=\"%0d\" at_least=\"%0d\"/>",
              weight(), goal(), at_least());
    foreach (dimensions[d])
      $fdisplay(fd, "          <crossExpr>%s</crossExpr>",
                coverage_registry::escaped(dimensions[d].name));
    foreach (counts[b]) begin
      $fdisplay(fd, "          <crossBin name=\"%s\" key=\"0\">", coverage_registry::escaped(b));
      $fdisplay(fd, "            <index>-1</index>");
      $fdisplay(fd, "            <contents coverageCount=\"%0d\"/>", counts[b]);
      $fdisplay(fd, "          </crossBin>");
    end
    $fdisplay(fd, "        </cross>");
  endfunction
endclass

// One instance of a covergroup (LRM 19.3): its options, its coverpoints and
// crosses, and whether it collects. What it counted is kept by its type after
// the object holding it ends.
class covergroup_instance;
  covergroup_option option;
  covergroup_type owner;
  coverage_item items[$];
  bit collecting = 1;
  // Told where a block the instance is sampled at begins or ends, for a
  // covergroup a class embeds (LRM 19.3).
  event block_event;
  local bit strobe_pending;

  function new(covergroup_type owner, string name);
    this.owner = owner;
    option.name = name;
    option.weight = 1;
    option.goal = 100;
    option.comment = "";
    option.at_least = 1;
    option.auto_bin_max = 64;
    option.cross_num_print_missing = 0;
    option.cross_retain_auto_bins = 1;
    option.detect_overlap = 0;
    option.per_instance = 0;
    option.get_inst_coverage = 0;
  endfunction

  function void add(coverage_item item);
    items.push_back(item);
  endfunction

  function void set_inst_name(string name);
    option.name = name;
  endfunction

  function void start();
    collecting = 1;
  endfunction

  function void stop();
    collecting = 0;
  endfunction

  // The weighted average of the coverage of its coverpoints and crosses (LRM
  // 19.11), counting bins in `covered` and `total`.
  function real coverage(output int covered, output int total);
    real weighted = 0.0;
    longint weights = 0;
    covered = 0;
    total = 0;
    foreach (items[i]) begin
      int item_covered;
      int item_total;
      items[i].parts(item_covered, item_total);
      covered += item_covered;
      total += item_total;
      if (item_total == 0 || items[i].weight() == 0) continue;
      weighted += items[i].weight() * 100.0 * item_covered / item_total;
      weights += items[i].weight();
    end
    if (weights == 0) begin
      covered = 0;
      total = 0;
      return option.weight == 0 ? 100.0 : 0.0;
    end
    return weighted / weights;
  endfunction

  // `get_inst_coverage` (LRM 19.8): this instance's coverage, or the type's
  // where the type merges instances and this one does not track its own
  // (Table 19-1).
  function real get_inst_coverage(ref int covered, ref int total);
    if (owner.type_option.merge_instances && !option.get_inst_coverage)
      return owner.get_coverage(covered, total);
    return coverage(covered, total);
  endfunction

  function real inst_coverage();
    int covered;
    int total;
    return get_inst_coverage(covered, total);
  endfunction

  function coverage_item item_named(string name);
    foreach (items[i]) if (items[i].name == name) return items[i];
    return null;
  endfunction

  // Whether the instance is sampled once a time slot, in its Postponed
  // region, however often its event occurs there (LRM 19.7, `strobe`).
  function bit strobed();
    return owner.type_option.strobe;
  endfunction

  // Whether the event is the first of its time slot, which places the slot's
  // sample; the sample, once taken, lets the next slot's first event place
  // another.
  function bit claim_strobe();
    if (strobe_pending) return 0;
    strobe_pending = 1;
    return 1;
  endfunction

  function void strobe_taken();
    strobe_pending = 0;
  endfunction

  // This instance in the coverage database (LRM 19.9), its coverpoints before
  // its crosses as the interchange format orders them.
  function void write_database(int fd);
    $fdisplay(fd, "      <cgInstance name=\"%s\" key=\"0\">", coverage_registry::escaped(option.name));
    $fwrite(fd, "        <options weight=\"%0d\" goal=\"%0d\" at_least=\"%0d\"",
            option.weight, option.goal, option.at_least);
    $fdisplay(fd, " per_instance=\"%s\" merge_instances=\"%s\"/>",
              coverage_registry::boolean(option.per_instance),
              coverage_registry::boolean(owner.type_option.merge_instances));
    $fdisplay(fd, "        <cgId cgName=\"%s\" moduleName=\"%s\">",
              coverage_registry::escaped(owner.name), coverage_registry::escaped(owner.name));
    $fdisplay(fd, "          <cginstSourceId file=\"1\" line=\"1\" inlineCount=\"1\"/>");
    $fdisplay(fd, "          <cgSourceId file=\"1\" line=\"1\" inlineCount=\"1\"/>");
    $fdisplay(fd, "        </cgId>");
    foreach (items[i]) if (!items[i].is_cross()) items[i].write_database(fd);
    foreach (items[i]) if (items[i].is_cross()) items[i].write_database(fd);
    $fdisplay(fd, "      </cgInstance>");
  endfunction
endclass

// A covergroup type (LRM 19.11.3): its type options and every instance ever
// made of it, from which its cumulative coverage is computed.
class covergroup_type;
  string name;
  covergroup_type_option type_option;
  coverpoint_type_option coverpoint_type_options[string];
  cross_type_option cross_type_options[string];
  covergroup_instance instances[$];
  // Counts read back from a database (LRM 19.9), by item and bin name.
  longint loaded[string][string];
  // Whether the declaration has stated its items and type options, which it
  // does the first time the type is reached.
  bit declared;

  function new(string name);
    this.name = name;
    type_option.weight = 1;
    type_option.goal = 100;
    type_option.comment = "";
    type_option.strobe = 0;
    type_option.merge_instances = 0;
    type_option.distribute_first = 0;
    type_option.real_interval = 1.0;
  endfunction

  function void declare_coverpoint(string item);
    if (coverpoint_type_options.exists(item)) return;
    coverpoint_type_options[item].weight = 1;
    coverpoint_type_options[item].goal = 100;
    coverpoint_type_options[item].comment = "";
    coverpoint_type_options[item].real_interval = type_option.real_interval;
  endfunction

  function void declare_cross(string item);
    if (cross_type_options.exists(item)) return;
    cross_type_options[item].weight = 1;
    cross_type_options[item].goal = 100;
    cross_type_options[item].comment = "";
  endfunction

  function covergroup_instance make_instance();
    covergroup_instance made = new(this, $sformatf("%s_%0d", name, instances.size() + 1));
    instances.push_back(made);
    coverage_registry::instance_made();
    return made;
  endfunction

  // One item's coverage over every instance: the union of its bins by name,
  // a bin covered where its counts sum to the largest `at_least` of them
  // (merging), or the instances' coverages weighted by the item's own weight
  // in each (LRM 19.11.3).
  function real get_item_coverage(string item, ref int covered, ref int total);
    covered = 0;
    total = 0;
    if (type_option.merge_instances) begin
      longint counts[string];
      int least = 1;
      bit any = 0;
      foreach (instances[i]) begin
        coverage_item found = instances[i].item_named(item);
        if (found == null) continue;
        any = 1;
        found.counts_by_name(counts);
        if (found.at_least() > least) least = found.at_least();
      end
      if (loaded.exists(item)) foreach (loaded[item][b])
        counts[b] = counts.exists(b) ? counts[b] + loaded[item][b] : loaded[item][b];
      foreach (counts[b]) begin
        total++;
        if (counts[b] >= least) covered++;
      end
      return coverage_item::ratio(covered, total, 1);
    end
    begin
      real weighted = 0.0;
      longint weights = 0;
      foreach (instances[i]) begin
        coverage_item found = instances[i].item_named(item);
        int item_covered;
        int item_total;
        if (found == null) continue;
        found.parts(item_covered, item_total);
        covered += item_covered;
        total += item_total;
        if (item_total == 0 || found.weight() == 0) continue;
        weighted += found.weight() * 100.0 * item_covered / item_total;
        weights += found.weight();
      end
      if (weights == 0) return 0.0;
      return weighted / weights;
    end
  endfunction

  // `get_coverage` (LRM 19.8, 19.11.3).
  function real get_coverage(ref int covered, ref int total);
    covered = 0;
    total = 0;
    if (type_option.merge_instances) begin
      real weighted = 0.0;
      longint weights = 0;
      string items[$];
      bit seen[string];
      foreach (instances[i]) foreach (instances[i].items[j])
        if (!seen.exists(instances[i].items[j].name)) begin
          seen[instances[i].items[j].name] = 1;
          items.push_back(instances[i].items[j].name);
        end
      foreach (loaded[item]) if (!seen.exists(item)) items.push_back(item);
      foreach (items[k]) begin
        int item_covered;
        int item_total;
        int item_weight = coverpoint_type_options.exists(items[k]) ?
            coverpoint_type_options[items[k]].weight :
            cross_type_options.exists(items[k]) ? cross_type_options[items[k]].weight : 1;
        real value = get_item_coverage(items[k], item_covered, item_total);
        covered += item_covered;
        total += item_total;
        if (item_total == 0 || item_weight == 0) continue;
        weighted += item_weight * value;
        weights += item_weight;
      end
      if (weights == 0) begin
        covered = 0;
        total = 0;
        return type_option.weight == 0 ? 100.0 : 0.0;
      end
      return weighted / weights;
    end
    begin
      real weighted = 0.0;
      longint weights = 0;
      foreach (instances[i]) begin
        int inst_covered;
        int inst_total;
        real value = instances[i].coverage(inst_covered, inst_total);
        covered += inst_covered;
        total += inst_total;
        if (inst_total == 0 || instances[i].option.weight == 0) continue;
        weighted += instances[i].option.weight * value;
        weights += instances[i].option.weight;
      end
      if (loaded.size() > 0) begin
        int loaded_covered;
        int loaded_total;
        real value = loaded_coverage(loaded_covered, loaded_total);
        covered += loaded_covered;
        total += loaded_total;
        if (loaded_total > 0) begin
          weighted += value;
          weights++;
        end
      end
      if (weights == 0) begin
        covered = 0;
        total = 0;
        return type_option.weight == 0 ? 100.0 : 0.0;
      end
      return weighted / weights;
    end
  endfunction

  function real coverage();
    int covered;
    int total;
    return get_coverage(covered, total);
  endfunction

  // `get_coverage` of one coverpoint or cross over every instance.
  function real item_coverage(string item);
    int covered;
    int total;
    return get_item_coverage(item, covered, total);
  endfunction

  // What was read back from a database, as one instance of weight 1 whose
  // items weigh what this type gives them: a bin is covered where any run
  // that wrote it hit it.
  local function real loaded_coverage(ref int covered, ref int total);
    real weighted = 0.0;
    longint weights = 0;
    covered = 0;
    total = 0;
    foreach (loaded[item]) begin
      int item_covered = 0;
      int item_total = 0;
      int item_weight = coverpoint_type_options.exists(item) ?
          coverpoint_type_options[item].weight :
          cross_type_options.exists(item) ? cross_type_options[item].weight : 1;
      foreach (loaded[item][b]) begin
        item_total++;
        if (loaded[item][b] > 0) item_covered++;
      end
      covered += item_covered;
      total += item_total;
      if (item_total == 0 || item_weight == 0) continue;
      weighted += item_weight * 100.0 * item_covered / item_total;
      weights += item_weight;
    end
    if (weights == 0) return 0.0;
    return weighted / weights;
  endfunction

  // Counts read back for one bin, added to what earlier loads read.
  function void load(string item, string bin_name, longint count);
    loaded[item][bin_name] = loaded.exists(item) && loaded[item].exists(bin_name) ?
        loaded[item][bin_name] + count : count;
  endfunction

  // This type in the coverage database (LRM 19.9): every instance made of it,
  // and what was read back as one more.
  function void write_database(int fd, int id);
    $fdisplay(fd, "  <instanceCoverages name=\"%s\" key=\"0\" instanceId=\"%0d\">",
              coverage_registry::escaped(name), id);
    $fdisplay(fd, "    <id file=\"1\" line=\"1\" inlineCount=\"1\"/>");
    $fdisplay(fd, "    <covergroupCoverage>");
    foreach (instances[i]) instances[i].write_database(fd);
    if (loaded.size() > 0) begin
      $fdisplay(fd, "      <cgInstance name=\"%s\" key=\"0\">",
                coverage_registry::escaped({name, "_loaded"}));
      $fdisplay(fd, "        <options/>");
      $fdisplay(fd, "        <cgId cgName=\"%s\" moduleName=\"%s\">",
                coverage_registry::escaped(name), coverage_registry::escaped(name));
      $fdisplay(fd, "          <cginstSourceId file=\"1\" line=\"1\" inlineCount=\"1\"/>");
      $fdisplay(fd, "          <cgSourceId file=\"1\" line=\"1\" inlineCount=\"1\"/>");
      $fdisplay(fd, "        </cgId>");
      foreach (loaded[item]) begin
        $fdisplay(fd, "        <coverpoint name=\"%s\" key=\"0\">", coverage_registry::escaped(item));
        $fdisplay(fd, "          <options/>");
        foreach (loaded[item][b]) begin
          $fdisplay(fd, "          <coverpointBin name=\"%s\" type=\"bins\" key=\"0\">",
                    coverage_registry::escaped(b));
          $fdisplay(fd, "            <range from=\"-1\" to=\"-1\">");
          $fdisplay(fd, "              <contents coverageCount=\"%0d\"/>", loaded[item][b]);
          $fdisplay(fd, "            </range>");
          $fdisplay(fd, "          </coverpointBin>");
        end
        $fdisplay(fd, "        </coverpoint>");
      end
      $fdisplay(fd, "      </cgInstance>");
    end
    $fdisplay(fd, "    </covergroupCoverage>");
    $fdisplay(fd, "  </instanceCoverages>");
  endfunction
endclass

// Every covergroup type of the run, by the name of its declaration, and the
// database the run's coverage is written to at its end (LRM 19.9).
class coverage_registry;
  static covergroup_type types[string];
  static string database = "coverage.xml";
  static bit any_instance;
  static bit writing_scheduled;

  static function covergroup_type type_named(string name);
    if (!types.exists(name)) types[name] = new(name);
    return types[name];
  endfunction

  static function void instance_made();
    any_instance = 1;
  endfunction

  // `$get_coverage` (LRM 19.9): the types' coverage weighted by their type
  // weights, 100 where there is no type to weigh.
  static function real get_coverage();
    real weighted = 0.0;
    longint weights = 0;
    foreach (types[name]) begin
      int covered;
      int total;
      real value = types[name].get_coverage(covered, total);
      if (total == 0 || types[name].type_option.weight == 0) continue;
      weighted += types[name].type_option.weight * value;
      weights += types[name].type_option.weight;
    end
    if (weights == 0) return 100.0;
    return weighted / weights;
  endfunction

  static function void set_coverage_db_name(string name);
    database = name;
  endfunction

  // Whether this is the first call of the run, so that the database is
  // written once whatever number of covergroup types the run declares.
  static function bit claim_database_writing();
    if (writing_scheduled) return 0;
    writing_scheduled = 1;
    return 1;
  endfunction

  // `value` as an XML Schema boolean.
  static function string boolean(bit value);
    return value ? "true" : "false";
  endfunction

  // `text` as an XML attribute value or character data.
  static function string escaped(string text);
    string out = "";
    for (int i = 0; i < text.len(); i++) begin
      case (text[i])
        "&": out = {out, "&amp;"};
        "<": out = {out, "&lt;"};
        ">": out = {out, "&gt;"};
        "\"": out = {out, "&quot;"};
        "'": out = {out, "&apos;"};
        default: out = {out, string'(text[i])};
      endcase
    end
    return out;
  endfunction

  local static function string unescaped(string text);
    string out = "";
    int i = 0;
    while (i < text.len()) begin
      if (text[i] == "&") begin
        int last = i;
        string entity;
        while (last < text.len() && text[last] != ";") last++;
        entity = text.substr(i, last);
        case (entity)
          "&amp;": out = {out, "&"};
          "&lt;": out = {out, "<"};
          "&gt;": out = {out, ">"};
          "&quot;": out = {out, "\""};
          "&apos;": out = {out, "'"};
          default: out = {out, entity};
        endcase
        i = last + 1;
      end else begin
        out = {out, string'(text[i])};
        i++;
      end
    end
    return out;
  endfunction

  // Where `pattern` first starts in `text`, or -1.
  local static function int position(string text, string pattern);
    for (int i = 0; i + pattern.len() <= text.len(); i++)
      if (text.substr(i, i + pattern.len() - 1) == pattern) return i;
    return -1;
  endfunction

  // The value of attribute `key` on the one element `line` holds, or "".
  local static function string attribute(string line, string key);
    string opening = {" ", key, "=\""};
    int first = position(line, opening);
    int last;
    if (first < 0) return "";
    first += opening.len();
    last = first;
    while (last < line.len() && line[last] != "\"") last++;
    return unescaped(line.substr(first, last - 1));
  endfunction

  // Writes the run's coverage to the database at the end of the run (LRM
  // 19.9), in the XML interchange format of Accellera's Unified Coverage
  // Interoperability Standard.
  static function void write_database();
    int fd;
    int id = 0;
    if (!any_instance) return;
    fd = $fopen(database, "w");
    if (fd == 0) begin
      $warning("cannot write the coverage database '%s'", database);
      return;
    end
    $fdisplay(fd, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
    $fdisplay(fd, "<UCIS ucisVersion=\"1.0\" writtenBy=\"lyra\" writtenTime=\"1970-01-01T00:00:00\">");
    $fdisplay(fd, "  <sourceFiles fileName=\"%s\" id=\"1\"/>", escaped(database));
    $fwrite(fd, "  <historyNodes historyNodeId=\"0\" logicalName=\"%s\"", escaped(database));
    $fwrite(fd, " testStatus=\"true\" date=\"1970-01-01T00:00:00\" toolCategory=\"simulator\"");
    $fdisplay(fd, " ucisVersion=\"1.0\" vendorId=\"lyra\" vendorTool=\"lyra\" vendorToolVersion=\"1\"/>");
    foreach (types[name]) begin
      types[name].write_database(fd, id);
      id++;
    end
    $fdisplay(fd, "</UCIS>");
    $fclose(fd);
  endfunction

  // `$load_coverage_db` (LRM 19.9): the counts a database holds, added to
  // each type's by the name of its coverpoint or cross and of the bin.
  static function void load_coverage_db(string name);
    int fd = $fopen(name, "r");
    string type_name = "";
    string item = "";
    string bin_name = "";
    string line;
    if (fd == 0) begin
      $warning("cannot read the coverage database '%s'", name);
      return;
    end
    while ($fgets(line, fd) != 0) begin
      if (position(line, "<instanceCoverages ") >= 0) begin
        type_name = attribute(line, "name");
      end else if (position(line, "<coverpoint ") >= 0 || position(line, "<cross ") >= 0) begin
        item = attribute(line, "name");
      end else if (position(line, "<coverpointBin ") >= 0 || position(line, "<crossBin ") >= 0) begin
        bin_name = attribute(line, "name");
      end else if (position(line, "coverageCount=") >= 0) begin
        longint count;
        if ($sscanf(attribute(line, "coverageCount"), "%d", count) == 1)
          type_named(type_name).load(item, bin_name, count);
      end
    end
    $fclose(fd);
  endfunction
endclass

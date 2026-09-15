// A subroutine a package declares runs with no enclosing instance, and the
// readings a type's own declaration decides are available to it all the same.
// An enumeration answers with the name it declares for a value and with its
// neighbour in the declared order (LRM 6.19.5), and %p prints an aggregate as
// the assignment pattern its declaration names the members of (LRM 21.2.1.6).
// None of these is applied to an object, so none of them depends on one
// enclosing the code that asks.
//
// The enumeration reached inside an aggregate prints the name its own type
// declares, which is the same reading `name()` asks for directly -- so a
// package subroutine that never writes `name()` still needs it.
package P;
  typedef enum {IDLE, RUN, STOP} state_t;
  typedef struct packed {
    bit [3:0] high;
    bit [3:0] low;
  } halves_t;
  typedef struct {
    int count;
    state_t state;
  } entry_t;

  function automatic string name_of(state_t s);
    return s.name();
  endfunction

  function automatic state_t after(state_t s);
    return s.next();
  endfunction

  function automatic state_t preceding(state_t s);
    return s.prev();
  endfunction

  function automatic string pattern_of_halves(halves_t h);
    return $sformatf("%p", h);
  endfunction

  function automatic string pattern_of_entry(entry_t e);
    return $sformatf("%p", e);
  endfunction
endpackage

module Top;
  string run_name;
  P::state_t next_state;
  P::state_t prev_state;
  string halves_text;
  string entry_text;

  initial begin
    run_name = P::name_of(P::RUN);
    next_state = P::after(P::RUN);
    prev_state = P::preceding(P::RUN);
    halves_text = P::pattern_of_halves('{high: 4'hA, low: 4'h5});
    entry_text = P::pattern_of_entry('{count: 7, state: P::STOP});
  end

  final begin
    if (run_name !== "RUN") $fatal(1, "run_name was '%s', expected RUN", run_name);
    if (next_state !== P::STOP)
      $fatal(1, "after(RUN) was '%s', expected STOP", next_state.name());
    if (prev_state !== P::IDLE)
      $fatal(1, "preceding(RUN) was '%s', expected IDLE", prev_state.name());
    if (halves_text !== "'{high:10, low:5}")
      $fatal(1, "halves_text was %s, expected '{high:10, low:5}", halves_text);
    if (entry_text !== "'{count:7, state:STOP}")
      $fatal(1, "entry_text was %s, expected '{count:7, state:STOP}",
             entry_text);
    $display("All checks passed");
  end
endmodule

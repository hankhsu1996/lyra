# Scheduling Semantics

Lyra implements the stratified event scheduler per IEEE 1800-2023 Section 4.4.

## Time Slot Structure

Each simulation time slot executes regions in this order:

```
TIME SLOT
-----------------------------------------
  Preponed        <- #1step sampling (stub)
      |
  +-- ACTIVE GROUP (iterates until empty) --+
  |  Active    <- blocking assigns, $display |
  |  Inactive  <- #0 delays                  |
  |  NBA       <- nonblocking assigns (<=)   |
  +------------------------------------------+
      |
  Observed        <- assertions (stub)
      |
  Reactive group  <- program blocks (stub)
      |
  Postponed       <- $strobe, $monitor
-----------------------------------------
```

## Region Status

| Region     | Status | Purpose                                             |
| ---------- | ------ | --------------------------------------------------- |
| Preponed   | stub   | `#1step` sampling                                   |
| Active     | yes    | Blocking assignments, `$display`, process execution |
| Inactive   | yes    | Processes waiting on `#0` delays                    |
| NBA        | yes    | Nonblocking assignment updates                      |
| Observed   | stub   | Assertion evaluation                                |
| Reactive   | stub   | Program block execution                             |
| ReInactive | stub   | `#0` in reactive context                            |
| ReNBA      | stub   | Nonblocking in reactive context                     |
| Postponed  | yes    | `$strobe`, `$monitor`                               |

## Key Design Decisions

**Active group iteration**: The Active/Inactive/NBA cycle repeats until all three queues are empty. This ensures `#0` delays see NBA updates, and NBA updates can trigger waiting processes.

**#0 delays go to Inactive**: A `#0` delay suspends the process to Inactive region, not Active. This allows other Active work and NBA commits to happen before the process resumes.

**NBA deferred to region**: Nonblocking assignments queue values but do not write immediately. Values commit only when the NBA region executes.

## Reference

IEEE 1800-2023 Section 4.4 (Stratified Event Scheduler) and Section 4.5 (Scheduling Semantics Algorithm).

/* LRM 35.5.2: the imported task's C body calls back an exported SV task that
   consumes simulation time. The exported entry returns the DPI
   disable-acknowledgment int, discarded here (no disable active). */
extern int step(void);

int run_it(void) {
  step();
  return 0;
}

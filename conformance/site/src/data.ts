// The files the publisher writes, as the pages read them. The pages take every
// verdict and every way a run ended from here as they are, and judge nothing.
import { useEffect, useState } from 'react';

export type VerdictKind = 'conforms' | 'does not conform' | 'unresolved';

// One case's verdict on one configuration, why, and how its run ended.
export interface Result {
  verdict: VerdictKind;
  why: string | null;
  end: string;
}

export interface Configuration {
  key: string;
  name: string;
  tool: {
    name: string;
    revision: string | null;
    programs: { role: string; path: string; version: string | null }[];
    options: string[];
  };
  suite_directory: string;
  host: string | null;
  limits: { jobs: number; time_limit_seconds_per_case: number | null; address_limit_mib: number | null };
  started: string;
  ended: string;
  record: string;
}

export interface SummaryCase {
  id: string;
  path: string;
  helper_check: boolean;
  // One per configuration, in the order of `Summary.configurations`.
  results: Result[];
}

export interface Summary {
  run: { source: string | null; started: string };
  suite: { revision: string; repository: string | null };
  configurations: Configuration[];
  cases: SummaryCase[];
}

export interface Step {
  program: string;
  arguments: string[];
  end: { kind: string; code?: number; signal?: number };
  stdout: string;
  stderr: string;
}

// One case's entry in one configuration's record, as the run wrote it.
export interface Entry {
  id: string;
  seconds: number;
  verdict: { kind: VerdictKind; why?: string };
  end: { kind: string; how?: string; limit?: string };
  outputs: { name: string; text: string | null }[];
  steps: Step[];
}

export interface CaseDetail {
  id: string;
  path: string;
  helper_check: boolean;
  // What the case says it checks, in the paragraphs it opens with.
  statement: string[];
  // How a run of it is judged, in one sentence.
  requirement: string;
  sources: { name: string; text: string }[];
  // By configuration key: the case file and the summary are fetched apart, so
  // a page pairs them by key and never by place.
  results: Record<string, Entry>;
}

export type Loaded<T> = { data: T } | { failed: true } | null;

// What has been fetched, and what is being: a page opened on data already
// here is drawn with it at once, never through a loading state first.
const cache = new Map<string, Promise<unknown>>();
const fetched = new Map<string, unknown>();

function fetchJson(path: string): Promise<unknown> {
  let pending = cache.get(path);
  if (!pending) {
    pending = fetch(path)
      .then((response) => {
        const type = response.headers.get('content-type') ?? '';
        if (!response.ok || !type.includes('json')) {
          throw new Error(path);
        }
        return response.json();
      })
      .then((data: unknown) => {
        fetched.set(path, data);
        return data;
      });
    pending.catch(() => cache.delete(path));
    cache.set(path, pending);
  }
  return pending;
}

export function useJson<T>(path: string | null): Loaded<T> {
  const [loaded, setLoaded] = useState<Loaded<T>>(() =>
    path && fetched.has(path) ? { data: fetched.get(path) as T } : null,
  );
  useEffect(() => {
    if (!path) {
      return;
    }
    if (fetched.has(path)) {
      setLoaded({ data: fetched.get(path) as T });
      return;
    }
    let current = true;
    setLoaded(null);
    fetchJson(path).then(
      (data) => current && setLoaded({ data: data as T }),
      () => current && setLoaded({ failed: true }),
    );
    return () => {
      current = false;
    };
  }, [path]);
  return loaded;
}

export function useSummary(): Loaded<Summary> {
  return useJson<Summary>('summary.json');
}

export function useTitle(title: string) {
  useEffect(() => {
    document.title = `${title} - SystemVerilog Conformance Test Suite`;
  }, [title]);
}

// The cases a configuration is counted over: the suite's own, without the
// checks of its helpers.
export function counted(summary: Summary): SummaryCase[] {
  return summary.cases.filter((c) => !c.helper_check);
}

export function clauseOf(id: string): string {
  return id.split('/')[0];
}

// The titles IEEE 1800-2023 gives its clauses, by number, read from its table
// of contents.
const CLAUSE_TITLES: Record<number, string> = {
  3: 'Design and verification building blocks',
  4: 'Scheduling semantics',
  5: 'Lexical conventions',
  6: 'Data types',
  7: 'Aggregate data types',
  8: 'Classes',
  9: 'Processes',
  10: 'Assignment statements',
  11: 'Operators and expressions',
  12: 'Procedural programming statements',
  13: 'Tasks and functions (subroutines)',
  14: 'Clocking blocks',
  15: 'Interprocess synchronization and communication',
  16: 'Assertions',
  17: 'Checkers',
  18: 'Constrained random value generation',
  19: 'Functional coverage',
  20: 'Utility system tasks and system functions',
  21: 'Input/output system tasks and system functions',
  22: 'Compiler directives',
  23: 'Modules and hierarchy',
  24: 'Programs',
  25: 'Interfaces',
  26: 'Packages',
  27: 'Generate constructs',
  28: 'Gate-level and switch-level modeling',
  29: 'User-defined primitives',
  30: 'Specify blocks',
  31: 'Timing checks',
  32: 'Backannotation using the standard delay format',
  33: 'Configuring the contents of a design',
  34: 'Protected envelopes',
  35: 'Direct programming interface',
};

// A clause directory's number and title. A directory that names no clause of
// the standard has no number and keeps its own name.
export function clauseTitle(clause: string): { number: string; title: string } {
  const [prefix, ...words] = clause.split('_');
  const number = Number(prefix);
  if (CLAUSE_TITLES[number]) {
    return { number: String(number), title: CLAUSE_TITLES[number] };
  }
  const name = (words.length > 0 ? words : [prefix]).join(' ');
  return { number: '', title: name.charAt(0).toUpperCase() + name.slice(1) };
}

export function clauseLabel(clause: string): string {
  const { number, title } = clauseTitle(clause);
  return number ? `${number} ${title}` : title;
}

export function caseTitle(id: string): string {
  const name = id.split('/').slice(1).join(' / ').replaceAll('_', ' ');
  return name.charAt(0).toUpperCase() + name.slice(1);
}

export function shortRevision(revision: string): string {
  return revision.slice(0, 8);
}

// Where a path of the repository can be read at the run's revision.
export function sourceUrl(summary: Summary, path: string): string | null {
  return summary.suite.repository ? `${summary.suite.repository}/tree/${summary.suite.revision}/${path}` : null;
}

export function when(stamp: string): string {
  return stamp.replace('T', ' ').replace(/:\d\dZ$/, ' UTC');
}

export function sentence(text: string | null | undefined): string {
  return text ? `${text.charAt(0).toUpperCase()}${text.slice(1)}${/[.!?]$/.test(text) ? '' : '.'}` : '';
}

export function plural(count: number, one: string, many: string): string {
  return `${count} ${count === 1 ? one : many}`;
}

// What each verdict means, in one sentence each.
export const MEANINGS = {
  'does not conform':
    'The tool did something the standard does not allow. For example, a check failed, the tool crashed, or the tool refused a program that the standard accepts.',
  unresolved:
    'The run does not show whether the tool conforms. For example, the program was stopped at a time or memory limit. An unresolved case is not counted against the tool.',
} as const;

// Why a case has its verdict, by how its run ended.
const DOES_NOT_CONFORM_BECAUSE: Record<string, string> = {
  'ran': 'The program ran but did not do what the case requires',
  'rejected': 'The tool refused a program that the standard accepts',
  'tool failed': 'The tool stopped with an error of its own',
  'tool lacks': 'The tool cannot run the case the way the standard requires',
};

const UNRESOLVED_BECAUSE: Record<string, string> = {
  'ran': 'The output of the run does not settle the case',
  'stopped at a limit': 'The run was stopped at a time or memory limit',
};

export function reasonOf(verdict: VerdictKind, end: string): string {
  return (verdict === 'unresolved' ? UNRESOLVED_BECAUSE : DOES_NOT_CONFORM_BECAUSE)[end] ?? end;
}

// The pages that list one configuration's cases of one verdict, by the
// address word that names the verdict.
export const LISTED: Record<string, VerdictKind> = {
  'does-not-conform': 'does not conform',
  unresolved: 'unresolved',
};

export function listLink(configuration: string, verdict: VerdictKind): string {
  return `/configuration/${configuration}/${verdict.replaceAll(' ', '-')}`;
}

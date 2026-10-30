import { Anchor, Paper, SimpleGrid, Stack, Table, Text, Title } from '@mantine/core';
import { Link, useParams } from 'react-router-dom';

import { NotFound, Pending } from '../components/PageState';
import {
  counted,
  listLink,
  MEANINGS,
  plural,
  reasonOf,
  shortRevision,
  useSummary,
  useTitle,
  when,
  type VerdictKind,
} from '../data';

// One configuration: how many cases it conforms on, why the others do not, and
// how it ran.
export function ConfigurationPage() {
  const { key = '' } = useParams();
  const loaded = useSummary();
  const summary = loaded && 'data' in loaded ? loaded.data : null;
  const n = summary ? summary.configurations.findIndex((c) => c.key === key) : -1;
  const configuration = summary && n >= 0 ? summary.configurations[n] : null;
  useTitle(configuration?.name ?? 'Page not found');
  if (!summary) {
    return <Pending failed={loaded !== null} />;
  }
  if (!configuration) {
    return <NotFound>These results have no configuration at this address.</NotFound>;
  }
  const results = counted(summary).map((c) => c.results[n]);
  const conforming = results.filter((r) => r.verdict === 'conforms').length;
  const limits = configuration.limits;

  // The cases of one verdict, counted by why, most first.
  const reasons = (verdict: VerdictKind) => {
    const counts = new Map<string, number>();
    for (const r of results.filter((r) => r.verdict === verdict)) {
      counts.set(r.end, (counts.get(r.end) ?? 0) + 1);
    }
    return [...counts.entries()].sort((a, b) => b[1] - a[1]);
  };

  return (
    <>
      <Stack gap={6}>
        <Title order={1} tabIndex={-1}>
          {configuration.name}
        </Title>
        <Text c="dimmed">
          Conforms on {conforming} of {plural(results.length, 'case', 'cases')}.
        </Text>
      </Stack>
      <SimpleGrid cols={{ base: 1, md: 2 }} spacing="lg">
        {(['does not conform', 'unresolved'] as const).map((verdict) => {
          const rows = reasons(verdict);
          const total = rows.reduce((sum, [, count]) => sum + count, 0);
          return (
            <Paper withBorder p="lg" key={verdict} component="section" aria-labelledby={`why-${verdict.replaceAll(' ', '-')}`}>
              <Stack gap="xs">
                <Title order={2} size="h4" id={`why-${verdict.replaceAll(' ', '-')}`}>
                  {verdict === 'unresolved' ? 'Unresolved' : 'Does not conform'}: {total}
                </Title>
                <Text size="sm" c="dimmed">
                  {MEANINGS[verdict]}
                </Text>
                {rows.length > 0 && (
                  <Table className="numbers" verticalSpacing="xs" aria-labelledby={`why-${verdict.replaceAll(' ', '-')}`}>
                    <Table.Tbody>
                      {rows.map(([end, count]) => (
                        <Table.Tr key={end}>
                          <Table.Td ta="right" w="5ch">
                            <Anchor component={Link} to={`${listLink(key, verdict)}#${end.replaceAll(' ', '-')}`} className="quiet-link" c="inherit">
                              {count}
                            </Anchor>
                          </Table.Td>
                          <Table.Td>{reasonOf(verdict, end)}</Table.Td>
                        </Table.Tr>
                      ))}
                    </Table.Tbody>
                  </Table>
                )}
                {total > 0 && (
                  <Anchor component={Link} to={listLink(key, verdict)} size="sm" style={{ alignSelf: 'flex-start' }}>
                    See every case
                  </Anchor>
                )}
              </Stack>
            </Paper>
          );
        })}
      </SimpleGrid>
      <Stack gap="xs">
        <Title order={2} size="h4" id="how-it-ran">
          How it ran
        </Title>
        <Table verticalSpacing="xs" className="wrap-anywhere" maw={760} aria-labelledby="how-it-ran">
          <Table.Tbody>
            {configuration.tool.revision && (
              <Table.Tr>
                <Table.Th fw={400} c="dimmed" w="30%">
                  Built from
                </Table.Th>
                <Table.Td>
                  {summary.suite.repository ? (
                    <Anchor href={`${summary.suite.repository}/tree/${configuration.tool.revision}`} className="quiet-link" c="inherit">
                      {shortRevision(configuration.tool.revision)}
                    </Anchor>
                  ) : (
                    shortRevision(configuration.tool.revision)
                  )}
                </Table.Td>
              </Table.Tr>
            )}
            <Table.Tr>
              <Table.Th fw={400} c="dimmed" w="30%">
                Build options
              </Table.Th>
              <Table.Td ff="monospace">{configuration.tool.options.join(' ') || 'None'}</Table.Td>
            </Table.Tr>
            {configuration.tool.programs.map((program) => (
              <Table.Tr key={program.role}>
                <Table.Th fw={400} c="dimmed">
                  {program.role.charAt(0).toUpperCase() + program.role.slice(1)}
                </Table.Th>
                <Table.Td>{program.version ?? 'Does not report a version'}</Table.Td>
              </Table.Tr>
            ))}
            <Table.Tr>
              <Table.Th fw={400} c="dimmed">
                Machine
              </Table.Th>
              <Table.Td>{configuration.host ?? 'Not known'}</Table.Td>
            </Table.Tr>
            <Table.Tr>
              <Table.Th fw={400} c="dimmed">
                Limits
              </Table.Th>
              <Table.Td>
                {plural(limits.jobs, 'case', 'cases')} at a time.
                {limits.time_limit_seconds_per_case ? ` Each case may take ${limits.time_limit_seconds_per_case} s.` : ' No time limit.'}
                {limits.address_limit_mib ? ` Each program may use ${limits.address_limit_mib} MiB of memory.` : ''}
              </Table.Td>
            </Table.Tr>
            <Table.Tr>
              <Table.Th fw={400} c="dimmed">
                Ran
              </Table.Th>
              <Table.Td>
                {when(configuration.started)} to {when(configuration.ended)}
              </Table.Td>
            </Table.Tr>
          </Table.Tbody>
        </Table>
        <Anchor href={configuration.record} download size="sm" style={{ alignSelf: 'flex-start' }}>
          Download the full record of this run (JSON)
        </Anchor>
      </Stack>
    </>
  );
}

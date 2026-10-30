import { Anchor, Breadcrumbs, Stack, Table, Text, Title } from '@mantine/core';
import { useEffect } from 'react';
import { Link, useLocation, useParams } from 'react-router-dom';

import { RowLink } from '../components/RowLink';
import { NotFound, Pending } from '../components/PageState';
import {
  caseTitle,
  clauseOf,
  clauseTitle,
  counted,
  LISTED,
  MEANINGS,
  plural,
  reasonOf,
  sentence,
  useSummary,
  useTitle,
  type SummaryCase,
} from '../data';

// The cases one configuration gave one verdict, grouped by why, each with its
// own reason.
export function CaseList() {
  const { key = '', verdict: word = '' } = useParams();
  const { hash } = useLocation();
  const loaded = useSummary();
  const summary = loaded && 'data' in loaded ? loaded.data : null;
  const n = summary ? summary.configurations.findIndex((c) => c.key === key) : -1;
  const configuration = summary && n >= 0 ? summary.configurations[n] : null;
  const verdict = LISTED[word];
  const label = verdict === 'unresolved' ? 'Unresolved' : 'Does not conform';
  useTitle(configuration && verdict ? `${label} on ${configuration.name}` : 'Page not found');
  useEffect(() => {
    if (summary && hash) {
      document.getElementById(hash.slice(1))?.scrollIntoView();
    }
  }, [summary, hash]);
  if (!summary) {
    return <Pending failed={loaded !== null} />;
  }
  if (!configuration || !verdict) {
    return <NotFound>These results have no list at this address.</NotFound>;
  }
  const cases = counted(summary).filter((c) => c.results[n].verdict === verdict);
  const groups = new Map<string, SummaryCase[]>();
  for (const c of cases) {
    groups.set(c.results[n].end, [...(groups.get(c.results[n].end) ?? []), c]);
  }
  return (
    <>
      <Breadcrumbs>
        <Anchor component={Link} to="/" size="sm" className="quiet-link" c="dimmed">
          Results
        </Anchor>
        <Anchor component={Link} to={`/configuration/${key}`} size="sm" className="quiet-link" c="dimmed">
          {configuration.name}
        </Anchor>
      </Breadcrumbs>
      <Stack gap={6}>
        <Title order={1} tabIndex={-1}>
          {label} on {configuration.name}
        </Title>
        <Text c="dimmed">
          {plural(cases.length, 'case', 'cases')}. {MEANINGS[verdict as 'does not conform' | 'unresolved']}
        </Text>
      </Stack>
      {cases.length === 0 && <Text>No case.</Text>}
      {[...groups.entries()]
        .sort((a, b) => b[1].length - a[1].length)
        .map(([end, inGroup]) => (
          <Stack gap="xs" key={end}>
            <Title order={2} size="h3" id={end.replaceAll(' ', '-')}>
              {reasonOf(verdict, end)} ({inGroup.length})
            </Title>
            <Table verticalSpacing="sm" className="numbers reflow" aria-labelledby={end.replaceAll(' ', '-')}>
              <Table.Thead>
                <Table.Tr>
                  <Table.Th>Case</Table.Th>
                  <Table.Th ta="right">Clause</Table.Th>
                  <Table.Th>Reason</Table.Th>
                </Table.Tr>
              </Table.Thead>
              <Table.Tbody>
                {inGroup.map((c) => {
                  const { number, title } = clauseTitle(clauseOf(c.id));
                  return (
                    <RowLink key={c.id} to={`/case/${c.id}`}>
                      <Table.Th scope="row" fw={400}>
                        <Anchor component={Link} to={`/case/${c.id}`} className="quiet-link" c="inherit">
                          {caseTitle(c.id)}
                        </Anchor>
                      </Table.Th>
                      <Table.Td ta="right" c="dimmed" title={title} data-label="Clause">
                        {number || title}
                      </Table.Td>
                      <Table.Td data-label="Reason">
                        <Text size="sm">{sentence(c.results[n].why)}</Text>
                      </Table.Td>
                    </RowLink>
                  );
                })}
              </Table.Tbody>
            </Table>
          </Stack>
        ))}
    </>
  );
}

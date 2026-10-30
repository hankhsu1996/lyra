import { Anchor, Breadcrumbs, Chip, Group, Stack, Table, Text, Title } from '@mantine/core';
import { Link, useParams, useSearchParams } from 'react-router-dom';

import { RowLink } from '../components/RowLink';
import { NotFound, Pending } from '../components/PageState';
import { VerdictLabel } from '../components/Verdict';
import { caseTitle, clauseLabel, clauseOf, plural, sourceUrl, useSummary, useTitle, type SummaryCase } from '../data';

interface Filter {
  value: string;
  label: string;
  keep: (c: SummaryCase) => boolean;
}

export function ClausePage() {
  const { clause = '' } = useParams();
  const [query, setQuery] = useSearchParams();
  const loaded = useSummary();
  const summary = loaded && 'data' in loaded ? loaded.data : null;
  const cases = summary ? summary.cases.filter((c) => clauseOf(c.id) === clause) : [];
  const helpers = cases.length > 0 && cases.every((c) => c.helper_check);
  const name = helpers ? "Checks of the suite's helper code" : clauseLabel(clause);
  const filters: Filter[] = summary
    ? [
        { value: 'all', label: 'All cases', keep: () => true },
        { value: 'disagree', label: 'Configurations disagree', keep: (c) => new Set(c.results.map((r) => r.verdict)).size > 1 },
        ...summary.configurations.map((configuration, n) => ({
          value: `not-on-${configuration.key}`,
          label: `Not conforming on ${configuration.name}`,
          keep: (c: SummaryCase) => c.results[n].verdict !== 'conforms',
        })),
      ]
    : [];
  const chosen = filters.find((f) => f.value === query.get('show')) ?? filters[0];
  useTitle(cases.length === 0 ? 'Page not found' : chosen.value === 'all' ? name : `${chosen.label}, ${name}`);
  if (!summary) {
    return <Pending failed={loaded !== null} />;
  }
  if (cases.length === 0) {
    return <NotFound>These results have no clause at this address.</NotFound>;
  }
  const shown = cases.filter(chosen.keep);
  const folder = cases[0].path.split('/').slice(0, -1).join('/');
  const source = sourceUrl(summary, folder);
  return (
    <>
      <Breadcrumbs>
        <Anchor component={Link} to="/" size="sm" className="quiet-link" c="dimmed">
          Results
        </Anchor>
      </Breadcrumbs>
      <Stack gap={6}>
        <Title order={1} tabIndex={-1}>
          {name}
        </Title>
        <Text c="dimmed">
          {plural(cases.length, 'case', 'cases')}.
          {helpers && ' They check code that the other cases use, not the standard, and are not counted.'}{' '}
          {source && (
            <Anchor href={source} className="quiet-link" c="var(--mantine-color-text)">
              See their source on GitHub
            </Anchor>
          )}
        </Text>
      </Stack>
      <Chip.Group value={chosen.value} onChange={(value) => setQuery(value === 'all' ? {} : { show: String(value) })}>
        <Group gap="xs" role="group" aria-label="Show">
          {filters.map((f) => (
            <Chip key={f.value} value={f.value} variant="outline">
              {f.label} ({cases.filter(f.keep).length})
            </Chip>
          ))}
        </Group>
      </Chip.Group>
      {shown.length === 0 ? (
        <Text>No case of this clause matches.</Text>
      ) : (
        <Table verticalSpacing="sm" className="reflow">
          <Table.Caption style={{ captionSide: 'top', textAlign: 'left' }}>
            {chosen.label} ({shown.length})
          </Table.Caption>
          <Table.Thead>
            <Table.Tr>
              <Table.Th>Case</Table.Th>
              {summary.configurations.map((c) => (
                <Table.Th key={c.key} className="configuration-column">
                  {c.name}
                </Table.Th>
              ))}
            </Table.Tr>
          </Table.Thead>
          <Table.Tbody>
            {shown.map((c) => (
              <RowLink key={c.id} to={`/case/${c.id}`}>
                <Table.Th scope="row" fw={400}>
                  <Anchor component={Link} to={`/case/${c.id}`} className="quiet-link" c="inherit">
                    {caseTitle(c.id)}
                  </Anchor>
                </Table.Th>
                {c.results.map((r, n) => (
                  <Table.Td key={summary.configurations[n].key} data-label={summary.configurations[n].name}>
                    <VerdictLabel verdict={r.verdict} />
                  </Table.Td>
                ))}
              </RowLink>
            ))}
          </Table.Tbody>
        </Table>
      )}
    </>
  );
}

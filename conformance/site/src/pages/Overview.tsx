import {
  Anchor,
  Autocomplete,
  Group,
  Paper,
  SimpleGrid,
  Stack,
  Table,
  Text,
  Title,
  type ComboboxItem,
  type OptionsFilter,
} from '@mantine/core';
import { IconSearch } from '@tabler/icons-react';
import { useMemo, useState } from 'react';
import { Link, useNavigate } from 'react-router-dom';

import { Pending } from '../components/PageState';
import { RowLink } from '../components/RowLink';
import {
  caseTitle,
  clauseLabel,
  clauseOf,
  clauseTitle,
  counted,
  listLink,
  MEANINGS,
  shortRevision,
  useSummary,
  useTitle,
  type Configuration,
  type Summary,
  type SummaryCase,
  type VerdictKind,
} from '../data';

function countOf(cases: SummaryCase[], n: number, verdict: VerdictKind): number {
  return cases.filter((c) => c.results[n].verdict === verdict).length;
}

// Which build of the tool ran: the revision it was built from where the run
// states one, else the version the tool reports.
function Build({ summary, configuration }: { summary: Summary; configuration: Configuration }) {
  const revision = configuration.tool.revision;
  if (revision && summary.suite.repository) {
    return (
      <>
        Built from{' '}
        <Anchor href={`${summary.suite.repository}/tree/${revision}`} className="quiet-link" c="inherit">
          {shortRevision(revision)}
        </Anchor>
      </>
    );
  }
  return <>{configuration.tool.programs[0]?.version ?? 'Does not report a version'}</>;
}

// A count that opens the list of the cases it counts, or a plain zero.
function Count({ count, to }: { count: number; to: string }) {
  if (count === 0) {
    return <>0</>;
  }
  return (
    <Anchor component={Link} to={to} className="quiet-link" c="inherit">
      {count}
    </Anchor>
  );
}

function Configurations({ summary, cases }: { summary: Summary; cases: SummaryCase[] }) {
  return (
    <Stack gap="lg">
      <Table verticalSpacing="sm" className="numbers reflow">
        <Table.Caption style={{ captionSide: 'top', textAlign: 'left' }}>
          Cases that each configuration conforms on, of {cases.length}
        </Table.Caption>
        <Table.Thead>
          <Table.Tr>
            <Table.Th>Configuration</Table.Th>
            <Table.Th ta="right">Conforms</Table.Th>
            <Table.Th ta="right">Does not conform</Table.Th>
            <Table.Th ta="right">Unresolved</Table.Th>
          </Table.Tr>
        </Table.Thead>
        <Table.Tbody>
          {summary.configurations.map((configuration, n) => {
            const conforming = countOf(cases, n, 'conforms');
            return (
              <RowLink key={configuration.key} to={`/configuration/${configuration.key}`}>
                <Table.Th scope="row" fw={400}>
                  <Anchor component={Link} to={`/configuration/${configuration.key}`} className="quiet-link" c="inherit" fw={600}>
                    {configuration.name}
                  </Anchor>
                  <Text size="xs" c="dimmed">
                    <Build summary={summary} configuration={configuration} />
                  </Text>
                </Table.Th>
                <Table.Td ta="right" data-label="Conforms">
                  <Group gap="sm" justify="flex-end" wrap="nowrap" display="inline-flex">
                    <span className="meter" aria-hidden>
                      <span style={{ width: `${(100 * conforming) / cases.length}%` }} />
                    </span>
                    <Text span fw={600} miw="4ch" ta="right">
                      {conforming}
                    </Text>
                  </Group>
                </Table.Td>
                <Table.Td ta="right" data-label="Does not conform">
                  <Count count={countOf(cases, n, 'does not conform')} to={listLink(configuration.key, 'does not conform')} />
                </Table.Td>
                <Table.Td ta="right" data-label="Unresolved">
                  <Count count={countOf(cases, n, 'unresolved')} to={listLink(configuration.key, 'unresolved')} />
                </Table.Td>
              </RowLink>
            );
          })}
        </Table.Tbody>
      </Table>
      <SimpleGrid cols={{ base: 1, sm: 2 }} spacing="lg">
        <Paper withBorder p="md">
          <Text fw={600} size="sm" mb={6}>
            Does not conform
          </Text>
          <Text size="sm" c="dimmed">
            {MEANINGS['does not conform']}
          </Text>
        </Paper>
        <Paper withBorder p="md">
          <Text fw={600} size="sm" mb={6}>
            Unresolved
          </Text>
          <Text size="sm" c="dimmed">
            {MEANINGS.unresolved}
          </Text>
        </Paper>
      </SimpleGrid>
    </Stack>
  );
}

// Each clause against each configuration: how many of its cases conform, with
// how many do not marked beside it where there are any. The clause's number of
// cases is written once, in a column of its own. No cell is shaded by its
// share, so a clause of one case never reads as strongly as one of eighty.
function ByClause({ summary, cases }: { summary: Summary; cases: SummaryCase[] }) {
  const clauses = [...new Set(cases.map((c) => clauseOf(c.id)))];
  return (
    <Stack gap="xs">
      <Title order={2} id="by-clause">
        By clause
      </Title>
      <Text size="sm" c="dimmed">
        How many of each clause's cases conform on each configuration. An orange number counts the cases that do not
        conform or are unresolved.
      </Text>
      <Table verticalSpacing="xs" className="numbers reflow" style={{ tableLayout: 'fixed' }} aria-labelledby="by-clause">
        <Table.Thead>
          <Table.Tr>
            <Table.Th ta="right" w="4.5rem">
              Clause
            </Table.Th>
            <Table.Th>Title</Table.Th>
            <Table.Th ta="right" w="5rem">
              Cases
            </Table.Th>
            {summary.configurations.map((c) => (
              <Table.Th key={c.key} ta="right" className="configuration-column">
                {c.name}
              </Table.Th>
            ))}
          </Table.Tr>
        </Table.Thead>
        <Table.Tbody>
          {clauses.map((clause) => {
            const inClause = cases.filter((c) => clauseOf(c.id) === clause);
            const { number, title } = clauseTitle(clause);
            return (
              <RowLink key={clause} to={`/clause/${clause}`}>
                <Table.Td ta="right" c="dimmed">
                  {number}
                </Table.Td>
                <Table.Th scope="row" fw={400}>
                  <Anchor component={Link} to={`/clause/${clause}`} className="quiet-link" c="inherit">
                    {title}
                  </Anchor>
                </Table.Th>
                <Table.Td ta="right" c="dimmed" data-label="Cases">
                  {inClause.length}
                </Table.Td>
                {summary.configurations.map((configuration, n) => {
                  const conforming = countOf(inClause, n, 'conforms');
                  const others = inClause.length - conforming;
                  return (
                    <Table.Td key={configuration.key} ta="right" data-label={configuration.name}>
                      <span className="cell-count">{conforming}</span>
                      <span className="cell-others">
                        {others > 0 && (
                          <Anchor
                            component={Link}
                            to={`/clause/${clause}?show=not-on-${configuration.key}`}
                            className="others-badge"
                            aria-label={`${others} of ${inClause.length} do not conform or are unresolved`}
                          >
                            {others}
                          </Anchor>
                        )}
                      </span>
                    </Table.Td>
                  );
                })}
              </RowLink>
            );
          })}
        </Table.Tbody>
      </Table>
    </Stack>
  );
}

// Finding a case by a word of its name, as a box whose matches open below it
// over the page, so the page itself never moves; picking one opens the case.
function FindCase({ summary }: { summary: Summary }) {
  const navigate = useNavigate();
  const [typed, setTyped] = useState('');
  const data = useMemo(
    () => summary.cases.map((c) => ({ value: c.id, label: caseTitle(c.id) })),
    [summary],
  );
  const filter: OptionsFilter = ({ options, search }) => {
    const words = search.toLowerCase().split(/\s+/).filter((w) => w.length > 0);
    return (options as ComboboxItem[]).filter((option) => {
      const name = `${option.label} ${option.value}`.toLowerCase();
      return words.every((w) => name.includes(w));
    });
  };
  return (
    <Autocomplete
      label="Find a case"
      placeholder="Type a word of its name, such as randomize"
      leftSection={<IconSearch size={16} aria-hidden />}
      data={data}
      value={typed}
      onChange={setTyped}
      filter={filter}
      limit={12}
      maxDropdownHeight={420}
      clearable
      onOptionSubmit={(id) => {
        setTyped('');
        navigate(`/case/${id}`);
      }}
      renderOption={({ option }) => (
        <div>
          <Text size="sm">{caseTitle(option.value)}</Text>
          <Text size="xs" c="dimmed">
            {clauseLabel(clauseOf(option.value))}
          </Text>
        </div>
      )}
      maw={520}
    />
  );
}

export function Overview() {
  useTitle('Results');
  const loaded = useSummary();
  if (!loaded || 'failed' in loaded) {
    return <Pending failed={loaded !== null} />;
  }
  const summary = loaded.data;
  const cases = counted(summary);
  const helpers = summary.cases.length - cases.length;
  const helperClause = summary.cases.find((c) => c.helper_check);
  return (
    <>
      <Stack gap={6}>
        <Title order={1} tabIndex={-1}>
          Results
        </Title>
        <Text c="dimmed">
          Each case checks one requirement of IEEE 1800-2023. This page shows how many cases each configuration conforms
          on.
        </Text>
      </Stack>
      <FindCase summary={summary} />
      <Configurations summary={summary} cases={cases} />
      <ByClause summary={summary} cases={cases} />
      {helperClause && (
        <Text size="sm" c="dimmed">
          The suite also has{' '}
          <Anchor component={Link} to={`/clause/${clauseOf(helperClause.id)}`} className="quiet-link" c="var(--mantine-color-text)">
            {helpers === 1 ? 'one case that checks its own helper code' : `${helpers} cases that check its own helper code`}
          </Anchor>
          . {helpers === 1 ? 'That case is' : 'Those cases are'} not counted.
        </Text>
      )}
    </>
  );
}

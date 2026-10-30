import { Accordion, Anchor, Breadcrumbs, Group, Paper, Stack, Text, Title } from '@mantine/core';
import { Link, useParams } from 'react-router-dom';

import { Listing } from '../components/Listing';
import { NotFound, Pending } from '../components/PageState';
import { VerdictLabel } from '../components/Verdict';
import {
  caseTitle,
  clauseLabel,
  clauseOf,
  sentence,
  shortRevision,
  sourceUrl,
  useJson,
  useSummary,
  useTitle,
  type CaseDetail,
  type Configuration,
  type Entry,
  type Step,
} from '../data';

// Text as the run wrote it, with the directory the suite was read from written
// as the suite's folder in the repository, so a command reads the same
// whichever machine ran it.
function inSuite(text: string, suiteDirectory: string): string {
  return suiteDirectory ? text.split(suiteDirectory).join('conformance') : text;
}

function endOf(step: Step): string {
  switch (step.end.kind) {
    case 'exited':
      return `exit code ${step.end.code}`;
    case 'signalled':
      return `ended by signal ${step.end.signal}`;
    case 'timed out':
      return 'stopped because it ran out of time';
    default:
      return step.end.kind;
  }
}

function transcript(entry: Entry, configuration: Configuration): string {
  return entry.steps
    .map((step) => {
      const words = [step.program, ...step.arguments].map((word) => {
        const shown = inSuite(word, configuration.suite_directory);
        return /[\s"'$]/.test(shown) ? `'${shown}'` : shown;
      });
      const streams = [step.stdout, step.stderr]
        .filter((s) => s)
        .map((s) => inSuite(s, configuration.suite_directory).replace(/\n$/, ''));
      return [`$ ${words.join(' ')}`, ...streams, `(${endOf(step)})`].join('\n');
    })
    .join('\n\n');
}

// The commands that judge this one case again on this configuration, from a
// checkout of the repository at the revision the run read.
function rerun(configuration: Configuration, id: string): string {
  const tool = configuration.tool;
  const words = ['bazel', 'run', '//conformance/runner:conformance', '--', '--tool', tool.name];
  if (tool.name === 'lyra') {
    words.push('--lyra', 'bazel-bin/lyra');
  }
  for (const option of tool.options) {
    words.push('--build-option', option);
  }
  words.push(id);
  return `${tool.name === 'lyra' ? 'bazel build //:lyra\n' : ''}${words.join(' ')}`;
}

function ConfigurationResult({ configuration, entry }: { configuration: Configuration; entry: Entry }) {
  const why = entry.verdict.kind === 'conforms' ? 'The tool did what the case requires.' : sentence(entry.verdict.why);
  const ran = entry.steps.length > 0;
  const written = entry.outputs.filter((output) => output.text !== null);
  return (
    <Paper withBorder p="lg" component="section" aria-labelledby={`result-${configuration.key}`}>
      <Stack gap="sm">
        <Group justify="space-between" gap="sm">
          <Title order={3} id={`result-${configuration.key}`}>
            {configuration.name}
          </Title>
          <VerdictLabel verdict={entry.verdict.kind} />
        </Group>
        <Text>{why}</Text>
        {!ran && (
          <Text size="sm" c="dimmed">
            No command was run: the tool cannot run the case the way it requires.
          </Text>
        )}
        {ran && (
          <Accordion variant="separated" defaultValue={entry.verdict.kind === 'conforms' ? null : 'commands'} multiple={false}>
            <Accordion.Item value="commands">
              <Accordion.Control>
                {entry.steps.length === 1 ? '1 command and its output' : `${entry.steps.length} commands and their output`},{' '}
                {entry.seconds.toFixed(1)} s
              </Accordion.Control>
              <Accordion.Panel>
                <Stack gap="sm">
                  <Listing title="Commands" text={transcript(entry, configuration)} />
                  {written.map((output) => (
                    <Listing key={output.name} title={`${output.name}, as the program wrote it`} text={output.text ?? ''} />
                  ))}
                </Stack>
              </Accordion.Panel>
            </Accordion.Item>
            <Accordion.Item value="again">
              <Accordion.Control>Run this case again</Accordion.Control>
              <Accordion.Panel>
                <Listing title="From a checkout of the repository" text={rerun(configuration, entry.id)} />
              </Accordion.Panel>
            </Accordion.Item>
          </Accordion>
        )}
      </Stack>
    </Paper>
  );
}

export function CasePage() {
  const id = useParams()['*'] ?? '';
  const loaded = useSummary();
  const summary = loaded && 'data' in loaded ? loaded.data : null;
  const known = summary?.cases.some((c) => c.id === id) ?? false;
  const detail = useJson<CaseDetail>(known ? `cases/${id}.json` : null);
  useTitle(summary && !known ? 'Page not found' : caseTitle(id));
  if (!summary) {
    return <Pending failed={loaded !== null} />;
  }
  if (!known) {
    return <NotFound>These results have no case at this address.</NotFound>;
  }
  // A case file from another night than the summary lacks one of its
  // configurations, and is told apart from one still loading.
  if (!detail || 'failed' in detail || summary.configurations.some((c) => !detail.data.results[c.key])) {
    return <Pending failed={detail !== null} />;
  }
  const clause = clauseOf(id);
  const source = sourceUrl(summary, detail.data.path);
  return (
    <>
      <Breadcrumbs>
        <Anchor component={Link} to="/" size="sm" className="quiet-link" c="dimmed">
          Results
        </Anchor>
        <Anchor component={Link} to={`/clause/${clause}`} size="sm" className="quiet-link" c="dimmed">
          {clauseLabel(clause)}
        </Anchor>
      </Breadcrumbs>
      <Stack gap="sm">
        <Title order={1} tabIndex={-1}>
          {caseTitle(id)}
        </Title>
        {detail.data.helper_check && (
          <Text c="dimmed">This case checks code that the other cases use, not the standard, and is not counted.</Text>
        )}
      </Stack>
      <Stack gap="xs">
        <Title order={2}>What the case checks</Title>
        {detail.data.statement.map((paragraph, n) => (
          <Text key={n} maw={820}>
            {paragraph}
          </Text>
        ))}
        {detail.data.statement.length === 0 && <Text>The case does not describe itself. Its source is below.</Text>}
        <Text maw={820}>
          <Text span fw={600}>
            How a run is judged:
          </Text>{' '}
          {detail.data.requirement}
        </Text>
      </Stack>
      <Stack gap="md">
        <Title order={2}>What each configuration did</Title>
        {summary.configurations.map((configuration) => (
          <ConfigurationResult
            key={configuration.key}
            configuration={configuration}
            entry={detail.data.results[configuration.key]}
          />
        ))}
      </Stack>
      <Stack gap="sm">
        <Title order={2}>Source of the case</Title>
        <Text size="sm" c="dimmed" className="wrap-anywhere">
          {id}.{' '}
          {source && (
            <Anchor href={source} className="quiet-link" c="var(--mantine-color-text)">
              See it on GitHub at {shortRevision(summary.suite.revision)}
            </Anchor>
          )}
        </Text>
        {detail.data.sources.map((file) => (
          <Listing
            key={file.name}
            title={file.name}
            text={file.text}
            numbered
            systemVerilog={/\.(sv|svh|v|vh)$/.test(file.name)}
          />
        ))}
      </Stack>
    </>
  );
}

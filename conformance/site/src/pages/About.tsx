import { Stack, Text, Title } from '@mantine/core';

import { MEANINGS, useTitle } from '../data';

export function About() {
  useTitle('About');
  return (
    <Stack gap="md" maw={760}>
      <Title order={1} tabIndex={-1}>
        About the suite
      </Title>
      <Text>
        The SystemVerilog Conformance Test Suite checks whether a tool, such as a simulator, does what IEEE 1800-2023 requires. Each case is
        a short SystemVerilog program. It states one requirement of the standard and checks itself against it.
      </Text>
      <Title order={2}>Configurations</Title>
      <Text>
        A configuration is one tool with one set of options. For example, Lyra with its LLVM backend and Lyra with its
        C++ backend are two configurations. No configuration is the reference for another. Each one is judged against the
        standard.
      </Text>
      <Title order={2}>Verdicts</Title>
      <Text>
        <Text span fw={600}>
          Conforms.
        </Text>{' '}
        The tool did what the case requires. For example, the program ran and passed every check, or the tool refused a
        program that the standard forbids.
      </Text>
      <Text>
        <Text span fw={600}>
          Does not conform.
        </Text>{' '}
        {MEANINGS['does not conform']}
      </Text>
      <Text>
        <Text span fw={600}>
          Unresolved.
        </Text>{' '}
        {MEANINGS.unresolved}
      </Text>
      <Title order={2}>When results are published</Title>
      <Text>
        Every night, each configuration runs every case on the same version of the suite. The results are published only
        when every configuration has run every case. If a night fails, this site keeps the results of the last night that
        succeeded.
      </Text>
    </Stack>
  );
}

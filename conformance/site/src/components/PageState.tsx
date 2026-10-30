import { Alert, Anchor, Group, Loader, Text, Title } from '@mantine/core';
import { useEffect, useState, type ReactNode } from 'react';
import { Link } from 'react-router-dom';

// What a page shows while its data loads, or when the data cannot be loaded.
// Data that arrives within a moment is shown without a loading state ever
// flashing up; only a wait longer than that says the results are loading.
export function Pending({ failed }: { failed: boolean }) {
  const [waited, setWaited] = useState(false);
  useEffect(() => {
    const timer = setTimeout(() => setWaited(true), 300);
    return () => clearTimeout(timer);
  }, []);
  if (failed) {
    return (
      <Alert color="gray" title="The results could not be loaded">
        Reload the page to try again.
      </Alert>
    );
  }
  if (!waited) {
    return null;
  }
  return (
    <Group gap="sm" role="status">
      <Loader size="sm" />
      <Text size="sm">Loading the results</Text>
    </Group>
  );
}

// A page for an address that names nothing in these results.
export function NotFound({ children }: { children: ReactNode }) {
  return (
    <>
      <Title order={1} tabIndex={-1}>
        Page not found
      </Title>
      <Text>{children}</Text>
      <Anchor component={Link} to="/">
        Go to the results
      </Anchor>
    </>
  );
}

import { Anchor, Container, Group, SimpleGrid, Stack, Text } from '@mantine/core';
import { useEffect, type ReactNode } from 'react';
import { Link, NavLink, Route, Routes, useLocation } from 'react-router-dom';

import { NotFound } from './components/PageState';
import { ThemeToggle } from './components/ThemeToggle';
import { shortRevision, sourceUrl, useSummary, useTitle, when } from './data';
import { About } from './pages/About';
import { CaseList } from './pages/CaseList';
import { CasePage } from './pages/CasePage';
import { ClausePage } from './pages/ClausePage';
import { ConfigurationPage } from './pages/ConfigurationPage';
import { Overview } from './pages/Overview';

// A new page starts at its top, and its heading takes focus, so a keyboard or
// screen reader user lands on the page that just opened.
function OnNewPage() {
  const { pathname } = useLocation();
  useEffect(() => {
    window.scrollTo(0, 0);
    document.querySelector<HTMLElement>('main h1')?.focus({ preventScroll: true });
  }, [pathname]);
  return null;
}

function Missing() {
  useTitle('Page not found');
  return <NotFound>There is no page at this address in the current results.</NotFound>;
}

function FooterColumn({ title, children }: { title: string; children: ReactNode }) {
  return (
    <Stack gap={8} align="flex-start">
      <Text size="sm" fw={600}>
        {title}
      </Text>
      {children}
    </Stack>
  );
}

function FooterLink({ href, to, children }: { href?: string; to?: string; children: ReactNode }) {
  return to ? (
    <Anchor component={Link} to={to} size="sm" c="dimmed" className="quiet-link">
      {children}
    </Anchor>
  ) : (
    <Anchor href={href} size="sm" c="dimmed" className="quiet-link">
      {children}
    </Anchor>
  );
}

// Every page ends with what the site is, which run it shows and where that
// came from, and where to read more.
function Footer() {
  const loaded = useSummary();
  const summary = loaded && 'data' in loaded ? loaded.data : null;
  const repository = summary?.suite.repository ?? null;
  return (
    <footer className="site-footer">
      <Container size="xl" py={48}>
        <SimpleGrid cols={{ base: 1, sm: 3 }} spacing={40}>
          <Stack gap={8}>
            <Text fw={600}>SystemVerilog Conformance Test Suite</Text>
            <Text size="sm" c="dimmed" maw={320}>
              Small SystemVerilog programs, each checking one requirement of IEEE 1800-2023, run every night on each
              configuration.
            </Text>
          </Stack>
          <FooterColumn title="These results">
            {summary ? (
              <>
                <Text size="sm" c="dimmed">
                  Run on {when(summary.run.started)}
                </Text>
                {sourceUrl(summary, 'conformance') && (
                  <FooterLink href={sourceUrl(summary, 'conformance') ?? undefined}>
                    The suite at {shortRevision(summary.suite.revision)}
                  </FooterLink>
                )}
                {summary.run.source && <FooterLink href={summary.run.source}>The run on GitHub</FooterLink>}
              </>
            ) : (
              <Text size="sm" c="dimmed">
                No results have been published yet.
              </Text>
            )}
          </FooterColumn>
          <FooterColumn title="The suite">
            <FooterLink to="/about">About the suite</FooterLink>
            {repository && <FooterLink href={`${repository}/tree/main/conformance`}>Source on GitHub</FooterLink>}
            {repository && (
              <FooterLink href={`${repository}/blob/main/conformance/README.md`}>How a case is written</FooterLink>
            )}
          </FooterColumn>
        </SimpleGrid>
      </Container>
    </footer>
  );
}

function NavItem({ to, children }: { to: string; children: string }) {
  return (
    <NavLink to={to} end className="nav-link" data-text={children}>
      {children}
    </NavLink>
  );
}

export function App() {
  return (
    <>
      <OnNewPage />
      <a className="skip-link" href="#main">
        Skip to the results
      </a>
      <header className="site-header">
        <Container size="xl" py="sm">
          <Group justify="space-between" gap="sm" wrap="nowrap">
            <Link to="/" className="site-name">
              SystemVerilog Conformance Test Suite
            </Link>
            <Group gap="md" wrap="nowrap" component="nav" aria-label="Site">
              <NavItem to="/">Results</NavItem>
              <NavItem to="/about">About</NavItem>
              <ThemeToggle />
            </Group>
          </Group>
        </Container>
      </header>
      <Container size="xl" py="xl" component="main" id="main">
        <Stack gap="xl">
          <Routes>
            <Route path="/" element={<Overview />} />
            <Route path="/configuration/:key" element={<ConfigurationPage />} />
            <Route path="/configuration/:key/:verdict" element={<CaseList />} />
            <Route path="/clause/:clause" element={<ClausePage />} />
            <Route path="/case/*" element={<CasePage />} />
            <Route path="/about" element={<About />} />
            <Route path="*" element={<Missing />} />
          </Routes>
        </Stack>
      </Container>
      <Footer />
    </>
  );
}

import '@mantine/core/styles.css';
import './site.css';

import { MantineProvider, createTheme, type CSSVariablesResolver } from '@mantine/core';
import { StrictMode } from 'react';
import { createRoot } from 'react-dom/client';
import { HashRouter } from 'react-router-dom';

import { App } from './App';

const theme = createTheme({
  fontFamily: 'system-ui, -apple-system, "Segoe UI", sans-serif',
  fontFamilyMonospace: 'ui-monospace, SFMono-Regular, Menlo, Consolas, monospace',
  headings: { fontWeight: '600' },
  primaryColor: 'blue',
  defaultRadius: 'md',
});

// Secondary text and links darker in light and lighter in dark than the
// library's own, so both read at 4.5:1 or more against the page.
const resolver: CSSVariablesResolver = () => ({
  variables: {},
  light: {
    '--mantine-color-dimmed': '#575f6b',
    '--mantine-color-anchor': '#1a5fb4',
  },
  dark: {
    '--mantine-color-dimmed': '#a9b0ba',
    '--mantine-color-anchor': '#8ab4f8',
  },
});

const root = document.getElementById('root');
if (root) {
  createRoot(root).render(
    <StrictMode>
      <MantineProvider theme={theme} defaultColorScheme="auto" cssVariablesResolver={resolver}>
        <HashRouter>
          <App />
        </HashRouter>
      </MantineProvider>
    </StrictMode>,
  );
}

import { ActionIcon, CopyButton, Paper, Text, Tooltip } from '@mantine/core';
import { IconCheck, IconCopy } from '@tabler/icons-react';
import { useEffect, useState } from 'react';

import { highlightSystemVerilog } from '../highlight';

// A text shown as written, with a title above it and a button that copies it.
// A source file is numbered by line, since requirements and reasons cite its
// lines, and a SystemVerilog one is coloured as the editor colours it once its
// grammar has loaded; a transcript is neither.
export function Listing({
  title,
  text,
  numbered = false,
  systemVerilog = false,
}: {
  title?: string;
  text: string;
  numbered?: boolean;
  systemVerilog?: boolean;
}) {
  const lines = text.replace(/\n$/, '').split('\n');
  const [coloured, setColoured] = useState<string | null>(null);
  useEffect(() => {
    if (!systemVerilog) {
      return;
    }
    let current = true;
    highlightSystemVerilog(text.replace(/\n$/, '')).then(
      (html) => current && setColoured(html),
      // The source stays readable uncoloured; the reason is left where a
      // developer looks.
      (error: unknown) => console.error('The source could not be coloured:', error),
    );
    return () => {
      current = false;
    };
  }, [text, systemVerilog]);
  return (
    <Paper withBorder radius="md" className="listing">
      <div className="listing-head">
        <Text size="sm" ff="monospace" fw={500}>
          {title}
        </Text>
        <CopyButton value={text}>
          {({ copied, copy }) => (
            <Tooltip label={copied ? 'Copied' : `Copy${title ? ` ${title}` : ''}`} withArrow>
              <ActionIcon variant="subtle" color="gray" onClick={copy} aria-label={`Copy${title ? ` ${title}` : ''}`}>
                {copied ? <IconCheck size={16} /> : <IconCopy size={16} />}
              </ActionIcon>
            </Tooltip>
          )}
        </CopyButton>
      </div>
      {coloured ? (
        <div
          className={`listing-body coloured${numbered ? ' numbered' : ''}`}
          dangerouslySetInnerHTML={{ __html: coloured }}
        />
      ) : (
        <pre className="listing-body">
          {numbered ? (
            <code className="plain-numbered">
              {lines.map((line, n) => (
                <span className="line" key={n}>
                  <span className="line-number" aria-hidden>
                    {n + 1}
                  </span>
                  <span>{line || ' '}</span>
                </span>
              ))}
            </code>
          ) : (
            <code>{text}</code>
          )}
        </pre>
      )}
    </Paper>
  );
}

import { Group, Text } from '@mantine/core';
import { IconCircleCheck, IconCircleMinus, IconCircleX } from '@tabler/icons-react';
import type { ReactNode } from 'react';

import type { VerdictKind } from '../data';

const SHOWN: Record<VerdictKind, { label: string; color: string; icon: ReactNode }> = {
  conforms: { label: 'Conforms', color: 'var(--verdict-conforms)', icon: <IconCircleCheck size={16} aria-hidden /> },
  'does not conform': { label: 'Does not conform', color: 'var(--verdict-fails)', icon: <IconCircleX size={16} aria-hidden /> },
  unresolved: { label: 'Unresolved', color: 'var(--verdict-other)', icon: <IconCircleMinus size={16} aria-hidden /> },
};

// A verdict as an icon and its words. The words are in the text colour, so
// they read the same to everyone; the icon's colour only marks the verdict.
export function VerdictLabel({ verdict }: { verdict: VerdictKind }) {
  const shown = SHOWN[verdict];
  return (
    <Group gap={6} wrap="nowrap" component="span">
      <span style={{ color: shown.color, display: 'inline-flex' }}>{shown.icon}</span>
      <Text span size="sm" style={{ whiteSpace: 'nowrap' }}>
        {shown.label}
      </Text>
    </Group>
  );
}

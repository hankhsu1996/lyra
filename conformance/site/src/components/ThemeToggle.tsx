import { Switch, useComputedColorScheme, useMantineColorScheme } from '@mantine/core';
import { IconMoonStars, IconSun } from '@tabler/icons-react';
import { flushSync } from 'react-dom';

// Light or dark, as a switch with the sun on one side and the moon on the
// other. Until the reader moves it the page follows the system's setting;
// after that it keeps their choice. The page fades from one theme to the
// other, unless the reader has asked for less motion.
export function ThemeToggle() {
  const { setColorScheme } = useMantineColorScheme();
  const dark = useComputedColorScheme('light', { getInitialValueInEffect: true }) === 'dark';

  const change = (toDark: boolean) => {
    const apply = () => flushSync(() => setColorScheme(toDark ? 'dark' : 'light'));
    if (matchMedia('(prefers-reduced-motion: reduce)').matches || !document.startViewTransition) {
      apply();
      return;
    }
    document.startViewTransition(apply);
  };

  return (
    <Switch
      size="lg"
      color="dark.4"
      checked={dark}
      onChange={(event) => change(event.currentTarget.checked)}
      aria-label="Dark theme"
      onLabel={<IconMoonStars size={16} stroke={2} color="var(--mantine-color-yellow-4)" aria-hidden />}
      offLabel={<IconSun size={16} stroke={2} color="var(--mantine-color-yellow-6)" aria-hidden />}
    />
  );
}

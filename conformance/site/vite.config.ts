import react from '@vitejs/plugin-react';
import { defineConfig } from 'vite';

// The site is served from a subpath of whatever host publishes it, so every
// asset is addressed relative to the page.
export default defineConfig({
  base: './',
  plugins: [react()],
});

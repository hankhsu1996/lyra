// Colouring SystemVerilog the way the Better SystemVerilog Syntax extension
// does, with its own TextMate grammar run by Shiki, the engine VS Code's
// colouring is. Everything is loaded the first time a source is shown, so the
// pages without one never fetch it.
import type { HighlighterCore } from 'shiki/core';

let highlighter: Promise<HighlighterCore> | null = null;

function load(): Promise<HighlighterCore> {
  highlighter ??= (async () => {
    const [{ createHighlighterCore }, { createOnigurumaEngine }, grammar] = await Promise.all([
      import('shiki/core'),
      import('shiki/engine/oniguruma'),
      import('./grammars/systemverilog.tmLanguage.json'),
    ]);
    return createHighlighterCore({
      themes: [import('shiki/themes/github-light.mjs'), import('shiki/themes/github-dark.mjs')],
      langs: [{ ...(grammar.default as object), name: 'systemverilog' } as never],
      engine: createOnigurumaEngine(import('shiki/wasm')),
    });
  })();
  return highlighter;
}

// The source as coloured HTML, its colours for both themes held in CSS
// variables the stylesheet picks between.
export async function highlightSystemVerilog(text: string): Promise<string> {
  const core = await load();
  return core.codeToHtml(text, {
    lang: 'systemverilog',
    themes: { light: 'github-light', dark: 'github-dark' },
    defaultColor: false,
  });
}

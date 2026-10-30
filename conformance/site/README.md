# The results site

The pages that show a published night of the suite: how many cases each configuration conforms on,
clause by clause, down to each case's requirement, its source and what each configuration did with
it. It is a React application built with Vite, its controls Mantine's (https://mantine.dev); the
pages read only what `//conformance/publish:publish_records` writes into `public/`, and judge
nothing.

## Preview it

Publish records into `public/`, which Git ignores, then start the development server:

```bash
bazel build //conformance/publish:publish_records
bazel-bin/conformance/publish/publish_records --revision <suite revision> \
  --site conformance/site/public <record>...
cd conformance/site
npm ci
npm run dev
```

`npm run build` writes the site to `dist/`, which is what the nightly deploys.

## How its text is written

Readers are simulator developers and users, and many read English as a second language. The text
follows the Google developer documentation style guide, above all its page on writing for a global
audience (https://developers.google.com/style/translation): short sentences, the subject and verb
near the start, common words, no idioms, and sentence case for every heading and title
(https://developers.google.com/style/headings). On top of that:

- Each concept has one name, used everywhere. A verdict is "conforms", "does not conform" or
  "unresolved"; a configuration is a tool with one set of options.
- Every heading, label and control is a complete phrase on its own. No sentence starts in a heading
  and ends in a table.
- A control says what it shows or does, and keeps that name whether it is open or closed.
- An error says what is missing in the reader's terms and offers a way on. It never names a file of
  the site.
- Nothing says how the site is built. A revision appears only as a link to the source it names.

## How its tables are laid out

Tables follow Material Design's data tables (https://m2.material.io/components/data-tables) and the
GOV.UK Design System's table (https://design-system.service.gov.uk/components/table/):

- A column of numbers is aligned right, with its heading, in digits of equal width, so that units
  line up. A column of text is aligned left, with its heading.
- A number that has a column of its own, such as a clause's number, is never written into the text
  beside it.
- Every table is named for what it holds, by a caption or by the heading just above it, and its
  headings say what each row and column are.
- A count is shown as a count, of its total where the totals differ, and never coloured by its
  share: a clause of one case would read as strongly as a clause of eighty.
- On a narrow screen a table's rows become labelled blocks, so nothing is off the screen.
- A link in a table or in running text is in the text colour and underlined only under the pointer,
  and a row that leads somewhere answers to a press anywhere on it.

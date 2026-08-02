<!--
Authoring constraints for this manual (the doc renderer only understands
this subset):
- `#` document title, `##` section headings, `###`/`####` subsection headings.
- Inline markup: `**bold**`, `code`, links [text](url).
- Code blocks are fenced with ``` (optionally a language tag).
- Lists use `- `. Wrap lines at 80 columns. Do not use tables.
-->

# burogu

## NAME

burogu - a static blog generator

## SYNOPSIS

**burogu** COMMAND [OPTION]...

Commands:

`build`      build site/ from src/
`clean`      remove the output directory
`preview`    build once, then serve the site locally
`watch`      rebuild when sources change, optionally serve
`init`       initialize an src/ tree
`new`        create a post (today's date)
`draft`      create a draft (not published)
`publish`    publish a draft
`deploy`     build and deploy the site (rsync or git)
`sync`       sync the site repository with a git remote
`format`     normalize config.yaml, posts and pages
`doc`        print this manual

Run `burogu --help` for every option, and `burogu doc SECTION` (see
COMMANDS below) for a single manual section.

## DESCRIPTION

burogu turns a plain-text site into a static website. It reads posts
and pages from `src/`, renders them into HTML with a shared layout,
and writes the finished site to `site/` for deployment anywhere (a
VPS, GitHub Pages, a static host).

- Posts are dated, taggable articles; pages are standalone documents
  listed in the navigation.
- Every page ships a light and a dark color scheme; the visitor's
  system preference picks the one that is used, and a button in the
  footer overrides it (stored per visitor).
- `preview` and `watch --serve` run a local HTTP server for instant
  iteration.
- A site-wide search page is built in (see SEARCH below); a yearly
  archive, a tag index, and a 404 page are generated automatically
  when declared (see SITE LAYOUT).
- Built-in themes are selected with `theme.preset`: `aria` (the
  default, minimal and flat) and `shaft` (an editorial-print look
  with a single red accent). Font stacks can be overridden or
  embedded per site (see CONFIGURATION).

## COMMANDS

### build

Build the site: render every post and page, copy static files, and
write the output directory.

```
burogu build [--config PATH] [--src DIR] [--out DIR]
```

`--config`  configuration file (default: config.yaml)
`--src`     source directory; posts live in DIR/_post (default: src)
`--out`     output directory (default: site)

The output directory is rebuilt from scratch on every build. If any
page or post fails validation, the build stops before touching the
output and the previous site is left untouched.

Example:

```
burogu build --out /var/www/lizi.moe
```

### clean

Remove the output directory.

```
burogu clean [--out DIR]
```

### preview

Build once, then serve the output with the built-in HTTP server.

```
burogu preview [--port PORT]
```

`--port`  port to serve on (default: 8000)

The server listens on 127.0.0.1, serves generated pages with the
right content types, and serves `404.html` for missing pages (a plain
text answer if the site has no 404 page). Open
http://127.0.0.1:8000/ to look at the site. Ctrl-C stops the server.

### watch

Rebuild whenever `src/` or `config.yaml` changes, until interrupted.

```
burogu watch [--serve PORT]
```

`--serve`  also serve the site on this port

The source tree is polled; on any change the site is rebuilt (a
failed build keeps the previous output). With `--serve`, changes are
picked up without restarting the server.

### init

Create an `src/` tree with a sample post, a starter theme, and a
`config.yaml` template with every option commented.

```
burogu init [DIR]
```

`DIR`  target directory (default: src)

### new

Create a post with today's date.

```
burogu new SLUG
```

`SLUG`  post slug; used in the filename and the URL. The characters
        / ? # % and spaces are not allowed

The post is named `YYYY-MM-DD-SLUG.md` with a `date:` frontmatter.
A slug that already exists (as a post or a draft) is rejected.

### draft

Create a draft (not published).

```
burogu draft SLUG
```

`SLUG`  post slug; used in the filename and the URL. The characters
        / ? # % and spaces are not allowed

The draft is named `YYYY-MM-DD-SLUG.md` (today's date) with
`draft: true` and no date field. The filename date is just the
creation date; the published date comes from the frontmatter.

### publish

Publish a draft: add the date and drop the draft flag, in place.

```
burogu publish SLUG
```

`SLUG`  post slug

The date is taken from an existing `date:` frontmatter, or today
when there is none; `draft: true` becomes `draft: false` and the
frontmatter is normalized. The filename does not change. Publish
fails when the slug is missing or the file is not a draft.

### deploy

Build the site, then publish it per the `deploy` section of
`config.yaml` (see DEPLOYMENT).

```
burogu deploy [--clear-cache]
```

`--clear-cache`  remove the persistent git cache (git mode only; the
                 next deploy re-fetches from scratch)

### sync

Synchronize the site repository (the directory holding `config.yaml`
and `src/`) with a git remote (see SYNC).

```
burogu sync ACTION [REPO]
```

`ACTION`  push or pull
`REPO`    git repository URL (default: `srcRepo` from config.yaml)

### format

Normalize `config.yaml`, every post and every page: fill in the
missing frontmatter fields with their defaults, order the keys
canonically, and rewrite the files in place.

```
burogu format [--dry-run] [--config PATH] [--src DIR]
```

`--dry-run`  show what would change without writing anything

Posts get `title, date, tags, description, draft, toc` (the date comes
from the filename prefix when the frontmatter has none; drafts may
omit it). Pages get `title, priority, hiddenInNavbar, redirectAs`.
Empty `description`/`redirectAs` fields are not written. Unknown
frontmatter keys are kept (sorted) with a warning; comments in the
frontmatter are not preserved.

For `config.yaml`, format rewrites the file as the fully documented
template (every key with a default and a comment, optional sections
commented) with your current values filled in. Unknown config keys
are dropped with a warning.

Files that fail validation are reported and skipped; the command
exits non-zero when any file failed. Re-running format changes
nothing (idempotent).

### doc

Print this manual.

```
burogu doc [SECTION] [--lang LANG] [--color MODE]
```

`SECTION`  a section name as printed in the table of contents
           (e.g. `config`, `commands`); default: the whole manual
`--lang`   `en`, `zh`, `zh-Hant` or `ja`; default: the system
           locale (ja* -> Japanese, zh_TW/zh_HK/zh_MO -> Traditional
           Chinese, other zh* -> Simplified Chinese, anything else
           English)
`--color`  `auto`, `always` or `never`; default: auto (ANSI styling
           when printing to a terminal, plain text when piped)

## CONFIGURATION

`config.yaml` sits next to `src/` (the `init` template documents every
key). All keys are optional; defaults are printed as warnings on
startup. Unknown keys are ignored.

### site

`siteName`         site title; used in the header, the HTML title and
                   og:site_name (default: burogu)
`siteAuthor`       author name; used as the default copyright holder
                   (default: empty)
`siteDescription`  site description; used in the HTML description
                   meta tag
`siteLang`         page language code, e.g. `en` or `zh-CN`
                   (default: zh-CN)
`baseUrl`          site URL, e.g. `https://lizi.moe`. Must start with
                   http:// or https://. When set, the feed and the
                   og:url meta tags are generated
`siteCopyright`    footer copyright text (default: © + siteAuthor)
`siteGeneratedBy`  footer credit line next to the copyright, e.g.
                   "Generated with Burogu". Omitted when unset

### deploy

`target`         rsync target (user@host:/path). When set, `deploy`
                 publishes with rsync (--delete)
`repo`           git repository URL. When set, `deploy` publishes by
                 committing the site to a git branch (GitHub Pages
                 etc.); `target` and `repo` are mutually exclusive
`branch`         branch to publish to (git mode; required with repo)
`commitName`     commit identity: name (git mode; required)
`commitEmail`    commit identity: email (git mode; required)

### theme

`preset`     built-in theme: `aria` or `shaft` (default: aria)
`math`       math rendering: `none`, `mathjax` or `katex`
             (default: mathjax)
`mathUrl`    CDN URL for the math script; defaults to the pandoc
             defaults for the chosen method
`extraCss`   list of files under `src/`, appended to the generated
             stylesheet (they win over generated rules)
`extraJs`    list of files under `src/`, loaded as deferred scripts
             on every page (missing files abort the build)

#### fonts

Override the preset's typography; every key is optional and falls
back to the preset's defaults:

`body`         font stack for body text (list of names; shaft
               defaults to a serif stack)
`body`         font stack for body text (list of names; shaft
               defaults to a serif stack)
`code`         font stack for code (list of names; default: a
               cross-platform monospace stack)
`size`         base font size, e.g. `17px`
`lineHeight`   base line height, e.g. `28px`
`files`        embedded font files (see below)

Names containing spaces are quoted automatically; the generic
keywords `serif`, `sans-serif`, `monospace` and the `system-ui`/`ui-*`
families are emitted bare.

`files` entries (each: `src`, `family`, optional `weight`, optional
`style`) copy a font file from `src/` into `site/fonts/` and generate
an @font-face rule; reference it from a stack by its family name:

```
theme:
  preset: shaft
  fonts:
    display: [Georgia, "Noto Serif CJK SC", "Songti SC", SimSun, serif]
    files:
      - src: fonts/my-serif.woff2
        family: My Serif
        weight: 400
        style: normal
```

A missing font file is a build error (the previous output is kept).

### srcRepo

`srcRepo`  default git repository URL for `sync` (see SYNC)

## SITE LAYOUT

```
config.yaml          site configuration
src/
  _post/             posts (YYYY-MM-DD-slug.md)
  _pages/            pages (slug.md)
  everything else    copied verbatim to the site
site/                build output (regenerated on every build)
```

### Posts

Posts live in `src/_post/*.md` with YAML frontmatter:

`title`        post title (default: the slug)
`date`         publication date, `YYYY-MM-DD` (default: taken from
               the filename prefix `YYYY-MM-DD-`)
`tags`         list of tags, e.g. `[essay, review]`
`description`  short summary (used in the home page list, the feed
               and og:description)
`draft`        `true` hides the post and allows a missing date
`toc`          `true` renders a table of contents for the post

A post's URL is `/posts/slug/`. Tags link to the tag archive, and
posts with the same tag must use the same spelling (a warning is
printed for tags differing only in case). Math (with `theme.math`) is
detected automatically per post. The meta line shows an estimated
reading time, and the bottom of the page links the previous and next
posts.

### Pages

Pages live in `src/_pages/*.md`; each becomes `/slug/` and is listed
in the navigation:

`title`            navigation label and page title
`priority`         navigation position, lower first (default: 100)
`hiddenInNavbar`   `true` keeps the page out of the navigation (it
                   stays reachable at /slug/)
`redirectAs`       redirect this page: declare a special page, or
                   point anywhere else (see below)

With `redirectAs`, the page becomes a redirect stub: its slug URL
serves an instant meta-refresh page (with a rel=canonical link) to
the target, and the markdown body is not used. Targets must start
with `/` (a site path) or `http(s)://` (an external URL); anything
else is a build error. A target equal to the page's own slug URL is
ignored.

The special pages (redirectAs = one of `/tags/`, `/archive/`,
`/search/`, `/404.html`, `/`) get generated content instead of the
markdown body:

- `/tags/` - the tag index (every tag with a link to its archive)
- `/archive/` - all posts grouped by year
- `/search/` - the client-side search page (see SEARCH)
- `/404.html` - served by the preview server (and any static host)
  for missing pages; **its body is your own markdown**
- `/` - the home page: its content is rendered above the post list
  (which gets a section title); the page is not in the navigation

Every page type (normal, redirect stub, special) appears in the
navigation by default, 404 included; `hiddenInNavbar: true` hides
any of them.

### Static files and built-ins

Everything else under `src/` (images, CNAME, favicons, fonts...) is
copied to the site unchanged. The generator itself writes
`style.css`, `robots.txt`, `sitemap.xml`, the feed `feed.xml` (only
with `baseUrl`), and `search.json` (only with a search page). Files
with the same name as a built-in (e.g. `src/style.css`) override it.

## SEARCH

Declare a page with `redirectAs: /search/` to enable site-wide
client-side search. The search page is a text input plus a results
list; typing filters the whole site (posts and pages) with
case-insensitive substring matching, and matches are highlighted.
`search.json` (generated when the search page exists) carries the
index: full text, title, URL, and for posts the date and tags.

The default behavior can be replaced entirely: define a global
`window.buroguSearch` function in a `theme.extraJs` file and the
built-in script hands over to it (it receives `{ url:
"/search.json" }`). Styling can be customized through
`theme.extraCss`.

## DEPLOYMENT

`burogu deploy` builds the site and publishes it. Two modes are
configured in the `deploy` section of `config.yaml`:

### rsync

With `deploy.target`, the site is mirrored to the target with
`rsync --delete`: `user@host:/var/www/lizi.moe`. Suitable for VPS
deployments.

### git

With `deploy.repo` and `deploy.branch`, the site is committed to a
git branch (GitHub Pages, Gitee Pages...). `deploy.commitName` and
`deploy.commitEmail` identify the commits and are required.

The branch is fetched into a persistent cache directory
(`~/.cache/burogu-deploy`, honoring XDG_CACHE_HOME), the fresh build
is committed on top with a timestamped message, and the result is
pushed as a fast-forward (never forced). `--clear-cache` starts over
from an empty cache.

## SYNC

`sync` treats the directory holding `config.yaml` and `src/` as a git
repository of its own (the `site/` output is ignored):

- `burogu sync push` commits every change (`config.yaml`, `src/`) to
  the local repo and pushes it to the remote
- `burogu sync pull` resets the local repo to the remote branch
  (remote wins; local changes are discarded)

The remote defaults to `srcRepo` in `config.yaml` and can be
overridden with a `REPO` argument. `push` reports when there is
nothing to push.

## FILES

`config.yaml`        site configuration (next to src/)
`src/`               posts, pages and static files
`site/`              build output (regenerated; safe to delete)
`~/.cache/burogu-deploy/`  persistent cache for git deployments

## EXIT STATUS

0 on success. Non-zero on any error, including invalid configuration
or frontmatter, unknown theme presets, missing extra files, and
failed deploys.

## SEE ALSO

`burogu --help`, the README, and the comments in the `config.yaml`
template written by `burogu init`.

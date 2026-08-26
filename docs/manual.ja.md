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

burogu - 静的ブログ生成ツール

## SYNOPSIS

**burogu** COMMAND [OPTION]...

コマンド：

`build`       src/ から site/ を生成する
`clean`       出力ディレクトリを削除する
`preview`     一度生成してから、ローカルでサイトを配信する
`watch`       ソース変更時に再生成する（任意で配信も行う）
`init`        src/ ディレクトリツリーを初期化する
`new`         記事を作成する（今日の日付）
`draft`       下書きを作成する（公開されない）
`publish`     下書きを公開する
`deploy`      サイトを生成してデプロイする（rsync または git）
`sync`        サイトリポジトリと git リモートを同期する
`format`      config.yaml・記事・ページを正規化する
`doc`         本マニュアルを表示する

`burogu --help` で全オプション、`burogu doc SECTION`（下記
COMMANDS 節参照）でマニュアルの個別セクションを表示できます。

## DESCRIPTION

burogu はプレーンテキストのサイトを静的サイトに変換します。
`src/` から記事とページを読み込み、共通レイアウトで HTML に
レンダリングし、完成したサイトを `site/` に書き出します。VPS、
GitHub Pages、任意の静的ホストにデプロイできます。

- 記事は日付とタグを持つエントリで、ページはナビゲーションに
  表示される独立したドキュメントです
- すべてのページはライト・ダークの二種類の配色を持ち、訪問者の
  システム設定に応じて切り替わります。フッターのボタンで手動
  上書きもできます（訪問者ごとに保存）
- `preview` と `watch --serve` は組み込みのローカル HTTP サーバーを
  起動し、すぐに確認できます
- サイト内検索ページが組み込まれています（SEARCH 節参照）。宣言
  すると年別アーカイブ、タグインデックス、404 ページも自動生成
  されます（SITE LAYOUT 節参照）
- 組み込みテーマは `theme.preset` で選択します：`aria`（デフォ
  ルト、ミニマルでフラット）と `shaft`（編集・印刷風、赤の
  アクセントひとつ）。フォントスタックはサイト単位で上書き・
  埋め込みできます（CONFIGURATION 節参照）

## COMMANDS

### build

サイトを生成します：すべての記事とページをレンダリングし、
静的ファイルをコピーして出力ディレクトリに書き出します。

```
burogu build [--config PATH] [--src DIR] [--out DIR]
```

`--config`  設定ファイル（デフォルト：config.yaml）
`--src`     ソースディレクトリ（記事は DIR/_post）（デフォルト：src）
`--out`     出力ディレクトリ（デフォルト：site）

出力ディレクトリは生成のたびに作り直されます。ページや記事の
検証に失敗した場合は、出力ディレクトリに触れる前に停止し、
以前のサイトがそのまま残ります。

例：

```
burogu build --out /var/www/lizi.moe
```

### clean

出力ディレクトリを削除します。

```
burogu clean [--out DIR]
```

### preview

一度生成してから、組み込み HTTP サーバーでサイトを配信します。

```
burogu preview [--port PORT]
```

`--port`  ポート番号（デフォルト：8000）

サーバーは 127.0.0.1 のみを監視し、生成済みページを適切な
Content-Type で配信します。存在しないページは `404.html` に
フォールバックします（404 ページがない場合はプレーンテキスト）。
ブラウザで http://127.0.0.1:8000/ を開いて確認します。
Ctrl-C で停止します。

### watch

`src/` または `config.yaml` が変更されるたびに再生成し、中断
されるまで実行し続けます。

```
burogu watch [--serve PORT]
```

`--serve`  指定ポートでもサイトを配信する

ソースディレクトリをポーリングし、変更があれば再生成します
（失敗した生成は以前の出力を保持）。`--serve` と組み合わせると、
サーバーを再起動せずに変更が反映されます。

### init

サンプル記事、スターターテーマ、`config.yaml` テンプレート
（すべての項目にコメント付き）を含む `src/` ディレクトリツリーを
作成します。

```
burogu init [DIR]
```

`DIR`  対象ディレクトリ（デフォルト：src）

### new

今日の日付で記事を作成します。

```
burogu new SLUG
```

`SLUG`  記事スラッグ。ファイル名と URL に使用。/ ? # % と
        スペースは使用できません

記事は `YYYY-MM-DD-SLUG.md` として作成され、`date:` frontmatter
を持ちます。スラッグが既に存在する場合（記事・下書きのどちらも）
作成は拒否されます。

### draft

下書きを作成します（公開されません）。

```
burogu draft SLUG
```

`SLUG`  記事スラッグ。ファイル名と URL に使用。/ ? # % と
        スペースは使用できません

下書きは `YYYY-MM-DD-SLUG.md`（今日の日付）として作成され、
`draft: true` で date フィールドはありません。ファイル名の日付は
作成日を示すだけです。公開日は frontmatter が基準になります。

### publish

下書きを公開します：日付を追加し、下書きフラグを外します。

```
burogu publish SLUG
```

`SLUG`  記事スラッグ

日付は frontmatter の既存の `date:` を優先し、なければ今日を
使います。`draft: true` は `draft: false` になり、frontmatter は
正規化されます。ファイル名は変わりません。スラッグが見つから
ないか、ファイルが下書きでない場合はエラーになります。

### deploy

サイトを生成し、`config.yaml` の `deploy` 節に従って公開します
（DEPLOYMENT 節参照）。

```
burogu deploy [--clear-cache]
```

`--clear-cache`  永続 git キャッシュを削除する（git モードのみ。
                 次回デプロイはゼロから取得し直す）

### sync

サイトリポジトリ（`config.yaml` と `src/` を置くディレクトリ）を
git リモートと同期します（SYNC 節参照）。

```
burogu sync ACTION [REPO]
```

`ACTION`  push または pull
`REPO`    git リポジトリ URL（デフォルト：config.yaml の `srcRepo`）

### format

`config.yaml`・全記事・全ページを正規化します：frontmatter の
欠落フィールドにデフォルト値を補い、キーを正規の順序に並べ、
ファイルをその場で書き換えます。

```
burogu format [--dry-run] [--config PATH] [--src DIR]
```

`--dry-run`  書き込まずに変更内容だけを表示する

記事には `title, date, tags, description, draft, toc` が付きます
（frontmatter に日付がない場合はファイル名プレフィックスから
取得。下書きは日付を省略可）。ページには `title, priority,
placement, redirectAs` が付きます。空の `description`/
`redirectAs` は書き込みません。未知の frontmatter キーは
（辞書順で）保持され警告が出ます。frontmatter 内のコメントは
保持されません。

`config.yaml` については、format は完全なドキュメント化
テンプレート（すべてのキーにデフォルト値とコメント、任意セク
ションはコメント例）として書き直し、現在の値を埋め込みます。
未知の設定キーは警告のうえ破棄されます。

検証に失敗したファイルは報告してスキップされ、コマンドは
非ゼロで終了します。format を再実行しても何も変わりません
（冪等）。

### doc

本マニュアルを表示します。

```
burogu doc [SECTION] [--lang LANG] [--color MODE]
```

`SECTION`  セクション名（例：`config`、`commands`）；デフォルト：
           全文
`--lang`   `en`、`zh`、`zh-Hant` または `ja`；デフォルト：
           システムのロケールに従う（ja* ロケールは日本語、
           zh_TW/zh_HK/zh_MO は繁体中国語、その他の zh* は簡体
           中国語、それ以外は英語）
`--color`  `auto`、`always` または `never`；デフォルト：auto
           （端末出力時は ANSI スタイル、パイプ/リダイレクト時は
           プレーンテキスト）

## CONFIGURATION

`config.yaml` は `src/` と同じ階層に置きます（`init` テンプレートが
各キーを説明しています）。すべてのキーは任意です。デフォルト値は
起動時に警告として表示されます。未知のキーは無視されます。

### site

`siteName`         サイトタイトル。ヘッダー・HTML タイトル・
                   og:site_name に使用（デフォルト：burogu）
`siteAuthor`       著者名。デフォルトの著作権表示に使用
                   （デフォルト：空）
`siteDescription`  サイト説明。HTML description meta タグに使用
`siteLang`         ページ言語コード（例：`en`、`zh-CN`）
                   （デフォルト：zh-CN）
`baseUrl`          サイト URL（例：`https://lizi.moe`）。
                   http:// または https:// で始まる必要があります。
                   設定すると feed と og:url meta タグが生成されます
`siteCopyright`    フッターの著作権テキスト
                   （デフォルト：© + siteAuthor）
`siteGeneratedBy`  フッターの著作権の横に表示する生成元クレジット
                   （例："Generated with Burogu"）。未設定時は非表示

### deploy

`target`         rsync ターゲット（user@host:/path）。設定すると
                 `deploy` は rsync（--delete）で公開します
`repo`           git リポジトリ URL。設定すると `deploy` はサイトを
                 git ブランチにコミットして公開します（GitHub Pages
                 など）。`target` とは排他です
`branch`         公開先ブランチ（git モード。repo と併用が必須）
`commitName`     コミットの作者名（git モード。必須）
`commitEmail`    コミットの作者メール（git モード。必須）

### theme

`preset`     組み込みテーマ：`aria` または `shaft`
             （デフォルト：aria）
`math`       数式レンダリング：`none`、`mathjax` または `katex`
             （デフォルト：mathjax）
`mathUrl`    数式スクリプトの CDN URL。デフォルトは選択方式の
             pandoc デフォルト
`extraCss`   `src/` 配下のファイルリスト。生成スタイルシートの末尾
             に追加されます（生成ルールを上書き可能）
`extraJs`    `src/` 配下のファイルリスト。全ページに遅延スクリプト
             として読み込まれます（ファイル欠落時はビルド中断）

#### fonts

プリセットのタイポグラフィを上書きします。すべてのキーは任意で、
デフォルトはプリセットの値にフォールバックします：

`body`         本文フォントスタック（名前リスト。shaft はデフォル
               トでセリフスタック）
`display`      見出し・日付・年の表示用フォントスタック（名前
               リスト。shaft はデフォルトでセリフスタック）
`code`         コード用フォントスタック（名前リスト。デフォルト：
               クロスプラットフォームの等幅スタック）
`size`         基本フォントサイズ（例：`17px`）
`lineHeight`   基本行の高さ（例：`28px`）
`files`        埋め込みフォントファイル（下記参照）

スペースを含むフォント名は自動的に引用符で囲まれます。汎用
キーワードの `serif`、`sans-serif`、`monospace` と
`system-ui`/`ui-*` 系はそのまま出力されます。

`files` の各エントリ（`src`、`family`、任意の `weight`、`style`）
は `src/` のフォントファイルを `site/fonts/` にコピーし、
@font-face ルールを生成します。フォントスタック内では family 名で
参照します：

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

フォントファイルの欠落はビルドエラーになります（以前の出力は
保持）。

### srcRepo

`srcRepo`  `sync` のデフォルト git リポジトリ URL（SYNC 節参照）

## SITE LAYOUT

```
config.yaml          サイト設定
src/
  _post/             記事（YYYY-MM-DD-slug.md）
  _pages/            ページ（slug.md）
  その他すべてのファイル   そのままサイトにコピー
site/                生成出力（生成のたびに作り直される）
```

### Posts 記事

記事は `src/_post/*.md` に置き、YAML frontmatter を持ちます：

`title`        記事タイトル（デフォルト：slug）
`date`         公開日、`YYYY-MM-DD`（デフォルト：ファイル名
               プレフィックス `YYYY-MM-DD-` から取得）
`tags`         タグリスト（例：`[essay, review]`）
`description`  要約（ホームのリスト・feed・og:description に使用）
`draft`        `true` で記事を非公開にし、日付省略を許可
`toc`          `true` で記事に見出し目次を表示する

記事の URL は `/posts/slug/` です。タグはタグアーカイブにリンク
されます。同じタグは同じ綴りでなければなりません（大文字小文字
だけが異なるタグは警告が表示されます）。数式（`theme.math`）は
記事ごとに自動検出されます。メタ行には推定読了時間が表示され、
ページ下部に前後の記事へのリンクがあります。

前後の記事へのリンクがあります。

テキストに言語を指定して、地域に合った字形をブラウザに選ばせる
ことができます（例: 「骨」は zh と ja で字形が異なります）:

    [日本語の骨]{lang=ja}               インライン
    ::: {lang=zh-TW}
    繁體段落
    :::

`ja` と `zh-Hant`/`zh-TW` のフォントはテーマに従います（
`--font-ja`、`--font-hant`。`theme.fonts.ja`/`theme.fonts.hant` で
上書き可能）。

### Pages ページ

ページは `src/_pages/*.md` に置き、それぞれ `/slug/` になり
ナビゲーションに表示されます：

`title`            ナビゲーションラベルとページタイトル
`priority`         ナビゲーション位置。小さい方が先（負の値で先頭に固定）
                   （デフォルト：100）
`placement`       ページリンクの表示位置: `nav`（デフォルト、
                   ヘッダーナビ）、`footer`（フッターのリンク行）
                   または `none`（どこにも表示しない。ページ自体
                   は /slug/ でアクセス可能）
`redirectAs`       このページをリダイレクトする：特殊ページを
                   宣言するか、任意のアドレスを指定（下記参照）

`redirectAs` を設定すると、このページはリダイレクトスタブに
なります：slug URL がターゲットへの即時 meta-refresh ページ
（rel=canonical リンク付き）を配信し、markdown 本文は使われません。
ターゲットは `/`（サイト内パス）または `http(s)://`（外部 URL）で
始まる必要があり、それ以外はビルドエラーです。ターゲットが
このページ自身の slug URL と等しい場合は無視されます。

特殊ページ（`redirectAs` が `/tags/`、`/archive/`、`/search/`、
`/404.html`、`/` のいずれか）は markdown 本文ではなく生成された
内容を持ちます：

- `/tags/` - タグインデックス（各タグにアーカイブリンク）
- `/archive/` - 全記事を年ごとにグループ化
- `/search/` - クライアントサイド検索ページ（SEARCH 節参照）
- `/404.html` - プレビューサーバー（および静的ホスト）が存在
  しないページに対して配信する内容。**本文はあなたの markdown です**
- `/` - ホームページ：コンテンツは記事リストの上にレンダリング
  されます（リストにセクション見出しが付きます）。ナビゲーション
  には表示されません

すべてのページタイプ（通常ページ、リダイレクトスタブ、特殊
ページ、404 も含む）はデフォルトでナビゲーションに表示され、
`placement: none` または `placement: footer` でヘッダーから
非表示にできます。フッターリンクは著作権行の上に
`footerSeparator`（config。空文字で無効）区切りで表示され、
`redirectAs` が http(s) URL のフッターページは直接リンクします。
旧 `hiddenInNavbar` キーはビルド時に拒否されます。
`burogu format` で移行してください。

### Scripts スクリプト

ページは markdown の代わりにスクリプトで生成できます。frontmatter
に `script:` フィールドを追加します。そのファイル（`src/_scripts/`
からの相対パス）はビルド時に評価され、文字列の結果がページ本文
（raw HTML）になります。markdown 本文は無視されます。

`script`   スクリプトファイル（`src/_scripts/` 配下）、例: `hello.d`
`output`   サイトルート内の相対パス: スクリプト結果はページの代わりに
           そのファイルへ書き出されます（例: `data.json`）。ページは
           レンダリングされずナビゲーションにも表示されません。
           `script` 必須。`redirectAs` との併用不可。重複パスはエラー

スクリプトはサイトコンテキストを束縛した状態で実行されます:

`site`     サイト設定: siteName、siteAuthor、siteDescription、
           siteLang、siteCopyright、baseUrl、siteGeneratedBy
`posts`    全記事: title、date、tags、url、draft、text、
           description
`pages`    カスタムページ: slug、url、title、redirectAs
`tags`     全タグと投稿数: name、count
`config`   生の設定: theme（preset、math、mathUrl、extraCss、
           extraJs）、srcRepo
`data`     ユーザーデータファイル: `src/_data/` の各 YAML ファイル
           （拡張子 `.yaml` を除いたファイル名 → 内容）。他の拡張子
           は無視されます

スクリプトは文字列を生成します。その行き先はページ宣言で決まり
ます: `script:` だけならページ本文（通常のレイアウト内）、
`script:` + `output:` なら指定したサイトパスへ書き出します
（レイアウトなし）。出力ファイルは最後に書き込まれるため、
ジェネレーターや静的ファイルが書いた任意のものを上書きできます
——`index.html`（完全にカスタムなホームページ）や記事ページも
含みます。

`puts(...)` はビルド中に引数を stderr へ出力します。スクリプトの
エラー（構文・実行時）は他のページエラーと同様にビルドを失敗させ、
出力ディレクトリは変更されません。

#### スクリプト言語

小さな Ruby 風、動的型付け、純計算の言語です。関数呼び出しは
`f(a, b)` の一通りだけです。単独の `f` は関数値そのものです。
すべてが式であり、プログラム・`def` 本体・ラムダ本体は隣接する
式の列で、値は最後の式です。

リテラル: `42`、`1.5`、`"text #{expr}"`（補間、ネスト可）、
`true`、`false`、`nil`、`[1, 2]`、`{"a" => 1}`。ラムダ:
`{ x, y -> expr ... }`。定義: `def f(a, b) expr ... end`
（トップレベルの `def` は順序に関係なく相互参照できます）。
条件: `if cond then expr else expr end`（`else` は省略可）。
演算子: `+ - * / % == != < > <= >= && || !`、単項マイナス。
偽になるのは `false` と `nil` だけです。ループも代入もありません。
再帰と `map`/`filter` を使います。

組み込み関数:

    len(x)          文字列・配列・マップの長さ
    at(x, i)        配列・文字列のインデックス i の要素
    get(m, k)       マップのキー k の値（無ければ nil）
    append(a, v)    配列 a に v を追加したコピー
    concat(a, b)    配列・文字列の連結
    join(a, sep)    配列を文字列に結合
    split(s, sep)   文字列を区切り文字で分割
    map(a, f)       各要素に f を適用した配列
    filter(a, f)    f が真の要素だけの配列
    sort(a)         数値・文字列の配列をソート
    reverse(a)      配列を反転
    first(a)        最初の要素（空なら nil）
    last(a)         最後の要素（空なら nil）
    keys(m)         マップのキー
    values(m)       マップの値
    contains(a, x)  部分文字列・要素の包含判定
    trim(s)         前後の空白を除いた文字列
    lower(s)        小文字の文字列
    upper(s)        大文字の文字列
    replace(s, f, t) 文字列中の f を t に置換
    take(a, n)      先頭 n 個の要素（または文字）
    drop(a, n)      先頭 n 個を除いた要素（または文字）
    toStr(v)        数値・真偽値・nil・文字列を文字列化
    toJson(v)       値を pretty JSON に（nil は null）
    formatDate(d, f)  strftime 風の日付フォーマット（ISO 日付）

指令: %Y %y %m %d %b %B %a %A %%；%-m/%-d はゼロ埋めなし。
不明な指令はエラー。例: formatDate(date, "%Y年%-m月%-d日") → 2026年8月2日
    formatDate(d, f)  strftime 風の日付フォーマット（ISO 日付）

HTML ヘルパー（コンテンツはそのまま挿入。テキストは `esc` でエスケープ）:
    el(name, attrs, content)  任意のタグ。attrs: true=裸の属性、
                              false/nil=省略、キーと値はエスケープ
    esc(s)                    & < > " ' をエスケープ
    h1 h2 p div span strong em time ul ol li  コンテンツのみのタグ
    a(content, href)          href をエスケープしたアンカー
    img(src, alt)             空要素の画像。alt は nil で省略
    空要素（br、hr、img、input、meta、link、source）は閉じタグなし。
    puts(...)       引数を stderr に出力（nil を返す）

#### 例

    # src/_scripts/hello.d
    "<h2>Hello #{get(site, "siteName")}!</h2>"
      + join(map(posts, { p -> "<li>" + get(p, "title") + "</li>" }), "")

    # src/_pages/hello.md
    ---
    title: Hello
    script: hello.d
    ---

### 静的ファイルとビルトイン出力

`src/` 配下のその他のファイル（画像、CNAME、favicon、フォント…）
はそのままサイトにコピーされます。生成ツール自身は
`style.css`、`robots.txt`、`sitemap.xml`、feed `feed.xml`
（baseUrl 設定時のみ）と `search.json`（検索ページ宣言時のみ）を
書き出します。ビルトインと同名のファイル（例：`src/style.css`）は
それを上書きします。

## SEARCH

`redirectAs: /search/` のページを宣言するとサイト全体のクライア
ントサイド検索が有効になります。検索ページはテキスト入力と結果
リストで構成され、入力するとサイト全体（記事とページ）を大文字
小文字を無視した部分文字列マッチでフィルタリングし、一致箇所を
ハイライトします。`search.json`（検索ページ宣言時に生成）が
インデックスを保持します：本文全文、タイトル、URL、記事には日付
とタグも含まれます。

デフォルトの動作は丸ごと置き換えられます：`theme.extraJs`
ファイルでグローバル関数 `window.buroguSearch` を定義すると、
組み込みスクリプトは制御をそれに委ねます（`{ url:
"/search.json" }` を受け取ります）。スタイルは `theme.extraCss`
でカスタマイズできます。

## DEPLOYMENT

`burogu deploy` はサイトを生成して公開します。二つのモードを
`config.yaml` の `deploy` 節で設定します：

### rsync

`deploy.target` を設定すると、サイトは `rsync --delete` で
`user@host:/var/www/lizi.moe` のようなターゲットにミラーリング
されます。VPS デプロイ向きです。

### git

`deploy.repo` と `deploy.branch` を設定すると、サイトは git
ブランチにコミットされます（GitHub Pages、Gitee Pages…）。
`deploy.commitName` と `deploy.commitEmail` はコミットの作者を
識別するため必須です。

ブランチは永続キャッシュディレクトリ（`~/.cache/burogu-deploy`、
XDG_CACHE_HOME に従う）にフェッチされ、新しいビルドがタイム
スタンプ付きのコミットメッセージでその上にコミットされ、
fast-forward でプッシュされます（強制は決してしません）。
`--clear-cache` で空のキャッシュからやり直します。

## SYNC

`sync` は `config.yaml` と `src/` を置くディレクトリを独立した git
リポジトリとして扱います（`site/` 出力は無視）：

- `burogu sync push` はすべての変更（`config.yaml`、`src/`）を
  ローカルリポジトリにコミットし、リモートにプッシュします
- `burogu sync pull` はローカルリポジトリをリモートブランチに
  リセットします（リモート優先。ローカルの変更は破棄）

リモートはデフォルトで `config.yaml` の `srcRepo` を使用し、
`REPO` 引数で上書きできます。`push` はプッシュするものが
ない場合にその旨を表示します。

## FILES

`config.yaml`       サイト設定（src/ と同じ階層）
`src/`              記事・ページ・静的ファイル
`site/`             生成出力（毎回作り直し。削除しても安全）
`~/.cache/burogu-deploy/`  git デプロイの永続キャッシュ

## EXIT STATUS

成功時は 0。無効な設定や frontmatter、未知のテーマプリセット、
追加ファイルの欠落、デプロイ失敗など、あらゆるエラーで非ゼロを
返します。

## SEE ALSO

`burogu --help`、README、および `burogu init` が生成する
`config.yaml` テンプレートのコメント。

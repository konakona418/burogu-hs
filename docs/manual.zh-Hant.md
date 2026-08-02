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

burogu - 靜態部落格產生器

## SYNOPSIS

**burogu** COMMAND [OPTION]...

命令：

`build`      從 src/ 建置 site/
`clean`      刪除輸出目錄
`preview`    建置一次，然後在本地提供站台服務
`watch`      來源檔案變更時重新建置，可選附帶服務
`init`       初始化 src/ 目錄樹
`new`        建立一篇正式文章（今天日期）
`draft`      建立一篇草稿（不發布）
`publish`    發布草稿
`deploy`     建置並部署站台（rsync 或 git）
`sync`       將站台儲存庫與 git 遠端同步
`format`     正規化 config.yaml、文章與頁面
`doc`        列印本手冊

執行 `burogu --help` 檢視全部選項，執行 `burogu doc SECTION`（見
下方 COMMANDS 節）檢視手冊的單一章節。

## DESCRIPTION

burogu 把純文字的站台變成一個靜態網站：從 `src/` 讀取文章與頁面，
以共享版面配置轉譯成 HTML，並把成品寫到 `site/`，部署到任何地方
（VPS、GitHub Pages、任意靜態託管）。

- 文章是有日期、可標籤的條目；頁面是獨立的文件，出現在導覽列裡
- 每個頁面同時帶淺色與暗色兩套配色，訪客的系統偏好決定用哪套；
  頁尾按鈕可手動覆蓋（依訪客持久化）
- `preview` 與 `watch --serve` 內建本地 HTTP 伺服器，方便即時疊代
- 內建站內搜尋頁（見 SEARCH 節）；宣告後自動產生年度歸檔、標籤
  索引與 404 頁（見 SITE LAYOUT 節）
- 內建主題由 `theme.preset` 選擇：`aria`（預設，極簡平面）與
  `shaft`（編輯印刷感，單一紅色錨點）。字型堆疊可按站覆蓋或內嵌
  （見 CONFIGURATION 節）

## COMMANDS

### build

建置站台：轉譯所有文章與頁面、複製靜態檔案、寫出輸出目錄。

```
burogu build [--config PATH] [--src DIR] [--out DIR]
```

`--config`  設定檔（預設：config.yaml）
`--src`     來源目錄；文章位於 DIR/_post（預設：src）
`--out`     輸出目錄（預設：site）

每次建置都會從零重建輸出目錄。若任何頁面或文章驗證失敗，建置
會在碰觸輸出目錄之前停止，舊的站台原樣保留。

範例：

```
burogu build --out /var/www/lizi.moe
```

### clean

刪除輸出目錄。

```
burogu clean [--out DIR]
```

### preview

建置一次，然後用內建 HTTP 伺服器提供站台。

```
burogu preview [--port PORT]
```

`--port`  監聽連接埠（預設：8000）

伺服器只監聽 127.0.0.1，以正確的 Content-Type 提供產生的頁面；
缺失的頁面回退到 `404.html`（站台沒有 404 頁時回傳純文字）。用
瀏覽器開啟 http://127.0.0.1:8000/ 檢視效果。Ctrl-C 停止。

### watch

`src/` 或 `config.yaml` 一旦變更就重新建置，直到被中斷。

```
burogu watch [--serve PORT]
```

`--serve`  同時在指定連接埠提供站台

來源目錄被輪詢偵測；任何變更都會觸發重建（失敗的建置保留舊輸出）。
配合 `--serve`，變更無需重新啟動伺服器即可生效。

### init

建立帶範例文章、起步主題和 `config.yaml` 範本的 `src/` 目錄樹
（範本裡每個設定項都帶註解）。

```
burogu init [DIR]
```

`DIR`  目標目錄（預設：src）

### new

建立一篇今天日期的正式文章。

```
burogu new SLUG
```

`SLUG`  文章 slug，用於檔案名稱與 URL；不允許 / ? # % 與空格

文章命名為 `YYYY-MM-DD-SLUG.md`，帶 `date:` frontmatter。slug
已存在（無論是文章還是草稿）時拒絕建立。

### draft

建立一篇草稿（不發布）。

```
burogu draft SLUG
```

`SLUG`  文章 slug，用於檔案名稱與 URL；不允許 / ? # % 與空格

草稿命名為 `YYYY-MM-DD-SLUG.md`（今天的日期），帶 `draft: true`
且無 date 欄位。檔名裡的日期只是建立日期；發布時的日期以
frontmatter 為準。

### publish

發布草稿：補上日期、去掉草稿標記，原地完成。

```
burogu publish SLUG
```

`SLUG`  文章 slug

日期取 frontmatter 已有的 `date:`，沒有則用今天；`draft: true`
變為 `draft: false` 並正規化 frontmatter。檔名不變。slug 不存在
或檔案不是草稿時報錯。

### deploy

建置站台，然後依 `config.yaml` 的 `deploy` 節發布（見
DEPLOYMENT 節）。

```
burogu deploy [--clear-cache]
```

`--clear-cache`  清空持久 git 快取（僅 git 模式；下次部署會從零
                 重新抓取）

### sync

把站台儲存庫（存放 `config.yaml` 與 `src/` 的目錄）與 git 遠端
同步（見 SYNC 節）。

```
burogu sync ACTION [REPO]
```

`ACTION`  push 或 pull
`REPO`    git 儲存庫位址（預設：config.yaml 的 `srcRepo`）

### format

正規化 `config.yaml`、全部文章與頁面：把缺失的 frontmatter 欄位
補上預設值、依規範順序排列鍵、原地重寫檔案。

```
burogu format [--dry-run] [--config PATH] [--src DIR]
```

`--dry-run`  只展示將寫內容，不落盤

文章得到 `title, date, tags, description, draft, toc`（frontmatter 沒有
日期時從檔案名稱前綴取；草稿可省略日期）。頁面得到 `title,
priority, hiddenInNavbar, redirectAs`。空的 `description`/
`redirectAs` 不寫入。未知 frontmatter 鍵保留（依字典序）並警告；
frontmatter 裡的註解不保留。

對 `config.yaml`，format 把它重寫為完整文件化範本（每個鍵帶
預設值與註解、可選節註解範例），並填入你的現有值。未知設定鍵
丟棄並警告。

驗證失敗的檔案會報錯並跳過，命令以非零碼結束。重複執行
format 不會改變任何內容（冪等）。

### doc

列印本手冊。

```
burogu doc [SECTION] [--lang LANG] [--color MODE]
```

`SECTION`  章節名（如 `config`、`commands`）；預設：全文
`--lang`   `en`、`zh`、`zh-Hant` 或 `ja`；預設：跟隨系統 locale
           （ja* 語言環境得到日文，zh_TW/zh_HK/zh_MO 得到繁體中文，
           其餘 zh* 得到簡體中文，其他得到英文）
`--color`  `auto`、`always` 或 `never`；預設：auto（輸出到終端機時
           用 ANSI 樣式，被管道/重新導向時輸出純文字）

## CONFIGURATION

`config.yaml` 與 `src/` 同層（`init` 範本解釋了每個鍵）。所有鍵
均可選；預設值會在啟動時以警告形式列印。未知鍵會被忽略。

### site

`siteName`         站台標題；用於頁頭、HTML 標題與 og:site_name
                   （預設：burogu）
`siteAuthor`       作者名；作為預設版權署名（預設：空）
`siteDescription`  站台描述；用於 HTML description meta 標籤
`siteLang`         頁面語言代碼，如 `en` 或 `zh-CN`（預設：zh-CN）
`baseUrl`          站台位址，如 `https://lizi.moe`，必須以
                   http:// 或 https:// 開頭。設定後產生 feed 與
                   og:url meta 標籤
`siteCopyright`    頁尾版權文字（預設：© + siteAuthor）
`siteGeneratedBy`  頁尾版權旁的署名行，如 "Generated with Burogu"；
                   未設定時不輸出

### deploy

`target`         rsync 目標（user@host:/path）。設定後 `deploy`
                 用 rsync 發布（--delete）
`repo`           git 儲存庫位址。設定後 `deploy` 把站台提交到 git
                 分支（GitHub Pages 等）；與 `target` 互斥
`branch`         要發布到的分支（git 模式；與 repo 搭配必填）
`commitName`     提交身分：名字（git 模式；必填）
`commitEmail`    提交身分：信箱（git 模式；必填）

### theme

`preset`     內建主題：`aria` 或 `shaft`（預設：aria）
`math`       數學轉譯：`none`、`mathjax` 或 `katex`
             （預設：mathjax）
`mathUrl`    數學腳本的 CDN 位址；缺省為所選方式的 pandoc 預設值
`extraCss`   `src/` 下的檔案清單，附加到產生的樣式表末尾（可覆蓋
             產生規則）
`extraJs`    `src/` 下的檔案清單，作為延遲腳本注入每個頁面
             （檔案缺失會中止建置）

#### fonts

覆蓋預設的排版；每個鍵均可選，缺省回退到預設預設值：

`body`         正文字型堆疊（名稱清單；shaft 預設用襯線堆疊）
`display`      標題/日期/年份的展示字型堆疊（名稱清單；shaft 預設
               用襯線堆疊）
`code`         程式碼字型堆疊（名稱清單；預設：跨平台等寬堆疊）
`size`         基礎字級，如 `17px`
`lineHeight`   基礎行高，如 `28px`
`files`        內嵌字型檔案（見下）

含空格的字型名會自動加引號；通用關鍵字 `serif`、
`sans-serif`、`monospace` 與 `system-ui`/`ui-*` 系列原樣輸出。

`files` 條目（每個：`src`、`family`，可選 `weight`、`style`）把
`src/` 下的字型檔案拷到 `site/fonts/` 並產生 @font-face 規則；在
字型堆疊裡用 family 名引用即可：

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

字型檔案缺失是建置錯誤（保留舊輸出）。

### srcRepo

`srcRepo`  `sync` 的預設 git 儲存庫位址（見 SYNC 節）

## SITE LAYOUT

```
config.yaml          站台設定
src/
  _post/             文章（YYYY-MM-DD-slug.md）
  _pages/            頁面（slug.md）
  其餘所有檔案       原樣複製到站台
site/                建置輸出（每次建置重新產生）
```

### Posts 文章

文章位於 `src/_post/*.md`，帶 YAML frontmatter：

`title`        文章標題（預設：slug）
`date`         發布日期，`YYYY-MM-DD`（預設：取檔案名稱前綴
               `YYYY-MM-DD-`）
`tags`         標籤清單，如 `[essay, review]`
`description`  摘要（用於首頁清單、feed 與 og:description）
`draft`        `true` 隱藏文章，並允許缺省日期
`toc`          `true` 為文章轉譯目錄

文章 URL 為 `/posts/slug/`。標籤連結到標籤歸檔；相同標籤必須使用
相同拼寫（僅大小寫不同的標籤會列印警告）。數學轉譯（
`theme.math`）依文章自動偵測。meta 行顯示估算閱讀時間，頁尾有
上一篇/下一篇連結。

### Pages 頁面

頁面位於 `src/_pages/*.md`；每個成為 `/slug/`，並出現在導覽列中：

`title`            導覽標籤與頁面標題
`priority`         導覽位置，小的在前（預設：100）
`hiddenInNavbar`   `true` 讓頁面不出現在導覽列裡（仍可透過 /slug/
                   存取）
`redirectAs`       重新導向本頁：宣告特殊頁，或指向任意位址（見下）

設定 `redirectAs` 後，本頁變為重新導向 stub：slug URL 提供指向
目標的即時 meta-refresh 頁面（帶 rel=canonical 連結），markdown
本文不再使用。目標必須以 `/`（站內路徑）或 `http(s)://`（外部
URL）開頭，其餘格式是建置錯誤；目標等於本頁自己的 slug URL 時
被忽略。

特殊頁（`redirectAs` 為 `/tags/`、`/archive/`、`/search/`、
`/404.html`、`/` 之一）獲得產生的內容而非 markdown 本文：

- `/tags/` - 標籤索引（每個標籤帶歸檔連結）
- `/archive/` - 全部文章依年份分組
- `/search/` - 客戶端搜尋頁（見 SEARCH 節）
- `/404.html` - 預覽伺服器（以及靜態託管）對缺失頁面提供的內容；
  **它的本文就是你的 markdown**
- `/` - 首頁：內容轉譯在文章清單之前（清單帶小節標題）；不進導覽列

所有頁面類型（普通頁、重新導向 stub、特殊頁，404 也不例外）預設
都出現在導覽列裡；`hiddenInNavbar: true` 可隱藏其中任意一個。

### Scripts 腳本

頁面可以用腳本產生，取代 markdown。在 frontmatter 加入 `script:`
欄位；該檔案（相對 `src/_scripts/`）在建置時求值，其字串結果就是
頁面內文（raw HTML）。markdown 內文被忽略。

`script`   腳本檔案，位於 `src/_scripts/`，如 `hello.d`

腳本求值時注入站台上下文：

`site`     站台設定：siteName、siteAuthor、siteDescription、
           siteLang、siteCopyright、baseUrl、siteGeneratedBy
`posts`    全部文章：title、date、tags、url、draft、text、
           description
`pages`    自訂頁面：slug、url、title、redirectAs
`tags`     全部標籤及其文章數：name、count
`config`   原始設定：theme（preset、math、mathUrl、extraCss、
           extraJs）、srcRepo

`puts(...)` 在建置時把參數印到 stderr。腳本的任何錯誤（語法或
執行期）都像其他頁面錯誤一樣使建置失敗，輸出目錄保持不變。

#### 腳本語言

一門微型 Ruby 風格、動態型別、純計算的語言。函式呼叫只有一種
寫法：`f(a, b)`；單獨的 `f` 只是函式值。一切皆運算式；程式、
`def` 本體和 lambda 本體都是相鄰運算式序列，序列的值是最後一個。

字面值：`42`、`1.5`、`"text #{expr}"`（插值，可巢狀字串）、
`true`、`false`、`nil`、`[1, 2]`、`{"a" => 1}`。lambda：
`{ x, y -> expr ... }`。定義：`def f(a, b) expr ... end`
（頂層 `def` 彼此可見，與順序無關）。條件：`if cond then expr
else expr end`（`else` 分支可省）。運算子：`+ - * / % == != <
> <= >= && || !`、一元負號。只有 `false` 和 `nil` 為假。沒有
迴圈也沒有指派；用遞迴和 `map`/`filter`。

內建函式：

    len(x)          字串/陣列/映射的長度
    at(x, i)        陣列或字串下標 i 的元素
    get(m, k)       映射中鍵 k 的值（缺失回傳 nil）
    append(a, v)    陣列 a 追加 v 的副本
    concat(a, b)    陣列或字串串接
    join(a, sep)    陣列拼成字串
    split(s, sep)   字串依分隔符拆成陣列
    map(a, f)       對每個元素套用 f 的陣列
    filter(a, f)    f 為真的元素組成的陣列
    sort(a)         數字或字串陣列排序
    reverse(a)      反轉陣列
    first(a)        第一個元素（空回傳 nil）
    last(a)         最後一個元素（空回傳 nil）
    keys(m)         映射的鍵
    values(m)       映射的值
    contains(a, x)  子字串或元素包含判斷
    trim(s)         去掉前後空白的字串
    lower(s)        小寫字串
    upper(s)        大寫字串
    replace(s, f, t) 字串中 f 取代為 t
    take(a, n)      前 n 個元素（或字元）
    drop(a, n)      去掉前 n 個元素（或字元）
    toStr(v)        數字/布林/nil/字串轉字串
    puts(...)       印出參數到 stderr（回傳 nil）

#### 範例

    # src/_scripts/hello.d
    "<h2>Hello #{get(site, "siteName")}!</h2>"
      + join(map(posts, { p -> "<li>" + get(p, "title") + "</li>" }), "")

    # src/_pages/hello.md
    ---
    title: Hello
    script: hello.d
    ---

### 靜態檔案與內建產物

`src/` 下其餘所有檔案（圖片、CNAME、favicon、字型……）原樣複製
到站台。產生器自身會寫 `style.css`、`robots.txt`、`sitemap.xml`、
feed `feed.xml`（僅在設定 baseUrl 時）與 `search.json`（僅在宣告
搜尋頁時）。與內建產物同名的檔案（如 `src/style.css`）會覆蓋它。

## SEARCH

宣告 `redirectAs: /search/` 的頁面即可啟用全站客戶端搜尋。搜尋
頁是一個文字輸入框加一個結果清單；輸入時以大小寫不敏感的子字串
比對過濾全站（文章與頁面），命中處高亮。`search.json`（宣告搜尋
頁時產生）攜帶索引：全文、標題、URL，文章還含日期與標籤。

預設行為可以整體替換：在 `theme.extraJs` 檔案裡定義全域函式
`window.buroguSearch`，內建腳本會把控制權交給它（它收到
`{ url: "/search.json" }`）。樣式可用 `theme.extraCss` 自訂。

## DEPLOYMENT

`burogu deploy` 建置站台並發布。兩種模式在 `config.yaml` 的
`deploy` 節設定：

### rsync

設定 `deploy.target` 後，站台以 `rsync --delete` 鏡像到目標：
`user@host:/var/www/lizi.moe`。適合 VPS 部署。

### git

設定 `deploy.repo` 與 `deploy.branch` 後，站台提交到 git 分支
（GitHub Pages、Gitee Pages……）。`deploy.commitName` 與
`deploy.commitEmail` 標識提交身分，必填。

分支被抓取到持久快取目錄（`~/.cache/burogu-deploy`，遵循
XDG_CACHE_HOME），新建置以帶時間戳的提交訊息提交其上，再以
快進方式推送（絕不強制）。`--clear-cache` 從空快取重新開始。

## SYNC

`sync` 把存放 `config.yaml` 與 `src/` 的目錄視為獨立的 git 儲存庫
（`site/` 輸出被忽略）：

- `burogu sync push` 把全部變更（`config.yaml`、`src/`）提交到本地
  儲存庫並推送到遠端
- `burogu sync pull` 把本地儲存庫重置到遠端分支（遠端為準；本地
  變更被丟棄）

遠端預設取 `config.yaml` 的 `srcRepo`，可用 `REPO` 參數覆蓋。
`push` 在無變更可推時會提示。

## FILES

`config.yaml`       站台設定（與 src/ 同層）
`src/`              文章、頁面與靜態檔案
`site/`             建置輸出（每次重建；可安全刪除）
`~/.cache/burogu-deploy/`  git 部署的持久快取

## EXIT STATUS

成功回傳 0。任何錯誤回傳非零，包括無效的設定或 frontmatter、
未知的主題預設、缺失的附加檔案、失敗的部署。

## SEE ALSO

`burogu --help`、README、以及 `burogu init` 產生的 `config.yaml`
範本中的註解。

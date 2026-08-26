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

burogu - 静态博客生成器

## SYNOPSIS

**burogu** COMMAND [OPTION]...

命令：

`build`      从 src/ 构建 site/
`clean`      删除输出目录
`preview`    构建一次，然后在本地提供站点服务
`watch`      源文件变化时重新构建，可选附带服务
`init`       初始化 src/ 目录树
`new`        创建一篇正式文章（今天日期）
`draft`      创建一篇草稿（不发布）
`publish`    发布草稿
`deploy`     构建并部署站点（rsync 或 git）
`sync`       将站点仓库与 git 远端同步
`format`     规范化 config.yaml、文章与页面
`doc`        打印本手册

运行 `burogu --help` 查看全部选项，运行 `burogu doc SECTION`（见
下方 COMMANDS 节）查看手册的单个章节。

## DESCRIPTION

burogu 把纯文本的站点变成一个静态网站：从 `src/` 读取文章与页面，
以共享布局渲染成 HTML，并把成品写到 `site/`，部署到任何地方（
VPS、GitHub Pages、任意静态托管）。

- 文章是有日期、可打标签的条目；页面是独立的文档，出现在导航里
- 每个页面同时带浅色与暗色两套配色，访客的系统偏好决定用哪套；
  页脚按钮可手动覆盖（按访客持久化）
- `preview` 与 `watch --serve` 内置本地 HTTP 服务器，方便即时迭代
- 内置站内搜索页（见 SEARCH 节）；声明后自动生成年度归档、标签
  索引与 404 页（见 SITE LAYOUT 节）
- 内置主题由 `theme.preset` 选择：`aria`（默认，极简平面）与
  `shaft`（编辑印刷感，单一红色锚点）。字体栈可按站覆盖或内嵌
  （见 CONFIGURATION 节）

## COMMANDS

### build

构建站点：渲染所有文章与页面、拷贝静态文件、写出输出目录。

```
burogu build [--config PATH] [--src DIR] [--out DIR]
```

`--config`  配置文件（默认：config.yaml）
`--src`     源目录；文章位于 DIR/_post（默认：src）
`--out`     输出目录（默认：site）

每次构建都会从零重建输出目录。若任何页面或文章校验失败，构建
会在触碰输出目录之前停止，旧的站点原样保留。

示例：

```
burogu build --out /var/www/lizi.moe
```

### clean

删除输出目录。

```
burogu clean [--out DIR]
```

### preview

构建一次，然后用内置 HTTP 服务器提供站点。

```
burogu preview [--port PORT]
```

`--port`  监听端口（默认：8000）

服务器只监听 127.0.0.1，以正确的 Content-Type 提供生成的页面；
缺失的页面回退到 `404.html`（站点没有 404 页时返回纯文本）。用
浏览器打开 http://127.0.0.1:8000/ 查看效果。Ctrl-C 停止。

### watch

`src/` 或 `config.yaml` 一旦变化就重新构建，直到被中断。

```
burogu watch [--serve PORT]
```

`--serve`  同时在指定端口提供站点

源目录被轮询检测；任何变化都会触发重建（失败的构建保留旧输出）。
配合 `--serve`，改动无需重启服务器即可生效。

### init

创建带示例文章、起步主题和 `config.yaml` 模板的 `src/` 目录树
（模板里每个配置项都带注释）。

```
burogu init [DIR]
```

`DIR`  目标目录（默认：src）

### new

创建一篇今天日期的正式文章。

```
burogu new SLUG
```

`SLUG`  文章 slug，用于文件名与 URL；不允许 / ? # % 与空格

文章命名为 `YYYY-MM-DD-SLUG.md`，带 `date:` frontmatter。slug
已存在（无论是文章还是草稿）时拒绝创建。

### draft

创建一篇草稿（不发布）。

```
burogu draft SLUG
```

`SLUG`  文章 slug，用于文件名与 URL；不允许 / ? # % 与空格

草稿命名为 `YYYY-MM-DD-SLUG.md`（今天的日期），带 `draft: true`
且无 date 字段。文件名里的日期只是创建日期；发布时的日期以
frontmatter 为准。

### publish

发布草稿：补上日期、去掉草稿标记，原地完成。

```
burogu publish SLUG
```

`SLUG`  文章 slug

日期取 frontmatter 已有的 `date:`，没有则用今天；`draft: true`
变为 `draft: false` 并规范化 frontmatter。文件名不变。slug 不存在
或文件不是草稿时报错。

### deploy

构建站点，然后按 `config.yaml` 的 `deploy` 节发布（见
DEPLOYMENT 节）。

```
burogu deploy [--clear-cache]
```

`--clear-cache`  清空持久 git 缓存（仅 git 模式；下次部署会从零
                 重新抓取）

### sync

把站点仓库（存放 `config.yaml` 与 `src/` 的目录）与 git 远端同步
（见 SYNC 节）。

```
burogu sync ACTION [REPO]
```

`ACTION`  push 或 pull
`REPO`    git 仓库地址（默认：config.yaml 的 `srcRepo`）

### format

规范化 `config.yaml`、全部文章与页面：把缺失的 frontmatter 字段
补上默认值、按规范顺序排列键、原地重写文件。

```
burogu format [--dry-run] [--config PATH] [--src DIR]
```

`--dry-run`  只展示将写内容，不落盘

文章得到 `title, date, tags, description, draft, toc`（frontmatter 没有
日期时从文件名前缀取；草稿可省略日期）。页面得到 `title,
priority, placement, redirectAs`。空的 `description`/
`redirectAs` 不写入。未知 frontmatter 键保留（按字典序）并警告；
frontmatter 里的注释不保留。

对 `config.yaml`，format 把它重写为完整文档化模板（每个键带
默认值与注释、可选节注释示例），并填入你的现有值。未知配置键
丢弃并警告。

校验失败的文件会报错并跳过，命令以非零码结束。重复运行
format 不会改变任何内容（幂等）。

### doc

打印本手册。

```
burogu doc [SECTION] [--lang LANG] [--color MODE]
```

`SECTION`  章节名（如 `config`、`commands`）；默认：全文
`--lang`   `en`、`zh`、`zh-Hant` 或 `ja`；默认：跟随系统 locale
           （ja* 语言环境得到日文，zh_TW/zh_HK/zh_MO 得到繁体中文，
           其余 zh* 得到简体中文，其他得到英文）
`--color`  `auto`、`always` 或 `never`；默认：auto（输出到终端时
           用 ANSI 样式，被管道/重定向时输出纯文本）

## CONFIGURATION

`config.yaml` 与 `src/` 同级（`init` 模板解释了每个键）。所有键
均可选；缺省值会在启动时以警告形式打印。未知键会被忽略。

### site

`siteName`         站点标题；用于页头、HTML 标题与 og:site_name
                   （默认：burogu）
`siteAuthor`       作者名；作为默认版权署名（默认：空）
`siteDescription`  站点描述；用于 HTML description meta 标签
`siteLang`         页面语言代码，如 `en` 或 `zh-CN`（默认：zh-CN）
`baseUrl`          站点地址，如 `https://lizi.moe`，必须以
                   http:// 或 https:// 开头。设置后生成 feed 与
                   og:url meta 标签
`siteCopyright`    页脚版权文本（默认：© + siteAuthor）
`siteGeneratedBy`  页脚版权旁的署名行，如 "Generated with Burogu"；
                   未设置时不输出

### deploy

`target`         rsync 目标（user@host:/path）。设置后 `deploy`
                 用 rsync 发布（--delete）
`repo`           git 仓库地址。设置后 `deploy` 把站点提交到 git
                 分支（GitHub Pages 等）；与 `target` 互斥
`branch`         要发布到的分支（git 模式；与 repo 搭配必填）
`commitName`     提交身份：名字（git 模式；必填）
`commitEmail`    提交身份：邮箱（git 模式；必填）

### theme

`preset`     内置主题：`aria` 或 `shaft`（默认：aria）
`math`       数学渲染：`none`、`mathjax` 或 `katex`
             （默认：mathjax）
`mathUrl`    数学脚本的 CDN 地址；缺省为所选方式的 pandoc 默认值
`extraCss`   `src/` 下的文件列表，追加到生成的样式表末尾（可覆盖
             生成规则）
`extraJs`    `src/` 下的文件列表，作为延迟脚本注入每个页面
             （文件缺失会中止构建）

#### fonts

覆盖预设的排版；每个键均可选，缺省回退到预设默认值：

`body`         正文字体栈（名称列表；shaft 默认用衬线栈）
`display`      标题/日期/年份的展示字体栈（名称列表；shaft 默认用衬线栈）
`code`         代码字体栈（名称列表；默认：跨平台等宽栈）
`size`         基础字号，如 `17px`
`lineHeight`   基础行高，如 `28px`
`files`        内嵌字体文件（见下）

含空格的字体名会自动加引号；通用关键字 `serif`、
`sans-serif`、`monospace` 与 `system-ui`/`ui-*` 系列原样输出。

`files` 条目（每个：`src`、`family`，可选 `weight`、`style`）把
`src/` 下的字体文件拷到 `site/fonts/` 并生成 @font-face 规则；在
字体栈里用 family 名引用即可：

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

字体文件缺失是构建错误（保留旧输出）。

### srcRepo

`srcRepo`  `sync` 的默认 git 仓库地址（见 SYNC 节）

## SITE LAYOUT

```
config.yaml          站点配置
src/
  _post/             文章（YYYY-MM-DD-slug.md）
  _pages/            页面（slug.md）
  其余所有文件       原样拷贝到站点
site/                构建输出（每次构建重新生成）
```

### Posts 文章

文章位于 `src/_post/*.md`，带 YAML frontmatter：

`title`        文章标题（默认：slug）
`date`         发布日期，`YYYY-MM-DD`（默认：取文件名前缀
               `YYYY-MM-DD-`）
`tags`         标签列表，如 `[essay, review]`
`description`  摘要（用于首页列表、feed 与 og:description）
`draft`        `true` 隐藏文章，并允许缺省日期
`toc`          `true` 为文章渲染目录

文章 URL 为 `/posts/slug/`。标签链接到标签归档；相同标签必须使用
相同拼写（仅大小写不同的标签会打印警告）。数学渲染（
`theme.math`）按文章自动检测。meta 行显示估算阅读时间，页脚有
上一篇/下一篇链接。

上一篇/下一篇链接。

可以给文本标注语言字形，让浏览器选用地区正确的字形（如「骨」在
简/繁/日字形不同）：

    [日本語の骨]{lang=ja}               行内标记
    ::: {lang=zh-TW}
    繁體段落
    :::

`ja` 与 `zh-Hant`/`zh-TW` 的字体跟随主题（`--font-ja`、`--font-hant`，
可用 `theme.fonts.ja`/`theme.fonts.hant` 覆盖）。

### Pages 页面

页面位于 `src/_pages/*.md`；每个成为 `/slug/`，并出现在导航中：

`title`            导航标签与页面标题
`priority`         导航位置，小的在前（默认：100；负数可置顶）
`placement`       页面链接出现的位置：`nav`（默认，头部导航）、
                   `footer`（footer 链接行）或 `none`（都不出现；
                   页面仍可通过 /slug/ 访问）
`redirectAs`       重定向本页：声明特殊页，或指向任意地址（见下）

设置 `redirectAs` 后，本页变为重定向 stub：slug URL 提供指向目标
的即时 meta-refresh 页面（带 rel=canonical 链接），markdown 正文
不再使用。目标必须以 `/`（站内路径）或 `http(s)://`（外部 URL）
开头，其余格式是构建错误；目标等于本页自己的 slug URL 时被忽略。

特殊页（`redirectAs` 为 `/tags/`、`/archive/`、`/search/`、
`/404.html`、`/` 之一）获得生成的内容而非 markdown 正文：

- `/tags/` - 标签索引（每个标签带归档链接）
- `/archive/` - 全部文章按年份分组
- `/search/` - 客户端搜索页（见 SEARCH 节）
- `/404.html` - 预览服务器（以及静态托管）对缺失页面提供的内容；
  **它的正文就是你的 markdown**
- `/` - 首页：内容渲染在文章列表之前（列表带小节标题）；不进导航

所有页面类型（普通页、重定向 stub、特殊页，404 也不例外）默认
都出现在导航里；`placement: none` 或 `placement: footer` 可从头部
导航隐藏任意一个。footer 链接渲染在版权行上方，以 `footerSeparator`
（config，空字符串禁用）分隔；`redirectAs` 为 http(s) URL 的
footer 页面直链目标。旧 `hiddenInNavbar` 键在构建时被拒绝；
运行 `burogu format` 迁移。

### Scripts 脚本

页面可以用脚本生成，替代 markdown。在 frontmatter 里加 `script:`
字段；该文件（相对 `src/_scripts/`）在构建时求值，其字符串结果
就是页面正文（raw HTML）。markdown 正文被忽略。

`script`   脚本文件，位于 `src/_scripts/`，如 `hello.d`
`output`   站点根目录内的相对路径：脚本结果写入该文件，不再生
           成页面（如 `data.json`）；页面本体不渲染、不进导航。
           需要 `script`；不能与 `redirectAs` 组合；路径重复是错误

脚本求值时注入站点上下文：

`site`     站点配置：siteName、siteAuthor、siteDescription、
           siteLang、siteCopyright、baseUrl、siteGeneratedBy
`posts`    全部文章：title、date、tags、url、draft、text、
           description
`pages`    自定义页面：slug、url、title、redirectAs
`tags`     全部标签及其文章数：name、count
`config`   原始配置：theme（preset、math、mathUrl、extraCss、
           extraJs）、srcRepo
`data`     用户数据文件：`src/_data/` 下每个 YAML 文件（文件名去
           `.yaml` → 内容）；其他扩展名被忽略

脚本产出一个字符串；去向由页面声明决定：只有 `script:` 时填入
页面正文（套用常规 layout）；`script:` + `output:` 时写入站点
指定路径（无 layout）。输出文件最后写入，因此脚本可以覆盖生成器
或静态文件写出的任何内容——包括 `index.html`（完全自定义首页）
和文章页。

`puts(...)` 在构建时把参数打印到 stderr。脚本的任何错误（语法或
运行时）都像其他页面错误一样使构建失败，输出目录保持不变。

#### 脚本语言

一门微型 Ruby 风格、动态类型、纯计算的语言。函数调用只有一种
写法：`f(a, b)`；单独的 `f` 只是函数值。一切皆表达式；程序、
`def` 体和 lambda 体都是相邻表达式序列，序列的值是最后一个。

字面量：`42`、`1.5`、`"text #{expr}"`（插值，可嵌套字符串）、
`true`、`false`、`nil`、`[1, 2]`、`{"a" => 1}`。lambda：
`{ x, y -> expr ... }`。定义：`def f(a, b) expr ... end`
（顶层 `def` 互相可见，与顺序无关）。条件：`if cond then expr
else expr end`（`else` 分支可省）。运算符：`+ - * / % == != <
> <= >= && || !`、一元负号。只有 `false` 和 `nil` 为假。没有
循环也没有赋值；用递归和 `map`/`filter`。

内置函数：

    len(x)          字符串/数组/映射的长度
    at(x, i)        数组或字符串下标 i 的元素
    get(m, k)       映射中键 k 的值（缺失返回 nil）
    append(a, v)    数组 a 追加 v 的副本
    concat(a, b)    数组或字符串拼接
    join(a, sep)    数组拼成字符串
    split(s, sep)   字符串按分隔符拆成数组
    map(a, f)       对每个元素应用 f 的数组
    filter(a, f)    f 为真的元素组成的数组
    sort(a)         数字或字符串数组排序
    reverse(a)      反转数组
    first(a)        第一个元素（空返回 nil）
    last(a)         最后一个元素（空返回 nil）
    keys(m)         映射的键
    values(m)       映射的值
    contains(a, x)  子串或元素包含判断
    trim(s)         去掉首尾空白的字符串
    lower(s)        小写字符串
    upper(s)        大写字符串
    replace(s, f, t) 字符串中 f 替换为 t
    take(a, n)      前 n 个元素（或字符）
    drop(a, n)      去掉前 n 个元素（或字符）
    toStr(v)        数字/布尔/nil/字符串转字符串
    toJson(v)       值转 pretty JSON（nil 变 null）
    formatDate(d, f)  strftime 风格日期格式化（ISO 日期）

指令：%Y %y %m %d %b %B %a %A %%；%-m/%-d 去补零；未知指令报错。
例：formatDate(date, "%Y年%-m月%-d日") → 2026年8月2日
    formatDate(d, f)  strftime 风格日期格式化（ISO 日期）

HTML helper（内容原样插入；文本用 `esc` 转义）：
    el(name, attrs, content)  任意 tag；attrs：true=裸属性名，
                              false/nil=省略，键值转义
    esc(s)                    转义 & < > " '
    h1 h2 p div span strong em time ul ol li  仅内容 tag
    a(content, href)          href 转义的链接
    img(src, alt)             空元素图片；alt 传 nil 省略
    空元素（br、hr、img、input、meta、link、source）不输出闭合标签。
    puts(...)       打印参数到 stderr（返回 nil）

#### 示例

    # src/_scripts/hello.d
    "<h2>Hello #{get(site, "siteName")}!</h2>"
      + join(map(posts, { p -> "<li>" + get(p, "title") + "</li>" }), "")

    # src/_pages/hello.md
    ---
    title: Hello
    script: hello.d
    ---

### 静态文件与内置产物

`src/` 下其余所有文件（图片、CNAME、favicon、字体……）原样拷贝
到站点。生成器自身会写 `style.css`、`robots.txt`、`sitemap.xml`、
feed `feed.xml`（仅在设置 baseUrl 时）与 `search.json`（仅在声明
搜索页时）。与内置产物同名的文件（如 `src/style.css`）会覆盖它。

## SEARCH

声明 `redirectAs: /search/` 的页面即可启用全站客户端搜索。搜索
页是一个文本输入框加一个结果列表；输入时以大小写不敏感的子串
匹配过滤全站（文章与页面），命中处高亮。`search.json`（声明搜索
页时生成）携带索引：全文、标题、URL，文章还含日期与标签。

默认行为可以整体替换：在 `theme.extraJs` 文件里定义全局函数
`window.buroguSearch`，内置脚本会把控制权交给它（它收到
`{ url: "/search.json" }`）。样式可用 `theme.extraCss` 定制。

## DEPLOYMENT

`burogu deploy` 构建站点并发布。两种模式在 `config.yaml` 的
`deploy` 节配置：

### rsync

设置 `deploy.target` 后，站点以 `rsync --delete` 镜像到目标：
`user@host:/var/www/lizi.moe`。适合 VPS 部署。

### git

设置 `deploy.repo` 与 `deploy.branch` 后，站点提交到 git 分支
（GitHub Pages、Gitee Pages……）。`deploy.commitName` 与
`deploy.commitEmail` 标识提交身份，必填。

分支被抓取到持久缓存目录（`~/.cache/burogu-deploy`，遵循
XDG_CACHE_HOME），新构建以带时间戳的提交信息提交其上，再以
快进方式推送（绝不强制）。`--clear-cache` 从空缓存重新开始。

## SYNC

`sync` 把存放 `config.yaml` 与 `src/` 的目录视为独立的 git 仓库
（`site/` 输出被忽略）：

- `burogu sync push` 把全部改动（`config.yaml`、`src/`）提交到本地
  仓库并推送到远端
- `burogu sync pull` 把本地仓库重置到远端分支（远端为准；本地改动
  被丢弃）

远端默认取 `config.yaml` 的 `srcRepo`，可用 `REPO` 参数覆盖。
`push` 在无改动可推时会提示。

## FILES

`config.yaml`       站点配置（与 src/ 同级）
`src/`              文章、页面与静态文件
`site/`             构建输出（每次重建；可安全删除）
`~/.cache/burogu-deploy/`  git 部署的持久缓存

## EXIT STATUS

成功返回 0。任何错误返回非零，包括无效的配置或 frontmatter、
未知的主题预设、缺失的附加文件、失败的部署。

## SEE ALSO

`burogu --help`、README、以及 `burogu init` 生成的 `config.yaml`
模板中的注释。

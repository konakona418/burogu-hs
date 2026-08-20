document.addEventListener("DOMContentLoaded", function () {
 var langNames = {
  haskell: "Haskell", c: "C", "c++": "C++", cpp: "C++", cxx: "C++",
  python: "Python", javascript: "JavaScript", js: "JavaScript",
  typescript: "TypeScript", ts: "TypeScript", rust: "Rust", go: "Go",
  java: "Java", kotlin: "Kotlin", swift: "Swift", ruby: "Ruby", php: "PHP",
  perl: "Perl", lua: "Lua", r: "R", julia: "Julia", scala: "Scala",
  elixir: "Elixir", erlang: "Erlang", clojure: "Clojure", lisp: "Lisp",
  ocaml: "OCaml", "f#": "F#", fsharp: "F#", "c#": "C#", cs: "C#",
  shell: "Shell", sh: "Shell", bash: "Shell", zsh: "Shell",
  sql: "SQL", html: "HTML", css: "CSS", xml: "XML", json: "JSON",
  yaml: "YAML", toml: "TOML", ini: "INI", markdown: "Markdown", md: "Markdown",
  text: "Text", diff: "Diff", makefile: "Makefile", dockerfile: "Dockerfile",
  cmake: "CMake", nix: "Nix", latex: "LaTeX", matlab: "MATLAB",
  pascal: "Pascal", fortran: "Fortran", ada: "Ada", assembly: "Assembly",
  asm: "Assembly", groovy: "Groovy", dart: "Dart", prolog: "Prolog",
  scheme: "Scheme", zig: "Zig", nim: "Nim"
 };
 var svgAttrs = 'xmlns="http://www.w3.org/2000/svg" width="1em" height="1em" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"';
 var copySvg = '<svg ' + svgAttrs + '><rect x="9" y="9" width="13" height="13" rx="2" ry="2"/><path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"/></svg>';
 var checkSvg = '<svg ' + svgAttrs + '><polyline points="20 6 9 17 4 12"/></svg>';
 var xSvg = '<svg ' + svgAttrs + '><line x1="18" y1="6" x2="6" y2="18"/><line x1="6" y1="6" x2="18" y2="18"/></svg>';
 var blocks = document.querySelectorAll("pre > code.sourceCode");
 blocks.forEach(function (code) {
  var pre = code.parentElement;
  var wrapper = document.createElement("div");
  wrapper.className = "code-block";
  pre.parentNode.insertBefore(wrapper, pre);
  wrapper.appendChild(pre);
  var lang = null;
  code.className.split(/\s+/).forEach(function (cls) {
   if (cls !== "sourceCode" && lang === null) { lang = cls; }
  });
  if (lang) {
   var label = document.createElement("span");
   label.className = "code-lang";
   label.textContent = langNames[lang] || lang;
   wrapper.appendChild(label);
  }
  var button = document.createElement("button");
  button.className = "copy-button";
  button.type = "button";
  button.setAttribute("aria-label", "Copy code");
  button.innerHTML = copySvg;
  button.addEventListener("click", function () {
   var fail = function () {
    button.innerHTML = xSvg;
    setTimeout(function () { button.innerHTML = copySvg; }, 800);
   };
   if (navigator.clipboard && navigator.clipboard.writeText) {
    navigator.clipboard.writeText(code.textContent).then(function () {
     button.innerHTML = checkSvg;
     setTimeout(function () { button.innerHTML = copySvg; }, 800);
    }, fail);
   } else {
    fail();
   }
  });
  wrapper.appendChild(button);
 });
});

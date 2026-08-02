document.addEventListener("DOMContentLoaded", function () {
 if (window.buroguSearch) {
  window.buroguSearch({ url: "/search.json" });
  return;
 }
 var input = document.querySelector(".search-input");
 var list = document.getElementById("search-results");
 function highlight(node, text, q) {
  var lower = text.toLowerCase();
  var i = lower.indexOf(q);
  if (i === -1) { node.textContent = text; return; }
  if (i > 0) { node.appendChild(document.createTextNode(text.slice(0, i))); }
  var mark = document.createElement("mark");
  mark.textContent = text.slice(i, i + q.length);
  node.appendChild(mark);
  var after = text.slice(i + q.length);
  if (after) { node.appendChild(document.createTextNode(after)); }
 }
 function snippetAround(text, q) {
  var lower = text.toLowerCase();
  var i = lower.indexOf(q);
  if (i === -1) { return text.slice(0, 80); }
  var start = Math.max(0, i - 20);
  var end = Math.min(text.length, i + q.length + 60);
  return (start > 0 ? "…" : "") + text.slice(start, end);
 }
 fetch("/search.json").then(function (r) { return r.json(); }).then(function (index) {
  input.addEventListener("input", function () {
   var q = input.value.trim().toLowerCase();
   while (list.firstChild) { list.removeChild(list.firstChild); }
   if (!q) { return; }
   index.forEach(function (entry) {
    var hay = (entry.title + " " + entry.text).toLowerCase();
    if (hay.indexOf(q) === -1) { return; }
    var li = document.createElement("li");
    li.className = "post-item";
    if (entry.date) {
     var time = document.createElement("time");
     time.className = "post-date";
     time.textContent = entry.date;
     li.appendChild(time);
    }
    var a = document.createElement("a");
    a.href = entry.url;
    highlight(a, entry.title, q);
    li.appendChild(a);
    var p = document.createElement("p");
    p.className = "post-desc";
    highlight(p, snippetAround(entry.text, q), q);
    li.appendChild(p);
    list.appendChild(li);
   });
  });
 });
});

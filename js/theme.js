document.addEventListener("DOMContentLoaded", function () {
 var stored = localStorage.getItem("burogu-theme");
 var button = document.createElement("button");
 button.className = "theme-toggle";
 button.type = "button";
 button.setAttribute("aria-label", "Toggle dark mode");
 function apply(theme) {
  if (theme === "dark" || theme === "light") {
   document.documentElement.setAttribute("data-theme", theme);
  } else {
   document.documentElement.removeAttribute("data-theme");
  }
  button.textContent = theme === "dark" ? "☀" : "☾";
 }
 apply(stored);
 button.addEventListener("click", function () {
  var next = document.documentElement.getAttribute("data-theme") === "dark" ? "light" : "dark";
  localStorage.setItem("burogu-theme", next);
  apply(next);
 });
 var footer = document.querySelector(".site-footer");
 if (footer) { footer.appendChild(button); }
});

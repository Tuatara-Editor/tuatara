// The manual's pages: filter the contents column to the headings holding what
// is typed in its box, change between light and dark colours (the choice kept
// in this browser for the next page), and take the corner link to the box when
// the column is in view. No address is ever loaded.
(function () {
  var root = document.documentElement;
  var key = "tuatara-manual-theme";
  var saved = null;
  try { saved = window.localStorage.getItem(key); } catch (e) { saved = null; }
  if (saved === "light" || saved === "dark") { root.setAttribute("data-theme", saved); }

  function dark() {
    var chosen = root.getAttribute("data-theme");
    if (chosen) { return chosen === "dark"; }
    return Boolean(window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches);
  }

  var button = document.querySelector("button.theme");
  if (button) {
    var label = function () {
      button.textContent = dark() ? "Light" : "Dark";
      button.setAttribute("aria-label", dark() ? "Use light colours" : "Use dark colours");
    };
    button.hidden = false;
    label();
    button.addEventListener("click", function () {
      var next = dark() ? "light" : "dark";
      root.setAttribute("data-theme", next);
      try { window.localStorage.setItem(key, next); } catch (e) { /* kept for this page only */ }
      label();
    });
  }

  var nav = document.querySelector("nav.toc");
  var box = nav && nav.querySelector("input");
  if (!box) { return; }
  box.addEventListener("input", function () {
    var words = box.value.trim().toLowerCase();
    nav.classList.toggle("filtering", words !== "");
    nav.querySelectorAll("li").forEach(function (item) {
      var hit = words === "" || item.textContent.toLowerCase().indexOf(words) !== -1;
      item.classList.toggle("hidden", !hit);
    });
  });

  var corner = document.querySelector("a.to-contents");
  if (corner) {
    corner.addEventListener("click", function (event) {
      if (window.getComputedStyle(nav).position === "sticky") {
        event.preventDefault();
        box.focus();
      }
    });
  }
}());
